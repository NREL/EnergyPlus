// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

// EnergyPlus Headers
#include <BranchInputManager.hh>
#include <BranchNodeConnections.hh>
#include <DXCoils.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataAirflowNetwork.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneControls.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <Furnaces.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <HVACControllers.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <HeatingCoils.hh>
#include <InputProcessing/InputProcessor.hh>
#include <IntegratedHeatPump.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SteamCoils.hh>
#include <WaterCoils.hh>
#include <WaterToAirHeatPump.hh>
#include <WaterToAirHeatPumpSimple.hh>

namespace EnergyPlus {

namespace Furnaces {
    // Module containing the Furnace and Unitary System simulation routines

    // MODULE INFORMATION:
    //       AUTHOR         Dan Fisher
    //       DATE WRITTEN   Jan 2001
    //       MODIFIED       Richard Liesen, Feb 2001; Don Shirey, Mar/Oct 2001, Oct 2003
    //                      Richard Raustad, Nov 2006 - allow draw through fan and alternate air flow in cooling,
    //                      heating, and when no cooling or heating is required.
    //                      Bereket Nigusse, FSEC, June 2010 - deprecated supply air flow fraction through controlled
    //                      zone from the furnace object input field. Now, the flow fraction is calculated internally
    //                      B. Nigusse, FSEC, Jan 2012 - added steam and hot water heating coils as an option
    //                      Bo Shen, ORNL, March 2012 - added variable-speed water source heat pump cooling and heating coils, using curve-fits
    //                      Bo Shen, ORNL, July 2012 - added variable-speed air source heat pump cooling and heating coils, using curve-fits
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms required to
    // manage the Furnace/Unitary System Compound Component

    // METHODOLOGY EMPLOYED:
    // Calculates the part-load ratio of the HVAC system to meet the zone sensible load. For non-heat pump HVAC systems,
    // if humidity control is specified and the latent capacity at the sensible PLR is insufficient to meet the latent load,
    // calculate a latent part-load ratio to meet the zone sensible load (MultiMode dehumidificaiton control) or the zone
    // latent load (CoolReheat dehumidification control). Use the greater of the sensible PLR and latent PLR to control
    // the HVAC system.
    // Subroutines:
    // SimFurnace - Top level simulate routine CALLed by other modules. Each child object is simulated a final time after
    //              the part-load ratio for child components has been determined.
    //  Note: A supplemental heater augments the heating capacity for both air-to-air and water-to-air heat pump systems.
    //        A reheat coil is used for the HeatCool furnace/unitarysystem to offset the sensible cooling when the
    //        dehumidification control type is COOLREHEAT. Both the supplemental and reheat heating coil load is calculated
    //        in the Calc routines. The actual simulation of these coils is performed in the SimFurnace routine (i.e. the
    //        supplemental and reheat coil loads are passed as 0 to CalcFurnaceOutput).
    // CalcNewZoneHeatOnlyFlowRates - HeatOnly furnace/unitarysystem routine.
    //                                Calculates a part-load ratio to meet the sensible load.
    // CalcNewZoneHeatCoolFlowRates - HeatCool furnace/unitarysystem and air-to-air HeatPump routine.
    //                                Calculates a part-load ratio for the system (sensible and/or latent).
    //                                For dehumidification control type COOLREHEAT, both a sensible and latent PLR
    //                                may exist for a single time step (heating and dehumidificaiton can occur). For all
    //                                other system types, only a single PLR is allowed for any given time step.
    //                                Order of simulation depends on dehumidification control option as described below.
    // Dehumidificaiton control options (non-heat pump versions):
    // Dehumidification Control NONE:   Cooling performance is simulated first and then heating performance. If a HX
    //                                  assisted cooling coil is selected, the HX is always active (cooling).
    // Dehumidification Control COOLREHEAT: For cooling operation, the sensible capacity is calculated to
    //                                      meet the thermostat setpoint. If a HX assisted cooling coil is selected,
    //                                      the HX is always active. If the latent load is not met by operating the
    //                                      system at the sensible PLR, a new PLR is calculated to meet the humidistat
    //                                      setpoint. The reheat coil load is then calculated to meet the HEATING
    //                                      setpoint temperature.
    // Dehumidification Control MULTIMODE: For cooling operation, the sensible capacity is calculated to
    //                                     meet the thermostat setpoint. If a HX assisted cooling coil is selected,
    //                                     the HX is off for this calculation. If the latent load is not met by operating
    //                                     the system at the sensible PLR, a new PLR is calculated with the HX operating
    //                                     and the target is the zone SENSIBLE load (thermostat setpoint). Humidity is not
    //                                     controlled in this mode. No reheat coil is used in this configuration.
    // CalcWaterToAirHeatPump - Water-to-air HeatPump routine.
    //                          Calculates a part-load ratio to meet the sensible load.
    // CalcFurnaceOutput - Simulates each child component in order.
    //                     For cooling operation, the heating coil is off.
    //                     For heating operation, the cooling coil is off.
    //                     Reheat or supplemental heating coil is simulated here just to pass the inlet node conditions
    //                     to the output node (actual simulation of these coils is done on the final simulation of the
    //                     child components in SimFurnace).
    //                     Fan is simulated based on placement (drawthru or blowthru).
    // REFERENCES:

    // OTHER NOTES:

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataLoopNode;
    using namespace DataGlobals;
    using namespace DataHVACGlobals;
    using DataEnvironment::OutDryBulbTemp;
    using DataEnvironment::StdBaroPress;
    using DataEnvironment::StdRhoAir;
    using namespace DataZoneEquipment;
    using Psychrometrics::PsyCpAirFnWTdb;
    using Psychrometrics::PsyHfgAirFnWTdb;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using Psychrometrics::PsyTdbFnHW;
    using namespace ScheduleManager;
    using DXCoils::SimDXCoil;
    using Fans::SimulateFanComponents;
    using VariableSpeedCoils::MaxSpedLevels;

    // Data
    // MODULE PARAMETER DEFINITIONS
    static std::string const BlankString;

    // Last mode of operation
    int const CoolingMode(1); // last compressor operating mode was in cooling
    int const HeatingMode(2); // last compressor operating mode was in heating
    int const NoCoolHeat(3);  // last operating mode was coil off
    // Airflow control for contant fan mode
    int const UseCompressorOnFlow(1);  // set compressor OFF air flow rate equal to compressor ON air flow rate
    int const UseCompressorOffFlow(2); // set compressor OFF air flow rate equal to user defined value
    // Compressor operation
    int const On(1);  // normal compressor operation
    int const Off(0); // signal DXCoil that compressor shouldn't run

    // Dehumidification control modes (DehumidControlMode)
    int const DehumidControl_None(0);
    int const DehumidControl_Multimode(1);
    int const DehumidControl_CoolReheat(2);

    static std::string const fluidNameSteam("STEAM");
    bool GetFurnaceInputFlag(true); // Logical to allow "GetInput" only once per simulation

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:
    int NumFurnaces(0); // The number of furnaces found in the input data file
    Array1D_bool MySizeFlag;
    Array1D_bool CheckEquipName;
    Real64 ModifiedHeatCoilLoad(0.0); // used to adjust heating coil capacity if outlet temp > DesignMaxOutletTemp,
    // used for Coil:Gas:Heating and Coil:Electric:Heating coils only.
    Real64 OnOffAirFlowRatioSave(0.0);        // Saves the OnOffAirFlowRatio calculated in RegulaFalsi CALLs.
    Real64 OnOffFanPartLoadFractionSave(0.0); // Global part-load fraction passed to fan object
    Real64 CompOnMassFlow(0.0);               // Supply air mass flow rate w/ compressor ON [kg/s]
    Real64 CompOffMassFlow(0.0);              // Supply air mass flow rate w/ compressor OFF [kg/s]
    Real64 CompOnFlowRatio(0.0);              // fan flow ratio when coil on
    Real64 CompOffFlowRatio(0.0);             // fan flow ratio when coil off
    Real64 FanSpeedRatio(0.0);                // ratio of air flow ratio passed to fan object
    Real64 CoolHeatPLRRat(1.0);               // ratio of cooling to heating PLR, used for cycling fan RH control
    bool HeatingLoad(false);
    bool CoolingLoad(false);
    bool EconomizerFlag(false);             // holds air loop economizer status
    int AirLoopPass(0);                     // Number of air loop pass
    bool HPDehumidificationLoadFlag(false); // true if there is dehumidification load (heat pumps only)
    Real64 TempSteamIn(100.0);              // steam coil steam inlet temperature
    // starting add variables for variable speed water source heat pump
    Real64 SaveCompressorPLR(0.0);   // holds compressor PLR from active DX coil
    std::string CurrentModuleObject; // Object type for getting and error messages
    // ending varibles for variable speed water source heat pump

    // Subroutine Specifications for the Module
    // Driver/Manager Routines

    // Get Input routines for module

    // Initialization routines for module

    // Calculate routines to check convergence

    // Supporting routines for module

    // modules for variable speed heat pump

    // Reporting routines for module

    // Object Data
    Array1D<FurnaceEquipConditions> Furnace;
    std::unordered_map<std::string, std::string> UniqueFurnaceNames;

    // Utility routines for module
    // na

    // MODULE SUBROUTINES:
    //*************************************************************************

    // Functions
    void clear_state()
    {
        NumFurnaces = 0;
        MySizeFlag.deallocate();
        CheckEquipName.deallocate();
        ModifiedHeatCoilLoad = 0.0;
        OnOffAirFlowRatioSave = 0.0;
        OnOffFanPartLoadFractionSave = 0.0;
        CompOnMassFlow = 0.0;
        CompOffMassFlow = 0.0;
        CompOnFlowRatio = 0.0;
        CompOffFlowRatio = 0.0;
        FanSpeedRatio = 0.0;
        CoolHeatPLRRat = 1.0;
        HeatingLoad = false;
        CoolingLoad = false;
        EconomizerFlag = false;
        AirLoopPass = 0;
        HPDehumidificationLoadFlag = false;
        GetFurnaceInputFlag = true;
        TempSteamIn = 100.0;
        SaveCompressorPLR = 0.0;
        CurrentModuleObject = "";
        Furnace.deallocate();
        UniqueFurnaceNames.clear();
    }

    void SimFurnace(std::string const &FurnaceName,
                    bool const FirstHVACIteration,
                    int const AirLoopNum, // Primary air loop number
                    int &CompIndex        // Pointer to which furnace
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Jan 2001
        //       MODIFIED       Richard Liesen, Oct 2001 - Richard Raustad; Bo Shen, March 2012, for VS WSHP
        //       RE-ENGINEERED  Feb 2001

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages Furnace component simulation.

        // METHODOLOGY EMPLOYED:
        // CALL the calc routine to determine an operating PLR. Resimulate child components at this PLR.
        // A supplemental heater augments the heating capacity for both air-to-air and water-to-air heat pump systems.
        // A reheat coil is used for the HeatCool furnace/unitarysystem to offset the sensible cooling when the
        // dehumidification control type is COOLREHEAT. Both the supplemental and reheat heating coil load is calculated
        // in the Calc routines and returned here through subroutine arguments. The actual simulation of these coils is
        // performed here (i.e. the supplemental and reheat coil loads are passed as 0 to CalcFurnaceOutput).

        // Using/Aliasing
        using DataAirLoop::AirLoopControlInfo;
        using DataAirLoop::AirLoopFlow;
        using HeatingCoils::SimulateHeatingCoilComponents;
        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using namespace DataZoneEnergyDemands;
        using DataHeatBalFanSys::TempControlType;
        using General::TrimSigDigits;
        using IntegratedHeatPump::IntegratedHeatPumps;
        using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FurnaceNum;           // Furnace number
        Real64 HeatCoilLoad(0.0); // Zone heating coil load
        Real64 ReheatCoilLoad;    // Load to be met by the reheat coil (if high humidity control)
        Real64 ZoneLoad;          // Control zone sensible load
        Real64 MoistureLoad;      // Control zone latent load
        Real64 H2OHtOfVap;        // Heat of vaporization of air
        int FurnaceInletNode;     // Inlet node to furnace or unitary system
        Real64 FurnaceSavMdot;    // saved furnace inlet air mass flow rate [m3/s]
        static Real64 Dummy(0.0);
        int CompOp;               // compressor operation; 1=on, 0=off
        Real64 OnOffAirFlowRatio; // Ratio of compressor ON air flow to AVERAGE air flow over time step
        int FanOpMode;            // Fan operating mode (1=CycFanCycCoil, 2=ContFanCycCoil)
        bool HXUnitOn;            // flag to control HX assisted cooling coil
        Real64 ZoneLoadToCoolSPSequenced;
        Real64 ZoneLoadToHeatSPSequenced;

        Real64 QActual;           // actual heating coil output (W)
        bool SuppHeatingCoilFlag; // true if supplemental heating coil
        Real64 TempMassFlowRateMaxAvail;

        // Obtains and Allocates Furnace related parameters from input file
        if (GetFurnaceInputFlag) { // First time subroutine has been entered
            // Get the furnace input
            GetFurnaceInput();
            GetFurnaceInputFlag = false;
        }

        // Find the correct Furnace
        if (CompIndex == 0) {
            FurnaceNum = UtilityRoutines::FindItemInList(FurnaceName, Furnace);
            if (FurnaceNum == 0) {
                ShowFatalError("SimFurnace: Unit not found=" + FurnaceName);
            }
            CompIndex = FurnaceNum;
        } else {
            FurnaceNum = CompIndex;
            if (FurnaceNum > NumFurnaces || FurnaceNum < 1) {
                ShowFatalError("SimFurnace:  Invalid CompIndex passed=" + TrimSigDigits(FurnaceNum) +
                               ", Number of Units=" + TrimSigDigits(NumFurnaces) + ", Entered Unit name=" + FurnaceName);
            }
            if (CheckEquipName(FurnaceNum)) {
                if (FurnaceName != Furnace(FurnaceNum).Name) {
                    ShowFatalError("SimFurnace: Invalid CompIndex passed=" + TrimSigDigits(FurnaceNum) + ", Unit name=" + FurnaceName +
                                   ", stored Unit Name for that index=" + Furnace(FurnaceNum).Name);
                }
                CheckEquipName(FurnaceNum) = false;
            }
        }

        HXUnitOn = false;
        OnOffAirFlowRatio = 0.0;
        // here we need to deal with sequenced zone equip
        ZoneLoad = 0.0;
        if (Furnace(FurnaceNum).ZoneSequenceCoolingNum > 0 && Furnace(FurnaceNum).ZoneSequenceHeatingNum > 0) {
            ZoneLoadToCoolSPSequenced = ZoneSysEnergyDemand(Furnace(FurnaceNum).ControlZoneNum)
                                            .SequencedOutputRequiredToCoolingSP(Furnace(FurnaceNum).ZoneSequenceCoolingNum);
            ZoneLoadToHeatSPSequenced = ZoneSysEnergyDemand(Furnace(FurnaceNum).ControlZoneNum)
                                            .SequencedOutputRequiredToHeatingSP(Furnace(FurnaceNum).ZoneSequenceHeatingNum);
            if (ZoneLoadToHeatSPSequenced > 0.0 && ZoneLoadToCoolSPSequenced > 0.0 &&
                TempControlType(Furnace(FurnaceNum).ControlZoneNum) != SingleCoolingSetPoint) {
                ZoneLoad = ZoneLoadToHeatSPSequenced;
            } else if (ZoneLoadToHeatSPSequenced > 0.0 && ZoneLoadToCoolSPSequenced > 0.0 &&
                       TempControlType(Furnace(FurnaceNum).ControlZoneNum) == SingleCoolingSetPoint) {
                ZoneLoad = 0.0;
            } else if (ZoneLoadToHeatSPSequenced < 0.0 && ZoneLoadToCoolSPSequenced < 0.0 &&
                       TempControlType(Furnace(FurnaceNum).ControlZoneNum) != SingleHeatingSetPoint) {
                ZoneLoad = ZoneLoadToCoolSPSequenced;
            } else if (ZoneLoadToHeatSPSequenced < 0.0 && ZoneLoadToCoolSPSequenced < 0.0 &&
                       TempControlType(Furnace(FurnaceNum).ControlZoneNum) == SingleHeatingSetPoint) {
                ZoneLoad = 0.0;
            } else if (ZoneLoadToHeatSPSequenced <= 0.0 && ZoneLoadToCoolSPSequenced >= 0.0) {
                ZoneLoad = 0.0;
            }
            MoistureLoad = ZoneSysMoistureDemand(Furnace(FurnaceNum).ControlZoneNum)
                               .SequencedOutputRequiredToDehumidSP(Furnace(FurnaceNum).ZoneSequenceCoolingNum);
        } else {
            ZoneLoad = ZoneSysEnergyDemand(Furnace(FurnaceNum).ControlZoneNum).RemainingOutputRequired;
            MoistureLoad = ZoneSysMoistureDemand(Furnace(FurnaceNum).ControlZoneNum).OutputRequiredToDehumidifyingSP;
        }

        H2OHtOfVap =
            PsyHfgAirFnWTdb(Node(Furnace(FurnaceNum).NodeNumOfControlledZone).HumRat, Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp);

        MoistureLoad *= H2OHtOfVap;

        // Initialize Furnace Flows
        InitFurnace(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, FanOpMode, ZoneLoad, MoistureLoad, FirstHVACIteration);

        FurnaceInletNode = Furnace(FurnaceNum).FurnaceInletNodeNum;

        // MassFlowRateMaxAvail issues are impeding non-VAV air loop equipment by limiting air flow
        // temporarily open up flow limits while simulating, and then set this same value at the INLET after this parent has simulated
        TempMassFlowRateMaxAvail = Node(FurnaceInletNode).MassFlowRateMaxAvail;
        Node(FurnaceInletNode).MassFlowRateMaxAvail = Furnace(FurnaceNum).DesignMassFlowRate;

        FurnaceSavMdot = Node(FurnaceInletNode).MassFlowRate;
        CompOp = On;
        CoolHeatPLRRat = 1.0;

        // Simulate correct system type (1 of 4 choices)
        {
            auto const SELECT_CASE_var(Furnace(FurnaceNum).FurnaceType_Num);

            // Simulate HeatOnly systems:
            if ((SELECT_CASE_var == Furnace_HeatOnly) || (SELECT_CASE_var == UnitarySys_HeatOnly)) {

                // Update the furnace flow rates
                CalcNewZoneHeatOnlyFlowRates(FurnaceNum, FirstHVACIteration, ZoneLoad, HeatCoilLoad, OnOffAirFlowRatio);

                if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                    // simulate fan
                    SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
                }

                // simulate furnace heating coil
                SuppHeatingCoilFlag = false; // if true simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, FanOpMode, QActual);

                if (Furnace(FurnaceNum).FanPlace == DrawThru) {
                    // simulate fan
                    SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
                }

                // Simulate HeatCool sytems:
            } else if ((SELECT_CASE_var == Furnace_HeatCool) || (SELECT_CASE_var == UnitarySys_HeatCool)) {

                if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
                    // variable speed cooling coil
                    HeatCoilLoad = 0.0;
                    if (Furnace(FurnaceNum).bIsIHP)
                        IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).ControlledZoneTemp =
                            Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp;
                    SimVariableSpeedHP(FurnaceNum, FirstHVACIteration, AirLoopNum, ZoneLoad, MoistureLoad, OnOffAirFlowRatio);
                } else {
                    // calculate the system flow rate
                    if (!FirstHVACIteration && Furnace(FurnaceNum).OpMode == CycFanCycCoil && CoolingLoad &&
                        AirLoopControlInfo(AirLoopNum).EconoActive) {
                        // for cycling fan, cooling load, check whether furnace can meet load with compressor off
                        CompOp = Off;
                        CalcNewZoneHeatCoolFlowRates(FurnaceNum,
                                                     FirstHVACIteration,
                                                     CompOp,
                                                     ZoneLoad,
                                                     MoistureLoad,
                                                     HeatCoilLoad,
                                                     ReheatCoilLoad,
                                                     OnOffAirFlowRatio,
                                                     HXUnitOn);
                        if (Furnace(FurnaceNum).CoolPartLoadRatio >= 1.0 || Furnace(FurnaceNum).HeatPartLoadRatio >= 1.0 ||
                            (Furnace(FurnaceNum).CoolPartLoadRatio <= 0.0 && Furnace(FurnaceNum).HeatPartLoadRatio <= 0.0)) {
                            // compressor on (reset inlet air mass flow rate to starting value)
                            Node(FurnaceInletNode).MassFlowRate = FurnaceSavMdot;
                            CompOp = On;
                            CalcNewZoneHeatCoolFlowRates(FurnaceNum,
                                                         FirstHVACIteration,
                                                         CompOp,
                                                         ZoneLoad,
                                                         MoistureLoad,
                                                         HeatCoilLoad,
                                                         ReheatCoilLoad,
                                                         OnOffAirFlowRatio,
                                                         HXUnitOn);
                        }
                    } else {
                        // compressor on
                        CalcNewZoneHeatCoolFlowRates(FurnaceNum,
                                                     FirstHVACIteration,
                                                     CompOp,
                                                     ZoneLoad,
                                                     MoistureLoad,
                                                     HeatCoilLoad,
                                                     ReheatCoilLoad,
                                                     OnOffAirFlowRatio,
                                                     HXUnitOn);
                    }

                    if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                        // simulate fan
                        SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
                    }

                    if (!Furnace(FurnaceNum).CoolingCoilUpstream) {
                        // simulate furnace heating coil
                        SuppHeatingCoilFlag = false; // if true simulates supplemental heating coil
                        CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, FanOpMode, QActual);
                    }

                    // simulate furnace DX cooling coil
                    if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {
                        SimHXAssistedCoolingCoil(BlankString,
                                                 FirstHVACIteration,
                                                 CompOp,
                                                 Furnace(FurnaceNum).CoolPartLoadRatio,
                                                 Furnace(FurnaceNum).CoolingCoilIndex,
                                                 FanOpMode,
                                                 HXUnitOn,
                                                 OnOffAirFlowRatio,
                                                 EconomizerFlag);
                    } else {
                        SimDXCoil(BlankString,
                                  CompOp,
                                  FirstHVACIteration,
                                  Furnace(FurnaceNum).CoolingCoilIndex,
                                  FanOpMode,
                                  Furnace(FurnaceNum).CoolPartLoadRatio,
                                  OnOffAirFlowRatio,
                                  CoolHeatPLRRat);
                    }

                    if (Furnace(FurnaceNum).CoolingCoilUpstream) {
                        // simulate furnace heating coil
                        SuppHeatingCoilFlag = false; // if true simulates supplemental heating coil
                        CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, FanOpMode, QActual);
                    }

                    if (Furnace(FurnaceNum).FanPlace == DrawThru) {
                        // simulate fan
                        SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
                    }

                    // Simulate furnace reheat coil if a humidistat is used or if the reheat coil is present
                    if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat || Furnace(FurnaceNum).SuppHeatCoilIndex > 0) {
                        SuppHeatingCoilFlag = true; // if truee simulates supplemental heating coil
                        CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, ReheatCoilLoad, FanOpMode, QActual);
                    }
                }

                // Simulate air-to-air heat pumps:
            } else if (SELECT_CASE_var == UnitarySys_HeatPump_AirToAir) {
                if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) {
                    // variable speed heat pump
                    HeatCoilLoad = 0.0;
                    if (Furnace(FurnaceNum).bIsIHP) {
                        IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).ControlledZoneTemp =
                            Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp;
                        IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).IDFanID = Furnace(FurnaceNum).FanIndex;
                        IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).IDFanName = BlankString;
                        IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).IDFanPlace = Furnace(FurnaceNum).FanPlace;
                    }

                    SimVariableSpeedHP(FurnaceNum, FirstHVACIteration, AirLoopNum, ZoneLoad, MoistureLoad, OnOffAirFlowRatio);
                } else {
                    // Update the furnace flow rates
                    if (!FirstHVACIteration && Furnace(FurnaceNum).OpMode == CycFanCycCoil && CoolingLoad &&
                        AirLoopControlInfo(AirLoopNum).EconoActive) {
                        // for cycling fan, cooling load, check whether furnace can meet load with compressor off
                        CompOp = Off;
                        CalcNewZoneHeatCoolFlowRates(FurnaceNum,
                                                     FirstHVACIteration,
                                                     CompOp,
                                                     ZoneLoad,
                                                     MoistureLoad,
                                                     HeatCoilLoad,
                                                     ReheatCoilLoad,
                                                     OnOffAirFlowRatio,
                                                     HXUnitOn);
                        if (Furnace(FurnaceNum).CoolPartLoadRatio >= 1.0 || Furnace(FurnaceNum).HeatPartLoadRatio >= 1.0 ||
                            (Furnace(FurnaceNum).CoolPartLoadRatio <= 0.0 && Furnace(FurnaceNum).HeatPartLoadRatio <= 0.0)) {
                            // compressor on (reset inlet air mass flow rate to starting value)
                            CompOp = On;
                            Node(FurnaceInletNode).MassFlowRate = FurnaceSavMdot;
                            CalcNewZoneHeatCoolFlowRates(FurnaceNum,
                                                         FirstHVACIteration,
                                                         CompOp,
                                                         ZoneLoad,
                                                         MoistureLoad,
                                                         HeatCoilLoad,
                                                         ReheatCoilLoad,
                                                         OnOffAirFlowRatio,
                                                         HXUnitOn);
                        }
                    } else {
                        // compressor on
                        CalcNewZoneHeatCoolFlowRates(FurnaceNum,
                                                     FirstHVACIteration,
                                                     CompOp,
                                                     ZoneLoad,
                                                     MoistureLoad,
                                                     HeatCoilLoad,
                                                     ReheatCoilLoad,
                                                     OnOffAirFlowRatio,
                                                     HXUnitOn);
                    }

                    if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                        SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
                    }

                    if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {
                        SimHXAssistedCoolingCoil(BlankString,
                                                 FirstHVACIteration,
                                                 CompOp,
                                                 Furnace(FurnaceNum).CoolPartLoadRatio,
                                                 Furnace(FurnaceNum).CoolingCoilIndex,
                                                 FanOpMode,
                                                 HXUnitOn,
                                                 OnOffAirFlowRatio,
                                                 EconomizerFlag);
                    } else {
                        SimDXCoil(BlankString,
                                  CompOp,
                                  FirstHVACIteration,
                                  Furnace(FurnaceNum).CoolingCoilIndex,
                                  FanOpMode,
                                  Furnace(FurnaceNum).CoolPartLoadRatio,
                                  OnOffAirFlowRatio);
                    }
                    SimDXCoil(BlankString,
                              CompOp,
                              FirstHVACIteration,
                              Furnace(FurnaceNum).HeatingCoilIndex,
                              FanOpMode,
                              Furnace(FurnaceNum).HeatPartLoadRatio,
                              OnOffAirFlowRatio);
                    if (Furnace(FurnaceNum).FanPlace == DrawThru) {
                        SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
                    }

                    // Simulate furnace reheat coil if a humidistat is present, the dehumidification type of coolreheat and
                    // reheat coil load exists
                    if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat && ReheatCoilLoad > 0.0) {
                        SuppHeatingCoilFlag = true; // if truee simulates supplemental heating coil
                        CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, ReheatCoilLoad, FanOpMode, QActual);
                    } else {
                        SuppHeatingCoilFlag = true; // if true simulates supplemental heating coil
                        CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, FanOpMode, QActual);
                    }
                }
                // Simulate water-to-air systems:
            } else if (SELECT_CASE_var == UnitarySys_HeatPump_WaterToAir) {

                if (Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_Simple) {
                    // Update the furnace flow rates
                    //   When CompOp logic is added to the child cooling coil (COIL:WaterToAirHP:EquationFit:Cooling), then this logic
                    //   needs to be reinstated.. to align with Unitary/Furnace HeatCool and Unitary Air-to-Air Heat Pump (see above).
                    if (!FirstHVACIteration && Furnace(FurnaceNum).OpMode == CycFanCycCoil && CoolingLoad &&
                        AirLoopControlInfo(AirLoopNum).EconoActive) {
                        // for cycling fan, cooling load, check whether furnace can meet load with compressor off
                        CompOp = Off;
                        CalcNewZoneHeatCoolFlowRates(FurnaceNum,
                                                     FirstHVACIteration,
                                                     CompOp,
                                                     ZoneLoad,
                                                     MoistureLoad,
                                                     HeatCoilLoad,
                                                     ReheatCoilLoad,
                                                     OnOffAirFlowRatio,
                                                     HXUnitOn);
                        if (Furnace(FurnaceNum).CoolPartLoadRatio >= 1.0 || Furnace(FurnaceNum).HeatPartLoadRatio >= 1.0 ||
                            (Furnace(FurnaceNum).CoolPartLoadRatio <= 0.0 && Furnace(FurnaceNum).HeatPartLoadRatio <= 0.0)) {
                            // compressor on (reset inlet air mass flow rate to starting value)
                            CompOp = On;
                            Node(FurnaceInletNode).MassFlowRate = FurnaceSavMdot;
                            CalcNewZoneHeatCoolFlowRates(FurnaceNum,
                                                         FirstHVACIteration,
                                                         CompOp,
                                                         ZoneLoad,
                                                         MoistureLoad,
                                                         HeatCoilLoad,
                                                         ReheatCoilLoad,
                                                         OnOffAirFlowRatio,
                                                         HXUnitOn);
                        }
                    } else {
                        // compressor on
                        CalcNewZoneHeatCoolFlowRates(FurnaceNum,
                                                     FirstHVACIteration,
                                                     CompOp,
                                                     ZoneLoad,
                                                     MoistureLoad,
                                                     HeatCoilLoad,
                                                     ReheatCoilLoad,
                                                     OnOffAirFlowRatio,
                                                     HXUnitOn);
                    }
                    if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                        SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
                    }

                    SimWatertoAirHPSimple(BlankString,
                                          Furnace(FurnaceNum).CoolingCoilIndex,
                                          Furnace(FurnaceNum).CoolingCoilSensDemand,
                                          Furnace(FurnaceNum).CoolingCoilLatentDemand,
                                          Furnace(FurnaceNum).OpMode,
                                          Furnace(FurnaceNum).WSHPRuntimeFrac,
                                          Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                          Furnace(FurnaceNum).HPTimeConstant,
                                          Furnace(FurnaceNum).FanDelayTime,
                                          CompOp,
                                          Furnace(FurnaceNum).CoolPartLoadRatio,
                                          FirstHVACIteration);
                    SimWatertoAirHPSimple(BlankString,
                                          Furnace(FurnaceNum).HeatingCoilIndex,
                                          Furnace(FurnaceNum).HeatingCoilSensDemand,
                                          Dummy,
                                          Furnace(FurnaceNum).OpMode,
                                          Furnace(FurnaceNum).WSHPRuntimeFrac,
                                          Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                          Furnace(FurnaceNum).HPTimeConstant,
                                          Furnace(FurnaceNum).FanDelayTime,
                                          CompOp,
                                          Furnace(FurnaceNum).HeatPartLoadRatio,
                                          FirstHVACIteration);

                    if (Furnace(FurnaceNum).FanPlace == DrawThru) {
                        SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
                    }
                    if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat && ReheatCoilLoad > 0.0) {
                        SuppHeatingCoilFlag = true; // if true simulates supplemental heating coil
                        CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, ReheatCoilLoad, FanOpMode, QActual);
                    } else {
                        SuppHeatingCoilFlag = true; // if true simulates supplemental heating coil
                        CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, FanOpMode, QActual);
                    }
                } else if (Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_ParEst) {

                    // simulate the heat pump
                    HeatCoilLoad = 0.0;
                    CalcWaterToAirHeatPump(AirLoopNum, FurnaceNum, FirstHVACIteration, CompOp, ZoneLoad, MoistureLoad);
                } else if (Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_VarSpeedEquationFit) {
                    // simulate the heat pump
                    HeatCoilLoad = 0.0;
                    if (Furnace(FurnaceNum).bIsIHP)
                        IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).ControlledZoneTemp =
                            Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp;
                    SimVariableSpeedHP(FurnaceNum, FirstHVACIteration, AirLoopNum, ZoneLoad, MoistureLoad, OnOffAirFlowRatio);

                } else if (Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_VarSpeedLooUpTable) {
                    HeatCoilLoad = 0.0; // Added: Used below
                } else {
                    assert(false); //? If all possible states covered by if conditions change to HeatCoilLoad = 0.0;
                }

            } else {
                // will never get here, all system types are simulated above
                assert(false);
            }
        }

        // set the econo lockout flags
        if (Furnace(FurnaceNum).CompPartLoadRatio > 0.0 && AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor) {
            AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithCompressor = true;
        } else {
            AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithCompressor = false;
        }

        if ((HeatCoilLoad > 0.0 || Furnace(FurnaceNum).HeatPartLoadRatio > 0.0) &&
            (AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor || AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithHeating)) {
            AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithHeating = true;
        } else {
            AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithHeating = false;
        }

        if (Furnace(FurnaceNum).OpMode == CycFanCycCoil) {
            AirLoopFlow(AirLoopNum).FanPLR = Furnace(FurnaceNum).FanPartLoadRatio;
        } else {
            AirLoopFlow(AirLoopNum).FanPLR = 1.0; // 1 means constant fan does not cycle.
        }

        // Report the current Furnace output
        ReportFurnace(FurnaceNum, AirLoopNum);

        // Reset OnOffFanPartLoadFraction to 1 in case another on/off fan is called without a part-load curve
        OnOffFanPartLoadFraction = 1.0;

        Node(FurnaceInletNode).MassFlowRateMaxAvail = TempMassFlowRateMaxAvail;
    }

    // Get Input Section of the Module
    //******************************************************************************

    void GetFurnaceInput()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Feb 2001
        //       MODIFIED       Don Shirey and Rich Raustad, Mar/Oct 2001, Mar 2003
        //                      Bereket Nigusse, April 2010 - deprecated supply air flow fraction through
        //                      controlled zone from the input field.
        //                      Bo Shen, March 2012, add inputs for VS WSHP,
        //                      Bo Shen, ORNL, July 2012 - added variable-speed air source heat pump cooling and heating coils, using curve-fits
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for fans and coils and stores it in the Furnace data structures

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in data.

        // Using/Aliasing
        using BranchNodeConnections::SetUpCompSets;
        using BranchNodeConnections::TestCompSet;
        using DataAirSystems::PrimaryAirSystem;
        using DataHeatBalance::Zone;
        using DataLoopNode::NodeID;
        using DataZoneControls::ComfortControlledZone;
        using DataZoneControls::HumidityControlZone;
        using DataZoneControls::NumComfortControlledZones;
        using DataZoneControls::NumHumidityControlZones;
        using DataZoneControls::NumTempControlledZones;
        using DataZoneControls::TempControlledZone;
        using NodeInputManager::GetOnlySingleNode;
        auto &GetWtoAHPSimpleCoilCapacity(WaterToAirHeatPumpSimple::GetCoilCapacity);
        auto &GetWtoAHPSimpleCoilInletNode(WaterToAirHeatPumpSimple::GetCoilInletNode);
        auto &GetWtoAHPSimpleCoilOutletNode(WaterToAirHeatPumpSimple::GetCoilOutletNode);
        auto &GetWtoAHPSimpleCoilIndex(WaterToAirHeatPumpSimple::GetCoilIndex);
        using WaterToAirHeatPumpSimple::SetSimpleWSHPData;
        auto &GetWtoAHPSimpleCoilAirFlow(WaterToAirHeatPumpSimple::GetCoilAirFlowRate);
        using VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed;
        using VariableSpeedCoils::GetCoilCapacityVariableSpeed;
        using VariableSpeedCoils::GetCoilIndexVariableSpeed;
        using VariableSpeedCoils::GetCoilInletNodeVariableSpeed;
        using VariableSpeedCoils::GetCoilOutletNodeVariableSpeed;
        using VariableSpeedCoils::GetVSCoilCondenserInletNode;
        using VariableSpeedCoils::GetVSCoilMinOATCompressor;
        using VariableSpeedCoils::SetVarSpeedCoilData;
        auto &GetWtoAHPCoilCapacity(WaterToAirHeatPump::GetCoilCapacity);
        auto &GetWtoAHPCoilInletNode(WaterToAirHeatPump::GetCoilInletNode);
        auto &GetWtoAHPCoilOutletNode(WaterToAirHeatPump::GetCoilOutletNode);
        auto &GetWtoAHPCoilIndex(WaterToAirHeatPump::GetCoilIndex);
        auto &GetHeatingCoilCapacity(HeatingCoils::GetCoilCapacity);
        auto &GetHeatingCoilInletNode(HeatingCoils::GetCoilInletNode);
        auto &GetHeatingCoilOutletNode(HeatingCoils::GetCoilOutletNode);
        auto &GetHeatingCoilIndex(HeatingCoils::GetCoilIndex);
        using HeatingCoils::GetHeatingCoilPLFCurveIndex;
        using HeatingCoils::GetHeatingCoilTypeNum;
        auto &GetDXCoilCapacity(DXCoils::GetCoilCapacity);
        auto &GetDXCoilInletNode(DXCoils::GetCoilInletNode);
        auto &GetDXCoilOutletNode(DXCoils::GetCoilOutletNode);
        auto &GetDXCoilCondenserInletNode(DXCoils::GetCoilCondenserInletNode);
        using DXCoils::GetCoilTypeNum;
        using DXCoils::GetDXCoilIndex;
        using DXCoils::SetDXCoolingCoilData;
        auto &GetDXHXAsstdCoilCapacity(HVACHXAssistedCoolingCoil::GetCoilCapacity);
        auto &GetDXHXAsstdCoilInletNode(HVACHXAssistedCoolingCoil::GetCoilInletNode);
        auto &GetDXHXAsstdCoilOutletNode(HVACHXAssistedCoolingCoil::GetCoilOutletNode);
        using HVACHXAssistedCoolingCoil::GetHXDXCoilIndex;
        using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
        auto &GetHXAssistedCoilTypeNum(HVACHXAssistedCoolingCoil::GetCoilGroupTypeNum);
        using HVACHXAssistedCoolingCoil::GetActualDXCoilIndex;
        using WaterCoils::GetCoilMaxWaterFlowRate;
        using WaterCoils::GetCoilWaterInletNode;
        auto &GetWaterCoilInletNode(WaterCoils::GetCoilInletNode);
        auto &GetWaterCoilOutletNode(WaterCoils::GetCoilOutletNode);
        auto &GetSteamCoilAirInletNode(SteamCoils::GetCoilAirInletNode);
        using SteamCoils::GetCoilAirOutletNode;
        using SteamCoils::GetCoilSteamInletNode;
        using SteamCoils::GetSteamCoilIndex;
        auto &GetCoilMaxSteamFlowRate(SteamCoils::GetCoilMaxSteamFlowRate);
        using DataSizing::AutoSize;
        using Fans::GetFanAvailSchPtr;
        using Fans::GetFanDesignVolumeFlowRate;
        using Fans::GetFanIndex;
        using Fans::GetFanInletNode;
        using Fans::GetFanOutletNode;
        using Fans::GetFanType;
        using FluidProperties::GetSatDensityRefrig;
        using General::RoundSigDigits;
        using General::TrimSigDigits;
        using OutAirNodeManager::CheckOutAirNodeNumber;
        using SteamCoils::GetTypeOfCoil;
        using namespace DataIPShortCuts;
        using EMSManager::ManageEMS;
        using HVACControllers::CheckCoilWaterInletNode;
        using IntegratedHeatPump::GetCoilIndexIHP;
        using IntegratedHeatPump::IntegratedHeatPumps;

        // Locals
        std::string CurrentModuleObject; // Object type for getting and error messages

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const getUnitaryHeatOnly("GetUnitaryHeatOnly");
        static std::string const getAirLoopHVACHeatCoolInput("GetAirLoopHVACHeatCoolInput");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FurnaceNum;                // The Furnace that you are currently loading input into
        int GetObjectNum;              // The index to each specific object name
        int NumFields;                 // Total number of fields in object
        int NumAlphas;                 // Total number of alpha fields in object
        int MaxAlphas;                 // Maximum number of alpha fields in all objects
        int NumNumbers;                // Total number of numeric fields in object
        int MaxNumbers;                // Maximum number of numeric fields in all objects
        int IOStatus;                  // Function call status
        Array1D<Real64> Numbers;       // Numeric data
        Array1D_string Alphas;         // Alpha data
        Array1D_string cAlphaFields;   // Alpha field names
        Array1D_string cNumericFields; // Numeric field names
        Array1D_bool lAlphaBlanks;     // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.
        std::string CompSetFanInlet;
        std::string CompSetFanOutlet;
        std::string CompSetCoolInlet;
        std::string CompSetHeatInlet;
        std::string CompSetHeatOutlet;
        static bool ErrorsFound(false); // If errors detected in input
        bool IsNotOK;                   // Flag to verify name
        int NumHeatOnly;                // Number of heat only furnaces
        int NumHeatCool;                // Number of heat/cool furnaces
        int HeatOnlyNum;                // Index to heat only furnaces
        int HeatCoolNum;                // Index to heat/cool furnaces
        int NumUnitaryHeatOnly;         // Number of heat only unitary systems
        int NumUnitaryHeatCool;         // Number of heat/cool unitary systems
        int UnitaryHeatOnlyNum;         // Index to heat only furnaces
        int UnitaryHeatCoolNum;         // Index to heat/cool unitary systems
        int NumWaterToAirHeatPump;      // Number of water-to-air heat pumps
        int NumHeatPump;                // Number of air-to-air or water-to-air heat pumps
        int HeatPumpNum;                // Index to air-to-air heat pumps
        int ControlledZoneNum;          // Index to controlled zones
        bool AirNodeFound;              // Used to determine if control zone is valid
        bool AirLoopFound;              // Used to determine if control zone is served by furnace air loop
        int BranchNum;                  // Used to determine if control zone is served by furnace air loop
        int CompNum;                    // Used to determine if control zone is served by furnace air loop
        int TstatZoneNum;               // Used to determine if control zone has a thermostat object
        int HStatZoneNum;               // Used to determine if control zone has a humidistat object
        bool errFlag;                   // Mining function error flag
        int FanInletNode;               // Used for node checking warning messages
        int FanOutletNode;              // Used for node checking warning messages
        int CoolingCoilInletNode;       // Used for node checking warning messages
        int CoolingCoilOutletNode;      // Used for node checking warning messages
        int HeatingCoilInletNode;       // Used for node checking warning messages
        int HeatingCoilOutletNode;      // Used for node checking warning messages
        int SupHeatCoilInletNode;       // Used for node checking warning messages
        int SupHeatCoilOutletNode;      // Used for node checking warning messages
        int ReheatCoilInletNode;        // Used for node checking warning messages
        int ReheatCoilOutletNode;       // Used for node checking warning messages
        Real64 FanVolFlowRate;          // Fan Max Flow Rate from Fan object (for comparisons to validity)
        int FurnaceType_Num;            // Integer equivalent of Furnace or UnitarySystem "type"
        std::string CoolingCoilType;    // Used in mining function CALLS
        std::string CoolingCoilName;    // Used in mining function CALLS
        std::string HeatingCoilType;    // Used in mining function CALLS
        std::string HeatingCoilName;    // Used in mining function CALLS
        std::string ReheatingCoilType;  // Used in mining function CALLS
        std::string ReheatingCoilName;  // Used in mining function CALLS
        std::string SuppHeatCoilType;   // Used in mining function CALLS
        std::string SuppHeatCoilName;   // Used in mining function CALLS
        std::string FanType;            // Used in mining function CALLS
        std::string FanName;            // Used in mining function CALLS
        bool PrintMessage;              // Used in mining function CALLS
        int HeatingCoilPLFCurveIndex;   // index of heating coil PLF curve
        int SteamIndex;                 // steam coil index
        Real64 SteamDensity;            // density of steam at 100C
        int DXCoilIndex;                // Index to DX coil in HXAssited object
        std::string IHPCoilName;        // IHP cooling coil name
        int IHPCoilIndex(0);            // IHP cooling coil id

        // Flow
        GetFurnaceInputFlag = false;
        MaxNumbers = 0;
        MaxAlphas = 0;

        CurrentModuleObject = "AirLoopHVAC:Unitary:Furnace:HeatOnly";
        NumHeatOnly = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
        MaxNumbers = max(MaxNumbers, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        CurrentModuleObject = "AirLoopHVAC:Unitary:Furnace:HeatCool";
        NumHeatCool = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
        MaxNumbers = max(MaxNumbers, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        CurrentModuleObject = "AirLoopHVAC:UnitaryHeatOnly";
        NumUnitaryHeatOnly = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
        MaxNumbers = max(MaxNumbers, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        CurrentModuleObject = "AirLoopHVAC:UnitaryHeatCool";
        NumUnitaryHeatCool = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
        MaxNumbers = max(MaxNumbers, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        CurrentModuleObject = "AirLoopHVAC:UnitaryHeatPump:AirToAir";
        NumHeatPump = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
        MaxNumbers = max(MaxNumbers, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        CurrentModuleObject = "AirLoopHVAC:UnitaryHeatPump:WaterToAir";
        NumWaterToAirHeatPump = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
        MaxNumbers = max(MaxNumbers, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        Alphas.allocate(MaxAlphas);
        Numbers.dimension(MaxNumbers, 0.0);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNumbers);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNumbers, true);

        NumFurnaces = NumHeatOnly + NumHeatCool + NumUnitaryHeatOnly + NumUnitaryHeatCool + NumHeatPump + NumWaterToAirHeatPump;

        if (NumFurnaces > 0) {
            Furnace.allocate(NumFurnaces);
            UniqueFurnaceNames.reserve(NumFurnaces);
        }
        CheckEquipName.dimension(NumFurnaces, true);

        IHPCoilIndex = 0;

        // Get the data for the HeatOnly Furnace
        for (HeatOnlyNum = 1; HeatOnlyNum <= NumHeatOnly + NumUnitaryHeatOnly; ++HeatOnlyNum) {

            FanInletNode = 0;
            FanOutletNode = 0;
            FanVolFlowRate = 0.0;
            HeatingCoilInletNode = 0;
            HeatingCoilOutletNode = 0;
            CoolingCoilType = ' ';
            CoolingCoilName = ' ';
            HeatingCoilType = ' ';
            HeatingCoilName = ' ';

            //       Furnace and UnitarySystem objects are both read in here.
            //       Will still have 2 differently named objects for the user, but read in with 1 DO loop.
            if (HeatOnlyNum <= NumHeatOnly) {
                CurrentModuleObject = "AirLoopHVAC:Unitary:Furnace:HeatOnly";
                FurnaceType_Num = Furnace_HeatOnly;
                GetObjectNum = HeatOnlyNum;
            } else {
                CurrentModuleObject = "AirLoopHVAC:UnitaryHeatOnly";
                FurnaceType_Num = UnitarySys_HeatOnly;
                GetObjectNum = HeatOnlyNum - NumHeatOnly;
            }

            FurnaceNum = HeatOnlyNum;
            Furnace(FurnaceNum).FurnaceType_Num = FurnaceType_Num;
            Furnace(FurnaceNum).iterationMode.allocate(20);

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          GetObjectNum,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            GlobalNames::VerifyUniqueInterObjectName(UniqueFurnaceNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);

            Furnace(FurnaceNum).Name = Alphas(1);
            if (lAlphaBlanks(2)) {
                Furnace(FurnaceNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                Furnace(FurnaceNum).SchedPtr = GetScheduleIndex(Alphas(2));
                if (Furnace(FurnaceNum).SchedPtr == 0) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("Illegal " + cAlphaFields(2) + " = " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            Furnace(FurnaceNum).FurnaceInletNodeNum =
                GetOnlySingleNode(Alphas(3), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent);
            Furnace(FurnaceNum).FurnaceOutletNodeNum =
                GetOnlySingleNode(Alphas(4), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent);

            TestCompSet(CurrentModuleObject, Alphas(1), Alphas(3), Alphas(4), "Air Nodes");

            Furnace(FurnaceNum).FanSchedPtr = GetScheduleIndex(Alphas(5));
            if (!lAlphaBlanks(5) && Furnace(FurnaceNum).FanSchedPtr == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(5) + " = " + Alphas(5));
                ErrorsFound = true;
            } else if (lAlphaBlanks(5)) {
                Furnace(FurnaceNum).OpMode = CycFanCycCoil;
            }

            // Get the Controlling Zone or Location of the Furnace Thermostat
            Furnace(FurnaceNum).ControlZoneNum = UtilityRoutines::FindItemInList(Alphas(6), Zone);
            if (Furnace(FurnaceNum).ControlZoneNum == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(6) + " = " + Alphas(6));
                ErrorsFound = true;
            }

            // Get the node number for the zone with the thermostat
            if (Furnace(FurnaceNum).ControlZoneNum > 0) {
                AirNodeFound = false;
                AirLoopFound = false;
                for (ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum) {
                    if (ZoneEquipConfig(ControlledZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                    //             Find the controlled zone number for the specified thermostat location
                    Furnace(FurnaceNum).NodeNumOfControlledZone = ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                    //             Determine if furnace is on air loop served by the thermostat location specified
                    for (int zoneInNode = 1; zoneInNode <= ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                        int AirLoopNumber = ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode);
                        if (AirLoopNumber > 0) {
                            for (BranchNum = 1; BranchNum <= PrimaryAirSystem(AirLoopNumber).NumBranches; ++BranchNum) {
                                for (CompNum = 1; CompNum <= PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).TotalComponents; ++CompNum) {
                                    if (!UtilityRoutines::SameString(PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).Comp(CompNum).Name,
                                                                     Furnace(FurnaceNum).Name) ||
                                        !UtilityRoutines::SameString(PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).Comp(CompNum).TypeOf,
                                                                     CurrentModuleObject))
                                        continue;
                                    AirLoopFound = true;
                                    Furnace(FurnaceNum).ZoneInletNode = ZoneEquipConfig(ControlledZoneNum).InletNode(zoneInNode);
                                    break;
                                }
                                if (AirLoopFound) break;
                            }
                            for (TstatZoneNum = 1; TstatZoneNum <= NumTempControlledZones; ++TstatZoneNum) {
                                if (TempControlledZone(TstatZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                                AirNodeFound = true;
                            }
                            for (TstatZoneNum = 1; TstatZoneNum <= NumComfortControlledZones; ++TstatZoneNum) {
                                if (ComfortControlledZone(TstatZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                                AirNodeFound = true;
                            }
                        }
                        if (AirLoopFound) break;
                    }
                    if (AirLoopFound) break;
                }
                if (!AirNodeFound) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowSevereError("Did not find Air Node (Zone with Thermostat).");
                    ShowContinueError("Specified " + cAlphaFields(6) + " = " + Alphas(6));
                    ShowContinueError(
                        "Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object must be specified for this zone.");
                    ErrorsFound = true;
                }
                if (!AirLoopFound) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowSevereError("Did not find correct Primary Air Loop.");
                    ShowContinueError("Specified " + cAlphaFields(6) + " = " + Alphas(6) + " is not served by this AirLoopHVAC equipment.");
                    ErrorsFound = true;
                }
            }

            // Get fan data
            FanType = Alphas(7);
            FanName = Alphas(8);
            errFlag = false;
            GetFanType(FanName, Furnace(FurnaceNum).FanType_Num, errFlag, CurrentModuleObject, Alphas(1));
            if (errFlag) {
                ErrorsFound = true;
            }
            if (Furnace(FurnaceNum).FanType_Num == FanType_SimpleOnOff || Furnace(FurnaceNum).FanType_Num == FanType_SimpleConstVolume) {

                ValidateComponent(FanType, FanName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;

                } else { // mine data from fan object

                    // Get the fan index
                    errFlag = false;
                    GetFanIndex(FanName, Furnace(FurnaceNum).FanIndex, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Set the Design Fan Volume Flow Rate
                    errFlag = false;
                    FanVolFlowRate = GetFanDesignVolumeFlowRate(FanType, FanName, errFlag);
                    Furnace(FurnaceNum).ActualFanVolFlowRate = FanVolFlowRate;

                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " =" + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the Fan Inlet Node
                    errFlag = false;
                    FanInletNode = GetFanInletNode(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the Fan Outlet Node
                    errFlag = false;
                    FanOutletNode = GetFanOutletNode(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the fan's availabitlity schedule
                    errFlag = false;
                    Furnace(FurnaceNum).FanAvailSchedPtr = GetFanAvailSchPtr(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Check fan's schedule for cycling fan operation if constant volume fan is used
                    if (Furnace(FurnaceNum).FanSchedPtr > 0 && Furnace(FurnaceNum).FanType_Num == FanType_SimpleConstVolume) {
                        if (!CheckScheduleValueMinMax(Furnace(FurnaceNum).FanSchedPtr, ">", 0.0, "<=", 1.0)) {
                            ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                            ShowContinueError("For " + cAlphaFields(7) + " = " + Alphas(7));
                            ShowContinueError("Fan operating mode must be continuous (fan operating mode schedule values > 0).");
                            ShowContinueError("Error found in " + cAlphaFields(5) + " = " + Alphas(5));
                            ShowContinueError("...schedule values must be (>0., <=1.)");
                            ErrorsFound = true;
                        }
                    } else if (lAlphaBlanks(5) && Furnace(FurnaceNum).FanType_Num != FanType_SimpleOnOff) {
                        ShowSevereError(CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ShowContinueError(cAlphaFields(7) + " = " + Alphas(7));
                        ShowContinueError("Fan type must be Fan:OnOff when " + cAlphaFields(5) + " = Blank.");
                        ErrorsFound = true;
                    }

                } // IF (IsNotOK) THEN

            } else { // wrong fan type
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(7) + " = " + Alphas(7));
                ErrorsFound = true;
            } // IF (Furnace(FurnaceNum)%FanType_Num == FanType_SimpleOnOff .OR. &

            if (UtilityRoutines::SameString(Alphas(9), "BlowThrough")) Furnace(FurnaceNum).FanPlace = BlowThru;
            if (UtilityRoutines::SameString(Alphas(9), "DrawThrough")) Furnace(FurnaceNum).FanPlace = DrawThru;
            if (Furnace(FurnaceNum).FanPlace == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(9) + " = " + Alphas(9));
                ErrorsFound = true;
            }

            // Get coil data
            HeatingCoilType = Alphas(10);
            HeatingCoilName = Alphas(11);
            Furnace(FurnaceNum).HeatingCoilType = HeatingCoilType;
            Furnace(FurnaceNum).HeatingCoilName = HeatingCoilName;
            if (UtilityRoutines::SameString(HeatingCoilType, "Coil:Heating:Fuel") ||
                UtilityRoutines::SameString(HeatingCoilType, "Coil:Heating:Electric")) {
                errFlag = false;
                Furnace(FurnaceNum).HeatingCoilType_Num = GetHeatingCoilTypeNum(HeatingCoilType, HeatingCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {
                    ValidateComponent(HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;

                    } else { // mine data from heating coil object

                        // Get index to Heating Coil
                        errFlag = false;
                        GetHeatingCoilIndex(HeatingCoilName, Furnace(FurnaceNum).HeatingCoilIndex, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                        // Get the furnace design capacity
                        errFlag = false;
                        Furnace(FurnaceNum).DesignHeatingCapacity = GetHeatingCoilCapacity(HeatingCoilType, HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " =" + Alphas(1));
                            ErrorsFound = true;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        HeatingCoilInletNode = GetHeatingCoilInletNode(HeatingCoilType, HeatingCoilName, errFlag);
                        Furnace(FurnaceNum).HWCoilAirInletNode = HeatingCoilInletNode;
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " =" + Alphas(1));
                            ErrorsFound = true;
                        }

                        // Get the Heating Coil Outlet Node
                        errFlag = false;
                        HeatingCoilOutletNode = GetHeatingCoilOutletNode(HeatingCoilType, HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " =" + Alphas(1));
                            ErrorsFound = true;
                        }

                    } // IF (IsNotOK) THEN
                }

            } else if (UtilityRoutines::SameString(HeatingCoilType, "Coil:Heating:Water")) {
                Furnace(FurnaceNum).HeatingCoilType_Num = Coil_HeatingWater;
                ValidateComponent(HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    // Get the Heating Coil water Inlet or control Node number
                    errFlag = false;
                    Furnace(FurnaceNum).CoilControlNode = GetCoilWaterInletNode("Coil:Heating:Water", HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil hot water max volume flow rate
                    errFlag = false;
                    Furnace(FurnaceNum).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate("Coil:Heating:Water", HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil Inlet Node
                    errFlag = false;
                    HeatingCoilInletNode = GetWaterCoilInletNode("Coil:Heating:Water", HeatingCoilName, errFlag);
                    Furnace(FurnaceNum).HWCoilAirInletNode = HeatingCoilInletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil Outlet Node
                    errFlag = false;
                    HeatingCoilOutletNode = GetWaterCoilOutletNode("Coil:Heating:Water", HeatingCoilName, errFlag);
                    Furnace(FurnaceNum).HWCoilAirOutletNode = HeatingCoilOutletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // check if user has also used a water coil controller, which they should not do
                    errFlag = false;
                    CheckCoilWaterInletNode(Furnace(FurnaceNum).CoilControlNode, errFlag);
                    if (!errFlag) { // then did find a controller so that is bad
                        ShowSevereError(CurrentModuleObject + " = " + Furnace(FurnaceNum).Name + " has a conflicting Controller:WaterCoil object");
                        ShowContinueError("Hot water coils are controlled directly by unitary and furnace systems.");
                        ShowContinueError("No water coil controller should be input for the coil.");
                        ErrorsFound = true;
                    }
                }

            } else if (UtilityRoutines::SameString(HeatingCoilType, "Coil:Heating:Steam")) {
                Furnace(FurnaceNum).HeatingCoilType_Num = Coil_HeatingSteam;
                ValidateComponent(HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    errFlag = false;
                    Furnace(FurnaceNum).HeatingCoilIndex = GetSteamCoilIndex("COIL:HEATING:STEAM", HeatingCoilName, errFlag);
                    if (Furnace(FurnaceNum).HeatingCoilIndex == 0) {
                        ShowSevereError(CurrentModuleObject + " illegal " + cAlphaFields(11) + " = " + HeatingCoilName);
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil steam inlet node number
                    errFlag = false;
                    Furnace(FurnaceNum).CoilControlNode = GetCoilSteamInletNode("COIL:HEATING:STEAM", HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil steam max volume flow rate
                    Furnace(FurnaceNum).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).HeatingCoilIndex, errFlag);
                    if (Furnace(FurnaceNum).MaxHeatCoilFluidFlow > 0.0) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getUnitaryHeatOnly);
                        Furnace(FurnaceNum).MaxHeatCoilFluidFlow *= SteamDensity;
                    }

                    // Get the Heating Coil Inlet Node
                    errFlag = false;
                    HeatingCoilInletNode = GetSteamCoilAirInletNode(Furnace(FurnaceNum).HeatingCoilIndex, HeatingCoilName, errFlag);
                    Furnace(FurnaceNum).HWCoilAirInletNode = HeatingCoilInletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil Outlet Node
                    errFlag = false;
                    HeatingCoilOutletNode = GetCoilAirOutletNode(Furnace(FurnaceNum).HeatingCoilIndex, HeatingCoilName, errFlag);
                    Furnace(FurnaceNum).HWCoilAirOutletNode = HeatingCoilOutletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }
                }

            } else {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(11) + " = " + Alphas(11));
                ErrorsFound = true;
            } // IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingGasOrOtherFuel .OR. &, etc.

            // Add component sets array
            if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                CompSetFanInlet = Alphas(3);
                CompSetFanOutlet = NodeID(FanOutletNode);
                CompSetHeatInlet = NodeID(FanOutletNode);
                CompSetHeatOutlet = Alphas(4);
                // Fan inlet node name must not be the same as the furnace inlet node name
                if (FanInletNode != Furnace(FurnaceNum).FurnaceInletNodeNum) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    if (FurnaceType_Num == Furnace_HeatOnly) {
                        ShowContinueError(
                            "When a blow through fan is specified, the fan inlet node name must be the same as the furnace inlet node name.");
                        ShowContinueError("...Fan inlet node name     = " + NodeID(FanInletNode));
                        ShowContinueError("...Furnace inlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                    } else {
                        ShowContinueError(
                            "When a blow through fan is specified, the fan inlet node name must be the same as the unitary system inlet node name.");
                        ShowContinueError("...Fan inlet node name            = " + NodeID(FanInletNode));
                        ShowContinueError("...Unitary System inlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                    }
                    ErrorsFound = true;
                }
                // Fan outlet node name must be the same as the heating coil inlet node name
                if (FanOutletNode != HeatingCoilInletNode) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError(
                        "When a blow through fan is specified, the fan outlet node name must be the same as the heating coil inlet node name.");
                    ShowContinueError("...Fan outlet node name         = " + NodeID(FanOutletNode));
                    ShowContinueError("...Heating coil inlet node name = " + NodeID(HeatingCoilInletNode));
                    ErrorsFound = true;
                }
                // Heating coil outlet node name must be the same as the furnace outlet node name
                if (HeatingCoilOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    if (FurnaceType_Num == Furnace_HeatOnly) {
                        ShowContinueError("When a blow through fan is specified, the heating coil outlet node name must be the same as the furnace "
                                          "outlet node name.");
                        ShowContinueError("...Heating coil outlet node name = " + NodeID(HeatingCoilOutletNode));
                        ShowContinueError("...Furnace outlet node name      = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                    } else {
                        ShowContinueError("When a blow through fan is specified, the heating coil outlet node name must be the same as the unitary "
                                          "system outlet node name.");
                        ShowContinueError("...Heating coil outlet node name  = " + NodeID(HeatingCoilOutletNode));
                        ShowContinueError("...UnitarySystem outlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                    }
                    ErrorsFound = true;
                }
            } else { // draw through fan
                CompSetHeatInlet = Alphas(3);
                CompSetHeatOutlet = NodeID(FanInletNode);
                CompSetFanInlet = NodeID(FanInletNode);
                CompSetFanOutlet = Alphas(4);
                // Heating coil inlet node name must not be the same as the furnace inlet node name
                if (HeatingCoilInletNode != Furnace(FurnaceNum).FurnaceInletNodeNum) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    if (FurnaceType_Num == Furnace_HeatOnly) {
                        ShowContinueError("When a draw through fan is specified, the heating coil inlet node name must be the same as the furnace "
                                          "inlet node name.");
                        ShowContinueError("...Heating coil inlet node name = " + NodeID(HeatingCoilInletNode));
                        ShowContinueError("...Furnace inlet node name      = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                    } else {
                        ShowContinueError("When a draw through fan is specified, the heating coil inlet node name must be the same as the unitary "
                                          "system inlet node name.");
                        ShowContinueError("...Heating coil inlet node name  = " + NodeID(HeatingCoilInletNode));
                        ShowContinueError("...UnitarySystem inlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                    }
                    ErrorsFound = true;
                }
                // Heating coil outlet node name must be the same as the fan inlet node name
                if (HeatingCoilOutletNode != FanInletNode) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError(
                        "When a draw through fan is specified, the heating coil outlet node name must be the same as the fan inlet node name.");
                    ShowContinueError("...Heating coil outlet node name = " + NodeID(HeatingCoilOutletNode));
                    ShowContinueError("...Fan inlet node name           = " + NodeID(FanInletNode));
                    ErrorsFound = true;
                }
                // Fan coil outlet node name must be the same as the furnace outlet node name
                if (FanOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    if (FurnaceType_Num == Furnace_HeatOnly) {
                        ShowContinueError(
                            "When a draw through fan is specified, the fan outlet node name must be the same as the furnace outlet node name.");
                        ShowContinueError("...Fan outlet node name     = " + NodeID(FanOutletNode));
                        ShowContinueError("...Furnace outlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                    } else {
                        ShowContinueError("When a draw through fan is specified, the fan outlet node name must be the same as the unitary system "
                                          "outlet node name.");
                        ShowContinueError("...Fan outlet node name           = " + NodeID(FanOutletNode));
                        ShowContinueError("...UnitarySystem outlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                    }
                    ErrorsFound = true;
                }
            }

            // Add fan to component sets array
            SetUpCompSets(CurrentModuleObject, Furnace(FurnaceNum).Name, Alphas(7), Alphas(8), CompSetFanInlet, CompSetFanOutlet);
            // Add heating coil to component sets array
            SetUpCompSets(CurrentModuleObject, Furnace(FurnaceNum).Name, Alphas(10), Alphas(11), CompSetHeatInlet, CompSetHeatOutlet);

            // Set the furnace max outlet temperature
            Furnace(FurnaceNum).DesignMaxOutletTemp = Numbers(1);

            // Set the furnace design fan volumetric flow rate
            Furnace(FurnaceNum).DesignFanVolFlowRate = Numbers(2);

            // Compare the flow rates.
            if (FanVolFlowRate != AutoSize && Furnace(FurnaceNum).DesignFanVolFlowRate != AutoSize) {
                if (Furnace(FurnaceNum).DesignFanVolFlowRate > FanVolFlowRate) {
                    ShowWarningError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("... The " + cNumericFields(2) + " > Max Volume Flow Rate defined in the associated fan object, should be <=.");
                    ShowContinueError("... Entered value = " + RoundSigDigits(Furnace(FurnaceNum).DesignFanVolFlowRate, 4) + "... Fan [" + FanType +
                                      " = " + FanName + "] Max Value = " + RoundSigDigits(FanVolFlowRate, 4));
                    ShowContinueError(" The HVAC system  flow rate is reset to the fan flow rate and the simulation continues.");
                    Furnace(FurnaceNum).DesignFanVolFlowRate = FanVolFlowRate;
                }
            }
            if (Furnace(FurnaceNum).DesignFanVolFlowRate != AutoSize) {
                if (Furnace(FurnaceNum).DesignFanVolFlowRate <= 0.0) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("... The " + cNumericFields(2) + " <= 0.0, it must be > 0.0.");
                    ShowContinueError("... Entered value = " + RoundSigDigits(Furnace(FurnaceNum).DesignFanVolFlowRate, 2));
                    ErrorsFound = true;
                }
            }

            //       HeatOnly furnace has only 1 flow rate, initialize other variables used in this module
            Furnace(FurnaceNum).MaxHeatAirVolFlow = Furnace(FurnaceNum).DesignFanVolFlowRate;
            Furnace(FurnaceNum).MaxCoolAirVolFlow = Furnace(FurnaceNum).DesignFanVolFlowRate;
            Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow = Furnace(FurnaceNum).DesignFanVolFlowRate;
            Furnace(FurnaceNum).AirFlowControl = UseCompressorOnFlow;

            // Set heating convergence tolerance
            Furnace(FurnaceNum).HeatingConvergenceTolerance = 0.001;

            // set minimum outdoor temperature for compressor operation
            SetMinOATCompressor(
                FurnaceNum, Alphas(1), cCurrentModuleObject, CoolingCoilType, CoolingCoilName, HeatingCoilType, HeatingCoilName, ErrorsFound);

        } // End of the HeatOnly Furnace Loop

        // Get the data for the HeatCool Furnace or UnitarySystem
        for (HeatCoolNum = 1; HeatCoolNum <= NumHeatCool + NumUnitaryHeatCool; ++HeatCoolNum) {

            FanInletNode = 0;
            FanOutletNode = 0;
            FanVolFlowRate = 0.0;
            CoolingCoilInletNode = 0;
            CoolingCoilOutletNode = 0;
            HeatingCoilInletNode = 0;
            HeatingCoilOutletNode = 0;
            ReheatCoilInletNode = 0;
            ReheatCoilOutletNode = 0;
            CoolingCoilType = ' ';
            CoolingCoilName = ' ';
            HeatingCoilType = ' ';
            HeatingCoilName = ' ';

            //      Furnace and UnitarySystem objects are both read in here.
            //      Will still have 2 differently named objects for the user, but read in with 1 DO loop.
            if (HeatCoolNum <= NumHeatCool) {
                CurrentModuleObject = "AirLoopHVAC:Unitary:Furnace:HeatCool";
                FurnaceType_Num = Furnace_HeatCool;
                GetObjectNum = HeatCoolNum;
            } else {
                CurrentModuleObject = "AirLoopHVAC:UnitaryHeatCool";
                FurnaceType_Num = UnitarySys_HeatCool;
                GetObjectNum = HeatCoolNum - NumHeatCool;
            }

            FurnaceNum = HeatCoolNum + NumHeatOnly + NumUnitaryHeatOnly;
            Furnace(FurnaceNum).FurnaceType_Num = FurnaceType_Num;
            Furnace(FurnaceNum).iterationMode.allocate(20);

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          GetObjectNum,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            GlobalNames::VerifyUniqueInterObjectName(UniqueFurnaceNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);

            Furnace(FurnaceNum).Name = Alphas(1);
            if (lAlphaBlanks(2)) {
                Furnace(FurnaceNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                Furnace(FurnaceNum).SchedPtr = GetScheduleIndex(Alphas(2));
                if (Furnace(FurnaceNum).SchedPtr == 0) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("Illegal " + cAlphaFields(2) + " = " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            Furnace(FurnaceNum).FurnaceInletNodeNum =
                GetOnlySingleNode(Alphas(3), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent);
            Furnace(FurnaceNum).FurnaceOutletNodeNum =
                GetOnlySingleNode(Alphas(4), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent);

            TestCompSet(CurrentModuleObject, Alphas(1), Alphas(3), Alphas(4), "Air Nodes");

            Furnace(FurnaceNum).FanSchedPtr = GetScheduleIndex(Alphas(5));
            if (!lAlphaBlanks(5) && Furnace(FurnaceNum).FanSchedPtr == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(5) + " = " + Alphas(5));
                ErrorsFound = true;
            } else if (lAlphaBlanks(5)) {
                Furnace(FurnaceNum).OpMode = CycFanCycCoil;
            }

            // Get the Controlling Zone or Location of the Furnace Thermostat
            Furnace(FurnaceNum).ControlZoneNum = UtilityRoutines::FindItemInList(Alphas(6), Zone);
            if (Furnace(FurnaceNum).ControlZoneNum == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(6) + " = " + Alphas(6));
                ErrorsFound = true;
            }

            // Get the node number for the zone with the thermostat
            if (Furnace(FurnaceNum).ControlZoneNum > 0) {
                AirNodeFound = false;
                AirLoopFound = false;
                for (ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum) {
                    if (ZoneEquipConfig(ControlledZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                    //             Find the controlled zone number for the specified thermostat location
                    Furnace(FurnaceNum).NodeNumOfControlledZone = ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                    //             Determine if system is on air loop served by the thermostat location specified
                    for (int zoneInNode = 1; zoneInNode <= ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                        int AirLoopNumber = ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode);
                        if (AirLoopNumber > 0) {
                            for (BranchNum = 1; BranchNum <= PrimaryAirSystem(AirLoopNumber).NumBranches; ++BranchNum) {
                                for (CompNum = 1; CompNum <= PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).TotalComponents; ++CompNum) {
                                    if (!UtilityRoutines::SameString(PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).Comp(CompNum).Name,
                                                                     Alphas(1)) ||
                                        !UtilityRoutines::SameString(PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).Comp(CompNum).TypeOf,
                                                                     CurrentModuleObject))
                                        continue;
                                    AirLoopFound = true;
                                    Furnace(FurnaceNum).ZoneInletNode = ZoneEquipConfig(ControlledZoneNum).InletNode(zoneInNode);
                                    break;
                                }
                                if (AirLoopFound) break;
                            }
                            for (TstatZoneNum = 1; TstatZoneNum <= NumTempControlledZones; ++TstatZoneNum) {
                                if (TempControlledZone(TstatZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                                AirNodeFound = true;
                            }
                            for (TstatZoneNum = 1; TstatZoneNum <= NumComfortControlledZones; ++TstatZoneNum) {
                                if (ComfortControlledZone(TstatZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                                AirNodeFound = true;
                            }
                        }
                        if (AirLoopFound) break;
                    }
                    if (AirLoopFound) break;
                }
                if (!AirNodeFound) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("Did not find air node (zone with thermostat).");
                    ShowContinueError("Specified " + cAlphaFields(6) + " = " + Alphas(6));
                    ShowContinueError(
                        "Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object must be specified for this zone.");
                    ErrorsFound = true;
                }
                if (!AirLoopFound) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowSevereError("Did not find correct AirLoopHVAC.");
                    ShowContinueError("Specified " + cAlphaFields(6) + " = " + Alphas(6));
                    ErrorsFound = true;
                }
            }

            // Get fan data
            FanType = Alphas(7);
            FanName = Alphas(8);

            errFlag = false;
            GetFanType(FanName, Furnace(FurnaceNum).FanType_Num, errFlag, CurrentModuleObject, Alphas(1));
            if (errFlag) {
                ErrorsFound = true;
            }

            if (Furnace(FurnaceNum).FanType_Num == FanType_SimpleOnOff || Furnace(FurnaceNum).FanType_Num == FanType_SimpleConstVolume) {
                ValidateComponent(FanType, FanName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("In Furnace=" + Alphas(1));
                    ErrorsFound = true;

                } else { // mine data from fan object

                    // Get the fan index
                    errFlag = false;
                    GetFanIndex(FanName, Furnace(FurnaceNum).FanIndex, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the Design Fan Volume Flow Rate
                    errFlag = false;
                    FanVolFlowRate = GetFanDesignVolumeFlowRate(FanType, FanName, errFlag);
                    Furnace(FurnaceNum).ActualFanVolFlowRate = FanVolFlowRate;
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                        ErrorsFound = true;
                    }

                    // Get the Fan Inlet Node
                    errFlag = false;
                    FanInletNode = GetFanInletNode(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the Fan Outlet Node
                    errFlag = false;
                    FanOutletNode = GetFanOutletNode(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the fan's availability schedule
                    errFlag = false;
                    Furnace(FurnaceNum).FanAvailSchedPtr = GetFanAvailSchPtr(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Check fan's schedule for cycling fan operation if constant volume fan is used
                    if (Furnace(FurnaceNum).FanSchedPtr > 0 && Furnace(FurnaceNum).FanType_Num == FanType_SimpleConstVolume) {
                        if (!CheckScheduleValueMinMax(Furnace(FurnaceNum).FanSchedPtr, ">", 0.0, "<=", 1.0)) {
                            ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                            ShowContinueError("For " + cAlphaFields(7) + " = " + Alphas(7));
                            ShowContinueError("Fan operating mode must be continuous (fan operating mode schedule values > 0).");
                            ShowContinueError("Error found in " + cAlphaFields(5) + " = " + Alphas(5));
                            ShowContinueError("...schedule values must be (>0., <=1.)");
                            ErrorsFound = true;
                        }
                    } else if (lAlphaBlanks(5) && Furnace(FurnaceNum).FanType_Num != FanType_SimpleOnOff) {
                        ShowSevereError(CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ShowContinueError(cAlphaFields(7) + " = " + Alphas(7));
                        ShowContinueError("Fan type must be Fan:OnOff when " + cAlphaFields(5) + " = Blank.");
                        ErrorsFound = true;
                    }

                } // IF (IsNotOK) THEN

            } else {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(7) + " = " + Alphas(7));
                ErrorsFound = true;
            } //  IF (TFurnace(FurnaceNum)%FanType_Num == FanType_SimpleOnOff .OR. &, etc.

            if (UtilityRoutines::SameString(Alphas(9), "BlowThrough")) Furnace(FurnaceNum).FanPlace = BlowThru;
            if (UtilityRoutines::SameString(Alphas(9), "DrawThrough")) Furnace(FurnaceNum).FanPlace = DrawThru;
            if (Furnace(FurnaceNum).FanPlace == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(9) + " = " + Alphas(9));
                ErrorsFound = true;
            }

            // Get coil data
            HeatingCoilType = Alphas(10);
            HeatingCoilName = Alphas(11);
            HeatingCoilPLFCurveIndex = 0;
            Furnace(FurnaceNum).HeatingCoilType = HeatingCoilType;
            Furnace(FurnaceNum).HeatingCoilName = HeatingCoilName;
            if (UtilityRoutines::SameString(HeatingCoilType, "Coil:Heating:Fuel") ||
                UtilityRoutines::SameString(HeatingCoilType, "Coil:Heating:Electric")) {
                errFlag = false;
                Furnace(FurnaceNum).HeatingCoilType_Num = GetHeatingCoilTypeNum(HeatingCoilType, HeatingCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {

                    ValidateComponent(HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;

                    } else { // mine data from heating coil

                        // Get heating coil index
                        errFlag = false;
                        GetHeatingCoilIndex(HeatingCoilName, Furnace(FurnaceNum).HeatingCoilIndex, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                        // Get the design heating capacity
                        errFlag = false;
                        Furnace(FurnaceNum).DesignHeatingCapacity = GetHeatingCoilCapacity(HeatingCoilType, HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        HeatingCoilInletNode = GetHeatingCoilInletNode(HeatingCoilType, HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                        // Get the Heating Coil Outlet Node
                        errFlag = false;
                        HeatingCoilOutletNode = GetHeatingCoilOutletNode(HeatingCoilType, HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                        // Get the Heating Coil PLF Curve Index
                        errFlag = false;
                        HeatingCoilPLFCurveIndex = GetHeatingCoilPLFCurveIndex(HeatingCoilType, HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                    } // IF (IsNotOK) THEN
                }

            } else if (UtilityRoutines::SameString(HeatingCoilType, "Coil:Heating:Water")) {
                Furnace(FurnaceNum).HeatingCoilType_Num = Coil_HeatingWater;
                ValidateComponent(HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    // Get the Heating Coil water Inlet or control Node number
                    errFlag = false;
                    Furnace(FurnaceNum).CoilControlNode = GetCoilWaterInletNode("Coil:Heating:Water", HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil hot water max volume flow rate
                    errFlag = false;
                    Furnace(FurnaceNum).MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate("Coil:Heating:Water", HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil Inlet Node
                    errFlag = false;
                    HeatingCoilInletNode = GetWaterCoilInletNode("Coil:Heating:Water", HeatingCoilName, errFlag);
                    Furnace(FurnaceNum).HWCoilAirInletNode = HeatingCoilInletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil Outlet Node
                    errFlag = false;
                    HeatingCoilOutletNode = GetWaterCoilOutletNode("Coil:Heating:Water", HeatingCoilName, errFlag);
                    Furnace(FurnaceNum).HWCoilAirOutletNode = HeatingCoilOutletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // check if user has also used a water coil controller, which they should not do
                    errFlag = false;
                    CheckCoilWaterInletNode(Furnace(FurnaceNum).CoilControlNode, errFlag);
                    if (!errFlag) { // then did find a controller so that is bad
                        ShowSevereError(CurrentModuleObject + " = " + Furnace(FurnaceNum).Name + " has a conflicting Controller:WaterCoil object");
                        ShowContinueError("Hot water coils are controlled directly by unitary and furnace systems.");
                        ShowContinueError("No water coil controller should be input for the coil.");
                        ErrorsFound = true;
                    }
                }

            } else if (UtilityRoutines::SameString(HeatingCoilType, "Coil:Heating:Steam")) {
                Furnace(FurnaceNum).HeatingCoilType_Num = Coil_HeatingSteam;
                ValidateComponent(HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    errFlag = false;
                    Furnace(FurnaceNum).HeatingCoilIndex = GetSteamCoilIndex("COIL:HEATING:STEAM", HeatingCoilName, errFlag);
                    if (Furnace(FurnaceNum).HeatingCoilIndex == 0) {
                        ShowSevereError(CurrentModuleObject + " illegal " + cAlphaFields(11) + " = " + HeatingCoilName);
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil steam inlet node number
                    errFlag = false;
                    Furnace(FurnaceNum).CoilControlNode = GetCoilSteamInletNode("Coil:Heating:Steam", HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil steam max volume flow rate
                    Furnace(FurnaceNum).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).HeatingCoilIndex, errFlag);
                    if (Furnace(FurnaceNum).MaxHeatCoilFluidFlow > 0.0) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getAirLoopHVACHeatCoolInput);
                        Furnace(FurnaceNum).MaxHeatCoilFluidFlow *= SteamDensity;
                    }

                    // Get the Heating Coil Inlet Node
                    errFlag = false;
                    HeatingCoilInletNode = GetSteamCoilAirInletNode(Furnace(FurnaceNum).HeatingCoilIndex, HeatingCoilName, errFlag);
                    Furnace(FurnaceNum).HWCoilAirInletNode = HeatingCoilInletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil Outlet Node
                    errFlag = false;
                    HeatingCoilOutletNode = GetCoilAirOutletNode(Furnace(FurnaceNum).HeatingCoilIndex, HeatingCoilName, errFlag);
                    Furnace(FurnaceNum).HWCoilAirOutletNode = HeatingCoilOutletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }
                }

            } else {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(11) + " = " + Alphas(11));
                ErrorsFound = true;
            } // IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingGasOrOtherFuel .OR. &, etc.

            // Get Cooling Coil Information if available
            CoolingCoilType = Alphas(12);
            CoolingCoilName = Alphas(13);
            //       Find the type of coil. Do not print message since this may not be the correct coil type.
            errFlag = false;
            PrintMessage = false;

            if (UtilityRoutines::SameString(CoolingCoilType, "COIL:COOLING:DX:VARIABLESPEED") ||
                UtilityRoutines::SameString(CoolingCoilType, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE")) {
                Furnace(FurnaceNum).CoolingCoilType_Num = Coil_CoolingAirToAirVariableSpeed;
                if (UtilityRoutines::SameString(CoolingCoilType, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE")) Furnace(FurnaceNum).bIsIHP = true;
            } else {
                Furnace(FurnaceNum).CoolingCoilType_Num = GetCoilTypeNum(CoolingCoilType, CoolingCoilName, errFlag, PrintMessage);
            }

            // If coil type not found, check to see if a HX assisted cooling coil is used.
            if (Furnace(FurnaceNum).CoolingCoilType_Num == 0) {
                errFlag = false;
                Furnace(FurnaceNum).CoolingCoilType_Num = GetHXAssistedCoilTypeNum(CoolingCoilType, CoolingCoilName, errFlag, PrintMessage);
            }

            if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) {
                ValidateComponent(CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;

                } else { // mine data from DX cooling coil

                    // Get DX cooling coil index
                    GetDXCoilIndex(CoolingCoilName, Furnace(FurnaceNum).CoolingCoilIndex, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get DX cooling coil capacity
                    errFlag = false;
                    Furnace(FurnaceNum).DesignCoolingCapacity = GetDXCoilCapacity(CoolingCoilType, CoolingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the Cooling Coil Nodes
                    errFlag = false;
                    CoolingCoilInletNode = GetDXCoilInletNode(CoolingCoilType, CoolingCoilName, errFlag);
                    CoolingCoilOutletNode = GetDXCoilOutletNode(CoolingCoilType, CoolingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get outdoor condenser node from DX coil object
                    errFlag = false;
                    if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
                        if (Furnace(FurnaceNum).bIsIHP) {
                            IHPCoilIndex = GetCoilIndexIHP(CoolingCoilType, CoolingCoilName, errFlag);
                            IHPCoilName = IntegratedHeatPumps(IHPCoilIndex).SCCoilName;
                            Furnace(FurnaceNum).CondenserNodeNum = GetVSCoilCondenserInletNode(IHPCoilName, errFlag);
                        } else {
                            Furnace(FurnaceNum).CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName, errFlag);
                        }
                    } else {
                        Furnace(FurnaceNum).CondenserNodeNum = GetDXCoilCondenserInletNode(CoolingCoilType, CoolingCoilName, errFlag);
                    }
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                } // IF (IsNotOK) THEN

                // Push heating coil PLF curve index to DX coil
                if (HeatingCoilPLFCurveIndex > 0) {
                    SetDXCoolingCoilData(Furnace(FurnaceNum).CoolingCoilIndex, ErrorsFound, HeatingCoilPLFCurveIndex);
                }

            } else if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {
                ValidateComponent(CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;

                } else { // mine data from heat exchanger assisted cooling coil

                    // Get DX heat exchanger assisted cooling coil index
                    GetHXDXCoilIndex(CoolingCoilName, Furnace(FurnaceNum).CoolingCoilIndex, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get DX cooling coil capacity
                    Furnace(FurnaceNum).DesignCoolingCapacity = GetDXHXAsstdCoilCapacity(CoolingCoilType, CoolingCoilName, errFlag);
                    errFlag = false;
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the Cooling Coil Nodes
                    errFlag = false;
                    CoolingCoilInletNode = GetDXHXAsstdCoilInletNode(CoolingCoilType, CoolingCoilName, errFlag);
                    CoolingCoilOutletNode = GetDXHXAsstdCoilOutletNode(CoolingCoilType, CoolingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get outdoor condenser node from heat exchanger assisted DX coil object
                    errFlag = false;
                    if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
                        if (Furnace(FurnaceNum).bIsIHP) {
                            IHPCoilIndex = GetCoilIndexIHP(CoolingCoilType, CoolingCoilName, errFlag);
                            IHPCoilName = IntegratedHeatPumps(IHPCoilIndex).SCCoilName;
                            Furnace(FurnaceNum).CondenserNodeNum = GetVSCoilCondenserInletNode(IHPCoilName, errFlag);
                        } else {
                            Furnace(FurnaceNum).CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName, errFlag);
                        }
                    } else {
                        Furnace(FurnaceNum).CondenserNodeNum = GetDXCoilCondenserInletNode(
                            "COIL:COOLING:DX:SINGLESPEED", GetHXDXCoilName(CoolingCoilType, CoolingCoilName, errFlag), errFlag);
                    }

                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Push heating coil PLF curve index to DX coil
                    if (HeatingCoilPLFCurveIndex > 0) {
                        // get the actual index to the DX cooling coil object
                        DXCoilIndex = GetActualDXCoilIndex(CoolingCoilType, CoolingCoilName, ErrorsFound);
                        Furnace(FurnaceNum).ActualDXCoilIndexForHXAssisted = DXCoilIndex;
                        int ActualCoolCoilType = HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(CoolingCoilType, CoolingCoilName, errFlag, true);
                        if (ActualCoolCoilType == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                            SetDXCoolingCoilData(DXCoilIndex, ErrorsFound, HeatingCoilPLFCurveIndex);
                        }
                        // what could we do for VS coil here? odd thing here
                    }

                } // IF (IsNotOK) THEN
            } else if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
                // BOS ADDED, AUG/2012, VARIIABLE SPEED DX COOLING COIL
                //  Furnace(FurnaceNum)%DXCoolCoilType = 'COIL:COOLING:DX:VARIABLESPEED'
                //  Furnace(FurnaceNum)%DXCoolCoilName = CoolingCoilName
                if (UtilityRoutines::SameString(CoolingCoilType, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE")) Furnace(FurnaceNum).bIsIHP = true;
                ValidateComponent(CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject);

                if (IsNotOK) {
                    ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    if (Furnace(FurnaceNum).bIsIHP) {
                        Furnace(FurnaceNum).CoolingCoilIndex = GetCoilIndexIHP(CoolingCoilType, CoolingCoilName, errFlag);
                        IHPCoilName = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SCCoilName;
                    } else {
                        Furnace(FurnaceNum).CoolingCoilIndex = GetCoilIndexVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                        IHPCoilName = CoolingCoilName;
                    }

                    if (errFlag) {
                        ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                        ErrorsFound = true;
                    }

                    if (Furnace(FurnaceNum).bIsIHP) {
                        CoolingCoilInletNode = GetCoilInletNodeVariableSpeed("COIL:COOLING:DX:VARIABLESPEED", IHPCoilName, errFlag);
                        CoolingCoilOutletNode = GetCoilOutletNodeVariableSpeed("COIL:COOLING:DX:VARIABLESPEED", IHPCoilName, errFlag);
                        Furnace(FurnaceNum).CondenserNodeNum = GetVSCoilCondenserInletNode(IHPCoilName, errFlag);
                    } else {
                        CoolingCoilInletNode = GetCoilInletNodeVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                        CoolingCoilOutletNode = GetCoilOutletNodeVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                        Furnace(FurnaceNum).CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName, errFlag);
                    }

                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                }
            } else {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(12) + " = " + Alphas(12));
                ErrorsFound = true;
            }

            if (UtilityRoutines::SameString(Alphas(14), "None") || UtilityRoutines::SameString(Alphas(14), "Multimode") ||
                UtilityRoutines::SameString(Alphas(14), "CoolReheat")) {
                AirNodeFound = false;
                if (UtilityRoutines::SameString(Alphas(14), "Multimode")) {
                    Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_Multimode;
                    Furnace(FurnaceNum).Humidistat = true;
                    if (Furnace(FurnaceNum).CoolingCoilType_Num != CoilDX_CoolingHXAssisted) {
                        ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError("Illegal " + cAlphaFields(14) + " = " + Alphas(14));
                        ShowContinueError("Multimode control must be used with a Heat Exchanger Assisted Cooling Coil.");
                        if (lAlphaBlanks(15)) {
                            ShowContinueError("Dehumidification control type is assumed to be None since a reheat coil has not been specified and "
                                              "the simulation continues.");
                            Furnace(FurnaceNum).Humidistat = false;
                            Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_None;
                        } else {
                            ShowContinueError("Dehumidification control type is assumed to be CoolReheat and the simulation continues.");
                            Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_CoolReheat;
                        }
                    }
                }
                if (UtilityRoutines::SameString(Alphas(14), "CoolReheat")) {
                    Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_CoolReheat;
                    Furnace(FurnaceNum).Humidistat = true;
                    if (lAlphaBlanks(15)) {
                        ShowWarningError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                        ShowContinueError("Dehumidification control type is assumed to be None since a reheat coil has not been specified and the "
                                          "simulation continues.");
                        Furnace(FurnaceNum).Humidistat = false;
                        Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_None;
                    }
                }
                if (UtilityRoutines::SameString(Alphas(14), "None")) {
                    Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_None;
                    Furnace(FurnaceNum).Humidistat = false;
                }
                if (Furnace(FurnaceNum).Humidistat) {
                    for (HStatZoneNum = 1; HStatZoneNum <= NumHumidityControlZones; ++HStatZoneNum) {
                        if (HumidityControlZone(HStatZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                        AirNodeFound = true;
                    }
                    if (!AirNodeFound) {
                        ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError("Did not find Air Node (Zone with Humidistat).");
                        ShowContinueError("Specified " + cAlphaFields(6) + " = " + Alphas(6));
                        ErrorsFound = true;
                    }
                }
            } else { // invalid input
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(14) + " = " + Alphas(14));
                Furnace(FurnaceNum).Humidistat = false;
                ErrorsFound = true;
            }

            //       Check placement of cooling coil with respect to fan placement and dehumidification control type
            if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                if (FanOutletNode == HeatingCoilInletNode && Furnace(FurnaceNum).DehumidControlType_Num != DehumidControl_CoolReheat) {
                    Furnace(FurnaceNum).CoolingCoilUpstream = false;
                }
            } else {
                if (HeatingCoilOutletNode == CoolingCoilInletNode && Furnace(FurnaceNum).DehumidControlType_Num != DehumidControl_CoolReheat) {
                    Furnace(FurnaceNum).CoolingCoilUpstream = false;
                }
            }

            // Get reheat coil data if humidistat is used
            ReheatingCoilType = Alphas(15);
            ReheatingCoilName = Alphas(16);
            Furnace(FurnaceNum).SuppHeatCoilType = ReheatingCoilType;
            Furnace(FurnaceNum).SuppHeatCoilName = ReheatingCoilName;
            errFlag = false;
            if (!lAlphaBlanks(15)) {
                if (UtilityRoutines::SameString(ReheatingCoilType, "Coil:Heating:Fuel") ||
                    UtilityRoutines::SameString(ReheatingCoilType, "Coil:Heating:Electric") ||
                    UtilityRoutines::SameString(ReheatingCoilType, "Coil:Heating:Desuperheater")) {

                    Furnace(FurnaceNum).SuppHeatCoilType_Num = GetHeatingCoilTypeNum(ReheatingCoilType, ReheatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    } else {

                        ValidateComponent(ReheatingCoilType, ReheatingCoilName, IsNotOK, CurrentModuleObject);
                        if (IsNotOK) {
                            ShowContinueError("In " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                            ErrorsFound = true;

                        } else { // mine data from reheat coil

                            // Get the heating coil index
                            GetHeatingCoilIndex(ReheatingCoilName, Furnace(FurnaceNum).SuppHeatCoilIndex, IsNotOK);
                            if (IsNotOK) {
                                ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                                ErrorsFound = true;
                            }

                            // Get the design supplemental heating capacity
                            errFlag = false;
                            Furnace(FurnaceNum).DesignSuppHeatingCapacity = GetHeatingCoilCapacity(ReheatingCoilType, ReheatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                                ErrorsFound = true;
                            }

                            // Get the Reheat Coil Inlet Node
                            errFlag = false;
                            ReheatCoilInletNode = GetHeatingCoilInletNode(ReheatingCoilType, ReheatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("...occurs in " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                                ErrorsFound = true;
                            }

                            // Get the Reheat Coil Outlet Node
                            errFlag = false;
                            ReheatCoilOutletNode = GetHeatingCoilOutletNode(ReheatingCoilType, ReheatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("...occurs in " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                                ErrorsFound = true;
                            }

                        } // IF (IsNotOK) THEN
                    }

                } else if (UtilityRoutines::SameString(ReheatingCoilType, "Coil:Heating:Water")) {
                    Furnace(FurnaceNum).SuppHeatCoilType_Num = Coil_HeatingWater;
                    ValidateComponent(ReheatingCoilType, ReheatingCoilName, IsNotOK, CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    } else { // mine data from heating coil object

                        // Get the Heating Coil water Inlet or control Node number
                        errFlag = false;
                        Furnace(FurnaceNum).SuppCoilControlNode = GetCoilWaterInletNode("Coil:Heating:Water", ReheatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the ReHeat Coil hot water max volume flow rate
                        errFlag = false;
                        Furnace(FurnaceNum).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate("Coil:Heating:Water", ReheatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the ReHeat Coil Inlet Node
                        errFlag = false;
                        ReheatCoilInletNode = GetWaterCoilInletNode("Coil:Heating:Water", ReheatingCoilName, errFlag);
                        Furnace(FurnaceNum).SuppCoilAirInletNode = ReheatCoilInletNode;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the ReHeat Coil Outlet Node
                        errFlag = false;
                        ReheatCoilOutletNode = GetWaterCoilOutletNode("Coil:Heating:Water", ReheatingCoilName, errFlag);
                        Furnace(FurnaceNum).SuppCoilAirOutletNode = ReheatCoilOutletNode;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                            ErrorsFound = true;
                        }

                        // check if user has also used a water coil controller, which they should not do
                        errFlag = false;
                        CheckCoilWaterInletNode(Furnace(FurnaceNum).CoilControlNode, errFlag);
                        if (!errFlag) { // then did find a controller so that is bad
                            ShowSevereError(CurrentModuleObject + " = " + Furnace(FurnaceNum).Name +
                                            " has a conflicting Controller:WaterCoil object");
                            ShowContinueError("Hot water coils are controlled directly by unitary and furnace systems.");
                            ShowContinueError("No water coil controller should be input for the coil.");
                            ErrorsFound = true;
                        }
                    }

                } else if (UtilityRoutines::SameString(ReheatingCoilType, "Coil:Heating:Steam")) {
                    Furnace(FurnaceNum).SuppHeatCoilType_Num = Coil_HeatingSteam;
                    ValidateComponent(ReheatingCoilType, ReheatingCoilName, IsNotOK, CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        Furnace(FurnaceNum).SuppHeatCoilIndex = GetSteamCoilIndex("COIL:HEATING:STEAM", ReheatingCoilName, errFlag);
                        if (Furnace(FurnaceNum).SuppHeatCoilIndex == 0) {
                            ShowSevereError(CurrentModuleObject + " illegal " + cAlphaFields(11) + " = " + ReheatingCoilName);
                            ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the Heating Coil steam inlet node number
                        errFlag = false;
                        Furnace(FurnaceNum).SuppCoilControlNode = GetCoilSteamInletNode("Coil:Heating:Steam", ReheatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the Heating Coil steam max volume flow rate
                        Furnace(FurnaceNum).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).SuppHeatCoilIndex, errFlag);
                        if (Furnace(FurnaceNum).MaxSuppCoilFluidFlow > 0.0) {
                            SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getAirLoopHVACHeatCoolInput);
                            Furnace(FurnaceNum).MaxSuppCoilFluidFlow =
                                GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).SuppHeatCoilIndex, errFlag) * SteamDensity;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        ReheatCoilInletNode = GetSteamCoilAirInletNode(Furnace(FurnaceNum).SuppHeatCoilIndex, ReheatingCoilName, errFlag);
                        Furnace(FurnaceNum).SuppCoilAirInletNode = ReheatCoilInletNode;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                            ErrorsFound = true;
                        }

                        // Get the Heating Coil Outlet Node
                        errFlag = false;
                        ReheatCoilOutletNode = GetCoilAirOutletNode(Furnace(FurnaceNum).SuppHeatCoilIndex, ReheatingCoilName, errFlag);
                        Furnace(FurnaceNum).SuppCoilAirOutletNode = ReheatCoilOutletNode;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                            ErrorsFound = true;
                        }
                    }

                } else { // Illeagal heating coil
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("Illegal " + cAlphaFields(15) + " = " + Alphas(15));
                    ErrorsFound = true;
                } // IF (Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingGasOrOtherFuel .OR. &, etc.

            } // IF(.NOT. lAlphaBlanks(15))THEN

            if (Furnace(FurnaceNum).FanPlace == BlowThru) {

                if (FanInletNode != Furnace(FurnaceNum).FurnaceInletNodeNum) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                    if (FurnaceType_Num == Furnace_HeatCool) {
                        ShowContinueError(
                            "When a blow through fan is specified, the fan inlet node name must be the same as the furnace inlet node name.");
                        ShowContinueError("...Fan inlet node name     = " + NodeID(FanInletNode));
                        ShowContinueError("...Furnace inlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                    } else {
                        ShowContinueError(
                            "When a blow through fan is specified, the fan inlet node name must be the same as the unitary system inlet node name.");
                        ShowContinueError("...Fan inlet node name           = " + NodeID(FanInletNode));
                        ShowContinueError("...UnitarySystem inlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                    }
                    ErrorsFound = true;
                }
                if (Furnace(FurnaceNum).CoolingCoilUpstream) {
                    if (FanOutletNode != CoolingCoilInletNode) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError(
                            "When a blow through fan is specified, the fan outlet node name must be the same as the cooling coil inlet node name.");
                        ShowContinueError("...Fan outlet node name         = " + NodeID(FanOutletNode));
                        ShowContinueError("...Cooling coil inlet node name = " + NodeID(CoolingCoilInletNode));
                        ErrorsFound = true;
                    }
                    if (CoolingCoilOutletNode != HeatingCoilInletNode) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError("The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                        ShowContinueError("...Cooling coil outlet node name = " + NodeID(CoolingCoilOutletNode));
                        ShowContinueError("...Heating coil inlet node name  = " + NodeID(HeatingCoilInletNode));
                        ErrorsFound = true;
                    }
                    if ((Furnace(FurnaceNum).Humidistat && Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat) ||
                        ReheatCoilInletNode > 0) {
                        if (HeatingCoilOutletNode != ReheatCoilInletNode) {
                            ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                            ShowContinueError("When a blow through fan is specified, the heating coil outlet node name must be the same as the "
                                              "reheat coil inlet node name.");
                            ShowContinueError("...Heating coil outlet node name = " + NodeID(HeatingCoilOutletNode));
                            ShowContinueError("...Reheat coil inlet node name   = " + NodeID(ReheatCoilInletNode));
                            ErrorsFound = true;
                        }
                        if (ReheatCoilOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                            ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                            if (FurnaceType_Num == Furnace_HeatCool) {
                                ShowContinueError("The reheat coil outlet node name must be the same as the furnace outlet node name.");
                                ShowContinueError("...Reheat coil outlet node name = " + NodeID(ReheatCoilOutletNode));
                                ShowContinueError("...Furnace outlet node name     = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                            } else {
                                ShowContinueError("The reheat coil outlet node name must be the same as the unitary system outlet node name.");
                                ShowContinueError("...Reheat coil outlet node name   = " + NodeID(ReheatCoilOutletNode));
                                ShowContinueError("...UnitarySystem outlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                            }
                            ErrorsFound = true;
                        }
                    } else { // IF((Furnace(FurnaceNum)%Humidistat ...
                        // Heating coil outlet node name must be the same as the furnace outlet node name
                        if (HeatingCoilOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                            ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                            if (FurnaceType_Num == Furnace_HeatOnly) {
                                ShowContinueError("When a blow through fan is specified, the heating coil outlet node name must be the same as the "
                                                  "furnace outlet node name.");
                                ShowContinueError("...Heating coil outlet node name = " + NodeID(HeatingCoilOutletNode));
                                ShowContinueError("...Furnace outlet node name      = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                            } else {
                                ShowContinueError("When a blow through fan is specified, the heating coil outlet node name must be the same as the "
                                                  "unitary system outlet node name.");
                                ShowContinueError("...Heating coil outlet node name  = " + NodeID(HeatingCoilOutletNode));
                                ShowContinueError("...UnitarySystem outlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                            }
                            ErrorsFound = true;
                        }
                    }
                } else { // IF(Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
                    if (FanOutletNode != HeatingCoilInletNode) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError(
                            "When a blow through fan is specified, the fan outlet node name must be the same as the heating coil inlet node name.");
                        ShowContinueError("...Fan outlet node name         = " + NodeID(FanOutletNode));
                        ShowContinueError("...Heating coil inlet node name = " + NodeID(HeatingCoilInletNode));
                        ErrorsFound = true;
                    }
                    if (HeatingCoilOutletNode != CoolingCoilInletNode) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError("The heating coil outlet node name must be the same as the cooling coil inlet node name.");
                        ShowContinueError("...Heating coil outlet node name = " + NodeID(HeatingCoilOutletNode));
                        ShowContinueError("...Cooling coil inlet node name  = " + NodeID(CoolingCoilInletNode));
                        ErrorsFound = true;
                    }
                    if (CoolingCoilOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        if (FurnaceType_Num == Furnace_HeatCool) {
                            ShowContinueError("When a blow through fan is specified, the cooling coil outlet node name must be the same as the "
                                              "furnace outlet node name.");
                            ShowContinueError("...Cooling coil outlet node name = " + NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...Furnace outlet node name      = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                        } else {
                            ShowContinueError("When a blow through fan is specified, the cooling coil outlet node name must be the same as the "
                                              "unitary system outlet node name.");
                            ShowContinueError("...Cooling coil outlet node name   = " + NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...UnitarySystem outlet node name  = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                        }
                        ErrorsFound = true;
                    }
                }

            } else { // ELSE from IF(Furnace(FurnaceNum)%FanPlace .EQ. BlowThru)THEN

                if (Furnace(FurnaceNum).CoolingCoilUpstream) {
                    if (CoolingCoilInletNode != Furnace(FurnaceNum).FurnaceInletNodeNum) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        if (FurnaceType_Num == Furnace_HeatCool) {
                            ShowContinueError("When a draw through fan is specified, the cooling coil inlet node name must be the same as the "
                                              "furnace inlet node name.");
                            ShowContinueError("...Cooling coil inlet node name = " + NodeID(CoolingCoilInletNode));
                            ShowContinueError("...Furnace inlet node name      = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                        } else {
                            ShowContinueError("When a draw through fan is specified, the cooling coil inlet node name must be the same as the "
                                              "unitary system inlet node name.");
                            ShowContinueError("...Cooling coil inlet node name  = " + NodeID(CoolingCoilInletNode));
                            ShowContinueError("...UnitarySystem inlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                        }
                        ErrorsFound = true;
                    }
                    if (CoolingCoilOutletNode != HeatingCoilInletNode) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError("The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                        ShowContinueError("...Cooling coil outlet node name = " + NodeID(CoolingCoilOutletNode));
                        ShowContinueError("...Heating coil inlet node name  = " + NodeID(HeatingCoilInletNode));
                        ErrorsFound = true;
                    }
                    if (HeatingCoilOutletNode != FanInletNode) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError(
                            "When a draw through fan is specified, the heating coil outlet node name must be the same as the fan inlet node name.");
                        ShowContinueError("...Heating coil outlet node name = " + NodeID(HeatingCoilOutletNode));
                        ShowContinueError("...Fan inlet node name           = " + NodeID(FanInletNode));
                        ErrorsFound = true;
                    }
                    if ((Furnace(FurnaceNum).Humidistat && Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat) ||
                        ReheatCoilInletNode > 0) {
                        if (FanOutletNode != ReheatCoilInletNode) {
                            ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                            ShowContinueError("When a draw through fan is specified, the fan outlet node name must be the same as the reheat coil "
                                              "inlet node name.");
                            ShowContinueError("...Fan outlet node name        = " + NodeID(FanOutletNode));
                            ShowContinueError("...Reheat coil inlet node name = " + NodeID(ReheatCoilInletNode));
                            ErrorsFound = true;
                        }
                        if (ReheatCoilOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                            ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                            if (FurnaceType_Num == Furnace_HeatCool) {
                                ShowContinueError("The reheat coil outlet node name must be the same as the furnace outlet node name.");
                                ShowContinueError("...Reheat coil outlet node name = " + NodeID(ReheatCoilOutletNode));
                                ShowContinueError("...Furnace outlet node name     = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                            } else {
                                ShowContinueError("The reheat coil outlet node name must be the same as the unitary system outlet node name.");
                                ShowContinueError("...Reheat coil outlet node name   = " + NodeID(ReheatCoilOutletNode));
                                ShowContinueError("...UnitarySystem outlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                            }
                            ErrorsFound = true;
                        }
                    } else {
                        if (FanOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                            ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                            ShowContinueError("When a draw through fan is specified, the fan outlet node name must be the same as the unitary system "
                                              "outlet node name.");
                            ShowContinueError("...Fan outlet node name        = " + NodeID(FanOutletNode));
                            ShowContinueError("...Unitary system outlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                            ErrorsFound = true;
                        }
                    }
                } else { // IF(Furnace(FurnaceNum)%CoolingCoilUpstream)THEN
                    if (HeatingCoilInletNode != Furnace(FurnaceNum).FurnaceInletNodeNum) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        if (FurnaceType_Num == Furnace_HeatCool) {
                            ShowContinueError("When a draw through fan is specified, the heating coil inlet node name must be the same as the "
                                              "furnace inlet node name.");
                            ShowContinueError("...Heating coil inlet node name = " + NodeID(HeatingCoilInletNode));
                            ShowContinueError("...Furnace inlet node name      = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                        } else {
                            ShowContinueError("When a draw through fan is specified, the heating coil inlet node name must be the same as the "
                                              "unitary system inlet node name.");
                            ShowContinueError("...Heating coil inlet node name  = " + NodeID(HeatingCoilInletNode));
                            ShowContinueError("...UnitarySystem inlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                        }
                        ErrorsFound = true;
                    }
                    if (HeatingCoilOutletNode != CoolingCoilInletNode) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError("The heating coil outlet node name must be the same as the cooling coil inlet node name.");
                        ShowContinueError("...Heating coil outlet node name = " + NodeID(HeatingCoilOutletNode));
                        ShowContinueError("...Cooling coil inlet node name  = " + NodeID(CoolingCoilInletNode));
                        ErrorsFound = true;
                    }
                    if (CoolingCoilOutletNode != FanInletNode) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError(
                            "When a draw through fan is specified, the cooling coil outlet node name must be the same as the fan inlet node name.");
                        ShowContinueError("...Cooling coil outlet node name = " + NodeID(CoolingCoilOutletNode));
                        ShowContinueError("...Fan inlet node name           = " + NodeID(FanInletNode));
                        ErrorsFound = true;
                    }
                    if (FanOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                        ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                        if (FurnaceType_Num == Furnace_HeatCool) {
                            ShowContinueError(
                                "When a draw through fan is specified, the fan outlet node name must be the same as the furnace outlet node name.");
                            ShowContinueError("...Fan outlet node name     = " + NodeID(FanOutletNode));
                            ShowContinueError("...Furnace outlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                        } else {
                            ShowContinueError("When a draw through fan is specified, the fan outlet node name must be the same as the unitary system "
                                              "outlet node name.");
                            ShowContinueError("...Fan outlet node name           = " + NodeID(FanOutletNode));
                            ShowContinueError("...UnitarySystem outlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                        }
                        ErrorsFound = true;
                    }
                }
            } // ELSE from IF(Furnace(FurnaceNum)%FanPlace .EQ. BlowThru)THEN

            // Add fan to component sets array
            SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(7), Alphas(8), NodeID(FanInletNode), NodeID(FanOutletNode));

            // Add DX cooling coil to component sets array
            if (Furnace(FurnaceNum).bIsIHP) {
                SetUpCompSets(CurrentModuleObject,
                              Alphas(1),
                              Alphas(12),
                              Alphas(13) + " Cooling Coil",
                              NodeID(CoolingCoilInletNode),
                              NodeID(CoolingCoilOutletNode));
            } else {
                SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(12), Alphas(13), NodeID(CoolingCoilInletNode), NodeID(CoolingCoilOutletNode));
            }

            // Add heating coil to component sets array
            if (Furnace(FurnaceNum).bIsIHP) {
                SetUpCompSets(CurrentModuleObject,
                              Alphas(1),
                              Alphas(10),
                              Alphas(11) + " Heating Coil",
                              NodeID(HeatingCoilInletNode),
                              NodeID(HeatingCoilOutletNode));
            } else {
                SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(10), Alphas(11), NodeID(HeatingCoilInletNode), NodeID(HeatingCoilOutletNode));
            }

            if (ReheatCoilInletNode > 0) {

                // Add reheating coil to component sets array
                SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(15), Alphas(16), NodeID(ReheatCoilInletNode), NodeID(ReheatCoilOutletNode));
            }

            // Set the furnace max outlet temperature
            Furnace(FurnaceNum).DesignMaxOutletTemp = Numbers(1);

            Furnace(FurnaceNum).MaxCoolAirVolFlow = Numbers(2);
            if (Furnace(FurnaceNum).MaxCoolAirVolFlow <= 0 && Furnace(FurnaceNum).MaxCoolAirVolFlow != AutoSize) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cNumericFields(2) + " = " + TrimSigDigits(Numbers(2), 7));
                ErrorsFound = true;
            }

            Furnace(FurnaceNum).MaxHeatAirVolFlow = Numbers(3);
            if (Furnace(FurnaceNum).MaxHeatAirVolFlow <= 0 && Furnace(FurnaceNum).MaxHeatAirVolFlow != AutoSize) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cNumericFields(3) + " = " + TrimSigDigits(Numbers(3), 7));
                ErrorsFound = true;
            }

            Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow = Numbers(4);
            if (Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow < 0 && Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow != AutoSize) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cNumericFields(4) + " = " + TrimSigDigits(Numbers(4), 7));
                ErrorsFound = true;
            }

            if (Numbers(2) != AutoSize && Numbers(3) != AutoSize && Numbers(4) != AutoSize) {
                Furnace(FurnaceNum).DesignFanVolFlowRate = max(Numbers(2), Numbers(3), Numbers(4));
            } else {
                Furnace(FurnaceNum).DesignFanVolFlowRate = AutoSize;
            }

            if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
                errFlag = false;
                if (Furnace(FurnaceNum).bIsIHP) {
                    Furnace(FurnaceNum).CoolingCoilIndex = GetCoilIndexIHP(CoolingCoilType, CoolingCoilName, errFlag);
                    IHPCoilName = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SCCoilName;
                    Furnace(FurnaceNum).MaxCoolAirVolFlow = GetCoilAirFlowRateVariableSpeed("COIL:COOLING:DX:VARIABLESPEED", IHPCoilName, errFlag);
                } else {
                    Furnace(FurnaceNum).MaxCoolAirVolFlow = GetCoilAirFlowRateVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                }

                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }

                Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow = min(Furnace(FurnaceNum).MaxHeatAirVolFlow, Furnace(FurnaceNum).MaxCoolAirVolFlow);
                if (Furnace(FurnaceNum).MaxHeatAirVolFlow != AutoSize && Furnace(FurnaceNum).MaxCoolAirVolFlow != AutoSize) {
                    Furnace(FurnaceNum).DesignFanVolFlowRate = max(Furnace(FurnaceNum).MaxHeatAirVolFlow, Furnace(FurnaceNum).MaxCoolAirVolFlow);
                } else {
                    Furnace(FurnaceNum).DesignFanVolFlowRate = AutoSize;
                }
            }

            if (FanVolFlowRate != AutoSize) {
                if (FanVolFlowRate < Furnace(FurnaceNum).MaxCoolAirVolFlow && Furnace(FurnaceNum).MaxCoolAirVolFlow != AutoSize) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("... air flow rate = " + TrimSigDigits(FanVolFlowRate, 7) + " in fan object " + FanName +
                                      " is less than the maximum HVAC system air flow rate in cooling mode.");
                    ShowContinueError(" The " + cNumericFields(2) + " is reset to the fan flow rate and the simulation continues.");
                    Furnace(FurnaceNum).MaxCoolAirVolFlow = FanVolFlowRate;
                    Furnace(FurnaceNum).DesignFanVolFlowRate = FanVolFlowRate;
                }
                if (FanVolFlowRate < Furnace(FurnaceNum).MaxHeatAirVolFlow && Furnace(FurnaceNum).MaxHeatAirVolFlow != AutoSize) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("... air flow rate = " + TrimSigDigits(FanVolFlowRate, 7) + " in fan object " + FanName +
                                      " is less than the maximum HVAC system air flow rate in heating mode.");
                    ShowContinueError(" The " + cNumericFields(3) + " is reset to the fan flow rate and the simulation continues.");
                    Furnace(FurnaceNum).MaxHeatAirVolFlow = FanVolFlowRate;
                    Furnace(FurnaceNum).DesignFanVolFlowRate = FanVolFlowRate;
                }
            }

            if (Furnace(FurnaceNum).FanSchedPtr > 0) {
                if (!CheckScheduleValueMinMax(Furnace(FurnaceNum).FanSchedPtr, ">=", 0.0, "<=", 0.0)) {
                    //           set air flow control mode:
                    //             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
                    //             UseCompressorOffFlow = operate at value specified by user
                    //           AirFlowControl only valid if fan opmode = ContFanCycComp
                    if (Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow == 0.0) {
                        Furnace(FurnaceNum).AirFlowControl = UseCompressorOnFlow;
                    } else {
                        Furnace(FurnaceNum).AirFlowControl = UseCompressorOffFlow;
                    }
                }
            }

            if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
                errFlag = false;
                if (Furnace(FurnaceNum).bIsIHP) {
                    Furnace(FurnaceNum).CoolingCoilIndex = GetCoilIndexIHP(CoolingCoilType, CoolingCoilName, errFlag);
                    IHPCoilName = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SCCoilName;
                    Furnace(FurnaceNum).DesignCoolingCapacity = GetCoilCapacityVariableSpeed("COIL:COOLING:DX:VARIABLESPEED", IHPCoilName, errFlag);
                } else {
                    Furnace(FurnaceNum).DesignCoolingCapacity = GetCoilCapacityVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                }

                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }
            }

            // Set heating convergence tolerance
            Furnace(FurnaceNum).HeatingConvergenceTolerance = 0.001;

            // Set cooling convergence tolerance
            Furnace(FurnaceNum).CoolingConvergenceTolerance = 0.001;

            // set minimum outdoor temperature for compressor operation
            SetMinOATCompressor(
                FurnaceNum, Alphas(1), cCurrentModuleObject, CoolingCoilType, CoolingCoilName, HeatingCoilType, HeatingCoilName, ErrorsFound);

        } // End of the HeatCool Furnace Loop

        // Get the data for the Unitary System HeatPump AirToAir (UnitarySystem:HeatPump:AirToAir)
        for (HeatPumpNum = 1; HeatPumpNum <= NumHeatPump; ++HeatPumpNum) {

            CurrentModuleObject = "AirLoopHVAC:UnitaryHeatPump:AirToAir";
            FanInletNode = 0;
            FanOutletNode = 0;
            CoolingCoilInletNode = 0;
            CoolingCoilOutletNode = 0;
            HeatingCoilInletNode = 0;
            HeatingCoilOutletNode = 0;
            SupHeatCoilInletNode = 0;
            SupHeatCoilOutletNode = 0;
            CoolingCoilType = ' ';
            CoolingCoilName = ' ';
            HeatingCoilType = ' ';
            HeatingCoilName = ' ';

            FurnaceNum = NumHeatOnly + NumHeatCool + NumUnitaryHeatOnly + NumUnitaryHeatCool + HeatPumpNum;
            Furnace(FurnaceNum).iterationMode.allocate(20);

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          HeatPumpNum,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            GlobalNames::VerifyUniqueInterObjectName(UniqueFurnaceNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);

            Furnace(FurnaceNum).FurnaceType_Num = UnitarySys_HeatPump_AirToAir;
            Furnace(FurnaceNum).Name = Alphas(1);
            if (lAlphaBlanks(2)) {
                Furnace(FurnaceNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                Furnace(FurnaceNum).SchedPtr = GetScheduleIndex(Alphas(2));
                if (Furnace(FurnaceNum).SchedPtr == 0) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("Illegal " + cAlphaFields(2) + " = " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            Furnace(FurnaceNum).FurnaceInletNodeNum =
                GetOnlySingleNode(Alphas(3), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent);

            Furnace(FurnaceNum).FurnaceOutletNodeNum =
                GetOnlySingleNode(Alphas(4), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent);

            TestCompSet(CurrentModuleObject, Alphas(1), Alphas(3), Alphas(4), "Air Nodes");

            // Get the Controlling Zone or Location of the Furnace Thermostat
            Furnace(FurnaceNum).ControlZoneNum = UtilityRoutines::FindItemInList(Alphas(5), Zone);
            if (Furnace(FurnaceNum).ControlZoneNum == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(5) + " = " + Alphas(5));
                ErrorsFound = true;
            }

            // Get the node number for the zone with the thermostat
            if (Furnace(FurnaceNum).ControlZoneNum > 0) {
                AirNodeFound = false;
                AirLoopFound = false;
                for (ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum) {
                    if (ZoneEquipConfig(ControlledZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                    //             Find the controlled zone number for the specified thermostat location
                    Furnace(FurnaceNum).NodeNumOfControlledZone = ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                    //             Determine if furnace is on air loop served by the thermostat location specified
                    for (int zoneInNode = 1; zoneInNode <= ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                        int AirLoopNumber = ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode);
                        if (AirLoopNumber > 0) {
                            for (BranchNum = 1; BranchNum <= PrimaryAirSystem(AirLoopNumber).NumBranches; ++BranchNum) {
                                for (CompNum = 1; CompNum <= PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).TotalComponents; ++CompNum) {
                                    if (!UtilityRoutines::SameString(PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).Comp(CompNum).Name,
                                                                     Alphas(1)) ||
                                        !UtilityRoutines::SameString(PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).Comp(CompNum).TypeOf,
                                                                     CurrentModuleObject))
                                        continue;
                                    AirLoopFound = true;
                                    Furnace(FurnaceNum).ZoneInletNode = ZoneEquipConfig(ControlledZoneNum).InletNode(zoneInNode);
                                    break;
                                }
                                if (AirLoopFound) break;
                            }
                            for (TstatZoneNum = 1; TstatZoneNum <= NumTempControlledZones; ++TstatZoneNum) {
                                if (TempControlledZone(TstatZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                                AirNodeFound = true;
                            }
                            for (TstatZoneNum = 1; TstatZoneNum <= NumComfortControlledZones; ++TstatZoneNum) {
                                if (ComfortControlledZone(TstatZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                                AirNodeFound = true;
                            }
                        }
                        if (AirLoopFound) break;
                    }
                    if (AirLoopFound) break;
                }
                if (!AirNodeFound) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("Did not find air node (zone with thermostat).");
                    ShowContinueError("Specified " + cAlphaFields(5) + " = " + Alphas(5));
                    ShowContinueError(
                        "Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object must be specified for this zone.");
                    ErrorsFound = true;
                }
                if (!AirLoopFound) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowSevereError("Did not find correct AirLoopHVAC.");
                    ShowContinueError("Specified " + cAlphaFields(5) + " = " + Alphas(5));
                    ErrorsFound = true;
                }
            }

            // Get fan data
            FanType = Alphas(6);
            FanName = Alphas(7);

            errFlag = false;
            GetFanType(FanName, Furnace(FurnaceNum).FanType_Num, errFlag, CurrentModuleObject, Alphas(1));
            if (errFlag) {
                ErrorsFound = true;
            }

            if (Furnace(FurnaceNum).FanType_Num == FanType_SimpleOnOff || Furnace(FurnaceNum).FanType_Num == FanType_SimpleConstVolume) {
                ValidateComponent(FanType, FanName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;

                } else { // mine data from fan object

                    // Get the fan index
                    errFlag = false;
                    GetFanIndex(FanName, Furnace(FurnaceNum).FanIndex, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the fan inlet node number
                    errFlag = false;
                    FanInletNode = GetFanInletNode(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the fan outlet node number
                    errFlag = false;
                    FanOutletNode = GetFanOutletNode(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the fan availability schedule
                    errFlag = false;
                    Furnace(FurnaceNum).FanAvailSchedPtr = GetFanAvailSchPtr(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the Design Fan Volume Flow Rate
                    errFlag = false;
                    FanVolFlowRate = GetFanDesignVolumeFlowRate(FanType, FanName, errFlag);
                    Furnace(FurnaceNum).ActualFanVolFlowRate = FanVolFlowRate;
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                } // IF (IsNotOK) THEN

            } else {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(6) + " = " + Alphas(6));
                ErrorsFound = true;
            }

            // Get heating coil type and name data
            HeatingCoilType = Alphas(8);
            HeatingCoilName = Alphas(9);

            errFlag = false;

            if (UtilityRoutines::SameString(HeatingCoilType, "COIL:HEATING:DX:VARIABLESPEED") ||
                UtilityRoutines::SameString(HeatingCoilType, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE")) {
                Furnace(FurnaceNum).HeatingCoilType_Num = Coil_HeatingAirToAirVariableSpeed;
                if (UtilityRoutines::SameString(HeatingCoilType, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE")) Furnace(FurnaceNum).bIsIHP = true;
            } else {
                Furnace(FurnaceNum).HeatingCoilType_Num = GetCoilTypeNum(HeatingCoilType, HeatingCoilName, errFlag);
            }

            if (errFlag) {
                ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            if (Furnace(FurnaceNum).HeatingCoilType_Num == CoilDX_HeatingEmpirical) {
                ValidateComponent(HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;

                } else { // mine data from DX heating coil

                    GetDXCoilIndex(HeatingCoilName, Furnace(FurnaceNum).HeatingCoilIndex, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError("...occurs " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil Node Names
                    errFlag = false;
                    HeatingCoilInletNode = GetDXCoilInletNode(HeatingCoilType, HeatingCoilName, errFlag);
                    HeatingCoilOutletNode = GetDXCoilOutletNode(HeatingCoilType, HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the design heating capacity
                    errFlag = false;
                    Furnace(FurnaceNum).DesignHeatingCapacity = GetDXCoilCapacity(HeatingCoilType, HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " =" + Alphas(1));
                        ErrorsFound = true;
                    }

                } // IF (IsNotOK) THEN
            } else if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) {
                ValidateComponent(HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {
                    if (Furnace(FurnaceNum).bIsIHP) {
                        Furnace(FurnaceNum).HeatingCoilIndex = GetCoilIndexIHP(HeatingCoilType, HeatingCoilName, errFlag);
                        IHPCoilIndex = Furnace(FurnaceNum).HeatingCoilIndex;
                        IHPCoilName = IntegratedHeatPumps(IHPCoilIndex).SHCoilName;
                        HeatingCoilInletNode = GetCoilInletNodeVariableSpeed("COIL:HEATING:DX:VARIABLESPEED", IHPCoilName, errFlag);
                        HeatingCoilOutletNode = GetCoilOutletNodeVariableSpeed("COIL:HEATING:DX:VARIABLESPEED", IHPCoilName, errFlag);
                    } else {
                        Furnace(FurnaceNum).HeatingCoilIndex = GetCoilIndexVariableSpeed(HeatingCoilType, HeatingCoilName, errFlag);
                        HeatingCoilInletNode = GetCoilInletNodeVariableSpeed(HeatingCoilType, HeatingCoilName, errFlag);
                        HeatingCoilOutletNode = GetCoilOutletNodeVariableSpeed(HeatingCoilType, HeatingCoilName, errFlag);
                    }
                }
            } else {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(8) + " = " + Alphas(8));
                ErrorsFound = true;
            }

            // Get Cooling Coil Information if available
            CoolingCoilType = Alphas(10);
            CoolingCoilName = Alphas(11);

            if (UtilityRoutines::SameString(CoolingCoilType, "COIL:COOLING:DX:VARIABLESPEED") ||
                UtilityRoutines::SameString(CoolingCoilType, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE")) {
                Furnace(FurnaceNum).CoolingCoilType_Num = Coil_CoolingAirToAirVariableSpeed;
                if (UtilityRoutines::SameString(CoolingCoilType, "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE")) Furnace(FurnaceNum).bIsIHP = true;
            }

            ValidateComponent(CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject);

            if (IsNotOK) {
                ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;

            } else { // mine data from DX cooling coil

                errFlag = false;
                PrintMessage = false;

                if (Furnace(FurnaceNum).CoolingCoilType_Num != Coil_CoolingAirToAirVariableSpeed) {
                    Furnace(FurnaceNum).CoolingCoilType_Num = GetCoilTypeNum(CoolingCoilType, CoolingCoilName, errFlag, PrintMessage);
                }

                // If coil type not found, check to see if a HX assisted cooling coil is used.
                if (Furnace(FurnaceNum).CoolingCoilType_Num == 0) {
                    errFlag = false;
                    PrintMessage = false;
                    Furnace(FurnaceNum).CoolingCoilType_Num = GetHXAssistedCoilTypeNum(CoolingCoilType, CoolingCoilName, errFlag, PrintMessage);
                }

                if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) {

                    // Get the cooling coil node numbers
                    errFlag = false;
                    GetDXCoilIndex(CoolingCoilName, Furnace(FurnaceNum).CoolingCoilIndex, errFlag);
                    CoolingCoilInletNode = GetDXCoilInletNode(CoolingCoilType, CoolingCoilName, errFlag);
                    CoolingCoilOutletNode = GetDXCoilOutletNode(CoolingCoilType, CoolingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the DX cooling coil design capacity
                    errFlag = false;
                    Furnace(FurnaceNum).DesignCoolingCapacity = GetDXCoilCapacity(CoolingCoilType, CoolingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                } else if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {

                    // Get the cooling coil node numbers
                    errFlag = false;
                    GetHXDXCoilIndex(CoolingCoilName, Furnace(FurnaceNum).CoolingCoilIndex, errFlag);
                    CoolingCoilInletNode = GetDXHXAsstdCoilInletNode(CoolingCoilType, CoolingCoilName, errFlag);
                    CoolingCoilOutletNode = GetDXHXAsstdCoilOutletNode(CoolingCoilType, CoolingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // Get the heat exchanger assisted cooling coil design capacity
                    errFlag = false;
                    Furnace(FurnaceNum).DesignCoolingCapacity = GetDXHXAsstdCoilCapacity(CoolingCoilType, CoolingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }

                    // get the actual index to the DX cooling coil object
                    DXCoilIndex = GetActualDXCoilIndex(CoolingCoilType, CoolingCoilName, ErrorsFound);
                    Furnace(FurnaceNum).ActualDXCoilIndexForHXAssisted = DXCoilIndex;

                } else if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
                    // BOS ADDED, AUG/2012, VARIIABLE SPEED DX COOLING COIL
                    //  Furnace(FurnaceNum)%DXCoolCoilType = 'COIL:COOLING:DX:VARIABLESPEED'
                    //  Furnace(FurnaceNum)%DXCoolCoilName = CoolingCoilName
                    ValidateComponent(CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                        ErrorsFound = true;
                    } else {
                        errFlag = false;
                        if (Furnace(FurnaceNum).bIsIHP) {
                            Furnace(FurnaceNum).CoolingCoilIndex = GetCoilIndexIHP(CoolingCoilType, CoolingCoilName, errFlag);
                            IHPCoilName = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SCCoilName;
                        } else {
                            Furnace(FurnaceNum).CoolingCoilIndex = GetCoilIndexVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                            IHPCoilName = CoolingCoilName;
                        }

                        if (errFlag) {
                            ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + Alphas(1) + "\".");
                            ErrorsFound = true;
                        }

                        if (Furnace(FurnaceNum).bIsIHP) {
                            CoolingCoilInletNode = GetCoilInletNodeVariableSpeed("COIL:COOLING:DX:VARIABLESPEED", IHPCoilName, errFlag);
                            CoolingCoilOutletNode = GetCoilOutletNodeVariableSpeed("COIL:COOLING:DX:VARIABLESPEED", IHPCoilName, errFlag);
                            Furnace(FurnaceNum).CondenserNodeNum = GetVSCoilCondenserInletNode(IHPCoilName, errFlag);
                        } else {
                            CoolingCoilInletNode = GetCoilInletNodeVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                            CoolingCoilOutletNode = GetCoilOutletNodeVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                            Furnace(FurnaceNum).CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName, errFlag);
                        }

                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }
                    }
                } else {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("Illegal " + cAlphaFields(10) + " = " + Alphas(10));
                    ErrorsFound = true;
                }
            }

            if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed &&
                Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) {
                // Furnace(FurnaceNum)%WatertoAirHPType = WatertoAir_VarSpeedEquationFit
                if (Furnace(FurnaceNum).bIsIHP) {
                    SetVarSpeedCoilData(IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SCCoilIndex,
                                        ErrorsFound,
                                        _,
                                        IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SHCoilIndex);
                } else {
                    SetVarSpeedCoilData(Furnace(FurnaceNum).CoolingCoilIndex, ErrorsFound, _, Furnace(FurnaceNum).HeatingCoilIndex);
                }
            }

            // Get supplemental heating coil information
            SuppHeatCoilType = Alphas(12);
            SuppHeatCoilName = Alphas(13);
            Furnace(FurnaceNum).SuppHeatCoilType = SuppHeatCoilType;
            Furnace(FurnaceNum).SuppHeatCoilName = SuppHeatCoilName;
            errFlag = false;
            if (UtilityRoutines::SameString(SuppHeatCoilType, "Coil:Heating:Fuel") ||
                UtilityRoutines::SameString(SuppHeatCoilType, "Coil:Heating:Electric")) {

                Furnace(FurnaceNum).SuppHeatCoilType_Num = GetHeatingCoilTypeNum(SuppHeatCoilType, SuppHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {
                    IsNotOK = false;
                    ValidateComponent(SuppHeatCoilType, SuppHeatCoilName, IsNotOK, CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError("In " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                        ErrorsFound = true;

                    } else { // mine data from the supplemental heating coil

                        GetHeatingCoilIndex(SuppHeatCoilName, Furnace(FurnaceNum).SuppHeatCoilIndex, IsNotOK);
                        if (IsNotOK) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                        // Get the Supplemental Heating Coil Inlet Node Number
                        errFlag = false;
                        SupHeatCoilInletNode = GetHeatingCoilInletNode(SuppHeatCoilType, SuppHeatCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                            ErrorsFound = true;
                        }

                        // Get the Supplemental Heating Coil Outlet Node Number
                        errFlag = false;
                        SupHeatCoilOutletNode = GetHeatingCoilOutletNode(SuppHeatCoilType, SuppHeatCoilName, errFlag);

                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                        // Get the supplemental heating coil design capacity
                        errFlag = false;
                        Furnace(FurnaceNum).DesignSuppHeatingCapacity = GetHeatingCoilCapacity(SuppHeatCoilType, SuppHeatCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                    } // IF (IsNotOK) THEN
                }
            } else if (UtilityRoutines::SameString(SuppHeatCoilType, "Coil:Heating:Water")) {
                Furnace(FurnaceNum).SuppHeatCoilType_Num = Coil_HeatingWater;
                ValidateComponent(SuppHeatCoilType, SuppHeatCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    // Get the Heating Coil water Inlet or control Node number
                    errFlag = false;
                    Furnace(FurnaceNum).SuppCoilControlNode = GetCoilWaterInletNode("Coil:Heating:Water", SuppHeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the ReHeat Coil hot water max volume flow rate
                    errFlag = false;
                    Furnace(FurnaceNum).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate("Coil:Heating:Water", SuppHeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the ReHeat Coil Inlet Node
                    errFlag = false;
                    SupHeatCoilInletNode = GetWaterCoilInletNode("Coil:Heating:Water", SuppHeatCoilName, errFlag);
                    Furnace(FurnaceNum).SuppCoilAirInletNode = SupHeatCoilInletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the ReHeat Coil Outlet Node
                    errFlag = false;
                    SupHeatCoilOutletNode = GetWaterCoilOutletNode("Coil:Heating:Water", SuppHeatCoilName, errFlag);
                    Furnace(FurnaceNum).SuppCoilAirOutletNode = SupHeatCoilOutletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }
                    errFlag = false;
                    CheckCoilWaterInletNode(Furnace(FurnaceNum).CoilControlNode, errFlag);
                    if (!errFlag) { // then did find a controller so that is bad
                        ShowSevereError(CurrentModuleObject + " = " + Furnace(FurnaceNum).Name + " has a conflicting Controller:WaterCoil object");
                        ShowContinueError("Hot water coils are controlled directly by unitary and furnace systems.");
                        ShowContinueError("No water coil controller should be input for the coil.");
                        ErrorsFound = true;
                    }
                }

            } else if (UtilityRoutines::SameString(SuppHeatCoilType, "Coil:Heating:Steam")) {
                Furnace(FurnaceNum).SuppHeatCoilType_Num = Coil_HeatingSteam;
                ValidateComponent(SuppHeatCoilType, SuppHeatCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    errFlag = false;
                    Furnace(FurnaceNum).SuppHeatCoilIndex = GetSteamCoilIndex("COIL:HEATING:STEAM", SuppHeatCoilName, errFlag);
                    if (Furnace(FurnaceNum).SuppHeatCoilIndex == 0) {
                        ShowSevereError(CurrentModuleObject + " illegal " + cAlphaFields(12) + " = " + SuppHeatCoilName);
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil steam inlet node number
                    errFlag = false;
                    Furnace(FurnaceNum).SuppCoilControlNode = GetCoilSteamInletNode("Coil:Heating:Steam", SuppHeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil steam max volume flow rate
                    Furnace(FurnaceNum).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).SuppHeatCoilIndex, errFlag);
                    if (Furnace(FurnaceNum).MaxSuppCoilFluidFlow > 0.0) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getAirLoopHVACHeatCoolInput);
                        Furnace(FurnaceNum).MaxSuppCoilFluidFlow =
                            GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).SuppHeatCoilIndex, errFlag) * SteamDensity;
                    }

                    // Get the Heating Coil Inlet Node
                    errFlag = false;
                    SupHeatCoilInletNode = GetSteamCoilAirInletNode(Furnace(FurnaceNum).SuppHeatCoilIndex, SuppHeatCoilName, errFlag);
                    Furnace(FurnaceNum).SuppCoilAirInletNode = SupHeatCoilInletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil Outlet Node
                    errFlag = false;
                    SupHeatCoilOutletNode = GetCoilAirOutletNode(Furnace(FurnaceNum).SuppHeatCoilIndex, SuppHeatCoilName, errFlag);
                    Furnace(FurnaceNum).SuppCoilAirOutletNode = SupHeatCoilOutletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }
                }

            } else {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(12) + " = " + Alphas(12));
                ErrorsFound = true;
            } // IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingGasOrOtherFuel .OR. &, etc.

            if (UtilityRoutines::SameString(Alphas(14), "BlowThrough")) Furnace(FurnaceNum).FanPlace = BlowThru;
            if (UtilityRoutines::SameString(Alphas(14), "DrawThrough")) Furnace(FurnaceNum).FanPlace = DrawThru;
            if (Furnace(FurnaceNum).FanPlace == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(14) + " = " + Alphas(14));
                ErrorsFound = true;
            }

            Furnace(FurnaceNum).FanSchedPtr = GetScheduleIndex(Alphas(15));
            if (!lAlphaBlanks(15) && Furnace(FurnaceNum).FanSchedPtr == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(15) + " = " + Alphas(15));
                ErrorsFound = true;
            } else if (lAlphaBlanks(15)) {
                Furnace(FurnaceNum).OpMode = CycFanCycCoil;
                if (Furnace(FurnaceNum).FanType_Num != FanType_SimpleOnOff) {
                    ShowSevereError(CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                    ShowContinueError(cAlphaFields(6) + " = " + Alphas(6));
                    ShowContinueError("Fan type must be Fan:OnOff when " + cAlphaFields(15) + " = Blank.");
                    ErrorsFound = true;
                }
            }

            if (Furnace(FurnaceNum).FanType_Num == FanType_SimpleConstVolume) {
                if (Furnace(FurnaceNum).FanSchedPtr > 0) {
                    if (!CheckScheduleValueMinMax(Furnace(FurnaceNum).FanSchedPtr, ">", 0.0, "<=", 1.0)) {
                        ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError("For " + cAlphaFields(7) + " = " + Alphas(7));
                        ShowContinueError("Fan operating mode must be continuous (fan operating mode schedule values > 0).");
                        ShowContinueError("Error found in " + cAlphaFields(15) + " = " + Alphas(15));
                        ShowContinueError("...schedule values must be (>0., <=1.)");
                        ErrorsFound = true;
                    }
                }
            }

            // Dehumidification Control Type
            if (UtilityRoutines::SameString(Alphas(16), "None") || UtilityRoutines::SameString(Alphas(16), "Multimode") ||
                UtilityRoutines::SameString(Alphas(16), "CoolReheat")) {
                AirNodeFound = false;
                if (UtilityRoutines::SameString(Alphas(16), "Multimode")) {
                    Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_Multimode;
                    Furnace(FurnaceNum).Humidistat = true;
                    if (Furnace(FurnaceNum).CoolingCoilType_Num != CoilDX_CoolingHXAssisted) {
                        ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError("Illegal " + cAlphaFields(16) + " = " + Alphas(16));
                        ShowContinueError("Multimode control must be used with a Heat Exchanger Assisted Cooling Coil.");
                        ErrorsFound = true;
                    }
                }
                if (UtilityRoutines::SameString(Alphas(16), "CoolReheat")) {
                    Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_CoolReheat;
                    Furnace(FurnaceNum).Humidistat = true;
                }
                if (UtilityRoutines::SameString(Alphas(16), "None")) {
                    Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_None;
                    Furnace(FurnaceNum).Humidistat = false;
                }
                if (Furnace(FurnaceNum).Humidistat) {
                    for (HStatZoneNum = 1; HStatZoneNum <= NumHumidityControlZones; ++HStatZoneNum) {
                        if (HumidityControlZone(HStatZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                        AirNodeFound = true;
                    }
                    if (!AirNodeFound) {
                        ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError("Did not find Air Node (Zone with Humidistat).");
                        ShowContinueError("Specified " + cAlphaFields(5) + " = " + Alphas(5));
                        ErrorsFound = true;
                    }
                }
            } else { // invalid input or blank
                if (!lAlphaBlanks(16)) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("Illegal " + cAlphaFields(16) + " = " + Alphas(16));
                    ErrorsFound = true;
                } else {
                    Furnace(FurnaceNum).Humidistat = false;
                    Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_None;
                }
            }

            // Check node names for child components
            if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                if (FanInletNode != Furnace(FurnaceNum).FurnaceInletNodeNum) {
                    ShowSevereError("For " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError(
                        "When a blow through fan is specified, the fan inlet node name must be the same as the unitary system inlet node name.");
                    ShowContinueError("...Fan inlet node name            = " + NodeID(FanInletNode));
                    ShowContinueError("...Unitary system inlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                    ErrorsFound = true;
                }
                if (FanOutletNode != CoolingCoilInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError(
                        "When a blow through fan is specified, the fan outlet node name must be the same as the cooling coil inlet node name.");
                    ShowContinueError("...Fan outlet node name         = " + NodeID(FanOutletNode));
                    ShowContinueError("...Cooling coil inlet node name = " + NodeID(CoolingCoilInletNode));
                    ErrorsFound = true;
                }
                if (CoolingCoilOutletNode != HeatingCoilInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                    ShowContinueError("...Cooling coil outlet node name = " + NodeID(CoolingCoilOutletNode));
                    ShowContinueError("...Heating coil inlet node name  = " + NodeID(HeatingCoilInletNode));
                    ErrorsFound = true;
                }
                if (HeatingCoilOutletNode != SupHeatCoilInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("When a blow through fan is specified, the heating coil outlet node name must be the same as the supplemental "
                                      "heating coil inlet node name.");
                    ShowContinueError("...Heating coil outlet node name              = " + NodeID(HeatingCoilOutletNode));
                    ShowContinueError("...Supplemental heating coil inlet node name  = " + NodeID(SupHeatCoilInletNode));
                    ErrorsFound = true;
                }
                if (SupHeatCoilOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                    ShowSevereError("For " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("The supplemental heating coil outlet node name must be the same as the unitary system outlet node name.");
                    ShowContinueError("...Supplemental heating coil outlet node name = " + NodeID(SupHeatCoilOutletNode));
                    ShowContinueError("...Unitary system outlet node name            = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                    ErrorsFound = true;
                }
            } else {
                if (CoolingCoilInletNode != Furnace(FurnaceNum).FurnaceInletNodeNum) {
                    ShowSevereError("For " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("When a draw through fan is specified, the cooling coil inlet node name must be the same as the unitary system "
                                      "inlet node name.");
                    ShowContinueError("...Cooling coil inlet node name   = " + NodeID(CoolingCoilInletNode));
                    ShowContinueError("...Unitary system inlet node name = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                    ErrorsFound = true;
                }
                if (CoolingCoilOutletNode != HeatingCoilInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                    ShowContinueError("...Cooling coil outlet node name = " + NodeID(CoolingCoilOutletNode));
                    ShowContinueError("...Heating coil inlet node name  = " + NodeID(HeatingCoilInletNode));
                    ErrorsFound = true;
                }
                if (HeatingCoilOutletNode != FanInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError(
                        "When a draw through fan is specified, the heating coil outlet node name must be the same as the fan inlet node name.");
                    ShowContinueError("...Heating coil outlet node name = " + NodeID(HeatingCoilOutletNode));
                    ShowContinueError("...Fan inlet node name           = " + NodeID(FanInletNode));
                    ErrorsFound = true;
                }
                if (FanOutletNode != SupHeatCoilInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("When a draw through fan is specified, the fan outlet node name must be the same as the supplemental heating "
                                      "coil inlet node name.");
                    ShowContinueError("...Fan outlet node name                       = " + NodeID(FanOutletNode));
                    ShowContinueError("...Supplemental heating coil inlet node name  = " + NodeID(SupHeatCoilInletNode));
                    ErrorsFound = true;
                }
                if (SupHeatCoilOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                    ShowSevereError("For " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("The supplemental heating coil outlet node name must be the same as the unitary system outlet node name.");
                    ShowContinueError("...Supplemental heating coil outlet node name = " + NodeID(SupHeatCoilOutletNode));
                    ShowContinueError("...Unitary system outlet node name            = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                    ErrorsFound = true;
                }
            }

            // Add component sets array
            if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                CompSetFanInlet = Alphas(3);
                CompSetCoolInlet = "UNDEFINED";
            } else {
                CompSetFanInlet = "UNDEFINED";
                CompSetCoolInlet = Alphas(3);
            }
            SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(6), Alphas(7), CompSetFanInlet, "UNDEFINED");

            // Add DX cooling coil to component sets array
            if (Furnace(FurnaceNum).bIsIHP) {
                SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(10), Alphas(11) + " Cooling Coil", CompSetCoolInlet, "UNDEFINED");
            } else {
                SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(10), Alphas(11), CompSetCoolInlet, "UNDEFINED");
            }
            // Add DX heating coil to component sets array
            if (Furnace(FurnaceNum).bIsIHP) {
                SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(8), Alphas(9) + " Heating Coil", "UNDEFINED", "UNDEFINED");
            } else {
                SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(8), Alphas(9), "UNDEFINED", "UNDEFINED");
            }

            // Add supplemental heating coil to component sets array
            SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(12), Alphas(13), "UNDEFINED", Alphas(4));

            Furnace(FurnaceNum).MaxCoolAirVolFlow = Numbers(1);
            if (Furnace(FurnaceNum).MaxCoolAirVolFlow <= 0 && Furnace(FurnaceNum).MaxCoolAirVolFlow != AutoSize) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cNumericFields(1) + " = " + TrimSigDigits(Numbers(1), 7));
                ErrorsFound = true;
            }

            Furnace(FurnaceNum).MaxHeatAirVolFlow = Numbers(2);
            if (Furnace(FurnaceNum).MaxHeatAirVolFlow <= 0 && Furnace(FurnaceNum).MaxHeatAirVolFlow != AutoSize) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cNumericFields(2) + " = " + TrimSigDigits(Numbers(2), 7));
                ErrorsFound = true;
            }

            Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow = Numbers(3);
            if (Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow < 0 && Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow != AutoSize) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cNumericFields(3) + " = " + TrimSigDigits(Numbers(3), 7));
                ErrorsFound = true;
            }

            if (Furnace(FurnaceNum).FanSchedPtr > 0) {
                if (!CheckScheduleValueMinMax(Furnace(FurnaceNum).FanSchedPtr, ">=", 0.0, "<=", 0.0)) { // Autodesk:Note Range is 0 to 0?
                    //           set air flow control mode:
                    //             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
                    //             UseCompressorOffFlow = operate at value specified by user
                    //           AirFlowControl only valid if fan opmode = ContFanCycComp
                    if (Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow == 0.0) {
                        Furnace(FurnaceNum).AirFlowControl = UseCompressorOnFlow;
                    } else {
                        Furnace(FurnaceNum).AirFlowControl = UseCompressorOffFlow;
                    }
                }
            }

            if (Numbers(1) != AutoSize && Numbers(2) != AutoSize && Numbers(3) != AutoSize) {
                Furnace(FurnaceNum).DesignFanVolFlowRate = max(Numbers(1), Numbers(2), Numbers(3));
            } else {
                Furnace(FurnaceNum).DesignFanVolFlowRate = AutoSize;
            }

            if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) {
                errFlag = false;

                if (Furnace(FurnaceNum).bIsIHP) {
                    IHPCoilName = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SHCoilName;
                    Furnace(FurnaceNum).MaxHeatAirVolFlow = GetCoilAirFlowRateVariableSpeed("COIL:HEATING:DX:VARIABLESPEED", IHPCoilName, errFlag);
                    IHPCoilName = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SCCoilName;
                    Furnace(FurnaceNum).MaxCoolAirVolFlow = GetCoilAirFlowRateVariableSpeed("COIL:COOLING:DX:VARIABLESPEED", IHPCoilName, errFlag);
                } else {
                    Furnace(FurnaceNum).MaxHeatAirVolFlow = GetCoilAirFlowRateVariableSpeed(HeatingCoilType, HeatingCoilName, errFlag);
                    Furnace(FurnaceNum).MaxCoolAirVolFlow = GetCoilAirFlowRateVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                }

                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }

                Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow = min(Furnace(FurnaceNum).MaxHeatAirVolFlow, Furnace(FurnaceNum).MaxCoolAirVolFlow);
                if (Furnace(FurnaceNum).MaxHeatAirVolFlow != AutoSize && Furnace(FurnaceNum).MaxCoolAirVolFlow != AutoSize) {
                    Furnace(FurnaceNum).DesignFanVolFlowRate = max(Furnace(FurnaceNum).MaxHeatAirVolFlow, Furnace(FurnaceNum).MaxCoolAirVolFlow);
                } else {
                    Furnace(FurnaceNum).DesignFanVolFlowRate = AutoSize;
                }
            }

            if (FanVolFlowRate != AutoSize) {
                if (FanVolFlowRate < Furnace(FurnaceNum).MaxCoolAirVolFlow && Furnace(FurnaceNum).MaxCoolAirVolFlow != AutoSize) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("... air flow rate = " + TrimSigDigits(FanVolFlowRate, 7) + " in fan object " + FanName +
                                      " is less than the maximum HVAC system air flow rate in cooling mode.");
                    ShowContinueError(" The " + cNumericFields(1) + " is reset to the fan flow rate and the simulation continues.");
                    Furnace(FurnaceNum).MaxCoolAirVolFlow = FanVolFlowRate;
                    Furnace(FurnaceNum).DesignFanVolFlowRate = FanVolFlowRate;
                }
                if (FanVolFlowRate < Furnace(FurnaceNum).MaxHeatAirVolFlow && Furnace(FurnaceNum).MaxHeatAirVolFlow != AutoSize) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("... air flow rate = " + TrimSigDigits(FanVolFlowRate, 7) + " in fan object " + FanName +
                                      " is less than the maximum HVAC system air flow rate in heating mode.");
                    ShowContinueError(" The " + cNumericFields(2) + " is reset to the fan flow rate and the simulation continues.");
                    Furnace(FurnaceNum).MaxHeatAirVolFlow = FanVolFlowRate;
                    Furnace(FurnaceNum).DesignFanVolFlowRate = FanVolFlowRate;
                }
            }

            // Set heating convergence tolerance
            Furnace(FurnaceNum).HeatingConvergenceTolerance = 0.001;

            //       Mine heatpump outdoor condenser node from DX coil object
            errFlag = false;
            if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) {
                Furnace(FurnaceNum).CondenserNodeNum = GetDXCoilCondenserInletNode(CoolingCoilType, CoolingCoilName, errFlag);
            } else if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
                if (Furnace(FurnaceNum).bIsIHP) {
                    IHPCoilName = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SCCoilName;
                    Furnace(FurnaceNum).CondenserNodeNum = GetVSCoilCondenserInletNode(IHPCoilName, errFlag);
                } else {
                    Furnace(FurnaceNum).CondenserNodeNum = GetVSCoilCondenserInletNode(CoolingCoilName, errFlag);
                }
            } else {
                Furnace(FurnaceNum).CondenserNodeNum =
                    GetDXCoilCondenserInletNode("Coil:Cooling:DX:SingleSpeed", GetHXDXCoilName(CoolingCoilType, CoolingCoilName, errFlag), errFlag);
            }
            if (errFlag) {
                ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) {
                errFlag = false;
                if (Furnace(FurnaceNum).bIsIHP) {
                    IHPCoilName = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SHCoilName;
                    Furnace(FurnaceNum).DesignHeatingCapacity = GetCoilCapacityVariableSpeed("Coil:Heating:DX:VariableSpeed", IHPCoilName, errFlag);
                } else {
                    Furnace(FurnaceNum).DesignHeatingCapacity = GetCoilCapacityVariableSpeed(HeatingCoilType, HeatingCoilName, errFlag);
                }

                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }
            }

            if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
                errFlag = false;
                if (Furnace(FurnaceNum).bIsIHP) {
                    IHPCoilName = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SCCoilName;
                    Furnace(FurnaceNum).DesignCoolingCapacity = GetCoilCapacityVariableSpeed("COIL:COOLING:DX:VARIABLESPEED", IHPCoilName, errFlag);
                } else {
                    Furnace(FurnaceNum).DesignCoolingCapacity = GetCoilCapacityVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                }

                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }
            }

            // Set cooling convergence tolerance
            Furnace(FurnaceNum).CoolingConvergenceTolerance = 0.001;

            // Set the furnace max outlet temperature
            Furnace(FurnaceNum).DesignMaxOutletTemp = Numbers(4);

            // Set maximum supply air temperature for supplemental heating coil
            Furnace(FurnaceNum).MaxOATSuppHeat = Numbers(5);

            // set minimum outdoor temperature for compressor operation
            SetMinOATCompressor(
                FurnaceNum, Alphas(1), cCurrentModuleObject, CoolingCoilType, CoolingCoilName, HeatingCoilType, HeatingCoilName, ErrorsFound);

        } // End of the Unitary System HeatPump Loop

        // Get the Input for the Water to Air Heat Pump (UnitarySystem:HeatPump:WaterToAir)
        for (HeatPumpNum = 1; HeatPumpNum <= NumWaterToAirHeatPump; ++HeatPumpNum) {

            CurrentModuleObject = "AirLoopHVAC:UnitaryHeatPump:WaterToAir";
            FanInletNode = 0;
            FanOutletNode = 0;
            CoolingCoilInletNode = 0;
            CoolingCoilOutletNode = 0;
            HeatingCoilInletNode = 0;
            HeatingCoilOutletNode = 0;
            SupHeatCoilInletNode = 0;
            SupHeatCoilOutletNode = 0;
            CoolingCoilType = ' ';
            CoolingCoilName = ' ';
            HeatingCoilType = ' ';
            HeatingCoilName = ' ';

            FurnaceNum = NumHeatOnly + NumHeatCool + NumUnitaryHeatOnly + NumUnitaryHeatCool + NumHeatPump + HeatPumpNum;
            Furnace(FurnaceNum).iterationMode.allocate(20);

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          HeatPumpNum,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            GlobalNames::VerifyUniqueInterObjectName(UniqueFurnaceNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);

            Furnace(FurnaceNum).FurnaceType_Num = UnitarySys_HeatPump_WaterToAir;
            Furnace(FurnaceNum).Name = Alphas(1);
            if (lAlphaBlanks(2)) {
                Furnace(FurnaceNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                Furnace(FurnaceNum).SchedPtr = GetScheduleIndex(Alphas(2));
                if (Furnace(FurnaceNum).SchedPtr == 0) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("Illegal " + cAlphaFields(2) + " = " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            Furnace(FurnaceNum).FurnaceInletNodeNum =
                GetOnlySingleNode(Alphas(3), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent);

            Furnace(FurnaceNum).FurnaceOutletNodeNum =
                GetOnlySingleNode(Alphas(4), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent);

            TestCompSet(CurrentModuleObject, Alphas(1), Alphas(3), Alphas(4), "Air Nodes");

            // Get the Controlling Zone or Location of the Furnace Thermostat
            Furnace(FurnaceNum).ControlZoneNum = UtilityRoutines::FindItemInList(Alphas(5), Zone);
            if (Furnace(FurnaceNum).ControlZoneNum == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(5) + " = " + Alphas(5));
                ErrorsFound = true;
            }

            // Get the node number for the zone with the thermostat
            if (Furnace(FurnaceNum).ControlZoneNum > 0) {
                AirNodeFound = false;
                AirLoopFound = false;
                for (ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum) {
                    if (ZoneEquipConfig(ControlledZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                    //             Find the controlled zone number for the specified thermostat location
                    Furnace(FurnaceNum).NodeNumOfControlledZone = ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                    //             Determine if furnace is on air loop served by the thermostat location specified
                    for (int zoneInNode = 1; zoneInNode <= ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                        int AirLoopNumber = ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode);
                        if (AirLoopNumber > 0) {
                            for (BranchNum = 1; BranchNum <= PrimaryAirSystem(AirLoopNumber).NumBranches; ++BranchNum) {
                                for (CompNum = 1; CompNum <= PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).TotalComponents; ++CompNum) {
                                    if (!UtilityRoutines::SameString(PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).Comp(CompNum).Name,
                                                                     Alphas(1)) ||
                                        !UtilityRoutines::SameString(PrimaryAirSystem(AirLoopNumber).Branch(BranchNum).Comp(CompNum).TypeOf,
                                                                     CurrentModuleObject))
                                        continue;
                                    AirLoopFound = true;
                                    Furnace(FurnaceNum).ZoneInletNode = ZoneEquipConfig(ControlledZoneNum).InletNode(zoneInNode);
                                    break;
                                }
                                if (AirLoopFound) break;
                            }
                            for (TstatZoneNum = 1; TstatZoneNum <= NumTempControlledZones; ++TstatZoneNum) {
                                if (TempControlledZone(TstatZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                                AirNodeFound = true;
                            }
                            for (TstatZoneNum = 1; TstatZoneNum <= NumComfortControlledZones; ++TstatZoneNum) {
                                if (ComfortControlledZone(TstatZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                                AirNodeFound = true;
                            }
                        }
                        if (AirLoopFound) break;
                    }
                    if (AirLoopFound) break;
                }
                if (!AirNodeFound) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("Did not find air node (zone with thermostat).");
                    ShowContinueError("Specified " + cAlphaFields(5) + " = " + Alphas(5));
                    ShowContinueError(
                        "Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object must be specified for this zone.");
                    ErrorsFound = true;
                }
                if (!AirLoopFound) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowSevereError("Did not find correct AirLoopHVAC.");
                    ShowContinueError("Specified " + cAlphaFields(5) + " = " + Alphas(5));
                    ErrorsFound = true;
                }
            }

            // Get fan data
            FanType = Alphas(6);
            FanName = Alphas(7);
            errFlag = false;
            GetFanType(FanName, Furnace(FurnaceNum).FanType_Num, errFlag, CurrentModuleObject, Alphas(1));
            if (errFlag) {
                ErrorsFound = true;
            }

            if (Furnace(FurnaceNum).FanType_Num == FanType_SimpleOnOff) {
                ValidateComponent(FanType, FanName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {
                    errFlag = false;
                    GetFanIndex(FanName, Furnace(FurnaceNum).FanIndex, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                    errFlag = false;
                    FanInletNode = GetFanInletNode(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                    errFlag = false;
                    FanOutletNode = GetFanOutletNode(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                    errFlag = false;
                    Furnace(FurnaceNum).FanAvailSchedPtr = GetFanAvailSchPtr(FanType, FanName, errFlag);
                    if (errFlag) {
                        ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                        ErrorsFound = true;
                    }
                }
            } else {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(6) + " = " + Alphas(6));
                ErrorsFound = true;
            }

            // Get heating coil type and name data
            if (Alphas(8) == "COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION") {
                HeatingCoilType = Alphas(8);
                Furnace(FurnaceNum).HeatingCoilType_Num = Coil_HeatingWaterToAirHP;
                HeatingCoilName = Alphas(9);
                ValidateComponent(HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {
                    Furnace(FurnaceNum).HeatingCoilIndex = GetWtoAHPCoilIndex(HeatingCoilType, HeatingCoilName, errFlag);
                    HeatingCoilInletNode = GetWtoAHPCoilInletNode(HeatingCoilType, HeatingCoilName, errFlag);
                    HeatingCoilOutletNode = GetWtoAHPCoilOutletNode(HeatingCoilType, HeatingCoilName, errFlag);
                }
            } else if (Alphas(8) == "COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT") {
                HeatingCoilType = Alphas(8);
                Furnace(FurnaceNum).HeatingCoilType_Num = Coil_HeatingWaterToAirHPSimple;
                HeatingCoilName = Alphas(9);
                ValidateComponent(HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {
                    Furnace(FurnaceNum).HeatingCoilIndex = GetWtoAHPSimpleCoilIndex(HeatingCoilType, HeatingCoilName, errFlag);
                    HeatingCoilInletNode = GetWtoAHPSimpleCoilInletNode(HeatingCoilType, HeatingCoilName, errFlag);
                    HeatingCoilOutletNode = GetWtoAHPSimpleCoilOutletNode(HeatingCoilType, HeatingCoilName, errFlag);
                }
            } else if (Alphas(8) == "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT") {
                HeatingCoilType = Alphas(8);
                Furnace(FurnaceNum).HeatingCoilType_Num = Coil_HeatingWaterToAirHPVSEquationFit;
                HeatingCoilName = Alphas(9);
                ValidateComponent(HeatingCoilType, HeatingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {
                    Furnace(FurnaceNum).HeatingCoilIndex = GetCoilIndexVariableSpeed(HeatingCoilType, HeatingCoilName, errFlag);
                    HeatingCoilInletNode = GetCoilInletNodeVariableSpeed(HeatingCoilType, HeatingCoilName, errFlag);
                    HeatingCoilOutletNode = GetCoilOutletNodeVariableSpeed(HeatingCoilType, HeatingCoilName, errFlag);
                }
            } else {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(8) + " = " + Alphas(8));
                ErrorsFound = true;
            }

            // Get Cooling Coil Information if available
            if (Alphas(10) == "COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION") {
                CoolingCoilType = Alphas(10);
                Furnace(FurnaceNum).CoolingCoilType_Num = Coil_CoolingWaterToAirHP;
                CoolingCoilName = Alphas(11);
                ValidateComponent(CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {
                    Furnace(FurnaceNum).CoolingCoilIndex = GetWtoAHPCoilIndex(CoolingCoilType, CoolingCoilName, errFlag);
                    CoolingCoilInletNode = GetWtoAHPCoilInletNode(CoolingCoilType, CoolingCoilName, errFlag);
                    CoolingCoilOutletNode = GetWtoAHPCoilOutletNode(CoolingCoilType, CoolingCoilName, errFlag);
                }
            } else if (Alphas(10) == "COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT") {
                CoolingCoilType = Alphas(10);
                Furnace(FurnaceNum).CoolingCoilType_Num = Coil_CoolingWaterToAirHPSimple;
                CoolingCoilName = Alphas(11);
                ValidateComponent(CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {
                    Furnace(FurnaceNum).CoolingCoilIndex = GetWtoAHPSimpleCoilIndex(CoolingCoilType, CoolingCoilName, errFlag);
                    CoolingCoilInletNode = GetWtoAHPSimpleCoilInletNode(CoolingCoilType, CoolingCoilName, errFlag);
                    CoolingCoilOutletNode = GetWtoAHPSimpleCoilOutletNode(CoolingCoilType, CoolingCoilName, errFlag);
                }
            } else if (Alphas(10) == "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT") {
                CoolingCoilType = Alphas(10);
                Furnace(FurnaceNum).CoolingCoilType_Num = Coil_CoolingWaterToAirHPVSEquationFit;
                CoolingCoilName = Alphas(11);
                ValidateComponent(CoolingCoilType, CoolingCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {
                    Furnace(FurnaceNum).CoolingCoilIndex = GetCoilIndexVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                    CoolingCoilInletNode = GetCoilInletNodeVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                    CoolingCoilOutletNode = GetCoilOutletNodeVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                }
            } else {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(10) + " = " + Alphas(10));
                ErrorsFound = true;
            }

            if (NumAlphas >= 18) {
                // get water flow mode info before CALL SetSimpleWSHPData
                if (UtilityRoutines::SameString(Alphas(18), "Constant")) Furnace(FurnaceNum).WaterCyclingMode = WaterConstant;
                if (UtilityRoutines::SameString(Alphas(18), "Cycling")) Furnace(FurnaceNum).WaterCyclingMode = WaterCycling;
                if (UtilityRoutines::SameString(Alphas(18), "ConstantOnDemand")) Furnace(FurnaceNum).WaterCyclingMode = WaterConstantOnDemand;
                // default to draw through if not specified in input
                if (lAlphaBlanks(18)) Furnace(FurnaceNum).WaterCyclingMode = WaterCycling;
            } else {
                Furnace(FurnaceNum).WaterCyclingMode = WaterCycling;
            }
            if (Furnace(FurnaceNum).WaterCyclingMode == 0) {
                ShowSevereError(CurrentModuleObject + " illegal " + cAlphaFields(18) + " = " + Alphas(18));
                ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                ErrorsFound = true;
            }

            // end get water flow mode info
            if (Alphas(8) == "COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT" && Alphas(10) == "COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT") {
                Furnace(FurnaceNum).WatertoAirHPType = WatertoAir_Simple;
                SetSimpleWSHPData(
                    Furnace(FurnaceNum).CoolingCoilIndex, ErrorsFound, Furnace(FurnaceNum).WaterCyclingMode, _, Furnace(FurnaceNum).HeatingCoilIndex);
            } else if (Alphas(8) == "COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION" &&
                       Alphas(10) == "COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION") {
                Furnace(FurnaceNum).WatertoAirHPType = WatertoAir_ParEst;
            } else if (Alphas(8) == "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT" &&
                       Alphas(10) == "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT") {
                Furnace(FurnaceNum).WatertoAirHPType = WatertoAir_VarSpeedEquationFit;
                SetVarSpeedCoilData(Furnace(FurnaceNum).CoolingCoilIndex, ErrorsFound, _, Furnace(FurnaceNum).HeatingCoilIndex);
            } else {
                ShowContinueError("For " + CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Cooling coil and heating coil should be of same general type");
                ErrorsFound = true;
            }

            // Get supplemental heating coil information

            SuppHeatCoilType = Alphas(12);
            SuppHeatCoilName = Alphas(13);
            Furnace(FurnaceNum).SuppHeatCoilType = SuppHeatCoilType;
            Furnace(FurnaceNum).SuppHeatCoilName = SuppHeatCoilName;
            errFlag = false;
            if (UtilityRoutines::SameString(SuppHeatCoilType, "Coil:Heating:Fuel") ||
                UtilityRoutines::SameString(SuppHeatCoilType, "Coil:Heating:Electric")) {

                Furnace(FurnaceNum).SuppHeatCoilType_Num = GetHeatingCoilTypeNum(SuppHeatCoilType, SuppHeatCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else {
                    IsNotOK = false;
                    ValidateComponent(SuppHeatCoilType, SuppHeatCoilName, IsNotOK, CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError("In " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                        ErrorsFound = true;

                    } else { // mine data from the supplemental heating coil

                        GetHeatingCoilIndex(SuppHeatCoilName, Furnace(FurnaceNum).SuppHeatCoilIndex, IsNotOK);
                        if (IsNotOK) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                        // Get the Supplemental Heating Coil Inlet Node Number
                        errFlag = false;
                        SupHeatCoilInletNode = GetHeatingCoilInletNode(SuppHeatCoilType, SuppHeatCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " \"" + Alphas(1) + "\"");
                            ErrorsFound = true;
                        }

                        // Get the Supplemental Heating Coil Outlet Node Number
                        errFlag = false;
                        SupHeatCoilOutletNode = GetHeatingCoilOutletNode(SuppHeatCoilType, SuppHeatCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                        // Get the supplemental heating coil design capacity
                        errFlag = false;
                        Furnace(FurnaceNum).DesignSuppHeatingCapacity = GetHeatingCoilCapacity(SuppHeatCoilType, SuppHeatCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                            ErrorsFound = true;
                        }

                    } // IF (IsNotOK) THEN
                }
            } else if (UtilityRoutines::SameString(SuppHeatCoilType, "Coil:Heating:Water")) {
                Furnace(FurnaceNum).SuppHeatCoilType_Num = Coil_HeatingWater;
                ValidateComponent(SuppHeatCoilType, SuppHeatCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    // Get the Heating Coil water Inlet or control Node number
                    errFlag = false;
                    Furnace(FurnaceNum).SuppCoilControlNode = GetCoilWaterInletNode("Coil:Heating:Water", SuppHeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the ReHeat Coil hot water max volume flow rate
                    errFlag = false;
                    Furnace(FurnaceNum).MaxSuppCoilFluidFlow = GetCoilMaxWaterFlowRate("Coil:Heating:Water", SuppHeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the ReHeat Coil Inlet Node
                    errFlag = false;
                    SupHeatCoilInletNode = GetWaterCoilInletNode("Coil:Heating:Water", SuppHeatCoilName, errFlag);
                    Furnace(FurnaceNum).SuppCoilAirInletNode = SupHeatCoilInletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the ReHeat Coil Outlet Node
                    errFlag = false;
                    SupHeatCoilOutletNode = GetWaterCoilOutletNode("Coil:Heating:Water", SuppHeatCoilName, errFlag);
                    Furnace(FurnaceNum).SuppCoilAirOutletNode = SupHeatCoilOutletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    errFlag = false;
                    CheckCoilWaterInletNode(Furnace(FurnaceNum).CoilControlNode, errFlag);
                    if (!errFlag) { // then did find a controller so that is bad
                        ShowSevereError(CurrentModuleObject + " = " + Furnace(FurnaceNum).Name + " has a conflicting Controller:WaterCoil object");
                        ShowContinueError("Hot water coils are controlled directly by unitary and furnace systems.");
                        ShowContinueError("No water coil controller should be input for the coil.");
                        ErrorsFound = true;
                    }
                }

            } else if (UtilityRoutines::SameString(SuppHeatCoilType, "Coil:Heating:Steam")) {
                Furnace(FurnaceNum).SuppHeatCoilType_Num = Coil_HeatingSteam;
                ValidateComponent(SuppHeatCoilType, SuppHeatCoilName, IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                } else { // mine data from heating coil object

                    errFlag = false;
                    Furnace(FurnaceNum).SuppHeatCoilIndex = GetSteamCoilIndex(SuppHeatCoilType, SuppHeatCoilName, errFlag);
                    if (Furnace(FurnaceNum).SuppHeatCoilIndex == 0) {
                        ShowSevereError(CurrentModuleObject + " illegal " + cAlphaFields(12) + " = " + SuppHeatCoilName);
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil steam inlet node number
                    errFlag = false;
                    Furnace(FurnaceNum).SuppCoilControlNode = GetCoilSteamInletNode("Coil:Heating:Steam", SuppHeatCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil steam max volume flow rate
                    Furnace(FurnaceNum).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).SuppHeatCoilIndex, errFlag);
                    if (Furnace(FurnaceNum).MaxSuppCoilFluidFlow > 0.0) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getAirLoopHVACHeatCoolInput);
                        Furnace(FurnaceNum).MaxSuppCoilFluidFlow =
                            GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).SuppHeatCoilIndex, errFlag) * SteamDensity;
                    }

                    // Get the Heating Coil Inlet Node
                    errFlag = false;
                    SupHeatCoilInletNode = GetSteamCoilAirInletNode(Furnace(FurnaceNum).SuppHeatCoilIndex, SuppHeatCoilName, errFlag);
                    Furnace(FurnaceNum).SuppCoilAirInletNode = SupHeatCoilInletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }

                    // Get the Heating Coil Outlet Node
                    errFlag = false;
                    SupHeatCoilOutletNode = GetCoilAirOutletNode(Furnace(FurnaceNum).SuppHeatCoilIndex, SuppHeatCoilName, errFlag);
                    Furnace(FurnaceNum).SuppCoilAirOutletNode = SupHeatCoilOutletNode;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        ErrorsFound = true;
                    }
                }

            } else {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(12) + " = " + Alphas(12));
                ErrorsFound = true;
            } // IF (Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingGasOrOtherFuel .OR. &, etc.

            if (lAlphaBlanks(14)) {
                Furnace(FurnaceNum).CondenserNodeNum = 0;
            } else {
                Furnace(FurnaceNum).CondenserNodeNum = GetOnlySingleNode(Alphas(14),
                                                                         ErrorsFound,
                                                                         CurrentModuleObject,
                                                                         Alphas(1),
                                                                         NodeType_Air,
                                                                         NodeConnectionType_OutsideAirReference,
                                                                         1,
                                                                         ObjectIsNotParent);
                // need better verification.
                if (!CheckOutAirNodeNumber(Furnace(FurnaceNum).CondenserNodeNum)) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError(" Node name of outdoor dry-bulb temperature sensor not valid outdoor air node= " + Alphas(14));
                    ShowContinueError("...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ErrorsFound = true;
                }
            }

            if (UtilityRoutines::SameString(Alphas(15), "BlowThrough")) Furnace(FurnaceNum).FanPlace = BlowThru;
            if (UtilityRoutines::SameString(Alphas(15), "DrawThrough")) Furnace(FurnaceNum).FanPlace = DrawThru;
            if (Furnace(FurnaceNum).FanPlace == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(15) + " = " + Alphas(15));
                ErrorsFound = true;
            }

            Furnace(FurnaceNum).FanSchedPtr = GetScheduleIndex(Alphas(16));
            if (!lAlphaBlanks(16) && Furnace(FurnaceNum).FanSchedPtr == 0) {
                ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                ShowContinueError("Illegal " + cAlphaFields(16) + " = " + Alphas(16));
                ErrorsFound = true;
            } else if (lAlphaBlanks(16)) {
                Furnace(FurnaceNum).OpMode = CycFanCycCoil;
                if (Furnace(FurnaceNum).FanType_Num != FanType_SimpleOnOff) {
                    ShowSevereError(CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                    ShowContinueError(cAlphaFields(6) + " = " + Alphas(6));
                    ShowContinueError("Fan type must be Fan:OnOff when " + cAlphaFields(16) + " = Blank.");
                    ErrorsFound = true;
                }
            }

            // add the Dehumidification Type
            if (UtilityRoutines::SameString(Alphas(17), "None") || UtilityRoutines::SameString(Alphas(17), "CoolReheat")) {
                AirNodeFound = false;
                if (UtilityRoutines::SameString(Alphas(17), "CoolReheat")) {
                    Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_CoolReheat;
                    Furnace(FurnaceNum).Humidistat = true;
                    if (lAlphaBlanks(17)) {
                        ShowWarningError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                        ShowContinueError("Dehumidification control type is assumed to be None since a supplemental reheat coil has not been "
                                          "specified and the simulation continues.");
                        Furnace(FurnaceNum).Humidistat = false;
                        Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_None;
                    }
                }
                if (UtilityRoutines::SameString(Alphas(17), "None")) {
                    Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_None;
                    Furnace(FurnaceNum).Humidistat = false;
                }
                if (Furnace(FurnaceNum).Humidistat) {
                    for (HStatZoneNum = 1; HStatZoneNum <= NumHumidityControlZones; ++HStatZoneNum) {
                        if (HumidityControlZone(HStatZoneNum).ActualZoneNum != Furnace(FurnaceNum).ControlZoneNum) continue;
                        AirNodeFound = true;
                    }
                    if (!AirNodeFound) {
                        ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                        ShowContinueError("Did not find Air Node (Zone with Humidistat).");
                        ShowContinueError("Specified " + cAlphaFields(5) + " = " + Alphas(5));
                        ErrorsFound = true;
                    }
                }
            } else { // invalid input or blank
                if (!lAlphaBlanks(17)) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("Illegal " + cAlphaFields(17) + " = " + Alphas(17));
                    ErrorsFound = true;
                } else {
                    Furnace(FurnaceNum).Humidistat = false;
                    Furnace(FurnaceNum).DehumidControlType_Num = DehumidControl_None;
                }
            }

            // Add fan to component sets array

            if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                CompSetFanInlet = Alphas(3);
                CompSetCoolInlet = "UNDEFINED";
                if (FanInletNode != Furnace(FurnaceNum).FurnaceInletNodeNum) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1) +
                                    ", Mismatch between unitary system inlet node and fan inlet node.");
                    ShowContinueError("..For \"BlowThrough\" fan, the inlet node name for the HeatPump should match the fan inlet node name.");
                    ShowContinueError("..HeatPump Inlet Node = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                    ShowContinueError("..Fan Inlet Node      = " + NodeID(FanInletNode));
                    ErrorsFound = true;
                }
                if (FanOutletNode != CoolingCoilInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1) +
                                    ", Mismatch between fan outlet node and cooling coil inlet node.");
                    ShowContinueError("..For \"BlowThrough\" fan, the fan outlet node name must match the cooling coil inlet node name.");
                    ShowContinueError("..Fan outlet node         = " + NodeID(FanOutletNode));
                    ShowContinueError("..Cooling coil inlet node = " + NodeID(CoolingCoilInletNode));
                    ErrorsFound = true;
                }
                if (CoolingCoilOutletNode != HeatingCoilInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1) +
                                    ", Mismatch between cooling coil outlet node and heating coil inlet node.");
                    ShowContinueError("..The cooling coil outlet node name must match the heating coil inlet node name.");
                    ShowContinueError("..Cooling coil outlet node = " + NodeID(CoolingCoilOutletNode));
                    ShowContinueError("..Heating coil inlet node  = " + NodeID(HeatingCoilInletNode));
                    ErrorsFound = true;
                }
                if (HeatingCoilOutletNode != SupHeatCoilInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1) +
                                    ", Mismatch between heating coil outlet node and supplemental heating coil inlet node.");
                    ShowContinueError(
                        "..For \"BlowThrough\" fan, the heating coil outlet node name must match the supplemental heating coil inlet node name.");
                    ShowContinueError("..Heating coil outlet node             = " + NodeID(HeatingCoilOutletNode));
                    ShowContinueError("..Supplemental heating coil inlet node = " + NodeID(SupHeatCoilInletNode));
                    ErrorsFound = true;
                }
                if (SupHeatCoilOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1) +
                                    ", Mismatch between supplemental heating coil outlet node and HeatPump outlet node.");
                    ShowContinueError("..The supplemental heating coil outlet node name must match the HeatPump outlet node name.");
                    ShowContinueError("..Supplemental heating coil outlet node = " + NodeID(SupHeatCoilOutletNode));
                    ShowContinueError("..HeatPump outlet node                  = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                    ErrorsFound = true;
                }
            } else {
                CompSetFanInlet = "UNDEFINED";
                CompSetCoolInlet = Alphas(3);
                if (CoolingCoilInletNode != Furnace(FurnaceNum).FurnaceInletNodeNum) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1) +
                                    ", Mismatch between unitary system inlet node and cooling coil inlet node.");
                    ShowContinueError(
                        "..For \"DrawThrough\" fan, the inlet node name for the HeatPump should match the cooling coil inlet node name.");
                    ShowContinueError("..HeatPump inlet node     = " + NodeID(Furnace(FurnaceNum).FurnaceInletNodeNum));
                    ShowContinueError("..Cooling coil inlet node = " + NodeID(CoolingCoilInletNode));
                    ErrorsFound = true;
                }
                if (CoolingCoilOutletNode != HeatingCoilInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1) +
                                    ", Mismatch between cooling coil outlet node and heating coil inlet node.");
                    ShowContinueError("..The outlet node name for the cooling coil should match the heating coil inlet node name.");
                    ShowContinueError("..Cooling coil outlet node = " + NodeID(CoolingCoilOutletNode));
                    ShowContinueError("..Heating coil inlet node  = " + NodeID(HeatingCoilInletNode));
                    ErrorsFound = true;
                }
                if (HeatingCoilOutletNode != FanInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1) +
                                    ", Mismatch between heating coil outlet node and fan inlet node.");
                    ShowContinueError("..For \"DrawThrough\" fan, the outlet node name for the heating coil should match the fan inlet node name.");
                    ShowContinueError("..Heating coil outlet node = " + NodeID(HeatingCoilOutletNode));
                    ShowContinueError("..Fan inlet node           = " + NodeID(FanInletNode));
                    ErrorsFound = true;
                }
                if (FanOutletNode != SupHeatCoilInletNode) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1) +
                                    ", Mismatch between fan outlet node and supplemental heating coil inlet node.");
                    ShowContinueError(
                        "..For \"DrawThrough\" fan, the outlet node name for the fan should match the supplemental heating coil inlet node name.");
                    ShowContinueError("..Fan outlet node                      = " + NodeID(FanOutletNode));
                    ShowContinueError("..Supplemental heating coil inlet node = " + NodeID(SupHeatCoilInletNode));
                    ErrorsFound = true;
                }
                if (SupHeatCoilOutletNode != Furnace(FurnaceNum).FurnaceOutletNodeNum) {
                    ShowSevereError("For " + CurrentModuleObject + " = " + Alphas(1) +
                                    ", Mismatch between supplemental heating coil outlet node and HeatPump outlet node.");
                    ShowContinueError("..The supplemental heating coil outlet node name must match the HeatPump outlet node name.");
                    ShowContinueError("..Supplemental heating coil outlet node = " + NodeID(SupHeatCoilOutletNode));
                    ShowContinueError("..HeatPump outlet node                  = " + NodeID(Furnace(FurnaceNum).FurnaceOutletNodeNum));
                    ErrorsFound = true;
                }
            }
            //  (Set up validation here for the fan or cooling coil inlet?)
            SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(6), Alphas(7), CompSetFanInlet, "UNDEFINED");

            // Add DX heating coil to component sets array
            SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(8), Alphas(9), "UNDEFINED", "UNDEFINED");

            // Add DX cooling coil to component sets array
            SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(10), Alphas(11), CompSetCoolInlet, "UNDEFINED");

            // Add supplemental heating coil to component sets array
            SetUpCompSets(CurrentModuleObject, Alphas(1), Alphas(12), Alphas(13), "UNDEFINED", Alphas(4));

            // Set the Design Fan Volume Flow Rate
            errFlag = false;
            FanVolFlowRate = GetFanDesignVolumeFlowRate(FanType, FanName, errFlag);
            Furnace(FurnaceNum).ActualFanVolFlowRate = FanVolFlowRate;
            if (errFlag) {
                ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                ErrorsFound = true;
            }

            // CR8094 - simple water to air heat pump MUST operate at the same flow rate specified in the coil objects
            //        Furnace(FurnaceNum)%DesignFanVolFlowRate = Numbers(1)
            //        Furnace(FurnaceNum)%MaxHeatAirVolFlow    = Furnace(FurnaceNum)%DesignFanVolFlowRate
            //        Furnace(FurnaceNum)%MaxCoolAirVolFlow    = Furnace(FurnaceNum)%DesignFanVolFlowRate

            // parameter estimate model only specifies air flow rate in parent object
            if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingWaterToAirHP) {
                Furnace(FurnaceNum).MaxHeatAirVolFlow = Numbers(1);
                Furnace(FurnaceNum).MaxCoolAirVolFlow = Numbers(1);
                // simple HP model specifies air flow rate in both the parent and child coils. Use coil air flow rates.
                // simple HP model air flow rate input will not be used.
            } else if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple) {
                errFlag = false;
                Furnace(FurnaceNum).MaxHeatAirVolFlow = GetWtoAHPSimpleCoilAirFlow(HeatingCoilType, HeatingCoilName, errFlag);
                Furnace(FurnaceNum).MaxCoolAirVolFlow = GetWtoAHPSimpleCoilAirFlow(CoolingCoilType, CoolingCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }
            } else if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit) {
                errFlag = false;
                Furnace(FurnaceNum).MaxHeatAirVolFlow = GetCoilAirFlowRateVariableSpeed(HeatingCoilType, HeatingCoilName, errFlag);
                Furnace(FurnaceNum).MaxCoolAirVolFlow = GetCoilAirFlowRateVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }
            }

            Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow = min(Furnace(FurnaceNum).MaxHeatAirVolFlow, Furnace(FurnaceNum).MaxCoolAirVolFlow);
            if (Furnace(FurnaceNum).MaxHeatAirVolFlow != AutoSize && Furnace(FurnaceNum).MaxCoolAirVolFlow != AutoSize) {
                Furnace(FurnaceNum).DesignFanVolFlowRate = max(Furnace(FurnaceNum).MaxHeatAirVolFlow, Furnace(FurnaceNum).MaxCoolAirVolFlow);
            } else {
                Furnace(FurnaceNum).DesignFanVolFlowRate = AutoSize;
            }

            Furnace(FurnaceNum).AirFlowControl = UseCompressorOnFlow;

            if (FanVolFlowRate != AutoSize && Furnace(FurnaceNum).DesignFanVolFlowRate != AutoSize) {
                if (Furnace(FurnaceNum).DesignFanVolFlowRate > FanVolFlowRate) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("... has a Cooling or Heating Air Flow Rate > Max Fan Volume Flow Rate, should be <=.");
                    ShowContinueError("... Entered value=" + RoundSigDigits(Furnace(FurnaceNum).DesignFanVolFlowRate, 2) + "... Fan [" + FanType +
                                      ':' + FanName + "] Max Value=" + RoundSigDigits(FanVolFlowRate, 2));
                }
            }
            if (FanVolFlowRate != AutoSize && Furnace(FurnaceNum).DesignFanVolFlowRate != AutoSize) {
                if (Furnace(FurnaceNum).DesignFanVolFlowRate <= 0.0) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("... has a Design Fan Flow Rate <= 0.0, it must be >0.0");
                    ShowContinueError("... Entered value=" + RoundSigDigits(Furnace(FurnaceNum).DesignFanVolFlowRate, 2));
                    ErrorsFound = true;
                }
            }

            // Set the heat pump heating coil capacity
            //  Get from coil module.
            if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingWaterToAirHP) {
                errFlag = false;
                Furnace(FurnaceNum).DesignHeatingCapacity = GetWtoAHPCoilCapacity(HeatingCoilType, HeatingCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }
            } else if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingWaterToAirHPSimple) {
                errFlag = false;
                Furnace(FurnaceNum).DesignHeatingCapacity = GetWtoAHPSimpleCoilCapacity(HeatingCoilType, HeatingCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }
            } else if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit) {
                errFlag = false;
                Furnace(FurnaceNum).DesignHeatingCapacity = GetCoilCapacityVariableSpeed(HeatingCoilType, HeatingCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }
            }
            // Set the heat pump heating coil convergence
            Furnace(FurnaceNum).HeatingConvergenceTolerance = Numbers(2);
            // Set the heat pump cooling coil capacity (Total capacity)
            //  Get from coil module.
            if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingWaterToAirHP) {
                errFlag = false;
                Furnace(FurnaceNum).DesignCoolingCapacity = GetWtoAHPCoilCapacity(CoolingCoilType, CoolingCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }
            } else if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple) {
                errFlag = false;
                Furnace(FurnaceNum).DesignCoolingCapacity = GetWtoAHPSimpleCoilCapacity(CoolingCoilType, CoolingCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }
            } else if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit) {
                errFlag = false;
                Furnace(FurnaceNum).DesignCoolingCapacity = GetCoilCapacityVariableSpeed(CoolingCoilType, CoolingCoilName, errFlag);
                if (errFlag) {
                    ShowContinueError("...occurs in " + CurrentModuleObject + " = " + Alphas(1));
                    ErrorsFound = true;
                }
            }
            // Set the heat pump cooling coil convergence
            Furnace(FurnaceNum).CoolingConvergenceTolerance = Numbers(3);
            // Set the heatpump cycling rate
            Furnace(FurnaceNum).MaxONOFFCyclesperHour = Numbers(4);

            // Set the heat pump time constant
            Furnace(FurnaceNum).HPTimeConstant = Numbers(5);

            // Set the heat pump on-cycle power use fraction
            Furnace(FurnaceNum).OnCyclePowerFraction = Numbers(6);

            // Set the heat pump fan delay time
            Furnace(FurnaceNum).FanDelayTime = Numbers(7);

            // Set the heatpump design supplemental heating capacity
            //  Get from coil module.

            // Set the heatpump max outlet temperature
            Furnace(FurnaceNum).DesignMaxOutletTemp = Numbers(8);

            // Set maximum supply air temperature for supplemental heating coil
            Furnace(FurnaceNum).MaxOATSuppHeat = Numbers(9);

            // set minimum outdoor temperature for compressor operation
            SetMinOATCompressor(
                FurnaceNum, Alphas(1), cCurrentModuleObject, CoolingCoilType, CoolingCoilName, HeatingCoilType, HeatingCoilName, ErrorsFound);

        } // End of the Unitary System WaterToAirHeatPump Loop

        Alphas.deallocate();
        Numbers.deallocate();

        if (ErrorsFound) {
            ShowFatalError("Errors found in getting Furnace or Unitary System input.");
        }

        for (HeatOnlyNum = 1; HeatOnlyNum <= NumHeatOnly; ++HeatOnlyNum) {
            FurnaceNum = HeatOnlyNum;
            // Setup Report variables for the Furnace that are not reported in the components themselves
            SetupOutputVariable("Unitary System Fan Part Load Ratio",
                                OutputProcessor::Unit::None,
                                Furnace(FurnaceNum).FanPartLoadRatio,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("AirLoopHVAC:Unitary:Furnace:HeatOnly",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideOn,
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideValue);
            }
        }

        for (UnitaryHeatOnlyNum = NumHeatOnly + 1; UnitaryHeatOnlyNum <= NumHeatOnly + NumUnitaryHeatOnly; ++UnitaryHeatOnlyNum) {
            FurnaceNum = UnitaryHeatOnlyNum;
            // Setup Report variables for Unitary System that are not reported in the components themselves
            SetupOutputVariable("Unitary System Fan Part Load Ratio",
                                OutputProcessor::Unit::None,
                                Furnace(FurnaceNum).FanPartLoadRatio,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("AirLoopHVAC:UnitaryHeatOnly",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideOn,
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideValue);
            }
        }

        for (HeatCoolNum = NumHeatOnly + NumUnitaryHeatOnly + 1; HeatCoolNum <= NumHeatOnly + NumUnitaryHeatOnly + NumHeatCool; ++HeatCoolNum) {
            FurnaceNum = HeatCoolNum;
            // Setup Report variables for the Furnace that are not reported in the components themselves
            SetupOutputVariable("Unitary System Fan Part Load Ratio",
                                OutputProcessor::Unit::None,
                                Furnace(FurnaceNum).FanPartLoadRatio,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            SetupOutputVariable("Unitary System Compressor Part Load Ratio",
                                OutputProcessor::Unit::None,
                                Furnace(FurnaceNum).CompPartLoadRatio,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);

            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("AirLoopHVAC:Unitary:Furnace:HeatCool",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideOn,
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideValue);
                SetupEMSActuator("AirLoopHVAC:Unitary:Furnace:HeatCool",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate During Cooling Operation",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).MaxCoolAirVolFlowEMSOverrideOn,
                                 Furnace(FurnaceNum).MaxCoolAirVolFlowEMSOverrideValue);
                SetupEMSActuator("AirLoopHVAC:Unitary:Furnace:HeatCool",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate During Heating Operation",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).MaxHeatAirVolFlowEMSOverrideOn,
                                 Furnace(FurnaceNum).MaxHeatAirVolFlowEMSOverrideValue);
                SetupEMSActuator("AirLoopHVAC:Unitary:Furnace:HeatCool",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate During No Heating or Cooling Operation",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlowEMSOverrideOn,
                                 Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlowEMSOverrideValue);
            }
        }

        for (UnitaryHeatCoolNum = NumHeatOnly + NumHeatCool + NumUnitaryHeatOnly + 1;
             UnitaryHeatCoolNum <= NumHeatOnly + NumHeatCool + NumUnitaryHeatOnly + NumUnitaryHeatCool;
             ++UnitaryHeatCoolNum) {
            FurnaceNum = UnitaryHeatCoolNum;
            // Setup Report variables for Unitary System that are not reported in the components themselves
            SetupOutputVariable("Unitary System Fan Part Load Ratio",
                                OutputProcessor::Unit::None,
                                Furnace(FurnaceNum).FanPartLoadRatio,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            SetupOutputVariable("Unitary System Compressor Part Load Ratio",
                                OutputProcessor::Unit::None,
                                Furnace(FurnaceNum).CompPartLoadRatio,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("AirLoopHVAC:UnitaryHeatCool",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideOn,
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideValue);
                SetupEMSActuator("AirLoopHVAC:UnitaryHeatCool",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate During Cooling Operation",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).MaxCoolAirVolFlowEMSOverrideOn,
                                 Furnace(FurnaceNum).MaxCoolAirVolFlowEMSOverrideValue);
                SetupEMSActuator("AirLoopHVAC:UnitaryHeatCool",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate During Heating Operation",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).MaxHeatAirVolFlowEMSOverrideOn,
                                 Furnace(FurnaceNum).MaxHeatAirVolFlowEMSOverrideValue);
                SetupEMSActuator("AirLoopHVAC:UnitaryHeatCool",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate During No Heating or Cooling Operation",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlowEMSOverrideOn,
                                 Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlowEMSOverrideValue);
            }
        }

        for (HeatPumpNum = NumHeatOnly + NumHeatCool + NumUnitaryHeatOnly + NumUnitaryHeatCool + 1;
             HeatPumpNum <= NumFurnaces - NumWaterToAirHeatPump;
             ++HeatPumpNum) {
            FurnaceNum = HeatPumpNum;
            // Setup Report variables for Unitary System that are not reported in the components themselves
            SetupOutputVariable("Unitary System Fan Part Load Ratio",
                                OutputProcessor::Unit::None,
                                Furnace(FurnaceNum).FanPartLoadRatio,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            SetupOutputVariable("Unitary System Compressor Part Load Ratio",
                                OutputProcessor::Unit::None,
                                Furnace(FurnaceNum).CompPartLoadRatio,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            SetupOutputVariable("Unitary System Dehumidification Induced Heating Demand Rate",
                                OutputProcessor::Unit::W,
                                Furnace(FurnaceNum).DehumidInducedHeatingDemandRate,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);

            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("AirLoopHVAC:UnitaryHeatPump:AirToAir",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideOn,
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideValue);
            }
        }

        for (HeatPumpNum = NumHeatOnly + NumHeatCool + NumUnitaryHeatOnly + NumUnitaryHeatCool + NumHeatPump + 1; HeatPumpNum <= NumFurnaces;
             ++HeatPumpNum) {
            FurnaceNum = HeatPumpNum;
            // Setup Report variables for Unitary System that are not reported in the components themselves
            SetupOutputVariable("Unitary System Fan Part Load Ratio",
                                OutputProcessor::Unit::None,
                                Furnace(FurnaceNum).FanPartLoadRatio,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            SetupOutputVariable("Unitary System Compressor Part Load Ratio",
                                OutputProcessor::Unit::None,
                                Furnace(FurnaceNum).CompPartLoadRatio,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            SetupOutputVariable("Unitary System Requested Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                Furnace(FurnaceNum).CoolingCoilSensDemand,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            SetupOutputVariable("Unitary System Requested Latent Cooling Rate",
                                OutputProcessor::Unit::W,
                                Furnace(FurnaceNum).CoolingCoilLatentDemand,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            SetupOutputVariable("Unitary System Requested Heating Rate",
                                OutputProcessor::Unit::W,
                                Furnace(FurnaceNum).HeatingCoilSensDemand,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);
            SetupOutputVariable("Unitary System Dehumidification Induced Heating Demand Rate",
                                OutputProcessor::Unit::W,
                                Furnace(FurnaceNum).DehumidInducedHeatingDemandRate,
                                "System",
                                "Average",
                                Furnace(FurnaceNum).Name);

            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("AirLoopHVAC:UnitaryHeatPump:WaterToAir",
                                 Furnace(FurnaceNum).Name,
                                 "Autosized Supply Air Flow Rate",
                                 "[m3/s]",
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideOn,
                                 Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideValue);
            }
        }

        if (AnyEnergyManagementSystemInModel) {
            for (FurnaceNum = 1; FurnaceNum <= NumFurnaces; ++FurnaceNum) {
                SetupEMSInternalVariable(
                    "Unitary HVAC Design Heating Capacity", Furnace(FurnaceNum).Name, "[W]", Furnace(FurnaceNum).DesignHeatingCapacity);
                SetupEMSInternalVariable(
                    "Unitary HVAC Design Cooling Capacity", Furnace(FurnaceNum).Name, "[W]", Furnace(FurnaceNum).DesignCoolingCapacity);
                SetupEMSActuator("Unitary HVAC",
                                 Furnace(FurnaceNum).Name,
                                 "Sensible Load Request",
                                 "[W]",
                                 Furnace(FurnaceNum).EMSOverrideSensZoneLoadRequest,
                                 Furnace(FurnaceNum).EMSSensibleZoneLoadValue);
                SetupEMSActuator("Unitary HVAC",
                                 Furnace(FurnaceNum).Name,
                                 "Moisture Load Request",
                                 "[W]",
                                 Furnace(FurnaceNum).EMSOverrideMoistZoneLoadRequest,
                                 Furnace(FurnaceNum).EMSMoistureZoneLoadValue);
            }
        }
        bool anyRan;
        ManageEMS(emsCallFromComponentGetInput, anyRan);
    }

    // End of Get Input subroutines for this Module
    //******************************************************************************

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void InitFurnace(int const FurnaceNum,         // index to Furnace
                     int const AirLoopNum,         // index to air loop
                     Real64 &OnOffAirFlowRatio,    // ratio of on to off air mass flow rate
                     int &OpMode,                  // fan operating mode
                     Real64 &ZoneLoad,             // zone sensible load to be met (modified here as needed) (W)
                     Real64 &MoistureLoad,         // zone moisture load (W)
                     bool const FirstHVACIteration // TRUE if first HVAC iteration
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   Feb 2001
        //       MODIFIED       Oct 2001, Richard Raustad
        //                      Sep 2008, R. Raustad - revised logic to determine load to be met
        //                      Bereket Nigusse, June 2010 - added a procedure to calculate supply air flow fraction
        //                      through controlled zone
        //                      Bo Shen, March 2012 - for VS WSHP
        //                      Bo Shen, ORNL, July 2012 - added variable-speed air source heat pump cooling and heating coils, using curve-fits

        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Furnace Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.
        // The HeatCool furnace/unitarysystem and air-to-air heat pump may have alternate air flow rates
        // in cooling, heating, and when no cooling or heating is needed. Set up the coil (comp) ON and OFF
        // air flow rates during InitFurnace. Use these flow rates during the Calc routines to set the
        // average mass flow rates based on PLR.

        // REFERENCES:

        // Using/Aliasing
        using DataAirflowNetwork::AirflowNetworkControlMultizone;
        using DataAirflowNetwork::SimulateAirflowNetwork;
        using DataAirLoop::AirLoopControlInfo;
        using DataAirLoop::AirToZoneNodeInfo;
        using DataAirLoop::AirLoopAFNInfo;
        using DataHeatBalance::Zone;
        using DataHeatBalFanSys::TempControlType;
        using DataPlant::PlantLoop;
        using DataPlant::TypeOf_CoilSteamAirHeating;
        using DataPlant::TypeOf_CoilWaterSimpleHeating;
        using DataSizing::AutoSize;
        using DataZoneEnergyDemands::CurDeadBandOrSetback;
        using DataZoneEnergyDemands::ZoneSysEnergyDemand;
        using DataZoneEnergyDemands::ZoneSysMoistureDemand;
        using Fans::GetFanDesignVolumeFlowRate;
        using Fans::GetFanSpeedRatioCurveIndex;
        using General::RoundSigDigits;
        using General::TrimSigDigits;
        using PlantUtilities::ScanPlantLoopsForObject;
        using ReportSizingManager::ReportSizingOutput;
        using SteamCoils::SimulateSteamCoilComponents;
        auto &GetCoilMaxSteamFlowRate(SteamCoils::GetCoilMaxSteamFlowRate);
        auto &GetSteamCoilCapacity(SteamCoils::GetCoilCapacity);
        using Fans::GetFanVolFlow;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSatDensityRefrig;
        using IntegratedHeatPump::IntegratedHeatPumps;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::SetComponentFlowRate;
        using WaterCoils::GetCoilMaxWaterFlowRate;
        using WaterCoils::SimulateWaterCoilComponents;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const Small5WLoad(5.0);
        static std::string const RoutineName("InitFurnace");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool MyOneTimeFlag(true);             // one time allocation flag
        static Array1D_bool MyEnvrnFlag;             // environment flag
        static Array1D_bool MySecondOneTimeFlag;     // additional one time flag
        static Array1D_bool MyFanFlag;               // used for sizing fan inputs one time
        static Array1D_bool MyCheckFlag;             // Used to obtain the zone inlet node number in the controlled zone
        static Array1D_bool MyFlowFracFlag;          // Used for calculatig flow fraction once
        static Array1D_bool MyPlantScanFlag;         // used to initializa plant comp for water and steam heating coils
        static Array1D_bool MySuppCoilPlantScanFlag; // used to initialize plant comp for water and steam heating coils
        bool errFlag;                                // error flag for mining functions
        Real64 FanVolFlowRate;                       // fan volumetric flow rate (m3/s)
        Real64 QZnReq;                               // furnace load based on control zone frac (W)
        Real64 PartLoadRatio;                        // furnace part-load ratio
        Real64 SensibleOutput;                       // no load sensible output (coils off) (W)
        Real64 LatentOutput;                         // no load latent output (coils off) (W)
        Real64 QToCoolSetPt;                         // sensible load to cooling setpoint (W)
        Real64 QToHeatSetPt;                         // sensible load to heating setpoint (W)
        int ZoneInNode;                              // Zone inlet node number in the controlled zone
        Real64 MinHumRat;                            // Minimum humidity ratio for sensible capacity
        // calculation (kg/kg)
        Real64 DeltaMassRate; // Difference of mass flow rate between
        // inlet node and system outlet node
        Real64 MassFlowRate; // mass flow rate to calculate loss
        Real64 MaxTemp;      // Maximum temperature used in latent loss calculation
        std::string FanType; // used in warning messages
        std::string FanName; // used in warning messages

        static int ZoneInSysIndex(0);                            // number of zone inlet nodes counter in an airloop
        static int NumAirLoopZones(0);                           // number of zone inlet nodes in an air loop
        static int ZoneInletNodeNum(0);                          // zone inlet nodes node number
        static bool FlowFracFlagReady(true);                     // one time flag for calculating flow fraction through controlled zone
        static Real64 SumOfMassFlowRateMax(0.0);                 // the sum of mass flow rates at inlet to zones in an airloop
        static Real64 CntrlZoneTerminalUnitMassFlowRateMax(0.0); // Maximum mass flow rate through controlled zone terminal unit

        static bool ErrorsFound(false);        // flag returned from mining call
        static Real64 mdot(0.0);               // local temporary for mass flow rate (kg/s)
        static Real64 rho(0.0);                // local for fluid density
        static int SteamIndex(0);              // index of steam quality for steam heating coil
        static Real64 SteamDensity(0.0);       // density of steam at 100C, used for steam heating coils
        static Real64 CoilMaxVolFlowRate(0.0); // coil fluid maximum volume flow rate
        static Real64 QActual(0.0);            // coil actual capacity
        static Real64 SUPHEATERLOAD(0.0);      // SUPPLEMENTAL HEATER LOAD
        int NumOfSpeedCooling;                 // Number of speeds for cooling
        int NumOfSpeedHeating;                 // Number of speeds for heating
        int InNode;                            // Inlet node number in MSHP loop
        int OutNode;                           // Outlet node number in MSHP loop
        Real64 RhoAir;                         // Air density at InNode
        static bool MyAirLoopPass(true);       // one time allocation flag
        int IHPIndex(0);                       // coil id of IHP coil
        int OperatingMode;                     // track cooling, heating, and no cooling or heating modes
        int OperatingModeMinusOne;
        int OperatingModeMinusTwo;
        bool Oscillate; // detection of oscillating operating modes

        InNode = Furnace(FurnaceNum).FurnaceInletNodeNum;
        OutNode = Furnace(FurnaceNum).FurnaceOutletNodeNum;

        // FLOW:
        if (MyOneTimeFlag) {
            // initialize the environment and sizing flags
            MyEnvrnFlag.allocate(NumFurnaces);
            MySizeFlag.allocate(NumFurnaces);
            MySecondOneTimeFlag.allocate(NumFurnaces);
            MyFanFlag.allocate(NumFurnaces);
            MyCheckFlag.allocate(NumFurnaces);
            MyFlowFracFlag.allocate(NumFurnaces);
            MyPlantScanFlag.allocate(NumFurnaces);
            MySuppCoilPlantScanFlag.allocate(NumFurnaces);
            MyEnvrnFlag = true;
            MySizeFlag = true;
            MySecondOneTimeFlag = true;
            MyFanFlag = true;
            MyCheckFlag = true;
            MyFlowFracFlag = true;
            MyOneTimeFlag = false;
            MyPlantScanFlag = true;
            MySuppCoilPlantScanFlag = true;
        }

        if (BeginEnvrnFlag && MyAirLoopPass) {
            AirLoopPass = 0;
            MyAirLoopPass = false;
        }
        if (!BeginEnvrnFlag) {
            MyAirLoopPass = true;
        }

        ++AirLoopPass;
        if (AirLoopPass > 2) AirLoopPass = 1;

        if (!SysSizingCalc && MySizeFlag(FurnaceNum)) {
            // for each furnace, do the sizing once.
            SizeFurnace(FurnaceNum, FirstHVACIteration);
            Furnace(FurnaceNum).ControlZoneMassFlowFrac = 1.0;

            MySizeFlag(FurnaceNum) = false;
            // Pass the fan cycling schedule index up to the air loop. Set the air loop unitary system flag.
            AirLoopControlInfo(AirLoopNum).CycFanSchedPtr = Furnace(FurnaceNum).FanSchedPtr;
            AirLoopControlInfo(AirLoopNum).UnitarySys = true;
            // RR this is wrong, Op mode needs to be updated each time atep
            AirLoopControlInfo(AirLoopNum).FanOpMode = Furnace(FurnaceNum).OpMode;

            // Check that heat pump heating capacity is within 20% of cooling capacity
            if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir) {
                if (std::abs(Furnace(FurnaceNum).DesignCoolingCapacity - Furnace(FurnaceNum).DesignHeatingCapacity) /
                        Furnace(FurnaceNum).DesignCoolingCapacity >
                    0.2) {
                    ShowWarningError(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                     "\" heating capacity is disproportionate (> 20% different) to total cooling capacity");
                }
            }
        }

        if (!DoingSizing && MySecondOneTimeFlag(FurnaceNum)) {
            // sizing all done.  check fan air flow rates
            errFlag = false;
            FanVolFlowRate = GetFanDesignVolumeFlowRate(BlankString, BlankString, errFlag, Furnace(FurnaceNum).FanIndex);
            Furnace(FurnaceNum).ActualFanVolFlowRate = FanVolFlowRate;
            if (errFlag) {
                ShowContinueError("...occurs in " + cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " =" + Furnace(FurnaceNum).Name);
            }
            if (FanVolFlowRate != AutoSize) {
                if (Furnace(FurnaceNum).DesignFanVolFlowRate > FanVolFlowRate) {
                    ShowWarningError(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + '=' + Furnace(FurnaceNum).Name +
                                     " has a Design Fan Volume Flow Rate > Max Fan Volume Flow Rate, should be <=");
                    ShowContinueError("... Entered value=" + RoundSigDigits(Furnace(FurnaceNum).DesignFanVolFlowRate, 2) + "... Fan [" +
                                      cFanTypes(Furnace(FurnaceNum).FanType_Num) + "] Max Value=" + RoundSigDigits(FanVolFlowRate, 2));
                }
                if (Furnace(FurnaceNum).DesignFanVolFlowRate <= 0.0) {
                    ShowSevereError(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + '=' + Furnace(FurnaceNum).Name +
                                    " has a Design Fan Volume Flow Rate <= 0.0, it must be >0.0");
                    ShowContinueError("... Entered value=" + RoundSigDigits(Furnace(FurnaceNum).DesignFanVolFlowRate, 2));
                }

                MySecondOneTimeFlag(FurnaceNum) = false;
            }
        }

        // Scan hot water and steam heating coil plant components for one time initializations
        if (MyPlantScanFlag(FurnaceNum) && allocated(PlantLoop)) {
            if ((Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingWater) || (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingSteam)) {

                if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingWater) {

                    errFlag = false;
                    ScanPlantLoopsForObject(Furnace(FurnaceNum).HeatingCoilName,
                                            TypeOf_CoilWaterSimpleHeating,
                                            Furnace(FurnaceNum).LoopNum,
                                            Furnace(FurnaceNum).LoopSide,
                                            Furnace(FurnaceNum).BranchNum,
                                            Furnace(FurnaceNum).CompNum,
                                            _,
                                            _,
                                            _,
                                            _,
                                            _,
                                            errFlag);
                    if (errFlag) {
                        ShowFatalError("InitFurnace: Program terminated for previous conditions.");
                    }
                    Furnace(FurnaceNum).MaxHeatCoilFluidFlow =
                        GetCoilMaxWaterFlowRate("Coil:Heating:Water", Furnace(FurnaceNum).HeatingCoilName, ErrorsFound);
                    if (Furnace(FurnaceNum).MaxHeatCoilFluidFlow > 0.0) {
                        rho = GetDensityGlycol(PlantLoop(Furnace(FurnaceNum).LoopNum).FluidName,
                                               CWInitConvTemp,
                                               PlantLoop(Furnace(FurnaceNum).LoopNum).FluidIndex,
                                               RoutineName);
                        Furnace(FurnaceNum).MaxHeatCoilFluidFlow *= rho;
                    }
                } else if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingSteam) {

                    errFlag = false;
                    ScanPlantLoopsForObject(Furnace(FurnaceNum).HeatingCoilName,
                                            TypeOf_CoilSteamAirHeating,
                                            Furnace(FurnaceNum).LoopNum,
                                            Furnace(FurnaceNum).LoopSide,
                                            Furnace(FurnaceNum).BranchNum,
                                            Furnace(FurnaceNum).CompNum,
                                            _,
                                            _,
                                            _,
                                            _,
                                            _,
                                            errFlag);
                    if (errFlag) {
                        ShowFatalError("InitFurnace: Program terminated for previous conditions.");
                    }
                    Furnace(FurnaceNum).MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).HeatingCoilIndex, ErrorsFound);
                    if (Furnace(FurnaceNum).MaxHeatCoilFluidFlow > 0.0) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName);
                        Furnace(FurnaceNum).MaxHeatCoilFluidFlow *= SteamDensity;
                    }
                }
                // fill outlet node for coil
                Furnace(FurnaceNum).CoilOutletNode = PlantLoop(Furnace(FurnaceNum).LoopNum)
                                                         .LoopSide(Furnace(FurnaceNum).LoopSide)
                                                         .Branch(Furnace(FurnaceNum).BranchNum)
                                                         .Comp(Furnace(FurnaceNum).CompNum)
                                                         .NodeNumOut;
                MyPlantScanFlag(FurnaceNum) = false;
            } else { // pthp not connected to plant
                MyPlantScanFlag(FurnaceNum) = false;
            }
        } else if (MyPlantScanFlag(FurnaceNum) && !AnyPlantInModel) {
            MyPlantScanFlag(FurnaceNum) = false;
        }

        // Scan Supplemental hot water and steam heating coil plant components for one time initializations
        if (MySuppCoilPlantScanFlag(FurnaceNum) && allocated(PlantLoop)) {
            if ((Furnace(FurnaceNum).SuppHeatCoilType_Num == Coil_HeatingWater) || (Furnace(FurnaceNum).SuppHeatCoilType_Num == Coil_HeatingSteam)) {

                if (Furnace(FurnaceNum).SuppHeatCoilType_Num == Coil_HeatingWater) {
                    errFlag = false;
                    ScanPlantLoopsForObject(Furnace(FurnaceNum).SuppHeatCoilName,
                                            TypeOf_CoilWaterSimpleHeating,
                                            Furnace(FurnaceNum).LoopNumSupp,
                                            Furnace(FurnaceNum).LoopSideSupp,
                                            Furnace(FurnaceNum).BranchNumSupp,
                                            Furnace(FurnaceNum).CompNumSupp,
                                            _,
                                            _,
                                            _,
                                            _,
                                            _,
                                            errFlag);
                    if (errFlag) {
                        ShowFatalError("InitFurnace: Program terminated for previous conditions.");
                    }
                    Furnace(FurnaceNum).MaxSuppCoilFluidFlow =
                        GetCoilMaxWaterFlowRate("Coil:Heating:Water", Furnace(FurnaceNum).SuppHeatCoilName, ErrorsFound);
                    if (Furnace(FurnaceNum).MaxSuppCoilFluidFlow > 0.0) {
                        rho = GetDensityGlycol(PlantLoop(Furnace(FurnaceNum).LoopNumSupp).FluidName,
                                               CWInitConvTemp,
                                               PlantLoop(Furnace(FurnaceNum).LoopNumSupp).FluidIndex,
                                               RoutineName);
                        Furnace(FurnaceNum).MaxSuppCoilFluidFlow *= rho;
                    }
                } else if (Furnace(FurnaceNum).SuppHeatCoilType_Num == Coil_HeatingSteam) {
                    errFlag = false;
                    ScanPlantLoopsForObject(Furnace(FurnaceNum).SuppHeatCoilName,
                                            TypeOf_CoilSteamAirHeating,
                                            Furnace(FurnaceNum).LoopNumSupp,
                                            Furnace(FurnaceNum).LoopSideSupp,
                                            Furnace(FurnaceNum).BranchNumSupp,
                                            Furnace(FurnaceNum).CompNumSupp,
                                            _,
                                            _,
                                            _,
                                            _,
                                            _,
                                            errFlag);
                    if (errFlag) {
                        ShowFatalError("InitFurnace: Program terminated for previous conditions.");
                    }
                    Furnace(FurnaceNum).MaxSuppCoilFluidFlow = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).SuppHeatCoilIndex, ErrorsFound);
                    if (Furnace(FurnaceNum).MaxSuppCoilFluidFlow > 0.0) {
                        SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName);
                        Furnace(FurnaceNum).MaxSuppCoilFluidFlow *= SteamDensity;
                    }
                }
                // fill outlet node for coil
                Furnace(FurnaceNum).SuppCoilOutletNode = PlantLoop(Furnace(FurnaceNum).LoopNumSupp)
                                                             .LoopSide(Furnace(FurnaceNum).LoopSideSupp)
                                                             .Branch(Furnace(FurnaceNum).BranchNumSupp)
                                                             .Comp(Furnace(FurnaceNum).CompNumSupp)
                                                             .NodeNumOut;
                MySuppCoilPlantScanFlag(FurnaceNum) = false;
            } else { // pthp not connected to plant
                MySuppCoilPlantScanFlag(FurnaceNum) = false;
            }

        } else if (MySuppCoilPlantScanFlag(FurnaceNum) && !AnyPlantInModel) {
            MySuppCoilPlantScanFlag(FurnaceNum) = false;
        }

        // Do the Begin Environment initializations
        if (BeginEnvrnFlag && MyEnvrnFlag(FurnaceNum)) {
            // Change the Volume Flow Rates to Mass Flow Rates
            Furnace(FurnaceNum).DesignMassFlowRate = Furnace(FurnaceNum).DesignFanVolFlowRate * StdRhoAir;
            Furnace(FurnaceNum).MaxCoolAirMassFlow = Furnace(FurnaceNum).MaxCoolAirVolFlow * StdRhoAir;
            Furnace(FurnaceNum).MaxHeatAirMassFlow = Furnace(FurnaceNum).MaxHeatAirVolFlow * StdRhoAir;
            Furnace(FurnaceNum).MaxNoCoolHeatAirMassFlow = Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow * StdRhoAir;
            Furnace(FurnaceNum).WSHPRuntimeFrac = 0.0;
            Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
            Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
            Furnace(FurnaceNum).CoolingCoilLatentDemand = 0.0;
            Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;

            Furnace(FurnaceNum).SenLoadLoss = 0.0;
            if (Furnace(FurnaceNum).Humidistat) {
                Furnace(FurnaceNum).LatLoadLoss = 0.0;
            }

            //   set fluid-side hardware limits
            if (Furnace(FurnaceNum).CoilControlNode > 0) {

                if (Furnace(FurnaceNum).MaxHeatCoilFluidFlow == AutoSize) {
                    // If water coil max water flow rate is autosized, simulate once in order to mine max flow rate
                    if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingWater) {
                        SimulateWaterCoilComponents(Furnace(FurnaceNum).HeatingCoilName, FirstHVACIteration, Furnace(FurnaceNum).HeatingCoilIndex);
                        CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate("Coil:Heating:Water", Furnace(FurnaceNum).HeatingCoilName, ErrorsFound);
                        if (CoilMaxVolFlowRate != AutoSize) {
                            rho = GetDensityGlycol(PlantLoop(Furnace(FurnaceNum).LoopNum).FluidName,
                                                   CWInitConvTemp,
                                                   PlantLoop(Furnace(FurnaceNum).LoopNum).FluidIndex,
                                                   RoutineName);
                            Furnace(FurnaceNum).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                    }
                    // If steam coil max steam flow rate is autosized, simulate once in order to mine max flow rate
                    if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingSteam) {
                        SimulateSteamCoilComponents(Furnace(FurnaceNum).HeatingCoilName,
                                                    FirstHVACIteration,
                                                    Furnace(FurnaceNum).HeatingCoilIndex,
                                                    1.0,
                                                    QActual); // QCoilReq, simulate any load > 0 to get max capacity
                        CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).HeatingCoilIndex, ErrorsFound);
                        if (CoilMaxVolFlowRate != AutoSize) {
                            SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName);
                            Furnace(FurnaceNum).MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                    }
                }

                InitComponentNodes(0.0,
                                   Furnace(FurnaceNum).MaxHeatCoilFluidFlow,
                                   Furnace(FurnaceNum).CoilControlNode,
                                   Furnace(FurnaceNum).CoilOutletNode,
                                   Furnace(FurnaceNum).LoopNum,
                                   Furnace(FurnaceNum).LoopSide,
                                   Furnace(FurnaceNum).BranchNum,
                                   Furnace(FurnaceNum).CompNum);
            }
            if (Furnace(FurnaceNum).SuppCoilControlNode > 0) {
                if (Furnace(FurnaceNum).MaxSuppCoilFluidFlow == AutoSize) {
                    if (Furnace(FurnaceNum).SuppHeatCoilType_Num == Coil_HeatingWater) {
                        // If water coil max water flow rate is autosized, simulate once in order to mine max flow rate
                        SimulateWaterCoilComponents(Furnace(FurnaceNum).SuppHeatCoilName, FirstHVACIteration, Furnace(FurnaceNum).SuppHeatCoilIndex);
                        CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate("Coil:Heating:Water", Furnace(FurnaceNum).SuppHeatCoilName, ErrorsFound);
                        if (CoilMaxVolFlowRate != AutoSize) {
                            rho = GetDensityGlycol(PlantLoop(Furnace(FurnaceNum).LoopNumSupp).FluidName,
                                                   CWInitConvTemp,
                                                   PlantLoop(Furnace(FurnaceNum).LoopNumSupp).FluidIndex,
                                                   RoutineName);
                            Furnace(FurnaceNum).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                    }
                    if (Furnace(FurnaceNum).SuppHeatCoilType_Num == Coil_HeatingSteam) {
                        SimulateSteamCoilComponents(Furnace(FurnaceNum).SuppHeatCoilName,
                                                    FirstHVACIteration,
                                                    Furnace(FurnaceNum).SuppHeatCoilIndex,
                                                    1.0,
                                                    QActual); // QCoilReq, simulate any load > 0 to get max capacity
                        CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(Furnace(FurnaceNum).SuppHeatCoilIndex, ErrorsFound);
                        if (CoilMaxVolFlowRate != AutoSize) {
                            SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, RoutineName);
                            Furnace(FurnaceNum).MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                    }
                    InitComponentNodes(0.0,
                                       Furnace(FurnaceNum).MaxSuppCoilFluidFlow,
                                       Furnace(FurnaceNum).SuppCoilControlNode,
                                       Furnace(FurnaceNum).SuppCoilOutletNode,
                                       Furnace(FurnaceNum).LoopNumSupp,
                                       Furnace(FurnaceNum).LoopSideSupp,
                                       Furnace(FurnaceNum).BranchNumSupp,
                                       Furnace(FurnaceNum).CompNumSupp);
                }
            }
            MyEnvrnFlag(FurnaceNum) = false;
        }

        if (!BeginEnvrnFlag) {
            MyEnvrnFlag(FurnaceNum) = true;
        }

        if (MyFanFlag(FurnaceNum)) {
            if (Furnace(FurnaceNum).ActualFanVolFlowRate != AutoSize) {
                if (Furnace(FurnaceNum).ActualFanVolFlowRate > 0.0) {
                    Furnace(FurnaceNum).HeatingSpeedRatio = Furnace(FurnaceNum).MaxHeatAirVolFlow / Furnace(FurnaceNum).ActualFanVolFlowRate;
                    Furnace(FurnaceNum).CoolingSpeedRatio = Furnace(FurnaceNum).MaxCoolAirVolFlow / Furnace(FurnaceNum).ActualFanVolFlowRate;
                    Furnace(FurnaceNum).NoHeatCoolSpeedRatio = Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow / Furnace(FurnaceNum).ActualFanVolFlowRate;
                }
                if (GetFanSpeedRatioCurveIndex(FanType, FanName, Furnace(FurnaceNum).FanIndex) > 0) {
                    if (Furnace(FurnaceNum).ActualFanVolFlowRate == Furnace(FurnaceNum).MaxHeatAirVolFlow &&
                        Furnace(FurnaceNum).ActualFanVolFlowRate == Furnace(FurnaceNum).MaxCoolAirVolFlow &&
                        Furnace(FurnaceNum).ActualFanVolFlowRate == Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow) {
                        ShowWarningError(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name + "\"");
                        ShowContinueError("...For fan type and name = " + FanType + " \"" + FanName + "\"");
                        ShowContinueError("...Fan power ratio function of speed ratio curve has no impact if fan volumetric flow rate is the same as "
                                          "the unitary system volumetric flow rate.");
                        ShowContinueError("...Fan volumetric flow rate            = " + RoundSigDigits(Furnace(FurnaceNum).ActualFanVolFlowRate, 5) +
                                          " m3/s.");
                        ShowContinueError("...Unitary system volumetric flow rate = " + RoundSigDigits(Furnace(FurnaceNum).MaxHeatAirVolFlow, 5) +
                                          " m3/s.");
                    }
                }
                MyFanFlag(FurnaceNum) = false;
            } else {
                Furnace(FurnaceNum).ActualFanVolFlowRate =
                    GetFanDesignVolumeFlowRate(BlankString, BlankString, errFlag, Furnace(FurnaceNum).FanIndex);
            }
        }

        if (allocated(ZoneEquipConfig) && MyCheckFlag(FurnaceNum)) {
            int zoneNum = Zone(Furnace(FurnaceNum).ControlZoneNum).ZoneEqNum;
            int zoneInlet = Furnace(FurnaceNum).ZoneInletNode;
            int coolingPriority = 0;
            int heatingPriority = 0;
            // setup furnace zone equipment sequence information based on finding matching air terminal
            if (ZoneEquipConfig(zoneNum).EquipListIndex > 0) {
                ZoneEquipList(ZoneEquipConfig(zoneNum).EquipListIndex).getPrioritiesforInletNode(zoneInlet, coolingPriority, heatingPriority);
                Furnace(FurnaceNum).ZoneSequenceCoolingNum = coolingPriority;
                Furnace(FurnaceNum).ZoneSequenceHeatingNum = heatingPriority;
            }
            MyCheckFlag(FurnaceNum) = false;
            if (Furnace(FurnaceNum).ZoneSequenceCoolingNum == 0) {
                ShowSevereError(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                "\": No matching air terminal found in the zone equipment list for zone = " +
                                Zone(Furnace(FurnaceNum).ControlZoneNum).Name + ".");
                ShowFatalError("Subroutine InitFurnace: Errors found in getting " + cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) +
                               " input.  Preceding condition(s) causes termination.");
            }
        }

        // Find the number of zones (zone Inlet Nodes) attached to an air loop from the air loop number
        NumAirLoopZones = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled + AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
        if (allocated(AirToZoneNodeInfo) && MyFlowFracFlag(FurnaceNum)) {
            FlowFracFlagReady = true;
            for (ZoneInSysIndex = 1; ZoneInSysIndex <= NumAirLoopZones; ++ZoneInSysIndex) {
                // zone inlet nodes for cooling
                if (AirToZoneNodeInfo(AirLoopNum).NumZonesCooled > 0) {
                    if (AirToZoneNodeInfo(AirLoopNum).TermUnitCoolInletNodes(ZoneInSysIndex) == -999) {
                        // the data structure for the zones inlet nodes has not been filled
                        FlowFracFlagReady = false;
                    }
                }
                // zone inlet nodes for heating
                if (AirToZoneNodeInfo(AirLoopNum).NumZonesHeated > 0) {
                    if (AirToZoneNodeInfo(AirLoopNum).TermUnitHeatInletNodes(ZoneInSysIndex) == -999) {
                        // the data structure for the zones inlet nodes has not been filled
                        FlowFracFlagReady = false;
                    }
                }
            }
        }

        if (MyFlowFracFlag(FurnaceNum)) {
            if (allocated(AirToZoneNodeInfo) && FlowFracFlagReady) {
                SumOfMassFlowRateMax = 0.0; // initialize the sum of the maximum flows
                for (ZoneInSysIndex = 1; ZoneInSysIndex <= NumAirLoopZones; ++ZoneInSysIndex) {
                    ZoneInletNodeNum = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolInletNodes(ZoneInSysIndex);
                    SumOfMassFlowRateMax += Node(ZoneInletNodeNum).MassFlowRateMax;
                    if (AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZoneInSysIndex) == Furnace(FurnaceNum).ControlZoneNum) {
                        CntrlZoneTerminalUnitMassFlowRateMax = Node(ZoneInletNodeNum).MassFlowRateMax;
                    }
                }
                if (SumOfMassFlowRateMax != 0.0) {
                    if (CntrlZoneTerminalUnitMassFlowRateMax >= SmallAirVolFlow) {
                        Furnace(FurnaceNum).ControlZoneMassFlowFrac = CntrlZoneTerminalUnitMassFlowRateMax / SumOfMassFlowRateMax;
                    } else {
                        ShowSevereError(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " = " + Furnace(FurnaceNum).Name);
                        ShowContinueError(" The Fraction of Supply Air Flow That Goes Through the Controlling Zone is set to 1.");
                    }
                    ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num),
                                       Furnace(FurnaceNum).Name,
                                       "Fraction of Supply Air Flow That Goes Through the Controlling Zone",
                                       Furnace(FurnaceNum).ControlZoneMassFlowFrac);
                    MyFlowFracFlag(FurnaceNum) = false;
                }
            }
        }

        // Calcuate air distribution losses
        if (!FirstHVACIteration && AirLoopPass == 1) {
            ZoneInNode = Furnace(FurnaceNum).ZoneInletNode;
            MinHumRat = Node(ZoneInNode).HumRat;
            MassFlowRate = Node(ZoneInNode).MassFlowRate / Furnace(FurnaceNum).ControlZoneMassFlowFrac;
            if (Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).Temp < Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp)
                MinHumRat = Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).HumRat;
            if (SimulateAirflowNetwork > AirflowNetworkControlMultizone) {
                DeltaMassRate = Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).MassFlowRate -
                                Node(ZoneInNode).MassFlowRate / Furnace(FurnaceNum).ControlZoneMassFlowFrac;
                if (DeltaMassRate < 0.0) DeltaMassRate = 0.0;
            } else {
                MassFlowRate = Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).MassFlowRate;
                DeltaMassRate = 0.0;
            }
            Furnace(FurnaceNum).SenLoadLoss = MassFlowRate * (PsyHFnTdbW(Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).Temp, MinHumRat) -
                                                              PsyHFnTdbW(Node(ZoneInNode).Temp, MinHumRat)) +
                                              DeltaMassRate * (PsyHFnTdbW(Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).Temp, MinHumRat) -
                                                               PsyHFnTdbW(Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp, MinHumRat));
            if (std::abs(Furnace(FurnaceNum).SensibleLoadMet) > 0.0) {
                if (std::abs(Furnace(FurnaceNum).SenLoadLoss / Furnace(FurnaceNum).SensibleLoadMet) < 0.001) Furnace(FurnaceNum).SenLoadLoss = 0.0;
            }
            if (Furnace(FurnaceNum).Humidistat) {
                MaxTemp = Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp;
                Furnace(FurnaceNum).LatLoadLoss = MassFlowRate * (PsyHFnTdbW(MaxTemp, Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).HumRat) -
                                                                  PsyHFnTdbW(MaxTemp, Node(ZoneInNode).HumRat)) +
                                                  DeltaMassRate * (PsyHFnTdbW(MaxTemp, Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).HumRat) -
                                                                   PsyHFnTdbW(MaxTemp, Node(Furnace(FurnaceNum).NodeNumOfControlledZone).HumRat));
                if (std::abs(Furnace(FurnaceNum).LatentLoadMet) > 0.0) {
                    if (std::abs(Furnace(FurnaceNum).LatLoadLoss / Furnace(FurnaceNum).LatentLoadMet) < 0.001) Furnace(FurnaceNum).LatLoadLoss = 0.0;
                }
            }
        }

        if (Furnace(FurnaceNum).FanSchedPtr > 0) {
            if (GetCurrentScheduleValue(Furnace(FurnaceNum).FanSchedPtr) == 0.0) {
                Furnace(FurnaceNum).OpMode = CycFanCycCoil;
            } else {
                Furnace(FurnaceNum).OpMode = ContFanCycCoil;
            }
            if (AirLoopNum > 0) {
                AirLoopControlInfo(AirLoopNum).FanOpMode = Furnace(FurnaceNum).OpMode;
            }
        }

        OpMode = Furnace(FurnaceNum).OpMode;
        EconomizerFlag = AirLoopControlInfo(AirLoopNum).EconoActive;

        if (Furnace(FurnaceNum).ControlZoneMassFlowFrac > 0.0) {
            QZnReq = ZoneLoad / Furnace(FurnaceNum).ControlZoneMassFlowFrac;
            MoistureLoad /= Furnace(FurnaceNum).ControlZoneMassFlowFrac;
            ZoneLoad = QZnReq;
        } else {
            QZnReq = ZoneLoad;
        }

        // Original thermostat control logic (works only for cycling fan systems)
        if (QZnReq > SmallLoad && QZnReq > (Small5WLoad / Furnace(FurnaceNum).ControlZoneMassFlowFrac) &&
            !CurDeadBandOrSetback(Furnace(FurnaceNum).ControlZoneNum)) {
            HeatingLoad = true;
            CoolingLoad = false;
        } else if (QZnReq < (-1.0 * SmallLoad) && std::abs(QZnReq) > (Small5WLoad / Furnace(FurnaceNum).ControlZoneMassFlowFrac) &&
                   !CurDeadBandOrSetback(Furnace(FurnaceNum).ControlZoneNum)) {
            HeatingLoad = false;
            CoolingLoad = true;
        } else {
            HeatingLoad = false;
            CoolingLoad = false;
        }

        if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir ||
            (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir &&
             (Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_Simple || Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_VarSpeedEquationFit))) {
            if (MoistureLoad < 0.0 && Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat) {
                HPDehumidificationLoadFlag = true;
                HeatingLoad = false;
                CoolingLoad = true;
            } else {
                HPDehumidificationLoadFlag = false;
            }
        }

        // Check for heat only furnace
        if (Furnace(FurnaceNum).FurnaceType_Num != Furnace_HeatOnly && Furnace(FurnaceNum).FurnaceType_Num != UnitarySys_HeatOnly) {

            if (GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0) {
                if ((HeatingLoad || CoolingLoad) || (Furnace(FurnaceNum).Humidistat && MoistureLoad < 0.0)) {
                    PartLoadRatio = 1.0;
                } else {
                    PartLoadRatio = 0.0;
                }
            } else {
                PartLoadRatio = 0.0;
            }
        } else {
            PartLoadRatio = 1.0;
        }

        // get current time step operating capacity of water and steam coils
        // (dependent on entering water and steam temperature)
        if (FirstHVACIteration) {
            if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingWater) {
                // set water-side mass flow rates
                Node(Furnace(FurnaceNum).HWCoilAirInletNode).MassFlowRate = CompOnMassFlow;
                mdot = Furnace(FurnaceNum).MaxHeatCoilFluidFlow;
                SetComponentFlowRate(mdot,
                                     Furnace(FurnaceNum).CoilControlNode,
                                     Furnace(FurnaceNum).CoilOutletNode,
                                     Furnace(FurnaceNum).LoopNum,
                                     Furnace(FurnaceNum).LoopSide,
                                     Furnace(FurnaceNum).BranchNum,
                                     Furnace(FurnaceNum).CompNum);
                //     simulate water coil to find operating capacity
                SimulateWaterCoilComponents(Furnace(FurnaceNum).HeatingCoilName, FirstHVACIteration, Furnace(FurnaceNum).HeatingCoilIndex, QActual);
                Furnace(FurnaceNum).DesignHeatingCapacity = QActual;

            } // from IF(Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN

            if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingSteam) {
                // set air-side and steam-side mass flow rates
                Node(Furnace(FurnaceNum).HWCoilAirInletNode).MassFlowRate = CompOnMassFlow;
                mdot = Furnace(FurnaceNum).MaxHeatCoilFluidFlow;
                SetComponentFlowRate(mdot,
                                     Furnace(FurnaceNum).CoilControlNode,
                                     Furnace(FurnaceNum).CoilOutletNode,
                                     Furnace(FurnaceNum).LoopNum,
                                     Furnace(FurnaceNum).LoopSide,
                                     Furnace(FurnaceNum).BranchNum,
                                     Furnace(FurnaceNum).CompNum);

                //     simulate steam coil to find operating capacity
                SimulateSteamCoilComponents(Furnace(FurnaceNum).HeatingCoilName,
                                            FirstHVACIteration,
                                            Furnace(FurnaceNum).HeatingCoilIndex,
                                            1.0,
                                            QActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
                Furnace(FurnaceNum).DesignHeatingCapacity =
                    GetSteamCoilCapacity(Furnace(FurnaceNum).HeatingCoilType, Furnace(FurnaceNum).HeatingCoilName, ErrorsFound);

            } // from IF(Furnace(FurnaceNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN

            if (Furnace(FurnaceNum).SuppHeatCoilType_Num == Coil_HeatingWater) {

                //     set air-side and steam-side mass flow rates
                Node(Furnace(FurnaceNum).SuppCoilAirInletNode).MassFlowRate = CompOnMassFlow;
                mdot = Furnace(FurnaceNum).MaxSuppCoilFluidFlow;
                SetComponentFlowRate(mdot,
                                     Furnace(FurnaceNum).SuppCoilControlNode,
                                     Furnace(FurnaceNum).SuppCoilOutletNode,
                                     Furnace(FurnaceNum).LoopNumSupp,
                                     Furnace(FurnaceNum).LoopSideSupp,
                                     Furnace(FurnaceNum).BranchNumSupp,
                                     Furnace(FurnaceNum).CompNumSupp);

                //     simulate water coil to find operating capacity
                SimulateWaterCoilComponents(Furnace(FurnaceNum).SuppHeatCoilName, FirstHVACIteration, Furnace(FurnaceNum).SuppHeatCoilIndex, QActual);
                Furnace(FurnaceNum).DesignSuppHeatingCapacity = QActual;

            } // from IF(Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN
            if (Furnace(FurnaceNum).SuppHeatCoilType_Num == Coil_HeatingSteam) {
                //     set air-side and steam-side mass flow rates
                Node(Furnace(FurnaceNum).SuppCoilAirInletNode).MassFlowRate = CompOnMassFlow;
                mdot = Furnace(FurnaceNum).MaxSuppCoilFluidFlow;
                SetComponentFlowRate(mdot,
                                     Furnace(FurnaceNum).SuppCoilControlNode,
                                     Furnace(FurnaceNum).SuppCoilOutletNode,
                                     Furnace(FurnaceNum).LoopNumSupp,
                                     Furnace(FurnaceNum).LoopSideSupp,
                                     Furnace(FurnaceNum).BranchNumSupp,
                                     Furnace(FurnaceNum).CompNumSupp);

                //     simulate steam coil to find operating capacity
                SimulateSteamCoilComponents(Furnace(FurnaceNum).SuppHeatCoilName,
                                            FirstHVACIteration,
                                            Furnace(FurnaceNum).SuppHeatCoilIndex,
                                            1.0,
                                            QActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
                Furnace(FurnaceNum).DesignSuppHeatingCapacity =
                    GetSteamCoilCapacity(Furnace(FurnaceNum).SuppHeatCoilType, Furnace(FurnaceNum).SuppHeatCoilName, ErrorsFound);

            } // from IF(Furnace(FurnaceNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
        }     // from IF( FirstHVACIteration ) THEN

        if (Furnace(FurnaceNum).NumOfSpeedCooling > 0) { // BoS, variable-speed water source hp
            // Furnace(FurnaceNum)%IdleMassFlowRate = RhoAir*Furnace(FurnaceNum)%IdleVolumeAirRate
            NumOfSpeedCooling = Furnace(FurnaceNum).NumOfSpeedCooling;
            NumOfSpeedHeating = Furnace(FurnaceNum).NumOfSpeedHeating;
            // IF MSHP system was not autosized and the fan is autosized, check that fan volumetric flow rate is greater than MSHP flow rates
            if (Furnace(FurnaceNum).CheckFanFlow) {
                CurrentModuleObject = "AirLoopHVAC:UnitaryHeatPump:VariableSpeed";
                GetFanVolFlow(Furnace(FurnaceNum).FanIndex, Furnace(FurnaceNum).FanVolFlow);

                if (Furnace(FurnaceNum).bIsIHP) // set max fan flow rate to the IHP collection
                {
                    IHPIndex = Furnace(FurnaceNum).CoolingCoilIndex;
                };

                if (Furnace(FurnaceNum).FanVolFlow != AutoSize) {
                    //     Check fan versus system supply air flow rates
                    if (Furnace(FurnaceNum).FanVolFlow + 1e-10 < Furnace(FurnaceNum).CoolVolumeFlowRate(NumOfSpeedCooling)) {
                        ShowWarningError(CurrentModuleObject + " - air flow rate = " + TrimSigDigits(Furnace(FurnaceNum).FanVolFlow, 7) +
                                         " in fan object is less than the MSHP system air flow rate when cooling is required (" +
                                         TrimSigDigits(Furnace(FurnaceNum).CoolVolumeFlowRate(NumOfSpeedCooling), 7) + ").");
                        ShowContinueError(
                            " The MSHP system flow rate when cooling is required is reset to the fan flow rate and the simulation continues.");
                        ShowContinueError(" Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        Furnace(FurnaceNum).CoolVolumeFlowRate(NumOfSpeedCooling) = Furnace(FurnaceNum).FanVolFlow;

                        if (Furnace(FurnaceNum).bIsIHP) // set max fan flow rate to the IHP collection
                        {
                            IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).MaxCoolAirVolFlow = Furnace(FurnaceNum).FanVolFlow;
                            IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).MaxCoolAirMassFlow = Furnace(FurnaceNum).FanVolFlow * StdRhoAir;
                        };

                        // Check flow rates in other speeds and ensure flow rates are not above the max flow rate
                        for (int i = NumOfSpeedCooling - 1; i >= 1; --i) {
                            if (Furnace(FurnaceNum).CoolVolumeFlowRate(i) > Furnace(FurnaceNum).CoolVolumeFlowRate(i + 1)) {
                                ShowContinueError(" The MSHP system flow rate when cooling is required is reset to the flow rate at higher speed and "
                                                  "the simulation continues at Speed" +
                                                  TrimSigDigits(i) + '.');
                                ShowContinueError(" Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                                Furnace(FurnaceNum).CoolVolumeFlowRate(i) = Furnace(FurnaceNum).CoolVolumeFlowRate(i + 1);
                            }
                        }
                    }
                    if (NumOfSpeedHeating > 0) {
                        if (Furnace(FurnaceNum).FanVolFlow + 1e-10 < Furnace(FurnaceNum).HeatVolumeFlowRate(NumOfSpeedHeating)) {
                            ShowWarningError(CurrentModuleObject + " - air flow rate = " + TrimSigDigits(Furnace(FurnaceNum).FanVolFlow, 7) +
                                             " in fan object is less than the MSHP system air flow rate when heating is required (" +
                                             TrimSigDigits(Furnace(FurnaceNum).HeatVolumeFlowRate(NumOfSpeedHeating), 7) + ").");
                            ShowContinueError(
                                " The MSHP system flow rate when heating is required is reset to the fan flow rate and the simulation continues.");
                            ShowContinueError(" Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                            Furnace(FurnaceNum).HeatVolumeFlowRate(NumOfSpeedHeating) = Furnace(FurnaceNum).FanVolFlow;

                            if (Furnace(FurnaceNum).bIsIHP) // set max fan flow rate to the IHP collection
                            {
                                IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).MaxHeatAirVolFlow = Furnace(FurnaceNum).FanVolFlow;
                                IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).MaxHeatAirMassFlow =
                                    Furnace(FurnaceNum).FanVolFlow * StdRhoAir;
                            };

                            for (int i = NumOfSpeedHeating - 1; i >= 1; --i) {
                                if (Furnace(FurnaceNum).HeatVolumeFlowRate(i) > Furnace(FurnaceNum).HeatVolumeFlowRate(i + 1)) {
                                    ShowContinueError(" The MSHP system flow rate when heating is required is reset to the flow rate at higher speed "
                                                      "and the simulation continues at Speed" +
                                                      TrimSigDigits(i) + '.');
                                    ShowContinueError(" Occurs in " + CurrentModuleObject + " system = " + Furnace(FurnaceNum).Name);
                                    Furnace(FurnaceNum).HeatVolumeFlowRate(i) = Furnace(FurnaceNum).HeatVolumeFlowRate(i + 1);
                                }
                            }
                        }
                    }
                    if (Furnace(FurnaceNum).FanVolFlow < Furnace(FurnaceNum).IdleVolumeAirRate && Furnace(FurnaceNum).IdleVolumeAirRate != 0.0) {
                        ShowWarningError(CurrentModuleObject + " - air flow rate = " + TrimSigDigits(Furnace(FurnaceNum).FanVolFlow, 7) +
                                         " in fan object is less than the MSHP system air flow rate when no heating or cooling is needed (" +
                                         TrimSigDigits(Furnace(FurnaceNum).IdleVolumeAirRate, 7) + ").");
                        ShowContinueError(" The MSHP system flow rate when no heating or cooling is needed is reset to the fan flow rate and the "
                                          "simulation continues.");
                        ShowContinueError(" Occurs in " + CurrentModuleObject + " = " + Furnace(FurnaceNum).Name);
                        Furnace(FurnaceNum).IdleVolumeAirRate = Furnace(FurnaceNum).FanVolFlow;
                    }
                    RhoAir = StdRhoAir;
                    // set the mass flow rates from the reset volume flow rates
                    for (int i = 1; i <= NumOfSpeedCooling; ++i) {
                        Furnace(FurnaceNum).CoolMassFlowRate(i) = RhoAir * Furnace(FurnaceNum).CoolVolumeFlowRate(i);
                        if (Furnace(FurnaceNum).FanVolFlow > 0.0) {
                            Furnace(FurnaceNum).MSCoolingSpeedRatio(i) = Furnace(FurnaceNum).CoolVolumeFlowRate(i) / Furnace(FurnaceNum).FanVolFlow;
                        }
                    }
                    for (int i = 1; i <= NumOfSpeedHeating; ++i) {
                        Furnace(FurnaceNum).HeatMassFlowRate(i) = RhoAir * Furnace(FurnaceNum).HeatVolumeFlowRate(i);
                        if (Furnace(FurnaceNum).FanVolFlow > 0.0) {
                            Furnace(FurnaceNum).MSHeatingSpeedRatio(i) = Furnace(FurnaceNum).HeatVolumeFlowRate(i) / Furnace(FurnaceNum).FanVolFlow;
                        }
                    }
                    Furnace(FurnaceNum).IdleMassFlowRate = RhoAir * Furnace(FurnaceNum).IdleVolumeAirRate;
                    if (Furnace(FurnaceNum).FanVolFlow > 0.0) {
                        Furnace(FurnaceNum).IdleSpeedRatio = Furnace(FurnaceNum).IdleVolumeAirRate / Furnace(FurnaceNum).FanVolFlow;
                    }
                    // set the node max and min mass flow rates based on reset volume flow rates
                    if (NumOfSpeedCooling > 0 && NumOfSpeedHeating == 0) {
                        Node(InNode).MassFlowRateMax =
                            max(Furnace(FurnaceNum).CoolMassFlowRate(NumOfSpeedCooling), Furnace(FurnaceNum).MaxHeatAirMassFlow);
                        Node(InNode).MassFlowRateMaxAvail =
                            max(Furnace(FurnaceNum).CoolMassFlowRate(NumOfSpeedCooling), Furnace(FurnaceNum).MaxHeatAirMassFlow);
                    } else if (NumOfSpeedCooling == 0 && NumOfSpeedHeating > 0) {
                        Node(InNode).MassFlowRateMax =
                            max(Furnace(FurnaceNum).MaxCoolAirMassFlow, Furnace(FurnaceNum).HeatMassFlowRate(NumOfSpeedHeating));
                        Node(InNode).MassFlowRateMaxAvail =
                            max(Furnace(FurnaceNum).MaxCoolAirMassFlow, Furnace(FurnaceNum).HeatMassFlowRate(NumOfSpeedHeating));
                    } else {
                        Node(InNode).MassFlowRateMax =
                            max(Furnace(FurnaceNum).CoolMassFlowRate(NumOfSpeedCooling), Furnace(FurnaceNum).HeatMassFlowRate(NumOfSpeedHeating));
                        Node(InNode).MassFlowRateMaxAvail =
                            max(Furnace(FurnaceNum).CoolMassFlowRate(NumOfSpeedCooling), Furnace(FurnaceNum).HeatMassFlowRate(NumOfSpeedHeating));
                    }
                    Node(InNode).MassFlowRateMin = 0.0;
                    Node(InNode).MassFlowRateMinAvail = 0.0;
                    Node(OutNode) = Node(InNode);
                }
            }

            Furnace(FurnaceNum).CheckFanFlow = false;

            // CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
            //        AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
            SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
        } else {
            SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
        }

        // Check ventilation/fan load for constant fan systems to see if load to be met changes
        // Same IF logic used in Subroutine SetAverageAirFlow to determine if unit is ON or OFF

        QToCoolSetPt = 0.0;
        QToHeatSetPt = 0.0;
        if (OpMode == ContFanCycCoil && GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0 &&
            ((GetCurrentScheduleValue(Furnace(FurnaceNum).FanAvailSchedPtr) > 0.0 || TurnFansOn) && !TurnFansOff)) {

            if (Furnace(FurnaceNum).NumOfSpeedCooling > 0) {
                CalcVarSpeedHeatPump(FurnaceNum, false, Off, 1, 0.0, 0.0, SensibleOutput, LatentOutput, 0.0, 0.0, OnOffAirFlowRatio, SUPHEATERLOAD);
            } else {
                CalcFurnaceOutput(FurnaceNum, false, 0, Off, 0.0, 0.0, 0.0, 0.0, SensibleOutput, LatentOutput, OnOffAirFlowRatio, false);
            }

            if (Furnace(FurnaceNum).ControlZoneMassFlowFrac > 0.0) {
                if (Furnace(FurnaceNum).ZoneSequenceCoolingNum > 0 && Furnace(FurnaceNum).ZoneSequenceHeatingNum > 0) {
                    QToCoolSetPt = ZoneSysEnergyDemand(Furnace(FurnaceNum).ControlZoneNum)
                                       .SequencedOutputRequiredToCoolingSP(Furnace(FurnaceNum).ZoneSequenceCoolingNum) /
                                   Furnace(FurnaceNum).ControlZoneMassFlowFrac;
                    QToHeatSetPt = ZoneSysEnergyDemand(Furnace(FurnaceNum).ControlZoneNum)
                                       .SequencedOutputRequiredToHeatingSP(Furnace(FurnaceNum).ZoneSequenceHeatingNum) /
                                   Furnace(FurnaceNum).ControlZoneMassFlowFrac;
                } else {
                    QToCoolSetPt = ZoneSysEnergyDemand(Furnace(FurnaceNum).ControlZoneNum).OutputRequiredToCoolingSP /
                                   Furnace(FurnaceNum).ControlZoneMassFlowFrac;
                    QToHeatSetPt = ZoneSysEnergyDemand(Furnace(FurnaceNum).ControlZoneNum).OutputRequiredToHeatingSP /
                                   Furnace(FurnaceNum).ControlZoneMassFlowFrac;
                }
                //     If the furnace has a net cooling capacity (SensibleOutput < 0) and
                //     the zone temp is above the Tstat heating setpoint (QToHeatSetPt < 0) and
                //     the net cooling capacity does not just offset the cooling load
                if (SensibleOutput < 0.0 && QToHeatSetPt < 0.0 &&
                    std::abs(QToCoolSetPt - SensibleOutput) > (Small5WLoad / Furnace(FurnaceNum).ControlZoneMassFlowFrac)) {
                    //       Only switch modes when humidistat is not used or no moisture load exists, otherwise let
                    //       reheat coil pick up load
                    //        IF((SensibleOutput .LT. QToHeatSetPt .AND. .NOT. Furnace(FurnaceNum)%Humidistat) .OR. &
                    //           (SensibleOutput .LT. QToHeatSetPt .AND. Furnace(FurnaceNum)%Humidistat .AND. MoistureLoad .GE. 0.0))THEN
                    if ((SensibleOutput < QToHeatSetPt && !Furnace(FurnaceNum).Humidistat) ||
                        (SensibleOutput < QToHeatSetPt && Furnace(FurnaceNum).Humidistat && MoistureLoad >= 0.0)) {
                        QZnReq = QToHeatSetPt;
                        CoolingLoad = false;
                        //         Don't set mode TRUE unless mode is allowed. Also check for floating zone.
                        if (TempControlType(Furnace(FurnaceNum).ControlZoneNum) == SingleCoolingSetPoint ||
                            TempControlType(Furnace(FurnaceNum).ControlZoneNum) == 0) {
                            HeatingLoad = false;
                        } else {
                            HeatingLoad = true;
                        }

                        if (Furnace(FurnaceNum).NumOfSpeedCooling > 0) {
                            // CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
                            //         AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
                            SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                            CalcVarSpeedHeatPump(
                                FurnaceNum, false, Off, 1, 0.0, 0.0, SensibleOutput, LatentOutput, 0.0, 0.0, OnOffAirFlowRatio, SUPHEATERLOAD);
                        } else {
                            SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                            CalcFurnaceOutput(FurnaceNum, false, 0, Off, 0.0, 0.0, 0.0, 0.0, SensibleOutput, LatentOutput, OnOffAirFlowRatio, false);
                        }
                        if (SensibleOutput > QToHeatSetPt) {
                            //           If changing operating mode (flow rates) does not overshoot heating setpoint, turn off heating
                            QZnReq = 0.0;
                            HeatingLoad = false;
                            if (Furnace(FurnaceNum).NumOfSpeedCooling > 0) {
                                SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                                //               CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
                                //                    AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
                            } else {
                                SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                            }
                        }
                    } else if (SensibleOutput < QZnReq) {
                        //         If the net cooling capacity meets the zone cooling load but does not overshoot heating setpoint, turn off cooling
                        //         (dehumidification may still occur)
                        QZnReq = 0.0;
                        CoolingLoad = false;
                        if (HPDehumidificationLoadFlag) {
                            CoolingLoad = true;
                            HeatingLoad = false;
                        }
                        if (Furnace(FurnaceNum).NumOfSpeedCooling > 0) {
                            SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                            //               CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
                            //                    AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
                        } else {
                            SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                        }
                    }
                    //     the net cooling capacity just offsets the cooling load, turn off cooling
                } else if (SensibleOutput < 0.0 && QToCoolSetPt < 0.0 &&
                           std::abs(QToCoolSetPt - SensibleOutput) < (Small5WLoad / Furnace(FurnaceNum).ControlZoneMassFlowFrac)) {
                    CoolingLoad = false;
                    if (HPDehumidificationLoadFlag) {
                        CoolingLoad = true;
                        HeatingLoad = false;
                    }
                } // SensibleOutput .LT. 0.0d0 .AND. QToHeatSetPt .LT. 0.0d0

                //     If the furnace has a net heating capacity and the zone temp is below the Tstat cooling setpoint and
                //     the net heating capacity does not just offset the heating load
                if (SensibleOutput > 0.0 && QToCoolSetPt > 0.0 &&
                    std::abs(SensibleOutput - QToHeatSetPt) > (Small5WLoad / Furnace(FurnaceNum).ControlZoneMassFlowFrac)) {
                    if (SensibleOutput > QToCoolSetPt) {
                        QZnReq = QToCoolSetPt;
                        //         Don't set mode TRUE unless mode is allowed. Also check for floating zone.
                        if (TempControlType(Furnace(FurnaceNum).ControlZoneNum) == SingleHeatingSetPoint ||
                            TempControlType(Furnace(FurnaceNum).ControlZoneNum) == 0) {
                            CoolingLoad = false;
                        } else {
                            CoolingLoad = true;
                        }
                        HeatingLoad = false;

                        if (Furnace(FurnaceNum).NumOfSpeedCooling > 0) {
                            SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                            //           CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
                            //                    AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
                            CalcVarSpeedHeatPump(
                                FurnaceNum, false, Off, 1, 0.0, 0.0, SensibleOutput, LatentOutput, 0.0, 0.0, OnOffAirFlowRatio, SUPHEATERLOAD);
                        } else {
                            SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                            CalcFurnaceOutput(FurnaceNum, false, 0, Off, 0.0, 0.0, 0.0, 0.0, SensibleOutput, LatentOutput, OnOffAirFlowRatio, false);
                        }
                        if (SensibleOutput < QToCoolSetPt) {
                            //           If changing operating mode (flow rates) does not overshoot cooling setpoint, turn off cooling
                            if (HPDehumidificationLoadFlag) {
                                CoolingLoad = true;
                                HeatingLoad = false;
                            } else {
                                QZnReq = 0.0;
                                CoolingLoad = false;
                            }
                            if (Furnace(FurnaceNum).NumOfSpeedCooling > 0) {
                                SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                                //               CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
                                //                     AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
                            } else {
                                SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                            }
                        }
                    } else if (SensibleOutput > QZnReq) {
                        //         If the net heating capacity meets the zone heating load but does not overshoot, turn off heating
                        QZnReq = 0.0;
                        HeatingLoad = false;
                        if (Furnace(FurnaceNum).NumOfSpeedCooling > 0) {
                            //            CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
                            //                        AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
                            SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                        } else {
                            SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                        }
                    }
                    //     the net heating capacity just offsets the heating load, turn off heating
                } else if (SensibleOutput > 0.0 && QToHeatSetPt > 0.0 &&
                           std::abs(SensibleOutput - QToHeatSetPt) < (Small5WLoad / Furnace(FurnaceNum).ControlZoneMassFlowFrac)) {
                    HeatingLoad = false;
                } // SensibleOutput .GT. 0.0d0 .AND. QToCoolSetPt .GT. 0.0d0
            }     // Furnace(FurnaceNum)%ControlZoneMassFlowFrac .GT. 0.0d0
            ZoneLoad = QZnReq;
        } // OpMode .EQ. ContFanCycCoil

        if (FirstHVACIteration) {
            Furnace(FurnaceNum).iterationCounter = 0;
            Furnace(FurnaceNum).iterationMode = 0;
        }
        Furnace(FurnaceNum).iterationCounter += 1;

        if (CoolingLoad && Furnace(FurnaceNum).iterationCounter <= 20) {
            Furnace(FurnaceNum).iterationMode(Furnace(FurnaceNum).iterationCounter) = CoolingMode;
        } else if (HeatingLoad && Furnace(FurnaceNum).iterationCounter <= 20) {
            Furnace(FurnaceNum).iterationMode(Furnace(FurnaceNum).iterationCounter) = HeatingMode;
        } else if (Furnace(FurnaceNum).iterationCounter <= 20) {
            Furnace(FurnaceNum).iterationMode(Furnace(FurnaceNum).iterationCounter) = NoCoolHeat;
        }

        // IF small loads to meet or not converging, just shut down unit
        if (std::abs(ZoneLoad) < Small5WLoad) {
            ZoneLoad = 0.0;
            CoolingLoad = false;
            HeatingLoad = false;
        } else if (Furnace(FurnaceNum).iterationCounter > 4) { // attempt to lock output (air flow) if oscillations are detected
            OperatingMode = Furnace(FurnaceNum).iterationMode(5);
            OperatingModeMinusOne = Furnace(FurnaceNum).iterationMode(4);
            OperatingModeMinusTwo = Furnace(FurnaceNum).iterationMode(3);
            Oscillate = true;
            if (OperatingMode == OperatingModeMinusOne && OperatingMode == OperatingModeMinusTwo) Oscillate = false;
            if (Oscillate) {
                if (QToCoolSetPt < 0.0) {
                    HeatingLoad = false;
                    CoolingLoad = true;
                    ZoneLoad = QToCoolSetPt;
                } else if (QToHeatSetPt > 0.0) {
                    HeatingLoad = true;
                    CoolingLoad = false;
                    ZoneLoad = QToHeatSetPt;
                } else {
                    HeatingLoad = false;
                    CoolingLoad = false;
                    ZoneLoad = 0.0;
                }
            }
        }

        // EMS override point
        if (Furnace(FurnaceNum).EMSOverrideSensZoneLoadRequest) ZoneLoad = Furnace(FurnaceNum).EMSSensibleZoneLoadValue;
        if (Furnace(FurnaceNum).EMSOverrideMoistZoneLoadRequest) MoistureLoad = Furnace(FurnaceNum).EMSMoistureZoneLoadValue;
        if (Furnace(FurnaceNum).EMSOverrideSensZoneLoadRequest || Furnace(FurnaceNum).EMSOverrideMoistZoneLoadRequest) {
            if ((ZoneLoad != 0.0) && (Furnace(FurnaceNum).EMSOverrideSensZoneLoadRequest)) {
                PartLoadRatio = 1.0;
            } else if ((MoistureLoad != 0.0) && (Furnace(FurnaceNum).EMSOverrideMoistZoneLoadRequest)) {
                PartLoadRatio = 1.0;
            } else {
                PartLoadRatio = 0.0;
            }
            if (Furnace(FurnaceNum).NumOfSpeedCooling > 0) {
                SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio);
                //       CALL SetOnOffMassFlowRateVSCoil(FurnaceNum, Furnace(FurnaceNum)%ControlZoneNum, FirstHVACIteration, &
                //                AirLoopNum, OnOffAirFlowRatio, OpMode, QZnReq, MoistureLoad, PartLoadRatio)
            } else {
                SetOnOffMassFlowRate(FurnaceNum, AirLoopNum, OnOffAirFlowRatio, OpMode, ZoneLoad, MoistureLoad, PartLoadRatio);
            }
        }

        // AirflowNetwork global variable
        if (SimulateAirflowNetwork > AirflowNetworkControlMultizone) {
            AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF = 0.0;
        }
    }

    void SetOnOffMassFlowRate(int const FurnaceNum,             // index to furnace
                              int const EP_UNUSED(AirLoopNum),  // index to air loop !unused1208
                              Real64 &OnOffAirFlowRatio,        // ratio of coil on to coil off air flow rate
                              int const OpMode,                 // fan operating mode
                              Real64 const EP_UNUSED(ZoneLoad), // sensible load to be met (W) !unused1208
                              Real64 const MoistureLoad,        // moisture load to be met (W)
                              Real64 const PartLoadRatio        // coil part-load ratio
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   Sep 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Furnace Components.

        // METHODOLOGY EMPLOYED:
        // The HeatCool furnace/unitarysystem and air-to-air heat pump may have alternate air flow rates
        // in cooling, heating, and when no cooling or heating is needed. Set up the coil (comp) ON and OFF
        // air flow rates. Use these flow rates during the Calc routines to set the average mass flow rates
        // based on PLR.

        // REFERENCES:
        // na

        // Using/Aliasing
        using General::RoundSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        // FLOW:
        // Check for heat only furnace
        if (Furnace(FurnaceNum).FurnaceType_Num != Furnace_HeatOnly && Furnace(FurnaceNum).FurnaceType_Num != UnitarySys_HeatOnly) {

            // Set the system mass flow rates
            if (OpMode == ContFanCycCoil) {
                // Set the compressor or coil ON mass flow rate
                // constant fan mode
                if (HeatingLoad) {
                    //       IF a heating and moisture load exists, operate at the cooling mass flow rate ELSE operate at the heating flow rate
                    if (MoistureLoad < 0.0 && Furnace(FurnaceNum).Humidistat &&
                        Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat) {
                        CompOnMassFlow = Furnace(FurnaceNum).MaxCoolAirMassFlow;
                        CompOnFlowRatio = Furnace(FurnaceNum).CoolingSpeedRatio;
                    } else {
                        CompOnMassFlow = Furnace(FurnaceNum).MaxHeatAirMassFlow;
                        CompOnFlowRatio = Furnace(FurnaceNum).HeatingSpeedRatio;
                    }
                    Furnace(FurnaceNum).LastMode = HeatingMode;
                    //     IF a cooling load exists, operate at the cooling mass flow rate
                } else if (CoolingLoad) {
                    CompOnMassFlow = Furnace(FurnaceNum).MaxCoolAirMassFlow;
                    CompOnFlowRatio = Furnace(FurnaceNum).CoolingSpeedRatio;
                    Furnace(FurnaceNum).LastMode = CoolingMode;
                    //     If no load exists, set the compressor on mass flow rate.
                    //     Set equal the mass flow rate when no heating or cooling is needed if no moisture load exists.
                    //     If the user has set the off mass flow rate to 0, set according to the last operating mode.
                } else {
                    if (MoistureLoad < 0.0 && Furnace(FurnaceNum).Humidistat &&
                        Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat) {
                        CompOnMassFlow = Furnace(FurnaceNum).MaxCoolAirMassFlow;
                        CompOnFlowRatio = Furnace(FurnaceNum).CoolingSpeedRatio;
                    } else {
                        CompOnMassFlow = Furnace(FurnaceNum).MaxNoCoolHeatAirMassFlow;
                        CompOnFlowRatio = Furnace(FurnaceNum).HeatingSpeedRatio;
                        //         User may have entered a 0 for MaxNoCoolHeatAirMassFlow
                        if (CompOnMassFlow == 0.0) {
                            if (Furnace(FurnaceNum).LastMode == HeatingMode) {
                                CompOnMassFlow = Furnace(FurnaceNum).MaxHeatAirMassFlow;
                                CompOnFlowRatio = Furnace(FurnaceNum).HeatingSpeedRatio;
                            } else {
                                CompOnMassFlow = Furnace(FurnaceNum).MaxCoolAirMassFlow;
                                CompOnFlowRatio = Furnace(FurnaceNum).CoolingSpeedRatio;
                            }
                        }
                    }
                }

                //     Set the compressor or coil OFF mass flow rate based on LOGICAL flag
                //     UseCompressorOnFlow is used when the user does not enter a value for no cooling or heating flow rate
                if (Furnace(FurnaceNum).AirFlowControl == UseCompressorOnFlow) {
                    if (Furnace(FurnaceNum).LastMode == HeatingMode) {
                        if (MoistureLoad < 0.0 && Furnace(FurnaceNum).Humidistat &&
                            Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat) {
                            CompOffMassFlow = Furnace(FurnaceNum).MaxCoolAirMassFlow;
                            CompOffFlowRatio = Furnace(FurnaceNum).CoolingSpeedRatio;
                        } else {
                            CompOffMassFlow = Furnace(FurnaceNum).MaxHeatAirMassFlow;
                            CompOffFlowRatio = Furnace(FurnaceNum).HeatingSpeedRatio;
                        }
                    } else {
                        CompOffMassFlow = Furnace(FurnaceNum).MaxCoolAirMassFlow;
                        CompOffFlowRatio = Furnace(FurnaceNum).CoolingSpeedRatio;
                    }
                    //     ELSE use the user specified value
                } else {
                    CompOffMassFlow = Furnace(FurnaceNum).MaxNoCoolHeatAirMassFlow;
                    CompOffFlowRatio = Furnace(FurnaceNum).NoHeatCoolSpeedRatio;
                }
            } else {
                //     cycling fan mode
                if (HeatingLoad || (Furnace(FurnaceNum).Humidistat && MoistureLoad < 0.0 &&
                                    Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat)) {

                    if (Furnace(FurnaceNum).Humidistat && MoistureLoad < 0.0 &&
                        Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat) {
                        CompOnMassFlow = Furnace(FurnaceNum).MaxCoolAirMassFlow;
                        CompOnFlowRatio = Furnace(FurnaceNum).CoolingSpeedRatio;
                        Furnace(FurnaceNum).LastMode = CoolingMode;
                    } else {
                        CompOnMassFlow = Furnace(FurnaceNum).MaxHeatAirMassFlow;
                        CompOnFlowRatio = Furnace(FurnaceNum).HeatingSpeedRatio;
                        Furnace(FurnaceNum).LastMode = HeatingMode;
                    }
                } else if (CoolingLoad) {
                    CompOnMassFlow = Furnace(FurnaceNum).MaxCoolAirMassFlow;
                    CompOnFlowRatio = Furnace(FurnaceNum).CoolingSpeedRatio;
                } else {
                    CompOnMassFlow = 0.0;
                    CompOnFlowRatio = 0.0;
                }
                CompOffMassFlow = 0.0;
                CompOffFlowRatio = 0.0;
            }
        } else { //  Is a HeatOnly furnace

            CompOnMassFlow = Furnace(FurnaceNum).DesignMassFlowRate;
            CompOnFlowRatio = Furnace(FurnaceNum).HeatingSpeedRatio;
            if (OpMode == ContFanCycCoil) {
                CompOffMassFlow = Furnace(FurnaceNum).MaxNoCoolHeatAirMassFlow;
                CompOffFlowRatio = Furnace(FurnaceNum).HeatingSpeedRatio;
            } else {
                CompOffMassFlow = 0.0;
                CompOffFlowRatio = 0.0;
            }

        } // End check for heat only furnace or water-to-air heat pump

        // Set the system mass flow rates
        SetAverageAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);
    }

    void SizeFurnace(int const FurnaceNum, bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   January 2002
        //       MODIFIED       Bereket Nigusse, May 2010, removed the autosize option for the input field supply air
        //                                                 flow fraction through controlled zone.
        //                      Bo Shen, March 2012, size the air flow rates at individual speed levels for VS WSHP
        //                      Bo Shen, ORNL, July 2012 - added variable-speed air source heat pump cooling and heating coils, using curve-fits
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Furnace Components for which nominal cpacities
        // and flow rates have not been specified in the input

        // METHODOLOGY EMPLOYED:
        // Obtains heating capacities and flow rates from the zone or system sizing arrays.
        // NOTE: In UNITARYSYSTEM:HEATPUMP:AIRTOAIR we are sizing the heating capacity to be
        // equal to the cooling capacity.  Thus the cooling and
        // and heating capacities of a DX heat pump system will be identical. In real life the ARI
        // heating and cooling capacities are close but not identical.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataSizing;
        using DataGlobals::emsCallFromUnitarySystemSizing;
        using EMSManager::ManageEMS;
        using General::TrimSigDigits;
        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using IntegratedHeatPump::IntegratedHeatPumps;
        using IntegratedHeatPump::SizeIHP;
        using ReportSizingManager::ReportSizingOutput;
        using VariableSpeedCoils::SimVariableSpeedCoils;
        using VariableSpeedCoils::VarSpeedCoil;
        using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ThisCtrlZoneNum;      // the controlled zone number of the control zone !!!
        int Iter;                 // iteration count
        Real64 MulSpeedFlowScale; // variable speed air flow scaling factor
        int IHPCoilIndex(0);      // refer to cooling or heating coil in IHP
        bool anyRan;
        ManageEMS(emsCallFromUnitarySystemSizing, anyRan); // calling point

        ThisCtrlZoneNum = 0;
        DXCoolCap = 0.0;
        UnitaryHeatCap = 0.0;
        SuppHeatCap = 0.0;
        if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) {
            SimDXCoil(BlankString, On, true, Furnace(FurnaceNum).CoolingCoilIndex, 1, 0.0);
        } else if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {
            SimHXAssistedCoolingCoil(BlankString, true, On, 0.0, Furnace(FurnaceNum).CoolingCoilIndex, 1, false, 1.0, false);
        } else if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingWaterToAirHPSimple) {
            SimWatertoAirHPSimple(BlankString,
                                  Furnace(FurnaceNum).CoolingCoilIndex,
                                  Furnace(FurnaceNum).CoolingCoilSensDemand,
                                  Furnace(FurnaceNum).CoolingCoilLatentDemand,
                                  0,
                                  0.0,
                                  Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                  Furnace(FurnaceNum).HPTimeConstant,
                                  Furnace(FurnaceNum).FanDelayTime,
                                  0,
                                  0.0,
                                  FirstHVACIteration); // CoolPartLoadRatio
        } else if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingWaterToAirHPVSEquationFit ||
                   Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
            if (Furnace(FurnaceNum).bIsIHP) {
                SizeIHP(Furnace(FurnaceNum).CoolingCoilIndex);
                IHPCoilIndex = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SCCoilIndex;
                Furnace(FurnaceNum).NumOfSpeedCooling = VarSpeedCoil(IHPCoilIndex).NumOfSpeeds;
                MulSpeedFlowScale = VarSpeedCoil(IHPCoilIndex).RatedAirVolFlowRate /
                                    VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(VarSpeedCoil(IHPCoilIndex).NormSpedLevel);
                IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).CoolVolFlowScale = MulSpeedFlowScale;
            } else {
                SimVariableSpeedCoils(BlankString,
                                      Furnace(FurnaceNum).CoolingCoilIndex,
                                      0,
                                      Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                      Furnace(FurnaceNum).HPTimeConstant,
                                      Furnace(FurnaceNum).FanDelayTime,
                                      0,
                                      0.0,
                                      1,
                                      0.0,
                                      0.0,
                                      0.0,
                                      0.0); // conduct the sizing operation in the VS WSHP
                Furnace(FurnaceNum).NumOfSpeedCooling = VarSpeedCoil(Furnace(FurnaceNum).CoolingCoilIndex).NumOfSpeeds;
                MulSpeedFlowScale = VarSpeedCoil(Furnace(FurnaceNum).CoolingCoilIndex).RatedAirVolFlowRate /
                                    VarSpeedCoil(Furnace(FurnaceNum).CoolingCoilIndex)
                                        .MSRatedAirVolFlowRate(VarSpeedCoil(Furnace(FurnaceNum).CoolingCoilIndex).NormSpedLevel);
                IHPCoilIndex = Furnace(FurnaceNum).CoolingCoilIndex;
            }

            for (Iter = 1; Iter <= Furnace(FurnaceNum).NumOfSpeedCooling; ++Iter) {
                Furnace(FurnaceNum).CoolVolumeFlowRate(Iter) = VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(Iter) * MulSpeedFlowScale;
                Furnace(FurnaceNum).CoolMassFlowRate(Iter) = VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(Iter) * MulSpeedFlowScale;
                Furnace(FurnaceNum).MSCoolingSpeedRatio(Iter) =
                    VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(Iter) /
                    VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(Furnace(FurnaceNum).NumOfSpeedCooling);
            }

            if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingWaterToAirHPVSEquationFit ||
                Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) {

                if (Furnace(FurnaceNum).bIsIHP) {
                    SizeIHP(Furnace(FurnaceNum).CoolingCoilIndex);
                    IHPCoilIndex = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SHCoilIndex;
                    Furnace(FurnaceNum).NumOfSpeedHeating = VarSpeedCoil(IHPCoilIndex).NumOfSpeeds;
                    MulSpeedFlowScale = VarSpeedCoil(IHPCoilIndex).RatedAirVolFlowRate /
                                        VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(VarSpeedCoil(IHPCoilIndex).NormSpedLevel);
                    IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).HeatVolFlowScale = MulSpeedFlowScale;
                } else {
                    SimVariableSpeedCoils(BlankString,
                                          Furnace(FurnaceNum).HeatingCoilIndex,
                                          0,
                                          Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                          Furnace(FurnaceNum).HPTimeConstant,
                                          Furnace(FurnaceNum).FanDelayTime,
                                          0,
                                          0.0,
                                          1,
                                          0.0,
                                          0.0,
                                          0.0,
                                          0.0); // conduct the sizing operation in the VS WSHP
                    Furnace(FurnaceNum).NumOfSpeedHeating = VarSpeedCoil(Furnace(FurnaceNum).HeatingCoilIndex).NumOfSpeeds;
                    MulSpeedFlowScale = VarSpeedCoil(Furnace(FurnaceNum).HeatingCoilIndex).RatedAirVolFlowRate /
                                        VarSpeedCoil(Furnace(FurnaceNum).HeatingCoilIndex)
                                            .MSRatedAirVolFlowRate(VarSpeedCoil(Furnace(FurnaceNum).HeatingCoilIndex).NormSpedLevel);
                    IHPCoilIndex = Furnace(FurnaceNum).HeatingCoilIndex;
                }

                for (Iter = 1; Iter <= Furnace(FurnaceNum).NumOfSpeedHeating; ++Iter) {
                    Furnace(FurnaceNum).HeatVolumeFlowRate(Iter) = VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(Iter) * MulSpeedFlowScale;
                    Furnace(FurnaceNum).HeatMassFlowRate(Iter) = VarSpeedCoil(IHPCoilIndex).MSRatedAirMassFlowRate(Iter) * MulSpeedFlowScale;
                    Furnace(FurnaceNum).MSHeatingSpeedRatio(Iter) =
                        VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(Iter) /
                        VarSpeedCoil(IHPCoilIndex).MSRatedAirVolFlowRate(Furnace(FurnaceNum).NumOfSpeedHeating);
                }
            }

            if (Furnace(FurnaceNum).NumOfSpeedHeating > 0) {
                Furnace(FurnaceNum).IdleMassFlowRate = min(Furnace(FurnaceNum).HeatMassFlowRate(1), Furnace(FurnaceNum).CoolMassFlowRate(1));
                Furnace(FurnaceNum).IdleSpeedRatio = min(Furnace(FurnaceNum).MSHeatingSpeedRatio(1), Furnace(FurnaceNum).MSCoolingSpeedRatio(1));
                Furnace(FurnaceNum).IdleVolumeAirRate = min(Furnace(FurnaceNum).HeatVolumeFlowRate(1), Furnace(FurnaceNum).CoolVolumeFlowRate(1));
            } else {
                Furnace(FurnaceNum).IdleMassFlowRate = Furnace(FurnaceNum).CoolMassFlowRate(1);
                Furnace(FurnaceNum).IdleSpeedRatio = Furnace(FurnaceNum).MSCoolingSpeedRatio(1);
                Furnace(FurnaceNum).IdleVolumeAirRate = Furnace(FurnaceNum).CoolVolumeFlowRate(1);
            }

            if (Furnace(FurnaceNum).OpMode == ContFanCycCoil) {
                Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow = Furnace(FurnaceNum).IdleVolumeAirRate;
                Furnace(FurnaceNum).MaxNoCoolHeatAirMassFlow = Furnace(FurnaceNum).IdleMassFlowRate;
                Furnace(FurnaceNum).NoHeatCoolSpeedRatio = Furnace(FurnaceNum).IdleSpeedRatio;
            }
        }

        if (Furnace(FurnaceNum).DesignFanVolFlowRate == AutoSize) {

            if (CurSysNum > 0) {

                CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num), Furnace(FurnaceNum).Name);
                if (FinalSysSizing(CurSysNum).DesMainVolFlow >= SmallAirVolFlow) {
                    Furnace(FurnaceNum).DesignFanVolFlowRate = FinalSysSizing(CurSysNum).DesMainVolFlow;
                } else {
                    Furnace(FurnaceNum).DesignFanVolFlowRate = 0.0;
                }

                if (Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideOn) {
                    Furnace(FurnaceNum).DesignFanVolFlowRate = Furnace(FurnaceNum).DesignFanVolFlowRateEMSOverrideValue;
                }

                ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num),
                                   Furnace(FurnaceNum).Name,
                                   "Supply Air Flow Rate [m3/s]",
                                   Furnace(FurnaceNum).DesignFanVolFlowRate);
            }
        }

        if (Furnace(FurnaceNum).MaxHeatAirVolFlow == AutoSize) {

            if (CurSysNum > 0) {

                CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num), Furnace(FurnaceNum).Name);
                if (FinalSysSizing(CurSysNum).DesMainVolFlow >= SmallAirVolFlow) {
                    Furnace(FurnaceNum).MaxHeatAirVolFlow = FinalSysSizing(CurSysNum).DesMainVolFlow;
                } else {
                    Furnace(FurnaceNum).MaxHeatAirVolFlow = 0.0;
                }

                if (Furnace(FurnaceNum).MaxHeatAirVolFlowEMSOverrideOn) {
                    Furnace(FurnaceNum).MaxHeatAirVolFlow = Furnace(FurnaceNum).MaxHeatAirVolFlowEMSOverrideValue;
                }
                ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num),
                                   Furnace(FurnaceNum).Name,
                                   "Supply Air Flow Rate During Heating Operation [m3/s]",
                                   Furnace(FurnaceNum).MaxHeatAirVolFlow);
            }
        }

        if (Furnace(FurnaceNum).MaxCoolAirVolFlow == AutoSize) {

            if (CurSysNum > 0) {

                CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num), Furnace(FurnaceNum).Name);
                if (FinalSysSizing(CurSysNum).DesMainVolFlow >= SmallAirVolFlow) {
                    Furnace(FurnaceNum).MaxCoolAirVolFlow = FinalSysSizing(CurSysNum).DesMainVolFlow;
                } else {
                    Furnace(FurnaceNum).MaxCoolAirVolFlow = 0.0;
                }

                if (Furnace(FurnaceNum).MaxCoolAirVolFlowEMSOverrideOn) {
                    Furnace(FurnaceNum).MaxCoolAirVolFlow = Furnace(FurnaceNum).MaxCoolAirVolFlowEMSOverrideValue;
                }

                ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num),
                                   Furnace(FurnaceNum).Name,
                                   "Supply Air Flow Rate During Cooling Operation [m3/s]",
                                   Furnace(FurnaceNum).MaxCoolAirVolFlow);
            }
        }

        if (Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow == AutoSize) {

            if (CurSysNum > 0) {

                CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num), Furnace(FurnaceNum).Name);
                if (FinalSysSizing(CurSysNum).DesMainVolFlow >= SmallAirVolFlow) {
                    Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow = FinalSysSizing(CurSysNum).DesMainVolFlow;
                } else {
                    Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow = 0.0;
                }

                if (Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlowEMSOverrideOn) {
                    Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow = Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlowEMSOverrideValue;
                }

                ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num),
                                   Furnace(FurnaceNum).Name,
                                   "Supply Air Flow Rate When No Cooling or Heating is Needed [m3/s]",
                                   Furnace(FurnaceNum).MaxNoCoolHeatAirVolFlow);
            }
        }

        if (Furnace(FurnaceNum).DesignHeatingCapacity == AutoSize) {

            if (CurSysNum > 0) {

                if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir ||
                    Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir) {

                    CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num), Furnace(FurnaceNum).Name);
                    Furnace(FurnaceNum).DesignHeatingCapacity = DXCoolCap;

                } else {

                    CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num), Furnace(FurnaceNum).Name);

                    Furnace(FurnaceNum).DesignHeatingCapacity = FinalSysSizing(CurSysNum).HeatCap;
                }

                if (Furnace(FurnaceNum).DesignHeatingCapacity < SmallLoad) {
                    Furnace(FurnaceNum).DesignHeatingCapacity = 0.0;
                }

                ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num),
                                   Furnace(FurnaceNum).Name,
                                   "Nominal Heating Capacity [W]",
                                   Furnace(FurnaceNum).DesignHeatingCapacity);
            }
        }

        if (Furnace(FurnaceNum).DesignCoolingCapacity == AutoSize) {

            if (CurSysNum > 0) {

                CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num), Furnace(FurnaceNum).Name);
                if (DXCoolCap >= SmallLoad) {
                    Furnace(FurnaceNum).DesignCoolingCapacity = DXCoolCap;
                } else {
                    Furnace(FurnaceNum).DesignCoolingCapacity = 0.0;
                }
                ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num),
                                   Furnace(FurnaceNum).Name,
                                   "Nominal Cooling Capacity [W]",
                                   Furnace(FurnaceNum).DesignCoolingCapacity);
            }
        }

        if (Furnace(FurnaceNum).DesignMaxOutletTemp == AutoSize) {

            if (CurSysNum > 0) {

                CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num), Furnace(FurnaceNum).Name);
                Furnace(FurnaceNum).DesignMaxOutletTemp = FinalSysSizing(CurSysNum).HeatSupTemp;
                ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num),
                                   Furnace(FurnaceNum).Name,
                                   "Maximum Supply Air Temperature from Supplemental Heater [C]",
                                   Furnace(FurnaceNum).DesignMaxOutletTemp);
            }
        }

        if (Furnace(FurnaceNum).DesignSuppHeatingCapacity == AutoSize) {

            if (CurSysNum > 0) {

                CheckSysSizing(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num), Furnace(FurnaceNum).Name);
                if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir ||
                    Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir) {
                    // set the supplemental heating capacity to the actual heating load
                    Furnace(FurnaceNum).DesignSuppHeatingCapacity = FinalSysSizing(CurSysNum).HeatCap;
                    // if reheat needed for humidity control, make sure supplemental heating is at least as big
                    // as the cooling capacity
                    if (Furnace(FurnaceNum).Humidistat && Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat) {
                        Furnace(FurnaceNum).DesignSuppHeatingCapacity =
                            max(Furnace(FurnaceNum).DesignSuppHeatingCapacity, Furnace(FurnaceNum).DesignCoolingCapacity);
                        if (Furnace(FurnaceNum).DesignSuppHeatingCapacity < SmallLoad) {
                            Furnace(FurnaceNum).DesignSuppHeatingCapacity = 0.0;
                        }
                    }

                } else {

                    if (Furnace(FurnaceNum).Humidistat && Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat) {
                        Furnace(FurnaceNum).DesignSuppHeatingCapacity = Furnace(FurnaceNum).DesignCoolingCapacity;
                    } else {
                        Furnace(FurnaceNum).DesignSuppHeatingCapacity = 0.0;
                    }
                }

                ReportSizingOutput(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num),
                                   Furnace(FurnaceNum).Name,
                                   "Supplemental Heating Coil Nominal Capacity [W]",
                                   Furnace(FurnaceNum).DesignSuppHeatingCapacity);
            }
        }

        UnitaryHeatCap = Furnace(FurnaceNum).DesignHeatingCapacity;
        SuppHeatCap = Furnace(FurnaceNum).DesignSuppHeatingCapacity;
    }

    // End Initialization Section of the Module
    //******************************************************************************

    // Beginning of Update subroutines for the Furnace Module
    // *****************************************************************************

    void CalcNewZoneHeatOnlyFlowRates(int const FurnaceNum,          // Index to furnace
                                      bool const FirstHVACIteration, // Iteration flag
                                      Real64 const ZoneLoad,         // load to be met by furnace (W)
                                      Real64 &HeatCoilLoad,          // actual load passed to heating coil (W)
                                      Real64 &OnOffAirFlowRatio      // ratio of coil on to coil off air flow rate
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Feb 2001
        //       MODIFIED       Don Shirey and R. Raustad, Mar 2001 & Mar 2003
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the coil outlet nodes by simulating a heat-only
        // furnace or unitary system.

        // METHODOLOGY EMPLOYED:
        // Determine the operating PLR to meet the zone sensible load.

        // REFERENCES:
        // na

        // Using/Aliasing
        using HeatingCoils::SimulateHeatingCoilComponents;
        using namespace ScheduleManager;
        using DataHeatBalFanSys::MAT;
        using General::TrimSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIter(15);    // maximum number of iterations
        Real64 const MinPLR(0.0); // minimum part load ratio allowed

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 cpair;
        static Real64 Error(1.0);
        Real64 SystemSensibleLoad;   // Sensible load to be met by furnace (W)
        Real64 FullSensibleOutput;   // Full sensible output of furnace (W)
        Real64 FullLatentOutput;     // Full latent output of furnace = 0 (W)
        Real64 NoSensibleOutput;     // Sensible output of furnace with no heating allowed (W)
        Real64 NoLatentOutput;       // Latent output of furnace = 0 (W)
        Real64 PartLoadRatio;        // Part load ratio of furnace
        Real64 HeatErrorToler;       // Error tolerance in heating mode
        Real64 IterRelax;            // Relaxation factor for iterations
        Real64 ActualSensibleOutput; // Actual furnace sensible capacity
        Real64 ActualLatentOutput;   // Actual furnace latent capacity = 0
        Real64 deltaT;               // Heater outlet temp minus design heater outlet temp
        //  CHARACTER(len=20) :: ErrNum = ' '         ! For displaying error message in cooling
        //  INTEGER,SAVE      :: ErrCount = 0
        static int Iter(0);    // Iteration counter
        int FurnaceInletNode;  // Node number of furnace inlet
        int FurnaceOutletNode; // Node number of furnace outlet
        int OpMode;            // Mode of Operation (fan cycling or fan continuous)
        // Set local variables

        // Retrieve the load on the controlled zone
        FurnaceOutletNode = Furnace(FurnaceNum).FurnaceOutletNodeNum;
        FurnaceInletNode = Furnace(FurnaceNum).FurnaceInletNodeNum;
        OpMode = Furnace(FurnaceNum).OpMode;
        Furnace(FurnaceNum).MdotFurnace = Furnace(FurnaceNum).DesignMassFlowRate;
        Furnace(FurnaceNum).CoolPartLoadRatio = 0.0;
        //  OnOffAirFlowRatio = 1.0

        // Calculate the Cp Air
        cpair = PsyCpAirFnWTdb(Node(FurnaceInletNode).HumRat, Node(FurnaceInletNode).Temp);

        if (FirstHVACIteration) {
            HeatCoilLoad = ZoneLoad;
            OnOffFanPartLoadFraction = 1.0;
            Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;
        } else {
            // If Furnace runs then set HeatCoilLoad on Heating Coil and the Mass Flow
            if ((GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0) && (Node(FurnaceInletNode).MassFlowRate > 0.0) && (HeatingLoad)) {

                Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;
                HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity;
                SystemSensibleLoad = ZoneLoad;

                // Get no load result
                if (OpMode == CycFanCycCoil) {
                    Node(FurnaceInletNode).MassFlowRate = 0.0;
                }
                if (OpMode == ContFanCycCoil) {
                    OnOffFanPartLoadFraction = 1.0; // The on/off fan will not cycle, so set part-load fraction = 1
                }

                //     Set the inlet mass flow rate based on user specified coil OFF flow rate
                PartLoadRatio = 0.0;
                SetAverageAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);

                CalcFurnaceOutput(
                    FurnaceNum, FirstHVACIteration, OpMode, On, 0.0, 0.0, 0.0, 0.0, NoSensibleOutput, NoLatentOutput, OnOffAirFlowRatio, false);

                Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;

                // Set fan part-load fraction equal to 1 while getting full load result
                OnOffFanPartLoadFraction = 1.0;
                OnOffAirFlowRatio = 1.0;

                // Get full load result
                CalcFurnaceOutput(FurnaceNum,
                                  FirstHVACIteration,
                                  OpMode,
                                  On,
                                  0.0,
                                  1.0,
                                  HeatCoilLoad,
                                  0.0,
                                  FullSensibleOutput,
                                  FullLatentOutput,
                                  OnOffAirFlowRatio,
                                  false);

                // Since we are heating, we expect FullSensibleOutput to be > 0 and FullSensibleOutput > NoSensibleOutput
                // Check that this is the case; if not set PartLoadRatio = 0.0d0 (off) and return

                if (FullSensibleOutput > NoSensibleOutput) {
                    PartLoadRatio =
                        max(MinPLR, min(1.0, std::abs(SystemSensibleLoad - NoSensibleOutput) / std::abs(FullSensibleOutput - NoSensibleOutput)));
                    if (OpMode == CycFanCycCoil) {
                        Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace * PartLoadRatio;
                        HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity * PartLoadRatio;
                    } else { // ContFanCycCoil
                        if (Node(FurnaceOutletNode).Temp > Furnace(FurnaceNum).DesignMaxOutletTemp) {
                            deltaT = Node(FurnaceOutletNode).Temp - Furnace(FurnaceNum).DesignMaxOutletTemp;
                            if (HeatCoilLoad > Furnace(FurnaceNum).DesignHeatingCapacity) HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity;
                            HeatCoilLoad -= Node(FurnaceInletNode).MassFlowRate * cpair * deltaT;
                        } else {
                            HeatCoilLoad = SystemSensibleLoad - NoSensibleOutput;
                        }
                    }

                    // Calculate the part load ratio through iteration
                    HeatErrorToler = Furnace(FurnaceNum).HeatingConvergenceTolerance; // Error tolerance for convergence from input deck
                    Error = 1.0;                                                      // initialize error value for comparison against tolerance
                    Iter = 0;                                                         // initialize iteration counter
                    IterRelax = 0.9;                                                  // relaxation factor for iterations
                    while (Iter <= MaxIter) {

                        if (OpMode == CycFanCycCoil) Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace * PartLoadRatio;
                        CalcFurnaceOutput(FurnaceNum,
                                          FirstHVACIteration,
                                          OpMode,
                                          On,
                                          0.0,
                                          PartLoadRatio,
                                          HeatCoilLoad,
                                          0.0,
                                          ActualSensibleOutput,
                                          ActualLatentOutput,
                                          OnOffAirFlowRatio,
                                          false);

                        if (SystemSensibleLoad != 0.0) Error = (SystemSensibleLoad - ActualSensibleOutput) / (SystemSensibleLoad);
                        if (std::abs(Error) <= HeatErrorToler) break;
                        PartLoadRatio = max(
                            MinPLR,
                            min(1.0,
                                PartLoadRatio + IterRelax * (SystemSensibleLoad - ActualSensibleOutput) / (FullSensibleOutput - NoSensibleOutput)));

                        //        limit the heating coil outlet air temperature to DesignMaxOutletTemp
                        if (Node(FurnaceOutletNode).Temp > Furnace(FurnaceNum).DesignMaxOutletTemp) {
                            deltaT = Node(FurnaceOutletNode).Temp - Furnace(FurnaceNum).DesignMaxOutletTemp;
                            if (HeatCoilLoad > Furnace(FurnaceNum).DesignHeatingCapacity) HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity;
                            HeatCoilLoad -= Node(FurnaceInletNode).MassFlowRate * cpair * deltaT;
                            CalcFurnaceOutput(FurnaceNum,
                                              FirstHVACIteration,
                                              OpMode,
                                              On,
                                              0.0,
                                              PartLoadRatio,
                                              HeatCoilLoad,
                                              0.0,
                                              ActualSensibleOutput,
                                              ActualLatentOutput,
                                              OnOffAirFlowRatio,
                                              false);

                            if (SystemSensibleLoad != 0.0) Error = (SystemSensibleLoad - ActualSensibleOutput) / (SystemSensibleLoad);
                            PartLoadRatio = max(MinPLR,
                                                min(1.0,
                                                    PartLoadRatio + IterRelax * (SystemSensibleLoad - ActualSensibleOutput) /
                                                                        (FullSensibleOutput - NoSensibleOutput)));
                        } else {
                            HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity * PartLoadRatio;
                        }

                        if (PartLoadRatio == MinPLR) break;
                        if (PartLoadRatio == 1.0) break;
                        ++Iter;
                        if (Iter == 7) IterRelax = 0.7;
                        if (Iter == 15) IterRelax = 0.4;
                    }

                    if (Iter > MaxIter) {
                        if (Furnace(FurnaceNum).HeatingMaxIterIndex2 == 0) {
                            ShowWarningMessage(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                               "\" -- Exceeded max heating iterations (" + TrimSigDigits(MaxIter) +
                                               ") while adjusting furnace runtime.");
                            ShowContinueErrorTimeStamp("");
                        }
                        ShowRecurringWarningErrorAtEnd(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                                           "\" -- Exceeded max heating iterations error continues...",
                                                       Furnace(FurnaceNum).HeatingMaxIterIndex2);
                    }

                } else { // ELSE from IF(FullSensibleOutput.GT.NoSensibleOutput)THEN above
                    // Set part load ratio to 1 and run heater at design heating capacity
                    PartLoadRatio = 1.0;
                    HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity;
                }
                // Set the final results
                //      IF (OpMode .EQ. CycFanCycCoil) THEN
                //        Furnace(FurnaceNum)%MdotFurnace = Furnace(FurnaceNum)%MdotFurnace * PartLoadRatio
                //      END IF
                Furnace(FurnaceNum).MdotFurnace = Node(FurnaceInletNode).MassFlowRate;

            } else if ((GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0) && (Node(FurnaceInletNode).MassFlowRate > 0.0) &&
                       (OpMode == ContFanCycCoil)) {
                HeatCoilLoad = 0.0;
            } else { // no heating and no flow
                Furnace(FurnaceNum).MdotFurnace = 0.0;
                HeatCoilLoad = 0.0;
            } // End of the Scheduled Furnace If block

        } // End of the FirstHVACIteration control of the mass flow If block

        // Set the fan inlet node flow rates
        Node(FurnaceInletNode).MassFlowRateMaxAvail = Furnace(FurnaceNum).MdotFurnace;
        Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;
    }

    void CalcNewZoneHeatCoolFlowRates(int const FurnaceNum,
                                      bool const FirstHVACIteration,
                                      int const CompOp,          // compressor operation flag (1=On, 0=Off)
                                      Real64 const ZoneLoad,     // the control zone load (watts)
                                      Real64 const MoistureLoad, // the control zone latent load (watts)
                                      Real64 &HeatCoilLoad,      // Heating load to be met by heating coil ( excluding heat pump DX coil)
                                      Real64 &ReheatCoilLoad,    // Heating load to be met by reheat coil using hstat (excluding HP DX coil)
                                      Real64 &OnOffAirFlowRatio, // Ratio of compressor ON air flow to AVERAGE air flow over time step
                                      bool &HXUnitOn             // flag to control HX based on zone moisture load
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Feb 2001
        //       MODIFIED       R. Raustad and D. Shirey, Feb/Mar/Sept/Oct/Dec 2001, Jan/Oct 2002
        //       RE-ENGINEERED  R. Raustad, Feb. 2005 (added RegulaFalsi for iteration technique)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the coil outlet nodes.

        // METHODOLOGY EMPLOYED:
        // Determine the operating PLR to meet the zone sensible load. If a humidistat is specified, determine
        // the operating PLR (greater of the sensible and latent PLR) to meet the zone SENSIBLE load
        // (Multimode dehumidification control) or zone LATENT load (CoolReheat dehumidification control).
        // For dehumidification control type COOLREHEAT, both a sensible and latent PLR may exist for a
        // single time step (heating and dehumidificaiton can occur). For all other sytem types,
        // only a single PLR is allowed for any given time step.
        // Order of simulation depends on dehumidification control option as described below.
        // Dehumidificaiton control options:
        // Dehumidification Control NONE:   Cooling performance is simulated first and then heating performance. If a HX
        //                                  assisted cooling coil is selected, the HX is always active.
        // Dehumidification Control COOLREHEAT: Continuous Fan Operation:
        //                                      For cooling operation, the sensible and latent capacities are calculated to
        //                                      meet the thermostat setpoint. If a HX assisted cooling coil is selected,
        //                                      the HX is always active. If the latent load is not met by operating the
        //                                      system at the sensible PLR, a new PLR is calculated to meet the humidistat
        //                                      setpoint. The reheat coil load is then calculated to meet the HEATING
        //                                      setpoint temperature.
        //                                      Cycling Fan Operation:
        //                                      The heating part-load ratio is calculated first. Since the fan will be
        //                                      controlled at the higher of the heating or cooling PLR's, a ratio of the
        //                                      cooling to heating PLR is used to pass to the cooling coil (MAX=1). This allows
        //                                      the cooling coil to operate at the heating PLR when the heating PLR is
        //                                      higher than the cooling PLR. The sensible and latent capacities are then
        //                                      calculated to meet the thermostat setpoint.
        //                                      If a HX assisted cooling coil is selected, the HX is always active.
        //                                      If the latent load is not met by operating the system at the sensible PLR,
        //                                      a new PLR is calculated to meet the humidistat setpoint.
        //                                      The reheat coil load is then calculated to meet the HEATING setpoint temperature.
        // Dehumidification Control MULTIMODE: For cooling operation, the sensible and latent capacities are calculated to
        //                                     meet the thermostat setpoint. If a HX assisted cooling coil is selected,
        //                                     the HX is off for this calculation. If the latent load is not met by operating
        //                                     the system at the sensible PLR, a new PLR is calculated with the HX operating
        //                                     and the target is the thermostat setpoint. Humidity is not controlled in this
        //                                     mode. No reheat coil is used in this configuration.
        //  Note: A supplemental heater augments the heating capacity for air-to-air heat pumps.
        //        A reheat coil is used for the HeatCool furnace/unitarysystem to offset the sensible cooling when the
        //        dehumidification control type is COOLREHEAT. Both the supplemental and reheat heating coil load is calculated
        //        in the Calc routines. The actual simulation of these coils is performed in the SimFurnace routine (i.e. the
        //        supplemental and reheat coil loads are passed as 0 to CalcFurnaceOutput).

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataHeatBalFanSys::MAT;
        using HeatingCoils::SimulateHeatingCoilComponents;
        using namespace ScheduleManager;
        using namespace DataZoneEnergyDemands;
        using DataHeatBalFanSys::ZT;
        using DXCoils::DXCoil;
        using DXCoils::DXCoilPartLoadRatio;
        using General::SolveRoot;
        using General::TrimSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIter(100);   // maximum number of iterations
        Real64 const MinPLR(0.0); // minimum part load ratio allowed

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 SystemMoistureLoad;        // Total latent load to be removed by furnace/unitary system
        Real64 cpair;                     // Heat capacity of air
        Real64 deltaT;                    // Temperature rise across heating coil (C)
        Real64 TempOutHeatingCoil;        // Temperature leaving heating coil (C)
        Real64 FullSensibleOutput;        // Full sensible output of AC (W)
        Real64 FullLatentOutput;          // Full latent output of AC (W)
        Real64 NoCoolOutput;              // Sensible output of AC with no cooling allowed (W)
        Real64 NoHeatOutput;              // Sensible output of heater with no heating allowed (W)
        Real64 NoLatentOutput;            // Latent output of AC with no cooling allowed (W)
        int FurnaceInletNode;             // Inlet node to furnace or unitary system
        int FurnaceOutletNode;            // Outlet node of furnace or unitary system
        int OpMode;                       // Mode of Operation (fan cycling = 1 or fan continuous = 2)
        static Real64 CoolCoilLoad;       // Negative value means cooling required
        static Real64 SystemSensibleLoad; // Positive value means heating required
        Real64 CoolErrorToler;            // Error tolerance in cooling mode
        Real64 HeatErrorToler;            // Error tolerance in heating mode
        Real64 ActualSensibleOutput;      // Actual furnace sensible capacity
        Real64 ActualLatentOutput;        // Actual furnace latent capacity
        Real64 PartLoadRatio;             // Part load ratio (greater of sensible or latent part load ratio for cooling,
        // or heating PLR)
        Real64 LatentPartLoadRatio; // Part load ratio to meet dehumidification load
        Real64 TempCoolOutput;      // Temporary Sensible output of AC while iterating on PLR (W)
        Real64 TempHeatOutput;      // Temporary Sensible output of heating coil while iterating on PLR (W)
        Real64 TempLatentOutput;    // Temporary Latent output of AC at increasing PLR (W)
        //                                           ! (Temp variables are used to find min PLR for positive latent removal)
        static bool HumControl(false); // Logical flag signaling when dehumidification is required
        Array1D<Real64> Par(10);       // parameters passed to RegulaFalsi function
        int SolFlag;                   // return flag from RegulaFalsi
        Real64 TempMinPLR;             // Temporary min latent PLR when hum control is required and iter is exceeded
        Real64 TempMinPLR2;            // Temporary min latent PLR when cyc fan hum control is required and iter is exceeded
        Real64 TempMaxPLR;             // Temporary max latent PLR when hum control is required and iter is exceeded
        Real64 QToHeatSetPt;           // Load required to meet heating setpoint temp (>0 is a heating load)
        Real64 CoolingHeatingPLRRatio; // ratio of cooling to heating PLR (MAX=1). Used in heating mode.
        Real64 HeatingSensibleOutput;
        Real64 HeatingLatentOutput;
        Real64 OutdoorDryBulbTemp; // secondary coil (condenser) entering dry bulb temperature

        // Set local variables
        FurnaceOutletNode = Furnace(FurnaceNum).FurnaceOutletNodeNum;
        FurnaceInletNode = Furnace(FurnaceNum).FurnaceInletNodeNum;
        OpMode = Furnace(FurnaceNum).OpMode;
        //  Furnace(FurnaceNum)%MdotFurnace = Furnace(FurnaceNum)%DesignMassFlowRate
        HumControl = false;
        // Calculate the Cp Air for all conditions
        cpair = PsyCpAirFnWTdb(Node(FurnaceInletNode).HumRat, Node(FurnaceInletNode).Temp);
        NoHeatOutput = 0.0;
        SystemSensibleLoad = 0.0;
        ReheatCoilLoad = 0.0;
        HeatCoilLoad = 0.0;
        ReheatCoilLoad = 0.0;
        PartLoadRatio = 0.0;

        if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir) {
            if (DXCoil(Furnace(FurnaceNum).HeatingCoilIndex).IsSecondaryDXCoilInZone) {
                OutdoorDryBulbTemp = ZT(DXCoil(Furnace(FurnaceNum).HeatingCoilIndex).SecZonePtr);
                Furnace(FurnaceNum).CondenserNodeNum = 0;
            } else if (DXCoil(Furnace(FurnaceNum).CoolingCoilIndex).IsSecondaryDXCoilInZone) {
                OutdoorDryBulbTemp = ZT(DXCoil(Furnace(FurnaceNum).CoolingCoilIndex).SecZonePtr);
                Furnace(FurnaceNum).CondenserNodeNum = 0;
            } else {
                OutdoorDryBulbTemp = OutDryBulbTemp;
            }
        } else {
            OutdoorDryBulbTemp = OutDryBulbTemp;
        }
        if (FirstHVACIteration) {
            // Set selected values during first HVAC iteration

            // Init for heating
            if (HeatingLoad) {
                if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir ||
                    (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir &&
                     Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_Simple)) {
                    Furnace(FurnaceNum).HeatPartLoadRatio = 1.0;
                    HeatCoilLoad = 0.0;
                    Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;
                    Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
                    Furnace(FurnaceNum).CoolingCoilLatentDemand = 0.0;
                } else { // for furnaces
                    Furnace(FurnaceNum).HeatPartLoadRatio = 0.0;
                    HeatCoilLoad = ZoneLoad;
                    Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;
                    Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;
                    Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
                    Furnace(FurnaceNum).CoolingCoilLatentDemand = 0.0;
                }
                ReheatCoilLoad = 0.0;
                Furnace(FurnaceNum).CoolPartLoadRatio = 0.0;

                // Init for cooling
            } else if (CoolingLoad) {
                // air to air heat pumps
                Furnace(FurnaceNum).CoolPartLoadRatio = 1.0;
                Furnace(FurnaceNum).HeatPartLoadRatio = 0.0;
                HeatCoilLoad = 0.0;
                ReheatCoilLoad = 0.0;

                // Init for moisture load only
            } else {
                Furnace(FurnaceNum).CoolPartLoadRatio = 0.0;
                Furnace(FurnaceNum).HeatPartLoadRatio = 0.0;
                HeatCoilLoad = 0.0;
                ReheatCoilLoad = 0.0;
                Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;
                Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
                Furnace(FurnaceNum).CoolingCoilLatentDemand = 0.0;
            }

            SetAverageAirFlow(FurnaceNum, max(Furnace(FurnaceNum).HeatPartLoadRatio, Furnace(FurnaceNum).CoolPartLoadRatio), OnOffAirFlowRatio);
            //  if dehumidification load exists (for heat pumps) turn on the supplmental heater
            if (HPDehumidificationLoadFlag) HumControl = true;
        } else { // not FirstHVACIteration
            // Init for heating
            if (HeatingLoad) {
                CoolCoilLoad = 0.0;
                if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir ||
                    (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir &&
                     Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_Simple)) {
                    SystemSensibleLoad = ZoneLoad;
                    SystemMoistureLoad = 0.0;
                    HeatCoilLoad = 0.0;
                    Furnace(FurnaceNum).HeatingCoilSensDemand = SystemSensibleLoad;
                    Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
                    Furnace(FurnaceNum).CoolingCoilLatentDemand = 0.0;
                } else {
                    SystemMoistureLoad = MoistureLoad;
                    HeatCoilLoad = ZoneLoad;
                }

                // Init for cooling
            } else if (CoolingLoad) {
                CoolCoilLoad = ZoneLoad;
                SystemMoistureLoad = MoistureLoad;
                HeatCoilLoad = 0.0;
                Furnace(FurnaceNum).CoolingCoilSensDemand = std::abs(CoolCoilLoad);
                Furnace(FurnaceNum).CoolingCoilLatentDemand = std::abs(SystemMoistureLoad);
                Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;

                // Init for latent
            } else {
                SystemMoistureLoad = MoistureLoad;
                CoolCoilLoad = 0.0;
                HeatCoilLoad = 0.0;
                // set report variables
                Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
                Furnace(FurnaceNum).CoolingCoilLatentDemand = SystemMoistureLoad;
                Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;
            }
            HeatingSensibleOutput = 0.0;
            HeatingLatentOutput = 0.0;
            ReheatCoilLoad = 0.0;
            Furnace(FurnaceNum).CoolPartLoadRatio = 0.0;
            Furnace(FurnaceNum).HeatPartLoadRatio = 0.0;
            Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
            Furnace(FurnaceNum).DehumidInducedHeatingDemandRate = 0.0;

            // When humidity control is used with cycling fan control and a heating load exists, if a moisture load
            // also exists, the heating PLR must be available for the cooling coil calculations.
            //*********** Heating Section ************
            // If Furnace runs with a heating load then set HeatCoilLoad on Heating Coil and the Mass Flow
            //         (Node(FurnaceInletNode)%MassFlowRate .gt. 0.0d0) .and. &
            if ((GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0) && (HeatingLoad)) {

                //    Heat pumps only calculate a single PLR each time step (i.e. only cooling or heating allowed in a single time step)
                if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir ||
                    (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir &&
                     Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_Simple)) {

                    Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;

                    // Get no load result
                    if (OpMode == CycFanCycCoil) {
                        Node(FurnaceInletNode).MassFlowRate = 0.0;
                    }

                    //     Set the inlet mass flow rate based on user specified coil OFF flow rate
                    PartLoadRatio = 0.0;

                    SetAverageAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);

                    // Set the input parameters for CalcFurnaceOutput
                    Furnace(FurnaceNum).CompPartLoadRatio = 0.0; // compressor off
                    Furnace(FurnaceNum).WSHPRuntimeFrac = 0.0;

                    CalcFurnaceOutput(FurnaceNum,
                                      FirstHVACIteration,
                                      OpMode,
                                      CompOp,
                                      0.0,
                                      MinPLR,
                                      0.0,
                                      0.0,
                                      NoHeatOutput,
                                      NoLatentOutput,
                                      OnOffAirFlowRatio,
                                      false);

                    PartLoadRatio = 1.0;
                    Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;

                    Furnace(FurnaceNum).CompPartLoadRatio = 1.0; // compressor ON
                    Furnace(FurnaceNum).WSHPRuntimeFrac = 1.0;

                    // Set fan part-load fraction equal to 1 while getting full load result
                    OnOffFanPartLoadFraction = 1.0;
                    OnOffAirFlowRatio = 1.0;

                    // Get full load result
                    CalcFurnaceOutput(FurnaceNum,
                                      FirstHVACIteration,
                                      OpMode,
                                      CompOp,
                                      0.0,
                                      PartLoadRatio,
                                      0.0,
                                      0.0,
                                      FullSensibleOutput,
                                      FullLatentOutput,
                                      OnOffAirFlowRatio,
                                      false);

                    // Check that SystemSensibleLoad is between FullSensibleOutput and NoHeatOutput
                    // If so then calculate PartLoadRatio for the DX Heating coil
                    if (SystemSensibleLoad < FullSensibleOutput && SystemSensibleLoad > NoHeatOutput) {

                        //       check bounds on sensible output prior to iteration using RegulaFalsi
                        if (FullSensibleOutput < SystemSensibleLoad) {
                            PartLoadRatio = 1.0;
                        } else if (NoHeatOutput > SystemSensibleLoad) {
                            PartLoadRatio = 0.0;
                        } else {

                            // Calculate the part load ratio through iteration
                            HeatErrorToler = Furnace(FurnaceNum).HeatingConvergenceTolerance; // Error tolerance for convergence from input deck

                            SolFlag = 0; // # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
                            Par(1) = double(FurnaceNum);
                            Par(2) = 0.0; // FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                            if (FirstHVACIteration) Par(2) = 1.0;
                            Par(3) = double(OpMode);
                            Par(4) = double(CompOp);
                            Par(5) = SystemSensibleLoad;
                            Par(6) = 0.0;               // FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
                            Par(7) = 1.0;               // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
                            Par(8) = OnOffAirFlowRatio; // Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
                            Par(9) = 0.0;               // HXUnitOn is always false for HX
                            Par(10) = 0.0;
                            //         HeatErrorToler is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
                            SolveRoot(HeatErrorToler, MaxIter, SolFlag, PartLoadRatio, CalcFurnaceResidual, 0.0, 1.0, Par);
                            //         OnOffAirFlowRatio is updated during the above iteration. Reset to correct value based on PLR.
                            OnOffAirFlowRatio = OnOffAirFlowRatioSave;
                            if (SolFlag < 0) {
                                if (SolFlag == -1) {
                                    CalcFurnaceOutput(FurnaceNum,
                                                      FirstHVACIteration,
                                                      OpMode,
                                                      CompOp,
                                                      0.0,
                                                      PartLoadRatio,
                                                      0.0,
                                                      0.0,
                                                      TempHeatOutput,
                                                      TempLatentOutput,
                                                      OnOffAirFlowRatio,
                                                      false);
                                    if (std::abs(SystemSensibleLoad - TempHeatOutput) > SmallLoad) {
                                        if (Furnace(FurnaceNum).DXHeatingMaxIterIndex == 0) {
                                            ShowWarningMessage("Heating coil control failed to converge for " +
                                                               cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + ':' + Furnace(FurnaceNum).Name);
                                            ShowContinueError("  Iteration limit exceeded in calculating DX heating coil sensible part-load ratio.");
                                            ShowContinueErrorTimeStamp(
                                                "Sensible load to be met by DX heating coil = " + TrimSigDigits(SystemSensibleLoad, 2) +
                                                " (watts), sensible output of DX heating coil = " + TrimSigDigits(TempHeatOutput, 2) +
                                                " (watts), and the simulation continues.");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                                "\" - Iteration limit exceeded in calculating DX sensible heating part-load ratio error continues. "
                                                "Sensible load statistics:",
                                            Furnace(FurnaceNum).DXHeatingMaxIterIndex,
                                            SystemSensibleLoad,
                                            SystemSensibleLoad);
                                    }
                                } else if (SolFlag == -2) {
                                    if (Furnace(FurnaceNum).DXHeatingRegulaFalsiFailedIndex == 0) {
                                        ShowWarningMessage("Heating coil control failed for " + cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) +
                                                           ':' + Furnace(FurnaceNum).Name);
                                        ShowContinueError("  DX sensible heating part-load ratio determined to be outside the range of 0-1.");
                                        ShowContinueErrorTimeStamp("Sensible load to be met by DX heating coil = " +
                                                                   TrimSigDigits(SystemSensibleLoad, 2) + " (watts), and the simulation continues.");
                                    }
                                    ShowRecurringWarningErrorAtEnd(
                                        cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                            "\" -  DX sensible heating part-load ratio out of range error continues. Sensible load statistics:",
                                        Furnace(FurnaceNum).DXHeatingRegulaFalsiFailedIndex,
                                        SystemSensibleLoad,
                                        SystemSensibleLoad);
                                }
                            }
                        }

                        Furnace(FurnaceNum).HeatPartLoadRatio = PartLoadRatio;
                        //       Check if Heat Pump compressor is allowed to run based on outdoor temperature
                        if (Furnace(FurnaceNum).CondenserNodeNum > 0) {
                            if (Node(Furnace(FurnaceNum).CondenserNodeNum).Temp > Furnace(FurnaceNum).MinOATCompressorHeating) {
                                Furnace(FurnaceNum).CompPartLoadRatio = PartLoadRatio;
                            } else {
                                Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
                            }
                        } else {
                            if (OutdoorDryBulbTemp > Furnace(FurnaceNum).MinOATCompressorHeating) {
                                Furnace(FurnaceNum).CompPartLoadRatio = PartLoadRatio;
                            } else {
                                Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
                            }
                        }
                    } else if (SystemSensibleLoad > FullSensibleOutput) {
                        //       SystemSensibleLoad is greater than full DX Heating coil output so heat pump runs entire
                        //       timestep and additional supplemental heating is required
                        Furnace(FurnaceNum).HeatPartLoadRatio = 1.0;
                        if (Furnace(FurnaceNum).CondenserNodeNum > 0) {
                            if (Node(Furnace(FurnaceNum).CondenserNodeNum).Temp > Furnace(FurnaceNum).MinOATCompressorHeating) {
                                //       Check to see if Heat Pump compressor was allowed to run based on outdoor temperature
                                Furnace(FurnaceNum).CompPartLoadRatio = 1.0;
                            } else {
                                Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
                            }
                        } else {
                            if (OutdoorDryBulbTemp > Furnace(FurnaceNum).MinOATCompressorHeating) {
                                //       Check to see if Heat Pump compressor was allowed to run based on outdoor temperature
                                Furnace(FurnaceNum).CompPartLoadRatio = 1.0;
                            } else {
                                Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
                            }
                        }
                    } else if (SystemSensibleLoad < NoHeatOutput) {
                        //       SystemSensibleLoad is less than minimum DX Heating coil output so heat pump does not run and
                        //       the load will be met by the supplemental heater
                        Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
                        Furnace(FurnaceNum).HeatPartLoadRatio = 1.0;
                    }
                    if (Furnace(FurnaceNum).HeatPartLoadRatio == 1.0) {
                        //       Determine the load on the supplemental heating coil
                        if ((SystemSensibleLoad - FullSensibleOutput) > Furnace(FurnaceNum).DesignSuppHeatingCapacity) {
                            HeatCoilLoad = Furnace(FurnaceNum).DesignSuppHeatingCapacity;
                            TempOutHeatingCoil = Node(FurnaceOutletNode).Temp + HeatCoilLoad / (cpair * Furnace(FurnaceNum).MdotFurnace);
                        } else if (SystemSensibleLoad < NoHeatOutput) {
                            HeatCoilLoad = max(0.0, SystemSensibleLoad); // BG 10/22/2008 need a case for when its all suppl heat
                            TempOutHeatingCoil = Node(FurnaceInletNode).Temp + HeatCoilLoad / (cpair * Furnace(FurnaceNum).MdotFurnace);
                        } else {
                            HeatCoilLoad = max(0.0, (SystemSensibleLoad - FullSensibleOutput));
                            TempOutHeatingCoil = Node(FurnaceOutletNode).Temp + HeatCoilLoad / (cpair * Furnace(FurnaceNum).MdotFurnace);
                        }
                        if (Furnace(FurnaceNum).CondenserNodeNum > 0) {
                            if (Node(Furnace(FurnaceNum).CondenserNodeNum).Temp > Furnace(FurnaceNum).MaxOATSuppHeat) {
                                HeatCoilLoad = 0.0;
                                if (SystemSensibleLoad < NoHeatOutput) {
                                    TempOutHeatingCoil = Node(FurnaceInletNode).Temp;
                                } else {
                                    TempOutHeatingCoil = Node(FurnaceOutletNode).Temp;
                                }
                            }
                        } else {
                            if (OutdoorDryBulbTemp > Furnace(FurnaceNum).MaxOATSuppHeat) {
                                HeatCoilLoad = 0.0;
                                if (SystemSensibleLoad < NoHeatOutput) {
                                    TempOutHeatingCoil = Node(FurnaceInletNode).Temp;
                                } else {
                                    TempOutHeatingCoil = Node(FurnaceOutletNode).Temp;
                                }
                            }
                        }
                        cpair = PsyCpAirFnWTdb(Node(FurnaceInletNode).HumRat, Node(FurnaceOutletNode).Temp);
                        // TempOutHeatingCoil = Node(FurnaceOutletNode)%Temp + HeatCoilLoad/(cpair*Furnace(FurnaceNum)%MdotFurnace)
                        if ((TempOutHeatingCoil > Furnace(FurnaceNum).DesignMaxOutletTemp) && (HeatCoilLoad > 0.0)) {
                            // deltaT = Furnace(FurnaceNum)%DesignMaxOutletTemp - Node(FurnaceOutletNode)%Temp
                            // BG 10/22/2008 above made no sense if DX heat is off and its all supplemental,
                            //  because Node(FurnaceOutletNode)%Temp will have been calc'd with full DX heat in last faux call to CalcFurnaceOutput

                            deltaT = (Furnace(FurnaceNum).DesignMaxOutletTemp - TempOutHeatingCoil);
                            HeatCoilLoad += (Node(FurnaceInletNode).MassFlowRate * cpair * deltaT);
                            HeatCoilLoad = max(0.0, HeatCoilLoad);
                        }
                    } else {
                        HeatCoilLoad = 0.0;
                    }
                    PartLoadRatio = 0.0;

                    //   HeatCool systems can have both a sensible and latent PLR in a single time step
                    //   (i.e. both cooling and heating can occur in a single time step)
                } else { // else not a heatpump DX coil ** non-HP heating coils are not DX so testing if OutdoorDryBulbTemp < MinOATCompressorHeating
                         // is not necessary **

                    Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;
                    HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity;
                    SystemSensibleLoad = ZoneLoad;

                    // Get no load result
                    if (OpMode == CycFanCycCoil) {
                        Node(FurnaceInletNode).MassFlowRate = 0.0;
                    }
                    if (OpMode == ContFanCycCoil) {
                        OnOffFanPartLoadFraction = 1.0; // The on/off fan will not cycle, so set part-load fraction = 1
                    }

                    //     Set the inlet mass flow rate based on user specified coil OFF flow rate
                    PartLoadRatio = 0.0;
                    SetAverageAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);

                    CalcFurnaceOutput(FurnaceNum,
                                      FirstHVACIteration,
                                      OpMode,
                                      CompOp,
                                      0.0,
                                      MinPLR,
                                      0.0,
                                      0.0,
                                      NoHeatOutput,
                                      NoLatentOutput,
                                      OnOffAirFlowRatio,
                                      false);

                    if (NoHeatOutput < SystemSensibleLoad) {
                        Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;

                        // Set fan part-load fraction equal to 1 while getting full load result
                        OnOffFanPartLoadFraction = 1.0;
                        OnOffAirFlowRatio = 1.0;

                        // Get full load result
                        CalcFurnaceOutput(FurnaceNum,
                                          FirstHVACIteration,
                                          OpMode,
                                          CompOp,
                                          0.0,
                                          1.0,
                                          HeatCoilLoad,
                                          0.0,
                                          FullSensibleOutput,
                                          FullLatentOutput,
                                          OnOffAirFlowRatio,
                                          false);
                    } else {
                        FullSensibleOutput = NoHeatOutput + 0.000000001;
                    }

                    // Since we are heating, we expect FullSensibleOutput to be > 0 and FullSensibleOutput > NoSensibleOutput
                    // Check that this is the case; if not set PartLoadRatio = 0.0 (off) and return

                    if (FullSensibleOutput > NoHeatOutput) {

                        //       check bounds on sensible output prior to iteration using RegulaFalsi
                        if (FullSensibleOutput <= SystemSensibleLoad) {
                            PartLoadRatio = 1.0;
                            //         save modified HeatCoilLoad in case it was reset because outlet temp > DesignMaxOutletTemp
                            if (ModifiedHeatCoilLoad > 0.0) {
                                HeatCoilLoad = ModifiedHeatCoilLoad;
                            } else {
                                HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity;
                            }
                        } else if (NoHeatOutput >= SystemSensibleLoad) {
                            PartLoadRatio = 0.0;
                            HeatCoilLoad = 0.0;
                        } else {

                            // Calculate the part load ratio through iteration
                            HeatErrorToler = Furnace(FurnaceNum).HeatingConvergenceTolerance; // Error tolerance for convergence from input deck

                            SolFlag = 0; // # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
                            Par(1) = double(FurnaceNum);
                            Par(2) = 0.0; // FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                            if (FirstHVACIteration) Par(2) = 1.0;
                            Par(3) = double(OpMode);
                            Par(4) = double(CompOp);
                            Par(5) = SystemSensibleLoad;
                            Par(6) = 0.0;               // FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
                            Par(7) = 1.0;               // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
                            Par(8) = OnOffAirFlowRatio; // Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
                            Par(9) = 0.0;               // HXUnitOn is always false for HX
                            Par(10) = 0.0;
                            //         HeatErrorToler is in fraction load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
                            SolveRoot(HeatErrorToler, MaxIter, SolFlag, PartLoadRatio, CalcFurnaceResidual, 0.0, 1.0, Par);
                            //         OnOffAirFlowRatio is updated during the above iteration. Reset to correct value based on PLR.
                            OnOffAirFlowRatio = OnOffAirFlowRatioSave;
                            //         Reset HeatCoilLoad calculated in CalcFurnaceResidual (in case it was reset because output temp >
                            //         DesignMaxOutletTemp)
                            if (ModifiedHeatCoilLoad > 0.0) {
                                HeatCoilLoad = ModifiedHeatCoilLoad;
                            } else {
                                HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity * PartLoadRatio;
                            }
                            if (SolFlag == -1) {

                                //           RegulaFalsi may not find heating PLR when the maximum supply air temperature is exceeded.
                                //           If iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                                TempMaxPLR = -0.1;
                                TempHeatOutput = NoHeatOutput;
                                while ((TempHeatOutput - SystemSensibleLoad) < 0.0 && TempMaxPLR < 1.0) {
                                    //             find upper limit of HeatingPLR
                                    TempMaxPLR += 0.1;
                                    HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity * TempMaxPLR;
                                    CalcFurnaceOutput(FurnaceNum,
                                                      FirstHVACIteration,
                                                      OpMode,
                                                      CompOp,
                                                      0.0,
                                                      TempMaxPLR,
                                                      HeatCoilLoad,
                                                      0.0,
                                                      TempHeatOutput,
                                                      TempLatentOutput,
                                                      OnOffAirFlowRatio,
                                                      false);
                                }
                                TempMinPLR = TempMaxPLR;
                                while ((TempHeatOutput - SystemSensibleLoad) > 0.0 && TempMinPLR > 0.0) {
                                    //             pull upper limit of HeatingPLR down to last valid limit (i.e. heat output still exceeds
                                    //             SystemSensibleLoad)
                                    TempMaxPLR = TempMinPLR;
                                    //             find minimum limit of HeatingPLR
                                    TempMinPLR -= 0.01;

                                    HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity * TempMinPLR;
                                    CalcFurnaceOutput(FurnaceNum,
                                                      FirstHVACIteration,
                                                      OpMode,
                                                      CompOp,
                                                      0.0,
                                                      TempMinPLR,
                                                      HeatCoilLoad,
                                                      0.0,
                                                      TempHeatOutput,
                                                      TempLatentOutput,
                                                      OnOffAirFlowRatio,
                                                      false);
                                }
                                //           Now solve again with tighter PLR limits
                                SolveRoot(HeatErrorToler, MaxIter, SolFlag, PartLoadRatio, CalcFurnaceResidual, TempMinPLR, TempMaxPLR, Par);
                                if (ModifiedHeatCoilLoad > 0.0) {
                                    HeatCoilLoad = ModifiedHeatCoilLoad;
                                } else {
                                    HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity * PartLoadRatio;
                                }
                                CalcFurnaceOutput(FurnaceNum,
                                                  FirstHVACIteration,
                                                  OpMode,
                                                  CompOp,
                                                  0.0,
                                                  PartLoadRatio,
                                                  HeatCoilLoad,
                                                  0.0,
                                                  TempHeatOutput,
                                                  TempLatentOutput,
                                                  OnOffAirFlowRatio,
                                                  false);

                                //           After iterating with tighter boundaries, if still out of tolerance, show warning.
                                if (SolFlag == -1 && std::abs(SystemSensibleLoad - TempHeatOutput) > SmallLoad) {
                                    if (Furnace(FurnaceNum).HeatingMaxIterIndex == 0) {
                                        ShowWarningMessage("Heating coil control failed to converge for " +
                                                           cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + ':' + Furnace(FurnaceNum).Name);
                                        ShowContinueError("  Iteration limit exceeded in calculating heating coil sensible part-load ratio.");
                                        ShowContinueErrorTimeStamp(
                                            "Sensible load to be met by heating coil = " + TrimSigDigits(SystemSensibleLoad, 2) +
                                            " (watts), sensible output of heating coil = " + TrimSigDigits(TempHeatOutput, 2) +
                                            " (watts), and the simulation continues.");
                                    }
                                    ShowRecurringWarningErrorAtEnd(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" +
                                                                       Furnace(FurnaceNum).Name +
                                                                       "\" - Iteration limit exceeded in calculating sensible heating part-load "
                                                                       "ratio error continues. Sensible load statistics:",
                                                                   Furnace(FurnaceNum).HeatingMaxIterIndex,
                                                                   SystemSensibleLoad,
                                                                   SystemSensibleLoad);
                                }
                            } else if (SolFlag == -2) {
                                if (Furnace(FurnaceNum).HeatingRegulaFalsiFailedIndex == 0) {
                                    ShowWarningMessage("Heating coil control failed for " + cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + ':' +
                                                       Furnace(FurnaceNum).Name);
                                    ShowContinueError("  Sensible heating part-load ratio determined to be outside the range of 0-1.");
                                    ShowContinueErrorTimeStamp("Sensible load to be met by heating coil = " + TrimSigDigits(SystemSensibleLoad, 2) +
                                                               " (watts), and the simulation continues.");
                                }
                                ShowRecurringWarningErrorAtEnd(
                                    cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                        "\" -  Sensible heating part-load ratio out of range error continues. Sensible load statistics:",
                                    Furnace(FurnaceNum).HeatingRegulaFalsiFailedIndex,
                                    SystemSensibleLoad,
                                    SystemSensibleLoad);
                            }
                        }

                    } else { // ELSE from IF(FullSensibleOutput.GT.NoSensibleOutput)THEN above
                        // Set part load ratio to 1 and run heater at design heating capacity
                        PartLoadRatio = 1.0;
                        HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity;
                    }

                } // End of IF HeatPump

            } // End of IF for heating

            // Non-heat pump systems do not set a heating PLR, set it here for use with the DX cooling coil calculations.
            // Set this variable back to 0 for non-heat pump systems at the end of this routine.
            Furnace(FurnaceNum).HeatPartLoadRatio = max(PartLoadRatio, Furnace(FurnaceNum).HeatPartLoadRatio);
            CalcFurnaceOutput(FurnaceNum,
                              FirstHVACIteration,
                              OpMode,
                              CompOp,
                              0.0,
                              Furnace(FurnaceNum).HeatPartLoadRatio,
                              HeatCoilLoad,
                              0.0,
                              HeatingSensibleOutput,
                              HeatingLatentOutput,
                              OnOffAirFlowRatio,
                              false);

            if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir ||
                (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir && Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_Simple &&
                 CoolingLoad)) {
                HeatingSensibleOutput = 0.0;
                HeatingLatentOutput = 0.0;
            }
            //***********Cooling Section*****************
            // Simulate if scheduled ON and cooling load or if a moisture load exists when using a humidistat
            // Check of HeatingLatentOutput is used to reduce overshoot during simultaneous heating and cooling
            // Setback flag is used to avoid continued RH control when Tstat is setback (RH should float down)
            if ((GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0 && CoolingLoad) ||
                (Furnace(FurnaceNum).Humidistat && Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat &&
                 (SystemMoistureLoad < 0.0 ||
                  (SystemMoistureLoad >= 0.0 && HeatingLatentOutput > SystemMoistureLoad && !Setback(Furnace(FurnaceNum).ControlZoneNum))))) {

                //     For cooling operation, the first step is to set the HX operation flag in case a HX assisted coil is used.
                //      (if a HX assisted coil is not used, this flag is not used. It's only used in the CALL to SimHXAssistedCoolingCoil)
                //     Check the dehumidification control type:
                //           For dehumidification control options CoolReheat and None, the HX is always active (locked ON).
                //           For dehumidification control option Multimode, the system is operated first with the HX off.
                //           If the moisture load is not met, the HX will then be turned on and the system is re-simulated.

                if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat ||
                    Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_None) {
                    HXUnitOn = true;
                } else {
                    HXUnitOn = false;
                }

                //     The next step is to determine the system output at no load (PLR=0) and full load (PLR=1)

                //     Set the inlet mass flow rate based on user specified coil OFF flow rate
                PartLoadRatio = 0.0;

                Furnace(FurnaceNum).CompPartLoadRatio = 0.0; // compressor off
                Furnace(FurnaceNum).WSHPRuntimeFrac = 0.0;

                //     SetAverageAirFlow calculates the operating mass flow rate based on PLR and the user specified inputs
                //     for MaxCoolAirMassFlow and MaxNoCoolHeatAirMassFlow.
                //     Air flow rate is set according to max of cooling and heating PLR if heating and latent load exists.
                if (OpMode == CycFanCycCoil && Furnace(FurnaceNum).HeatPartLoadRatio > 0.0 && Furnace(FurnaceNum).Humidistat &&
                    Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat &&
                    (SystemMoistureLoad < 0.0 ||
                     (SystemMoistureLoad >= 0.0 && HeatingLatentOutput > SystemMoistureLoad && !Setback(Furnace(FurnaceNum).ControlZoneNum)))) {
                    CoolingHeatingPLRRatio = min(1.0, PartLoadRatio / Furnace(FurnaceNum).HeatPartLoadRatio);
                    SetAverageAirFlow(FurnaceNum, max(PartLoadRatio, Furnace(FurnaceNum).HeatPartLoadRatio), OnOffAirFlowRatio);

                } else {
                    CoolingHeatingPLRRatio = 1.0;
                    SetAverageAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);
                }

                // Get no load result (coils simulated OFF)
                CalcFurnaceOutput(FurnaceNum,
                                  FirstHVACIteration,
                                  OpMode,
                                  CompOp,
                                  MinPLR,
                                  PartLoadRatio,
                                  0.0,
                                  0.0,
                                  NoCoolOutput,
                                  NoLatentOutput,
                                  OnOffAirFlowRatio,
                                  HXUnitOn,
                                  CoolingHeatingPLRRatio);

                //     Don't calculate full load output if no load output can meet sensible load
                if (NoCoolOutput >= CoolCoilLoad && (CoolCoilLoad != 0.0 || HPDehumidificationLoadFlag)) {
                    //       Set full mass flow rate for full load calculation
                    Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;

                    // Set fan part-load fraction equal to 1 while getting full load result
                    OnOffFanPartLoadFraction = 1.0;
                    OnOffAirFlowRatio = 1.0;
                    PartLoadRatio = 1.0;
                    Furnace(FurnaceNum).CompPartLoadRatio = 1.0; // compressor ON
                    Furnace(FurnaceNum).WSHPRuntimeFrac = 1.0;

                    // Get full load result (coils simulated full ON)
                    CalcFurnaceOutput(FurnaceNum,
                                      FirstHVACIteration,
                                      OpMode,
                                      CompOp,
                                      PartLoadRatio,
                                      0.0,
                                      0.0,
                                      0.0,
                                      FullSensibleOutput,
                                      FullLatentOutput,
                                      OnOffAirFlowRatio,
                                      HXUnitOn);
                } else {
                    FullSensibleOutput = NoCoolOutput - 0.00000001;
                }

                //     The next step is to compare the results of the full load and no load results
                //     1) Since we are cooling, we expect FullSensibleOutput < NoCoolOutput
                //        Check that this is the case; if not set PartLoadRatio = 0.0 (off)
                //     2) Verify that the load to be met is within the range of available output
                //        (i.e. between FullSensibleOutput and NoCoolOutput)
                //     3) Set PLR if load is out of range or RegulaFalsi on PLR if system can meet the load
                if (FullSensibleOutput < NoCoolOutput) {
                    if (CoolCoilLoad != 0.0 || HPDehumidificationLoadFlag) {

                        //           check bounds on sensible output prior to iteration using RegulaFalsi
                        //           Negative value represents cooling load, IF FullSensibleOutput .GT. CoolCoilLoad, load is greater than capacity
                        if (FullSensibleOutput >= CoolCoilLoad) {
                            PartLoadRatio = 1.0;
                            //           Likewise IF NoCoolOutput .LT. CoolCoilLoad, then load can be met using only the fan (constant fan mode only)
                        } else if (NoCoolOutput <= CoolCoilLoad) {
                            PartLoadRatio = 0.0;
                            //           ELSE load is between NoCoolOutput and FullSensibleOuput, find PLR required to meet load
                        } else {

                            // Calculate the sensible part load ratio through iteration
                            CoolErrorToler = Furnace(FurnaceNum).CoolingConvergenceTolerance; // Error tolerance for convergence from input deck
                            SolFlag = 0; // # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
                            Par(1) = double(FurnaceNum);
                            Par(2) = 0.0; // FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                            if (FirstHVACIteration) Par(2) = 1.0;
                            Par(3) = double(OpMode);
                            Par(4) = double(CompOp);
                            Par(5) = CoolCoilLoad;
                            Par(6) = 1.0;               // FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
                            Par(7) = 1.0;               // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
                            Par(8) = OnOffAirFlowRatio; // Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
                            if (HXUnitOn) {
                                Par(9) = 1.0;
                            } else {
                                Par(9) = 0.0;
                            }
                            //             Par(10) is the heating coil PLR, set this value to 0 for sensible PLR calculations.
                            Par(10) = 0.0;
                            //             CoolErrorToler is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
                            SolveRoot(CoolErrorToler, MaxIter, SolFlag, PartLoadRatio, CalcFurnaceResidual, 0.0, 1.0, Par);
                            //             OnOffAirFlowRatio is updated during the above iteration. Reset to correct value based on PLR.
                            OnOffAirFlowRatio = OnOffAirFlowRatioSave;
                            if (SolFlag < 0) {
                                if (SolFlag == -1) {
                                    CalcFurnaceOutput(FurnaceNum,
                                                      FirstHVACIteration,
                                                      OpMode,
                                                      CompOp,
                                                      PartLoadRatio,
                                                      0.0,
                                                      0.0,
                                                      0.0,
                                                      TempCoolOutput,
                                                      TempLatentOutput,
                                                      OnOffAirFlowRatio,
                                                      HXUnitOn);
                                    if (!WarmupFlag) {
                                        if (std::abs(CoolCoilLoad - TempCoolOutput) > SmallLoad) {
                                            if (Furnace(FurnaceNum).SensibleMaxIterIndex == 0) {
                                                ShowWarningMessage("Cooling coil control failed to converge for " +
                                                                   cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + ':' +
                                                                   Furnace(FurnaceNum).Name);
                                                ShowContinueError(
                                                    "  Iteration limit exceeded in calculating DX cooling coil sensible part-load ratio.");
                                                ShowContinueErrorTimeStamp(
                                                    "Sensible load to be met by DX coil = " + TrimSigDigits(CoolCoilLoad, 2) +
                                                    " (watts), sensible output of DX coil = " + TrimSigDigits(TempCoolOutput, 2) +
                                                    " (watts), and the simulation continues.");
                                            }
                                            ShowRecurringWarningErrorAtEnd(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" +
                                                                               Furnace(FurnaceNum).Name +
                                                                               "\" - Iteration limit exceeded in calculating sensible cooling "
                                                                               "part-load ratio error continues. Sensible load statistics:",
                                                                           Furnace(FurnaceNum).SensibleMaxIterIndex,
                                                                           CoolCoilLoad,
                                                                           CoolCoilLoad);
                                        }
                                    }
                                } else if (SolFlag == -2) {
                                    if (!WarmupFlag) {
                                        if (Furnace(FurnaceNum).SensibleRegulaFalsiFailedIndex == 0) {
                                            ShowWarningMessage("Cooling coil control failed for " +
                                                               cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + ':' + Furnace(FurnaceNum).Name);
                                            ShowContinueError("  Cooling sensible part-load ratio determined to be outside the range of 0-1.");
                                            ShowContinueErrorTimeStamp("  Cooling sensible load = " + TrimSigDigits(CoolCoilLoad, 2));
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                                "\" - Cooling sensible part-load ratio out of range error continues. Sensible cooling load "
                                                "statistics:",
                                            Furnace(FurnaceNum).SensibleRegulaFalsiFailedIndex,
                                            CoolCoilLoad,
                                            CoolCoilLoad);
                                    }
                                }
                            }
                        }

                    } else {
                        PartLoadRatio = 0.0;
                    } // EndIf for IF(CoolCoilLoad.NE.0.0)

                    //       Calculate the delivered capacity from the PLR caculated above
                    CalcFurnaceOutput(FurnaceNum,
                                      FirstHVACIteration,
                                      OpMode,
                                      CompOp,
                                      PartLoadRatio,
                                      Furnace(FurnaceNum).HeatPartLoadRatio,
                                      0.0,
                                      0.0,
                                      TempCoolOutput,
                                      TempLatentOutput,
                                      OnOffAirFlowRatio,
                                      HXUnitOn);

                    //       Calculate the latent part load ratio through iteration
                    //       Negative SystemMoistureLoad means dehumidification load is present
                    //       IF this furnace uses MultiMode control AND there is a moisture load AND the moisture load met by the furnace in
                    //       cooling only mode above is sufficient to meet the moisture demand OR there is no sensible load (PLR=0 from above)
                    //       then set LatentPartLoadRatio to 0 (no additional dehumidification is required).
                    if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_Multimode &&
                        ((SystemMoistureLoad < 0.0 && TempLatentOutput < SystemMoistureLoad) || PartLoadRatio == 0.0)) {
                        LatentPartLoadRatio = 0.0;
                        //       ELSE calculate a new PLR for valid dehumidification control types if a moisture load exists.
                    } else if (Furnace(FurnaceNum).DehumidControlType_Num != DehumidControl_None &&
                               (SystemMoistureLoad < 0.0 || (SystemMoistureLoad >= 0.0 && TempLatentOutput > SystemMoistureLoad &&
                                                             !Setback(Furnace(FurnaceNum).ControlZoneNum)))) {

                        //         IF the furnace uses dehumidification control MultiMode, turn on the HX and calculate the latent output with
                        //         the HX ON to compare to the moisture load predicted by the humidistat.
                        if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_Multimode) {
                            HXUnitOn = true;
                            Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;
                            // Set fan part-load fraction equal to 1 while getting full load result
                            OnOffFanPartLoadFraction = 1.0;
                            OnOffAirFlowRatio = 1.0;
                            // Get full load result
                            CalcFurnaceOutput(FurnaceNum,
                                              FirstHVACIteration,
                                              OpMode,
                                              CompOp,
                                              1.0,
                                              0.0,
                                              0.0,
                                              0.0,
                                              TempCoolOutput,
                                              TempLatentOutput,
                                              OnOffAirFlowRatio,
                                              HXUnitOn);
                        }

                        //         Set the global cooling to heating PLR ratio. CoolHeatPLRRat = MIN(1,CoolingPLR/HeatingPLR)
                        CoolHeatPLRRat = 1.0; // means cooling dominated operation (applies to cycling fan mode)

                        if (TempLatentOutput > SystemMoistureLoad) {
                            //           Set full mass flow rate for full load calculation
                            Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;

                            // Set fan part-load fraction equal to 1 while getting full load result
                            OnOffFanPartLoadFraction = 1.0;
                            OnOffAirFlowRatio = 1.0;
                            Furnace(FurnaceNum).CompPartLoadRatio = 1.0; // compressor ON
                            Furnace(FurnaceNum).WSHPRuntimeFrac = 1.0;

                            // Get full load result (coils simulated full ON)
                            CalcFurnaceOutput(FurnaceNum,
                                              FirstHVACIteration,
                                              OpMode,
                                              CompOp,
                                              1.0,
                                              0.0,
                                              0.0,
                                              0.0,
                                              TempCoolOutput,
                                              TempLatentOutput,
                                              OnOffAirFlowRatio,
                                              HXUnitOn);
                        }

                        //         check bounds on latent output prior to iteration using RegulaFalsi
                        if (TempLatentOutput > SystemMoistureLoad ||
                            (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_Multimode && TempCoolOutput > CoolCoilLoad)) {
                            LatentPartLoadRatio = 1.0;
                        } else if (NoLatentOutput < SystemMoistureLoad || HeatingLatentOutput < SystemMoistureLoad) {
                            LatentPartLoadRatio = 0.0;
                        } else {

                            CoolErrorToler = Furnace(FurnaceNum).CoolingConvergenceTolerance; // Error tolerance for convergence

                            SolFlag = 0; // # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
                            Par(1) = double(FurnaceNum);
                            Par(2) = 0.0; // FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                            if (FirstHVACIteration) Par(2) = 1.0;
                            Par(3) = double(OpMode);
                            Par(4) = double(CompOp);
                            //           Multimode always controls to meet the SENSIBLE load (however, HXUnitOn is now TRUE)
                            if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_Multimode) {
                                Par(5) = CoolCoilLoad;
                            } else {
                                Par(5) = SystemMoistureLoad;
                            }
                            Par(6) = 1.0; // FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
                            //           Multimode always controls to meet the SENSIBLE load (however, HXUnitOn is now TRUE)
                            if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_Multimode) {
                                Par(7) = 1.0; // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
                            } else {
                                Par(7) = 0.0;
                            }
                            Par(8) = OnOffAirFlowRatio; // Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
                            if (HXUnitOn) {
                                Par(9) = 1.0;
                            } else {
                                Par(9) = 0.0;
                            }
                            //           Par(10) used only with cycling fan.
                            //           Par(10) is the heating coil PLR, set this value only if there is a heating load (heating PLR > 0)
                            //           and the latent PLR is being calculated. Otherwise set Par(10) to 0.
                            if (OpMode == CycFanCycCoil && Furnace(FurnaceNum).HeatPartLoadRatio > 0.0 && Par(7) == 0.0) {
                                Par(10) = Furnace(FurnaceNum).HeatPartLoadRatio;
                            } else {
                                Par(10) = 0.0;
                            }
                            //           CoolErrorToler is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
                            SolveRoot(CoolErrorToler, MaxIter, SolFlag, LatentPartLoadRatio, CalcFurnaceResidual, 0.0, 1.0, Par);
                            //           OnOffAirFlowRatio is updated during the above iteration. Reset to correct value based on PLR.
                            OnOffAirFlowRatio = OnOffAirFlowRatioSave;
                            if (SolFlag == -1) {
                                //             RegulaFalsi may not find latent PLR when the latent degradation model is used.
                                //             If iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                                TempMaxPLR = -0.1;
                                TempLatentOutput = NoLatentOutput;
                                while ((TempLatentOutput - SystemMoistureLoad) > 0.0 && TempMaxPLR < 1.0) {
                                    //               find upper limit of LatentPLR
                                    TempMaxPLR += 0.1;

                                    //               Same calculation as is done in Function CalcFurnaceResidual for latent PLR calculation.
                                    //               Set cooling to heating PLR for use with Subroutine CalcFurnaceOutput. IF Par(10) = 0,
                                    //               heating PLR = 0 so set the CoolingHeatingPLRRatio to 1 so the cooling PLR is used in the
                                    //               DX cooling coil calculations.
                                    if (Par(10) > 0.0) {
                                        //                 Par(10) = Furnace(FurnaceNum)%HeatPartLoadRatio
                                        //                 OpMode = CycFan and Furnace(FurnaceNum)%HeatPartLoadRatio must be > 0 for Part(10) to be
                                        //                 greater than 0
                                        CoolingHeatingPLRRatio = min(1.0, TempMaxPLR / Furnace(FurnaceNum).HeatPartLoadRatio);
                                    } else {
                                        CoolingHeatingPLRRatio = 1.0;
                                    }

                                    CalcFurnaceOutput(FurnaceNum,
                                                      FirstHVACIteration,
                                                      OpMode,
                                                      CompOp,
                                                      TempMaxPLR,
                                                      0.0,
                                                      0.0,
                                                      0.0,
                                                      TempCoolOutput,
                                                      TempLatentOutput,
                                                      OnOffAirFlowRatio,
                                                      HXUnitOn,
                                                      CoolingHeatingPLRRatio);
                                }
                                TempMinPLR = TempMaxPLR;
                                while ((TempLatentOutput - SystemMoistureLoad) < 0.0 && TempMinPLR > 0.0) {
                                    //               pull upper limit of LatentPLR down to last valid limit (i.e. latent output still exceeds
                                    //               SystemMoisuterLoad) CR7558 - relax final limits to allow HX assisted coils to converge
                                    TempMaxPLR = TempMinPLR + 0.001;
                                    //               find minimum limit of Latent PLR
                                    TempMinPLR -= 0.001;

                                    //               Set cooling to heating PLR for use with Subroutine CalcFurnaceOutput.
                                    if (Par(10) > 0.0) {
                                        //                 Par(10) = Furnace(FurnaceNum)%HeatPartLoadRatio
                                        //                 OpMode = CycFan and Furnace(FurnaceNum)%HeatPartLoadRatio must be > 0 for Part(10) to be
                                        //                 greater than 0 Since the latent output of cycling fan systems is 0 at PLR=0, do not allow
                                        //                 the PLR to be 0, otherwise RegulaFalsi can fail when a heating and moisture load exists and
                                        //                 heating PLR > latent PLR.
                                        TempMinPLR2 = max(0.0000000001, TempMinPLR);
                                        CoolingHeatingPLRRatio = min(1.0, TempMinPLR2 / Furnace(FurnaceNum).HeatPartLoadRatio);
                                    } else {
                                        TempMinPLR2 = TempMinPLR;
                                        CoolingHeatingPLRRatio = 1.0;
                                    }

                                    CalcFurnaceOutput(FurnaceNum,
                                                      FirstHVACIteration,
                                                      OpMode,
                                                      CompOp,
                                                      TempMinPLR2,
                                                      0.0,
                                                      0.0,
                                                      0.0,
                                                      TempCoolOutput,
                                                      TempLatentOutput,
                                                      OnOffAirFlowRatio,
                                                      HXUnitOn,
                                                      CoolingHeatingPLRRatio);
                                }
                                //             tighter boundary of solution has been found, call RegulaFalsi a second time
                                SolveRoot(CoolErrorToler, MaxIter, SolFlag, LatentPartLoadRatio, CalcFurnaceResidual, TempMinPLR2, TempMaxPLR, Par);
                                //             OnOffAirFlowRatio is updated during the above iteration. Reset to correct value based on PLR.
                                OnOffAirFlowRatio = OnOffAirFlowRatioSave;
                                if (SolFlag == -1) {

                                    //               Set cooling to heating PLR for use with Subroutine CalcFurnaceOutput.
                                    if (Par(10) > 0.0) {
                                        //                 Par(10) = Furnace(FurnaceNum)%HeatPartLoadRatio
                                        //                 OpMode = CycFan and Furnace(FurnaceNum)%HeatPartLoadRatio must be > 0 for Part(10) to be
                                        //                 greater than 0
                                        CoolingHeatingPLRRatio = min(1.0, LatentPartLoadRatio / Furnace(FurnaceNum).HeatPartLoadRatio);
                                    } else {
                                        CoolingHeatingPLRRatio = 1.0;
                                    }

                                    CalcFurnaceOutput(FurnaceNum,
                                                      FirstHVACIteration,
                                                      OpMode,
                                                      CompOp,
                                                      LatentPartLoadRatio,
                                                      0.0,
                                                      0.0,
                                                      0.0,
                                                      TempCoolOutput,
                                                      TempLatentOutput,
                                                      OnOffAirFlowRatio,
                                                      HXUnitOn,
                                                      CoolingHeatingPLRRatio);
                                    if (std::abs((SystemMoistureLoad - TempLatentOutput) / SystemMoistureLoad) > CoolErrorToler &&
                                        std::abs(SystemMoistureLoad - TempLatentOutput) > 10.0) {
                                        if (!WarmupFlag) {
                                            if (Furnace(FurnaceNum).LatentMaxIterIndex == 0) {
                                                ShowWarningMessage("Cooling coil control failed to converge for " +
                                                                   cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + ':' +
                                                                   Furnace(FurnaceNum).Name);
                                                ShowContinueError("  Iteration limit exceeded in calculating cooling coil latent part-load ratio.");
                                                ShowContinueError(
                                                    "  Latent load convergence error (percent) = " +
                                                    TrimSigDigits(100.0 * std::abs((SystemMoistureLoad - TempLatentOutput) / SystemMoistureLoad), 2));
                                                ShowContinueErrorTimeStamp(
                                                    "Moisture load to be met by DX coil = " + TrimSigDigits(SystemMoistureLoad, 2) +
                                                    " (watts), Latent output of DX coil = " + TrimSigDigits(TempLatentOutput, 2) +
                                                    " (watts), and the simulation continues.");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                                    "\" - Iteration limit exceeded in calculating latent part-load ratio error continues. Latent "
                                                    "load convergence error (percent) statistics follow.",
                                                Furnace(FurnaceNum).LatentMaxIterIndex,
                                                100.0 * std::abs((SystemMoistureLoad - TempLatentOutput) / SystemMoistureLoad),
                                                100.0 * std::abs((SystemMoistureLoad - TempLatentOutput) / SystemMoistureLoad));
                                        }
                                    }
                                } else if (SolFlag == -2) {
                                    if (Furnace(FurnaceNum).LatentRegulaFalsiFailedIndex2 == 0) {
                                        ShowWarningMessage("Cooling coil control failed for " + cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) +
                                                           ':' + Furnace(FurnaceNum).Name);
                                        ShowContinueError("  Latent part-load ratio determined to be outside the range of " +
                                                          TrimSigDigits(TempMinPLR, 3) + " to " + TrimSigDigits(TempMaxPLR, 3) + '.');
                                        ShowContinueErrorTimeStamp("A PLR of " + TrimSigDigits(TempMinPLR, 3) +
                                                                   " will be used and the simulation continues.");
                                    }
                                    ShowRecurringWarningErrorAtEnd(
                                        cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                            "\" - Cooling sensible part-load ratio out of range error continues. System moisture load statistics:",
                                        Furnace(FurnaceNum).LatentRegulaFalsiFailedIndex2,
                                        SystemMoistureLoad,
                                        SystemMoistureLoad);
                                    LatentPartLoadRatio = TempMinPLR;
                                }
                            } else if (SolFlag == -2) {
                                if (Furnace(FurnaceNum).LatentRegulaFalsiFailedIndex == 0) {
                                    ShowWarningMessage("Cooling coil control failed for " + cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + ':' +
                                                       Furnace(FurnaceNum).Name);
                                    ShowContinueError("  Latent part-load ratio determined to be outside the range of 0-1.");
                                    ShowContinueErrorTimeStamp("A PLR of 0 will be used and the simulation continues.");
                                }
                                ShowRecurringWarningErrorAtEnd(
                                    cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                        "\" - Latent part-load ratio out of range or 0-1 error continues. System moisture load statistics:",
                                    Furnace(FurnaceNum).LatentRegulaFalsiFailedIndex,
                                    SystemMoistureLoad,
                                    SystemMoistureLoad);
                                LatentPartLoadRatio = 0.0;
                            }
                        }

                        //         Cooling to heating PLR ratio is now known as CoolHeatPLRRat (Module level global set in CalcFurnaceOutput
                        //         This same variable is use in Subroutine SimFurnace for final calculations.
                        //         Get the actual output in case reheat needs to be calculated (HumControl=TRUE [latent PLR > sensible PLR])
                        CalcFurnaceOutput(FurnaceNum,
                                          FirstHVACIteration,
                                          OpMode,
                                          CompOp,
                                          LatentPartLoadRatio,
                                          0.0,
                                          0.0,
                                          0.0,
                                          ActualSensibleOutput,
                                          ActualLatentOutput,
                                          OnOffAirFlowRatio,
                                          HXUnitOn,
                                          CoolHeatPLRRat);

                    } else {
                        LatentPartLoadRatio = 0.0;
                    } // ENDIF for valid dehumidification control types

                    //       IF a humidistat is used and there is a moisture load, check if the latent PLR is greater than the (sensible) PLR
                    //        IF(LatentPartLoadRatio .GT. PartLoadRatio .and. SystemMoistureLoad .lt. 0.0 .and. Furnace(FurnaceNum)%Humidistat) THEN
                    if (LatentPartLoadRatio > PartLoadRatio && Furnace(FurnaceNum).Humidistat) {
                        //         For dehumidification mode CoolReheat, compare the Sensible and Latent PLR values, if latentPLR is greater
                        //         than PLR (sensible), then overcooling is required and reheat will be activated using the HumControl flag.
                        if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat) {
                            PartLoadRatio = LatentPartLoadRatio;
                            HumControl = true;
                        }
                        //         For dehumidification mode MultiMode, compare the Sensible and Latent PLR values, if latentPLR is
                        //         greater than PLR (sensible), then use the latent PLR to control the unit.
                        //         For MultiMode control, the latent PLR is found by enabling the HX and calculating a PLR required to meet the
                        //         sensible load. Overcooling is not required, and reheat will not be activated using the HumControl flag.
                        if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_Multimode) {
                            PartLoadRatio = LatentPartLoadRatio;
                        }
                    }

                    Furnace(FurnaceNum).CoolPartLoadRatio = PartLoadRatio;
                    if (CompOp == Off) {
                        Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
                    } else {
                        Furnace(FurnaceNum).CompPartLoadRatio = PartLoadRatio;
                    }

                } else { // ELSE from IF(FullSensibleOutput.LT.NoCoolOutput)THEN above
                    // CR8679 - Unitary Heat Cool control problem, will not run to meeting cooling load
                    // underlying problem is that FullSensibleOutput is greater than 0 due to very high inlet temp, so the system should be on
                    // NoCoolOutput was 0 since the defect file is a cycling fan system and the system was turned off

                    // if FullSensibleOutput > NoCoolOutput, it means the system cannot meet the load and will run full out
                    // this same logic for WSHP does not seem to work (only the Unitary Heat Pump Compressor Part-Load Ratio report
                    // variable was affected in the HeatPumpWaterToAirRHControl.idf file while other variables showed very small diffs).
                    // The defect files meter.csv showed 2% diffs so this IF test is used to keep the results the same in that file.
                    // Additional logic is used here to make sure the coil actually turned on, e.g., if DX coil PLR > 0 then set to 1,
                    // otherwise 0 (to make sure coil is actually ON and not off due to schedule, OAT, or other reason).
                    // The global variable DXCoilPartLoadRatio(DXCoilNum) is not yet used for the WSHP to make the same check.
                    if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir) {
                        Furnace(FurnaceNum).CoolPartLoadRatio = 0.0;
                        Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
                    } else {
                        if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {

                            // VS coil issue here...
                            if (DXCoilPartLoadRatio(Furnace(FurnaceNum).ActualDXCoilIndexForHXAssisted) > 0.0) {
                                Furnace(FurnaceNum).CoolPartLoadRatio = 1.0;
                                Furnace(FurnaceNum).CompPartLoadRatio = 1.0;
                            } else {
                                Furnace(FurnaceNum).CoolPartLoadRatio = 0.0;
                                Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
                            }
                        } else {
                            if (DXCoilPartLoadRatio(Furnace(FurnaceNum).CoolingCoilIndex) > 0.0) {
                                Furnace(FurnaceNum).CoolPartLoadRatio = 1.0;
                                Furnace(FurnaceNum).CompPartLoadRatio = 1.0;
                            } else {
                                Furnace(FurnaceNum).CoolPartLoadRatio = 0.0;
                                Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
                            }
                        }
                    }
                }

                //     Calculate the reheat coil output
                if (HumControl) { // HumControl = .TRUE. if a Humidistat is installed and dehumdification control type is CoolReheat
                    if (Furnace(FurnaceNum).ZoneSequenceHeatingNum > 0) {
                        QToHeatSetPt = (ZoneSysEnergyDemand(Furnace(FurnaceNum).ControlZoneNum)
                                            .SequencedOutputRequiredToHeatingSP(Furnace(FurnaceNum).ZoneSequenceHeatingNum) /
                                        Furnace(FurnaceNum).ControlZoneMassFlowFrac);
                    } else {
                        QToHeatSetPt = (ZoneSysEnergyDemand(Furnace(FurnaceNum).ControlZoneNum).OutputRequiredToHeatingSP /
                                        Furnace(FurnaceNum).ControlZoneMassFlowFrac);
                    }
                    //       Cooling mode or floating condition and dehumidification is required
                    if (QToHeatSetPt < 0.0) {
                        //         Calculate the reheat coil load wrt the heating setpoint temperature. Reheat coil picks up
                        //         the entire excess sensible cooling (DX cooling coil and impact of outdoor air).
                        ReheatCoilLoad = max(0.0, (QToHeatSetPt - ActualSensibleOutput));
                        Furnace(FurnaceNum).DehumidInducedHeatingDemandRate = ReheatCoilLoad;
                        //       Heating mode and dehumidification is required
                    } else if (QToHeatSetPt >= 0.0) {
                        //         Calculate the reheat coil load as the sensible capacity of the DX cooling coil only. Let
                        //         the heating coil pick up the load due to outdoor air.
                        ReheatCoilLoad = max(0.0, (ActualSensibleOutput - NoCoolOutput) * (-1.0));
                        //         Dehumidification is not required
                        if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir ||
                            (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir &&
                             Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_Simple)) {
                            ReheatCoilLoad = max(QToHeatSetPt, QToHeatSetPt - ActualSensibleOutput);
                        }
                        Furnace(FurnaceNum).DehumidInducedHeatingDemandRate = max(0.0, ActualSensibleOutput * (-1.0));
                    } else {
                        ReheatCoilLoad = 0.0;
                    }
                } else {
                    //       No humidistat installed
                    ReheatCoilLoad = 0.0;
                }
            } // End of cooling section IF statement

            if (NoHeatOutput > SystemSensibleLoad && ReheatCoilLoad > 0.0) {
                // Reduce reheat coil load if you are controlling high humidity but outside air
                // and/or the supply air fan is providing enough heat to meet the system sensible load.
                // This will bring the zone temp closer to the heating setpoint temp.
                ReheatCoilLoad = max(0.0, ReheatCoilLoad - (NoHeatOutput - SystemSensibleLoad));
            }

            // Set the final air flow. MdotFurnace will be used to set the fan part-load ratio in ReportFurnace
            if (HumControl && SystemMoistureLoad < 0.0) {
                if (OpMode == CycFanCycCoil) {
                    //       set the flow rate at the maximum of the cooling and heating PLR's
                    SetAverageAirFlow(
                        FurnaceNum, max(Furnace(FurnaceNum).CoolPartLoadRatio, Furnace(FurnaceNum).HeatPartLoadRatio), OnOffAirFlowRatio);
                } else {
                    //       ELSE set the flow rate at the cooling PLR
                    SetAverageAirFlow(FurnaceNum, Furnace(FurnaceNum).CoolPartLoadRatio, OnOffAirFlowRatio);
                }
            } else {
                SetAverageAirFlow(FurnaceNum, max(Furnace(FurnaceNum).CoolPartLoadRatio, Furnace(FurnaceNum).HeatPartLoadRatio), OnOffAirFlowRatio);
            }
            Furnace(FurnaceNum).MdotFurnace = Node(FurnaceInletNode).MassFlowRate;

            if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir ||
                (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir &&
                 Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_Simple)) {
            } else {
                // Non-HeatPump (non-DX) heating coils do not set PLR, reset to 0 here. This variable was set for non-DX
                // coils to allow the SetAverageAirFlow CALL above to set the correct air mass flow rate. See this
                // IF block above in heating section. HeatPLR is not set in the ELSE part of the IF (only HeatCoilLoad is set).
                Furnace(FurnaceNum).HeatPartLoadRatio = 0.0;
            }

            //*********HVAC Scheduled OFF*************
            // No heating or cooling or dehumidification
            //!!LKL discrepancy with < 0?
            if (GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) == 0.0 || Node(FurnaceInletNode).MassFlowRate == 0.0) {
                Furnace(FurnaceNum).MdotFurnace = 0.0;
                CoolCoilLoad = 0.0;
                HeatCoilLoad = 0.0;
                ReheatCoilLoad = 0.0;
                OnOffFanPartLoadFraction = 1.0; // System off, so set on/off fan part-load fraction = 1
                Furnace(FurnaceNum).CoolPartLoadRatio = 0.0;
                Furnace(FurnaceNum).HeatPartLoadRatio = 0.0;
                Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
                // set report variables
                Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
                Furnace(FurnaceNum).CoolingCoilLatentDemand = 0.0;
                Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;
            }

        } // End of the FirstHVACIteration control of the mass flow If block

        // Set the fan inlet node flow rates
        Node(FurnaceInletNode).MassFlowRateMaxAvail = Furnace(FurnaceNum).MdotFurnace;
        Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;
    }

    void CalcWaterToAirHeatPump(int const AirLoopNum,          // index to air loop
                                int const FurnaceNum,          // index to Furnace
                                bool const FirstHVACIteration, // TRUE on first HVAC iteration
                                int const CompOp,              // compressor operation flag (1=On, 0=Off)
                                Real64 const ZoneLoad,         // the control zone load (watts)
                                Real64 const MoistureLoad      // the control zone latent load (watts)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   Feb 2004
        //       MODIFIED       R. Raustad (Oct 2006) Revised iteration technique
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages the heat pump simulation

        // METHODOLOGY EMPLOYED:
        // Calculate the part-load ratio required to meet the zone sensible load.

        // Using/Aliasing
        using DataAirLoop::AirToOANodeInfo;
        using DataHeatBalFanSys::MAT;
        using General::SolveRoot;
        using General::TrimSigDigits;
        using HeatingCoils::SimulateHeatingCoilComponents;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIter(600);   // maximum number of iterations
        Real64 const MinPLR(0.0); // minimum part load ratio allowed

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 OnOffAirFlowRatio;          // Ratio of compressor ON air mass flow to AVERAGE air mass flow over time step
        static Real64 TotalZoneLatentLoad; // Total ZONE latent load (not including outside air)
        // to be removed by furnace/unitary system
        static Real64 TotalZoneSensLoad; // Total ZONE heating load (not including outside air)
        // to be removed by furnace/unitary system
        Real64 cpair;                       // Heat capacity of air
        Real64 ZoneSensLoadMet;             // Actual zone sensible load met by heat pump (W)
        Real64 ZoneLatLoadMet;              // Actual zone latent load met by heat pump (W)
        Real64 ZoneSensLoadMetFanONCompON;  // Max Zone sensible load heat pump can meet (W)
        Real64 ZoneLatLoadMetFanONCompON;   // Max Zone latentload heat pump can meet (W)
        Real64 ZoneSensLoadMetFanONCompOFF; // control zone sensible load met using only outside air
        // and fan heat (no coil output) (W)
        Real64 ZoneLatLoadMetFanONCompOFF; // control zone Latent   load met using only outside air
        // and fan heat (no coil output) (W)
        Real64 HPCoilSensDemand;   // Heat pump sensible demand
        Real64 HPCoilSensCapacity; // Heat pump sensible capacity
        int FurnaceInletNode;      // heat pump Inlet node
        int FurnaceOutletNode;     // heat pump Outlet node

        int OASysInletNode;              // node number of return air inlet to OA sys
        int OASysOutletNode;             // node number of mixed air outlet of OA sys
        int OpMode;                      // Mode of Operation (fan cycling = 1 or fan continuous = 2)
        static Real64 CoolPartLoadRatio; // Part load ratio (greater of sensible or latent part load ratio for cooling)
        static Real64 HeatPartLoadRatio; // Part load ratio (greater of sensible or latent part load ratio for cooling)
        static Real64 Dummy(0.0);        // Dummy var. for generic calc. furnace output arg. (n/a for heat pump)
        bool HumControl;                 // Logical flag signaling when dehumidification is required
        Real64 SuppHeatCoilLoad;         // Load passed to supplemental heater (W)
        Real64 CoolErrorToler;           // convergence tolerance used in cooling mode
        Real64 HeatErrorToler;           // convergence tolerance used in heating mode
        int SolFlag;                     // flag returned from iteration routine to denote problems
        Array1D<Real64> Par(9);          // parameters passed to iteration routine

        // Set local variables
        Dummy = 0.0;
        OnOffAirFlowRatio = 1.0;
        FurnaceOutletNode = Furnace(FurnaceNum).FurnaceOutletNodeNum;
        FurnaceInletNode = Furnace(FurnaceNum).FurnaceInletNodeNum;
        if (AirToOANodeInfo(AirLoopNum).OASysExists) {
            OASysOutletNode = AirToOANodeInfo(AirLoopNum).OASysOutletNodeNum;
            OASysInletNode = AirToOANodeInfo(AirLoopNum).OASysInletNodeNum;
        }
        OpMode = Furnace(FurnaceNum).OpMode;
        Furnace(FurnaceNum).MdotFurnace = Furnace(FurnaceNum).DesignMassFlowRate;
        HumControl = false;

        //*********INITIAL CALCULATIONS****************
        // Calculate the Cp Air for all conditions
        cpair = PsyCpAirFnWTdb(Node(FurnaceInletNode).HumRat, Node(FurnaceInletNode).Temp);

        // set the fan part load fraction
        // Note: OnOffFanPartLoadFraction is passed to the
        //       fan module by DataHVACGlobals.  It should be
        //     set =1 for all cases except cycling fan/cycling
        //     coil. For this case it is set to the part load
        //     factor.  In SimOnOffFan, the part load ratio is
        //     divided by the part load factor (OnOffFanPartLoadFraction)
        //     in order to match the run time fraction of the cycling
        //     fan with the run time fraction of the cycling compressor
        if (FirstHVACIteration) OnOffFanPartLoadFraction = 1.0;

        // Calc Zone sensible loads for heating (+) and cooling (-)
        TotalZoneSensLoad = ZoneLoad;

        // Set latent load for heating
        if (HeatingLoad) {
            TotalZoneLatentLoad = 0.0;

            // Set latent load for cooling and no sensible load condition
        } else {
            TotalZoneLatentLoad = MoistureLoad;
        }

        //*********COOLING CALCULATIONS****************
        // IF scheduled on...
        // AND air flow rate is greater than zero...
        // AND the air system has a cooling load and is not set back or in the deadband...
        // OR the system is controlled by a humidistat and there is a latent load
        if ((GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0 && Node(FurnaceInletNode).MassFlowRate > 0.0) &&
            ((CoolingLoad) || (Furnace(FurnaceNum).Humidistat && Furnace(FurnaceNum).CoolingCoilLatentDemand < 0.0))) {

            // Set the air flow rate to the design flow rate and set the fan operation fraction to 1 (continuous operation)
            Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).DesignMassFlowRate;
            OnOffFanPartLoadFraction = 1.0; // see 'Note' under INITIAL CALCULATIONS

            //         !Set the operation flag to run the fan continuously
            //         OpMode = ContFanCycCoil

            // Set the input parameters for CalcFurnaceOutput
            Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;
            Furnace(FurnaceNum).CoolingCoilLatentDemand = 0.0;
            Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
            Furnace(FurnaceNum).CompPartLoadRatio = 0.0; // compressor off
            Furnace(FurnaceNum).InitHeatPump = true;     // initialization call to Calc Furnace
            Furnace(FurnaceNum).WSHPRuntimeFrac = 0.0;
            CoolPartLoadRatio = 0.0;

            // Get no load result in order to calculate the effect of the fan and the mixed air equipment
            CalcFurnaceOutput(FurnaceNum,
                              FirstHVACIteration,
                              OpMode,
                              CompOp,
                              CoolPartLoadRatio,
                              HeatPartLoadRatio,
                              Dummy,
                              Dummy,
                              ZoneSensLoadMetFanONCompOFF,
                              ZoneLatLoadMetFanONCompOFF,
                              OnOffAirFlowRatio,
                              false);

            // Set the input parameters for CalcFurnaceOutput
            Furnace(FurnaceNum).CoolingCoilSensDemand = 1.0;
            Furnace(FurnaceNum).CompPartLoadRatio = 1.0; // compressor ON
            Furnace(FurnaceNum).WSHPRuntimeFrac = 1.0;
            CoolPartLoadRatio = 1.0;

            // Get full load result in order to estimate the operating part load ratio for continuous fan operation
            CalcFurnaceOutput(FurnaceNum,
                              FirstHVACIteration,
                              OpMode,
                              CompOp,
                              CoolPartLoadRatio,
                              HeatPartLoadRatio,
                              Dummy,
                              Dummy,
                              ZoneSensLoadMetFanONCompON,
                              ZoneLatLoadMetFanONCompON,
                              OnOffAirFlowRatio,
                              false);

            // Calculate the heating coil demand for continuous fan operation as:
            //    (the zone sensible load - the zone sensible load met by fan heat and mixed air)
            // Note; The sensible zone load met by fan heat and mixed air is calculated as:
            //     mdotsys(control zone inlet enthalpy - control zone outlet enthalpy)
            // This accounts for the negative sign in the equation.
            HPCoilSensDemand = TotalZoneSensLoad - ZoneSensLoadMetFanONCompOFF;

            // Calculate the heating coil capacity for continuous fan operation as:
            //    (the zone sensible load met by fan heat and mixed air and coil
            //   - the zone sensible load met by fan heat and mixed air)
            HPCoilSensCapacity = ZoneSensLoadMetFanONCompON - ZoneSensLoadMetFanONCompOFF;

            // Calculate the part load ratio for continuous fan operation with cycling coil
            if (HPCoilSensCapacity == 0.0) {
                CoolPartLoadRatio = 0.0;
            } else {
                CoolPartLoadRatio = max(MinPLR, min(1.0, std::abs(HPCoilSensDemand) / std::abs(HPCoilSensCapacity)));
            }

            Furnace(FurnaceNum).InitHeatPump = false;

            //       check bounds on sensible output prior to iteration using RegulaFalsi
            if (ZoneSensLoadMetFanONCompON > TotalZoneSensLoad) {
                CoolPartLoadRatio = 1.0;
                HPCoilSensDemand = std::abs(ZoneSensLoadMetFanONCompON - ZoneSensLoadMetFanONCompOFF);
                Furnace(FurnaceNum).CoolingCoilSensDemand = HPCoilSensDemand;
            } else if (ZoneSensLoadMetFanONCompOFF < TotalZoneSensLoad) {
                CoolPartLoadRatio = 0.0;
                Furnace(FurnaceNum).CompPartLoadRatio = 0.0; // compressor OFF
                Furnace(FurnaceNum).WSHPRuntimeFrac = 0.0;
                Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
                CalcFurnaceOutput(FurnaceNum,
                                  FirstHVACIteration,
                                  OpMode,
                                  CompOp,
                                  CoolPartLoadRatio,
                                  HeatPartLoadRatio,
                                  Dummy,
                                  Dummy,
                                  ZoneSensLoadMetFanONCompOFF,
                                  ZoneLatLoadMetFanONCompOFF,
                                  OnOffAirFlowRatio,
                                  false);
            } else {
                //         Calculate the sensible part load ratio through iteration
                CoolErrorToler = Furnace(FurnaceNum).CoolingConvergenceTolerance;
                SolFlag = 0; // # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
                Par(1) = double(FurnaceNum);
                Par(2) = 0.0; // FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                if (FirstHVACIteration) Par(2) = 1.0;
                Par(3) = double(OpMode);
                Par(4) = double(CompOp);
                Par(5) = TotalZoneSensLoad;
                Par(6) = 1.0;                         // FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
                Par(7) = 1.0;                         // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
                Par(8) = ZoneSensLoadMetFanONCompOFF; // Output with fan ON compressor OFF
                Par(9) = 0.0;                         // HX is off for water-to-air HP
                //         CoolErrorToler is in fraction of load, MaxIter = 600, SolFalg = # of iterations or error as appropriate
                SolveRoot(CoolErrorToler, MaxIter, SolFlag, CoolPartLoadRatio, CalcWaterToAirResidual, 0.0, 1.0, Par);
                if (SolFlag == -1 && !WarmupFlag && !FirstHVACIteration) {
                    OnOffFanPartLoadFraction = OnOffFanPartLoadFractionSave;
                    CalcFurnaceOutput(FurnaceNum,
                                      FirstHVACIteration,
                                      OpMode,
                                      CompOp,
                                      CoolPartLoadRatio,
                                      0.0,
                                      0.0,
                                      0.0,
                                      ZoneSensLoadMet,
                                      ZoneLatLoadMet,
                                      OnOffAirFlowRatio,
                                      false);
                    if (std::abs(ZoneSensLoadMet - TotalZoneSensLoad) / TotalZoneSensLoad > CoolErrorToler) {
                        if (Furnace(FurnaceNum).SensibleMaxIterIndex == 0) {
                            ShowWarningMessage("Cooling coil control failed to converge for " + cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) +
                                               ':' + Furnace(FurnaceNum).Name);
                            ShowContinueError("  Iteration limit exceeded in calculating DX cooling coil sensible part-load ratio.");
                            ShowContinueErrorTimeStamp("Sensible load to be met by DX coil = " + TrimSigDigits(TotalZoneSensLoad, 2) +
                                                       " (watts), sensible output of DX coil = " + TrimSigDigits(ZoneSensLoadMet, 2) +
                                                       " (watts), and the simulation continues.");
                        }
                        ShowRecurringWarningErrorAtEnd(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                                           "\" - Iteration limit exceeded in calculating sensible cooling part-load ratio error "
                                                           "continues. Sensible load statistics:",
                                                       Furnace(FurnaceNum).SensibleMaxIterIndex,
                                                       TotalZoneSensLoad,
                                                       TotalZoneSensLoad);
                    }
                } else if (SolFlag == -2 && !WarmupFlag && !FirstHVACIteration) {
                    CoolPartLoadRatio = max(MinPLR, min(1.0, std::abs(HPCoilSensDemand) / std::abs(HPCoilSensCapacity)));
                    OnOffFanPartLoadFraction = 1.0;
                    CalcFurnaceOutput(FurnaceNum,
                                      FirstHVACIteration,
                                      OpMode,
                                      CompOp,
                                      CoolPartLoadRatio,
                                      0.0,
                                      0.0,
                                      0.0,
                                      ZoneSensLoadMet,
                                      ZoneLatLoadMet,
                                      OnOffAirFlowRatio,
                                      false);
                    if ((ZoneSensLoadMet - TotalZoneSensLoad) / TotalZoneSensLoad > CoolErrorToler) {
                        if (Furnace(FurnaceNum).SensibleRegulaFalsiFailedIndex == 0) {
                            ShowWarningMessage("Cooling coil control failed for " + cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + ':' +
                                               Furnace(FurnaceNum).Name);
                            ShowContinueError("  Cooling sensible part-load ratio determined to be outside the range of 0-1.");
                            ShowContinueError("  An estimated part-load ratio = " + TrimSigDigits(CoolPartLoadRatio, 2) +
                                              " will be used and the simulation continues.");
                            ShowContinueError("  The estimated part-load ratio provides a cooling sensible capacity = " +
                                              TrimSigDigits(ZoneSensLoadMet, 2));
                            ShowContinueErrorTimeStamp("  Cooling sensible load required = " + TrimSigDigits(TotalZoneSensLoad, 2));
                        }
                        ShowRecurringWarningErrorAtEnd(
                            cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                "\" - Cooling sensible part-load ratio out of range error continues. Sensible cooling load statistics:",
                            Furnace(FurnaceNum).SensibleRegulaFalsiFailedIndex,
                            TotalZoneSensLoad,
                            TotalZoneSensLoad);
                    }
                }
            }

            if (OpMode == CycFanCycCoil) {
                Furnace(FurnaceNum).MdotFurnace *= CoolPartLoadRatio;
            }

            //*********HEATING CALCULATIONS****************
            // If Furnace runs with a heating load then set HeatCoilLoad on Heating Coil and the Mass Flow
        } else if ((GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0) && (Node(FurnaceInletNode).MassFlowRate > 0.0) && HeatingLoad) {

            // Set the air flow rate to the design flow rate and set the fan operation fraction to 1 (continuous operation)
            Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).DesignMassFlowRate;
            OnOffFanPartLoadFraction = 1.0; // see 'Note' under INITIAL CALCULATIONS

            //         !Set the operation flag to run the fan continuously
            //         OpMode = ContFanCycCoil

            // Set the input parameters for CalcFurnaceOutput
            Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;
            Furnace(FurnaceNum).CoolingCoilLatentDemand = 0.0;
            Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
            Furnace(FurnaceNum).CompPartLoadRatio = 0.0; // compressor off
            Furnace(FurnaceNum).InitHeatPump = true;     // initialization call to Calc Furnace
            Furnace(FurnaceNum).WSHPRuntimeFrac = 0.0;
            HeatPartLoadRatio = 0.0;

            // Get no load result in order to calculate the effect of the fan and the mixed air equipment
            CalcFurnaceOutput(FurnaceNum,
                              FirstHVACIteration,
                              OpMode,
                              CompOp,
                              CoolPartLoadRatio,
                              HeatPartLoadRatio,
                              Dummy,
                              Dummy,
                              ZoneSensLoadMetFanONCompOFF,
                              ZoneLatLoadMetFanONCompOFF,
                              OnOffAirFlowRatio,
                              false);

            // Set the input parameters for CalcFurnaceOutput
            Furnace(FurnaceNum).HeatingCoilSensDemand = 1.0;
            Furnace(FurnaceNum).CompPartLoadRatio = 1.0; // compressor ON
            Furnace(FurnaceNum).WSHPRuntimeFrac = 1.0;
            HeatPartLoadRatio = 1.0;

            // Get full load result in order to estimate the operating part load ratio for continuous fan operation

            CalcFurnaceOutput(FurnaceNum,
                              FirstHVACIteration,
                              OpMode,
                              CompOp,
                              CoolPartLoadRatio,
                              HeatPartLoadRatio,
                              Dummy,
                              Dummy,
                              ZoneSensLoadMetFanONCompON,
                              ZoneLatLoadMetFanONCompON,
                              OnOffAirFlowRatio,
                              false);

            // Calculate the heating coil demand for continuous fan operation as:
            //    (the zone sensible load - the zone sensible load met by fan heat and mixed air)
            // Note; The sensible zone load met by fan heat and mixed air is calculated as:
            //     mdotsys(control zone inlet enthalpy - control zone outlet enthalpy)
            // This accounts for the negative sign in the equation.
            HPCoilSensDemand = TotalZoneSensLoad - ZoneSensLoadMetFanONCompOFF;

            // Calculate the heating coil capacity for continuous fan operation as:
            //    (the zone sensible load met by fan heat and mixed air and coil
            //   - the zone sensible load met by fan heat and mixed air)
            HPCoilSensCapacity = ZoneSensLoadMetFanONCompON - ZoneSensLoadMetFanONCompOFF;

            // Calculate the part load ratio for continuous fan operation with cycling coil
            if (HPCoilSensCapacity == 0.0) {
                HeatPartLoadRatio = 0.0;
            } else {
                HeatPartLoadRatio = max(MinPLR, min(1.0, std::abs(HPCoilSensDemand) / std::abs(HPCoilSensCapacity)));
            }

            Furnace(FurnaceNum).InitHeatPump = false;

            //       check bounds on sensible output prior to iteration using RegulaFalsi
            if (ZoneSensLoadMetFanONCompON < TotalZoneSensLoad) {
                HeatPartLoadRatio = 1.0;
                ZoneSensLoadMet = ZoneSensLoadMetFanONCompON;
                HPCoilSensDemand = std::abs(ZoneSensLoadMetFanONCompON - ZoneSensLoadMetFanONCompOFF);
                Furnace(FurnaceNum).HeatingCoilSensDemand = HPCoilSensDemand;
            } else if (ZoneSensLoadMetFanONCompOFF > TotalZoneSensLoad) {
                HeatPartLoadRatio = 0.0;
                ZoneSensLoadMet = ZoneSensLoadMetFanONCompOFF;
                Furnace(FurnaceNum).CompPartLoadRatio = 0.0; // compressor ON
                Furnace(FurnaceNum).WSHPRuntimeFrac = 0.0;
                CalcFurnaceOutput(FurnaceNum,
                                  FirstHVACIteration,
                                  OpMode,
                                  CompOp,
                                  CoolPartLoadRatio,
                                  HeatPartLoadRatio,
                                  Dummy,
                                  Dummy,
                                  ZoneSensLoadMet,
                                  ZoneLatLoadMet,
                                  OnOffAirFlowRatio,
                                  false);
            } else {
                //         Calculate the sensible part load ratio through iteration
                HeatErrorToler = Furnace(FurnaceNum).HeatingConvergenceTolerance;
                SolFlag = 0; // # of iterations if positive, -1 means failed to converge, -2 means bounds are incorrect
                Par(1) = double(FurnaceNum);
                Par(2) = 0.0; // FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                if (FirstHVACIteration) Par(2) = 1.0;
                Par(3) = double(OpMode);
                Par(4) = double(CompOp);
                Par(5) = TotalZoneSensLoad;
                Par(6) = 0.0;                         // FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
                Par(7) = 1.0;                         // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
                Par(8) = ZoneSensLoadMetFanONCompOFF; // Output with fan ON compressor OFF
                Par(9) = 0.0;                         // HX is OFF for water-to-air HP
                //         HeatErrorToler is in fraction of load, MaxIter = 600, SolFalg = # of iterations or error as appropriate
                SolveRoot(HeatErrorToler, MaxIter, SolFlag, HeatPartLoadRatio, CalcWaterToAirResidual, 0.0, 1.0, Par);
                OnOffFanPartLoadFraction = OnOffFanPartLoadFractionSave;
                CalcFurnaceOutput(FurnaceNum,
                                  FirstHVACIteration,
                                  OpMode,
                                  CompOp,
                                  CoolPartLoadRatio,
                                  HeatPartLoadRatio,
                                  Dummy,
                                  Dummy,
                                  ZoneSensLoadMet,
                                  ZoneLatLoadMet,
                                  OnOffAirFlowRatio,
                                  false);
                if (SolFlag == -1 && !WarmupFlag && !FirstHVACIteration) {
                    if (std::abs(ZoneSensLoadMet - TotalZoneSensLoad) / TotalZoneSensLoad > HeatErrorToler) {
                        if (Furnace(FurnaceNum).WSHPHeatMaxIterIndex == 0) {
                            ShowWarningMessage("Heating coil control failed to converge for " + cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) +
                                               ':' + Furnace(FurnaceNum).Name);
                            ShowContinueError("  Iteration limit exceeded in calculating DX heating coil sensible part-load ratio.");
                            ShowContinueErrorTimeStamp("Sensible load to be met by DX coil = " + TrimSigDigits(TotalZoneSensLoad, 2) +
                                                       " (watts), sensible output of DX coil = " + TrimSigDigits(ZoneSensLoadMet, 2) +
                                                       " (watts), and the simulation continues.");
                        }
                        ShowRecurringWarningErrorAtEnd(
                            cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                "\" - Iteration limit exceeded in calculating sensible heating part-load ratio error continues.",
                            Furnace(FurnaceNum).WSHPHeatMaxIterIndex,
                            TotalZoneSensLoad,
                            TotalZoneSensLoad);
                    }
                } else if (SolFlag == -2) {
                    HeatPartLoadRatio = max(MinPLR, min(1.0, std::abs(HPCoilSensDemand) / std::abs(HPCoilSensCapacity)));
                    CalcFurnaceOutput(FurnaceNum,
                                      FirstHVACIteration,
                                      OpMode,
                                      CompOp,
                                      0.0,
                                      HeatPartLoadRatio,
                                      0.0,
                                      0.0,
                                      ZoneSensLoadMet,
                                      ZoneLatLoadMet,
                                      OnOffAirFlowRatio,
                                      false);
                    if ((ZoneSensLoadMet - TotalZoneSensLoad) / TotalZoneSensLoad > HeatErrorToler) {
                        if (Furnace(FurnaceNum).WSHPHeatRegulaFalsiFailedIndex == 0) {
                            ShowWarningError("Heating coil control failed for " + cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + ':' +
                                             Furnace(FurnaceNum).Name);
                            ShowContinueError("  Heating sensible part-load ratio determined to be outside the range of 0-1.");
                            ShowContinueError("  An estimated part-load ratio = " + TrimSigDigits(HeatPartLoadRatio, 2) +
                                              " will be used and the simulation continues.");
                            ShowContinueError("  The estimated part-load ratio provides a heating sensible capacity = " +
                                              TrimSigDigits(ZoneSensLoadMet, 2));
                            ShowContinueErrorTimeStamp("  Heating sensible load required = " + TrimSigDigits(TotalZoneSensLoad, 2));
                        }
                        ShowRecurringWarningErrorAtEnd(cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + " \"" + Furnace(FurnaceNum).Name +
                                                           "\" - Heating sensible part-load ratio out of range error continues.",
                                                       Furnace(FurnaceNum).WSHPHeatRegulaFalsiFailedIndex,
                                                       TotalZoneSensLoad,
                                                       TotalZoneSensLoad);
                    }
                }
            }

            //       CALL supplemental heater if required
            if ((TotalZoneSensLoad - ZoneSensLoadMet) > SmallLoad && HeatPartLoadRatio >= 1.0) {
                SuppHeatCoilLoad = TotalZoneSensLoad - ZoneSensLoadMet;
                CalcFurnaceOutput(FurnaceNum,
                                  FirstHVACIteration,
                                  OpMode,
                                  CompOp,
                                  CoolPartLoadRatio,
                                  HeatPartLoadRatio,
                                  SuppHeatCoilLoad,
                                  Dummy,
                                  ZoneSensLoadMet,
                                  ZoneLatLoadMet,
                                  OnOffAirFlowRatio,
                                  false);
            }

            if (OpMode == CycFanCycCoil) {
                Furnace(FurnaceNum).MdotFurnace *= HeatPartLoadRatio;
            }

            //**********HVAC Scheduled ON, but no cooling, dehumidification or heating load*********
        } else if (GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0) {
            Furnace(FurnaceNum).InitHeatPump = true; // initialization call to Calc Furnace
            HeatPartLoadRatio = 0.0;
            CoolPartLoadRatio = 0.0;
            OnOffFanPartLoadFraction = 1.0; //! see 'Note' under INITIAL CALCULATIONS
            // set report variables
            Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
            Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
            Furnace(FurnaceNum).CoolingCoilLatentDemand = 0.0;
            Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;
            if (OpMode == CycFanCycCoil) {
                Furnace(FurnaceNum).MdotFurnace = 0.0;
                OnOffFanPartLoadFraction = 1.0; // see 'Note' under INITIAL CALCULATIONS
                CalcFurnaceOutput(FurnaceNum,
                                  FirstHVACIteration,
                                  OpMode,
                                  CompOp,
                                  CoolPartLoadRatio,
                                  HeatPartLoadRatio,
                                  Dummy,
                                  Dummy,
                                  ZoneSensLoadMet,
                                  ZoneLatLoadMet,
                                  OnOffAirFlowRatio,
                                  false);
                Furnace(FurnaceNum).MdotFurnace = 0.0;
            } else { // continuous fan, cycling coil
                CalcFurnaceOutput(FurnaceNum,
                                  FirstHVACIteration,
                                  OpMode,
                                  CompOp,
                                  CoolPartLoadRatio,
                                  HeatPartLoadRatio,
                                  Dummy,
                                  Dummy,
                                  ZoneSensLoadMet,
                                  ZoneLatLoadMet,
                                  OnOffAirFlowRatio,
                                  false);
            }
            //*********No heating or cooling or dehumidification*********
        } else {
            Furnace(FurnaceNum).InitHeatPump = true; // initialization call to Calc Furnace
            Furnace(FurnaceNum).MdotFurnace = 0.0;
            HeatPartLoadRatio = 0.0;
            CoolPartLoadRatio = 0.0;
            OnOffFanPartLoadFraction = 1.0; // see 'Note' under INITIAL CALCULATIONS
            Furnace(FurnaceNum).CompPartLoadRatio = 0.0;
            Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
            Furnace(FurnaceNum).CoolingCoilLatentDemand = 0.0;
            Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;
            CalcFurnaceOutput(FurnaceNum,
                              FirstHVACIteration,
                              OpMode,
                              CompOp,
                              CoolPartLoadRatio,
                              HeatPartLoadRatio,
                              Dummy,
                              Dummy,
                              ZoneSensLoadMet,
                              ZoneLatLoadMet,
                              OnOffAirFlowRatio,
                              false);
            Furnace(FurnaceNum).MdotFurnace = 0.0;
        }

        // Set the fan inlet node flow rates
        Node(FurnaceInletNode).MassFlowRateMaxAvail = Furnace(FurnaceNum).MdotFurnace;
        Node(FurnaceInletNode).MassFlowRate = Furnace(FurnaceNum).MdotFurnace;
    }

    void CalcFurnaceOutput(int const FurnaceNum,
                           bool const FirstHVACIteration,
                           int const FanOpMode,            // Cycling fan or constant fan
                           int const CompOp,               // Compressor on/off; 1=on, 0=off
                           Real64 const CoolPartLoadRatio, // DX cooling coil part load ratio
                           Real64 const HeatPartLoadRatio, // DX heating coil part load ratio (0 for other heating coil types)
                           Real64 const HeatCoilLoad,      // Heating coil load for gas heater
                           Real64 const ReheatCoilLoad,    // Reheating coil load for gas heater
                           Real64 &SensibleLoadMet,        // Sensible cooling load met (furnace outlet with respect to control zone temp)
                           Real64 &LatentLoadMet,          // Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
                           Real64 &OnOffAirFlowRatio,      // Ratio of compressor ON mass flow rate to AVERAGE
                           bool const HXUnitOn,            // flag to enable HX based on zone moisture load
                           Optional<Real64 const> CoolingHeatingPLRRat // cooling PLR to heating PLR ratio, used for cycling fan RH control
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   Sept 2001
        //       MODIFIED       Dec 2001
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates to sensible and latent loads met by the DX coils
        // specified.  Load met is the outlet node with respect to the control zone's
        // temperature and humidity ratio.

        // METHODOLOGY EMPLOYED:
        // Simulate each child object in the correct order for each system type. This routine is used in the
        // RegulaFalsi function CALL. Air mass flow rate is set each iteration based on PLR.

        // REFERENCES:
        // na

        // Using/Aliasing
        using HeatingCoils::SimulateHeatingCoilComponents;
        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using WaterToAirHeatPump::SimWatertoAirHP;
        using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FurnaceInletNode;     // Furnace inlet node number
        int FurnaceOutletNode;    // Furnace outlet node number
        Real64 AirMassFlow;       // Furnace inlet node temperature
        Real64 WSHPRuntimeFrac;   // Compressor runtime fraction
        Real64 CompPartLoadRatio; // Compressor part load ratio
        Real64 MinHumRatio;       // Minimum humidity ratio for calculating sensible load at a constant humidity ratio
        Real64 MaxTemp;           // Maximum temperature for calculating latent load at a constant temperature
        Real64 Dummy;             // dummy variable
        Real64 Tout;              // Temporary variable used when outlet temp > DesignMaxOutletTemp
        Real64 Wout;              // Temporary variable used when outlet temp > DesignMaxOutletTemp
        int CoolingCoilType_Num;  // Numeric Equivalent for CoolingCoilType
        int HeatingCoilType_Num;  // Numeric Equivalent for HeatingCoilType
        Real64 QActual;           // heating coil load met or delivered
        bool SuppHeatingCoilFlag; // .TRUE. if supplemental heating coil

        FurnaceOutletNode = Furnace(FurnaceNum).FurnaceOutletNodeNum;
        FurnaceInletNode = Furnace(FurnaceNum).FurnaceInletNodeNum;
        CoolingCoilType_Num = Furnace(FurnaceNum).CoolingCoilType_Num;
        HeatingCoilType_Num = Furnace(FurnaceNum).HeatingCoilType_Num;
        WSHPRuntimeFrac = Furnace(FurnaceNum).WSHPRuntimeFrac;
        CompPartLoadRatio = Furnace(FurnaceNum).CompPartLoadRatio;
        ModifiedHeatCoilLoad = 0.0;

        if (present(CoolingHeatingPLRRat)) {
            CoolHeatPLRRat = CoolingHeatingPLRRat;
        } else {
            CoolHeatPLRRat = 1.0;
        }

        // Cooling to Heating PLR Ratio (CoolHeatPLRRat) is used to track the air mass flow rate of both the heating
        // and cooling coils when RH control is used and the heating coil operates longer than the cooling coil.
        // When CoolPartLoadRatio/CoolHeatPLRRat is used, the PLR calculated is acutally the PLR for the heating
        // coil (heating PLR is greater than cooling PLR), it is this PLR that determines the air mass flow rate.
        // When MAX(HeatPartLoadRatio,CoolPartLoadRatio) is used, only one of these values is non-zero.
        if (FanOpMode == CycFanCycCoil) {
            if (CoolHeatPLRRat < 1.0) {
                if (CoolHeatPLRRat > 0.0) {
                    Node(FurnaceInletNode).MassFlowRate = CompOnMassFlow * CoolPartLoadRatio / CoolHeatPLRRat;
                    if (Furnace(FurnaceNum).FurnaceType_Num != UnitarySys_HeatPump_WaterToAir) {
                        SetAverageAirFlow(FurnaceNum, CoolPartLoadRatio / CoolHeatPLRRat, OnOffAirFlowRatio);
                    }
                } else {
                    Node(FurnaceInletNode).MassFlowRate = CompOnMassFlow * CoolPartLoadRatio;
                    if (Furnace(FurnaceNum).FurnaceType_Num != UnitarySys_HeatPump_WaterToAir) {
                        SetAverageAirFlow(FurnaceNum, max(HeatPartLoadRatio, CoolPartLoadRatio), OnOffAirFlowRatio);
                    }
                }
            } else {
                Node(FurnaceInletNode).MassFlowRate = CompOnMassFlow * max(HeatPartLoadRatio, CoolPartLoadRatio);
                if (Furnace(FurnaceNum).FurnaceType_Num != UnitarySys_HeatPump_WaterToAir) {
                    SetAverageAirFlow(FurnaceNum, max(HeatPartLoadRatio, CoolPartLoadRatio), OnOffAirFlowRatio);
                }
            }
        } else {
            if (Furnace(FurnaceNum).FurnaceType_Num != UnitarySys_HeatPump_WaterToAir) {
                SetAverageAirFlow(FurnaceNum, max(HeatPartLoadRatio, CoolPartLoadRatio), OnOffAirFlowRatio);
            }
        }

        AirMassFlow = Node(FurnaceInletNode).MassFlowRate;
        Node(FurnaceInletNode).MassFlowRateMaxAvail = AirMassFlow;

        // Simulate the air-to-air heat pump
        if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir) {
            //   Simulate blow-thru fan and non-linear coils twice to update PLF used by the ONOFF Fan
            if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
                if (CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {
                    SimHXAssistedCoolingCoil(BlankString,
                                             FirstHVACIteration,
                                             CompOp,
                                             CoolPartLoadRatio,
                                             Furnace(FurnaceNum).CoolingCoilIndex,
                                             FanOpMode,
                                             HXUnitOn,
                                             OnOffAirFlowRatio,
                                             EconomizerFlag);
                } else {
                    SimDXCoil(BlankString,
                              CompOp,
                              FirstHVACIteration,
                              Furnace(FurnaceNum).CoolingCoilIndex,
                              FanOpMode,
                              CoolPartLoadRatio,
                              OnOffAirFlowRatio);
                }
                SimDXCoil(
                    BlankString, CompOp, FirstHVACIteration, Furnace(FurnaceNum).HeatingCoilIndex, FanOpMode, HeatPartLoadRatio, OnOffAirFlowRatio);
                SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
            }
            //   Simulate cooling and heating coils
            if (CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {
                SimHXAssistedCoolingCoil(BlankString,
                                         FirstHVACIteration,
                                         CompOp,
                                         CoolPartLoadRatio,
                                         Furnace(FurnaceNum).CoolingCoilIndex,
                                         FanOpMode,
                                         HXUnitOn,
                                         OnOffAirFlowRatio,
                                         EconomizerFlag);
            } else {
                SimDXCoil(
                    BlankString, CompOp, FirstHVACIteration, Furnace(FurnaceNum).CoolingCoilIndex, FanOpMode, CoolPartLoadRatio, OnOffAirFlowRatio);
            }
            SimDXCoil(BlankString, CompOp, FirstHVACIteration, Furnace(FurnaceNum).HeatingCoilIndex, FanOpMode, HeatPartLoadRatio, OnOffAirFlowRatio);
            //   Simulate the draw-thru fan
            if (Furnace(FurnaceNum).FanPlace == DrawThru) {
                SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
            }
            //   Simulate the supplemental heating coil
            if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat && ReheatCoilLoad > 0.0) {
                SuppHeatingCoilFlag = true;
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, ReheatCoilLoad, FanOpMode, QActual);
            } else {
                // equivalent to QCoilReq=0.0d0 or ReHeatCoilLoad = 0.0d0
                SuppHeatingCoilFlag = true;
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, ReheatCoilLoad, FanOpMode, QActual);
            }
            // Simulate the parameter estimate water-to-air heat pump
        } else if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir &&
                   Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_Simple) {
            //    Simulate blow-thru fan and non-linear coils twice to update PLF used by the ONOFF Fan
            if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
                // COIL:WATERTOAIRHPSIMPLE:COOLING
                SimWatertoAirHPSimple(BlankString,
                                      Furnace(FurnaceNum).CoolingCoilIndex,
                                      Furnace(FurnaceNum).CoolingCoilSensDemand,
                                      Furnace(FurnaceNum).CoolingCoilLatentDemand,
                                      FanOpMode,
                                      WSHPRuntimeFrac,
                                      Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                      Furnace(FurnaceNum).HPTimeConstant,
                                      Furnace(FurnaceNum).FanDelayTime,
                                      CompOp,
                                      CoolPartLoadRatio,
                                      FirstHVACIteration); // CoolPartLoadRatio
                Dummy = 0.0;
                // COIL:WATERTOAIRHPSIMPLE:HEATING
                SimWatertoAirHPSimple(BlankString,
                                      Furnace(FurnaceNum).HeatingCoilIndex,
                                      Furnace(FurnaceNum).HeatingCoilSensDemand,
                                      Dummy,
                                      FanOpMode,
                                      WSHPRuntimeFrac,
                                      Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                      Furnace(FurnaceNum).HPTimeConstant,
                                      Furnace(FurnaceNum).FanDelayTime,
                                      CompOp,
                                      HeatPartLoadRatio,
                                      FirstHVACIteration); // HeatPartLoadRatio
                //      Simulate the whole thing a second time so that the correct PLF required by the coils is used by the Fan. *******
                SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
            }
            //    Simulate the cooling and heating coils
            // COIL:WATERTOAIRHPSIMPLE:COOLING
            SimWatertoAirHPSimple(BlankString,
                                  Furnace(FurnaceNum).CoolingCoilIndex,
                                  Furnace(FurnaceNum).CoolingCoilSensDemand,
                                  Furnace(FurnaceNum).CoolingCoilLatentDemand,
                                  FanOpMode,
                                  WSHPRuntimeFrac,
                                  Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                  Furnace(FurnaceNum).HPTimeConstant,
                                  Furnace(FurnaceNum).FanDelayTime,
                                  CompOp,
                                  CoolPartLoadRatio,
                                  FirstHVACIteration); // CoolPartLoadRatio
            Dummy = 0.0;
            // COIL:WATERTOAIRHPSIMPLE:HEATING
            SimWatertoAirHPSimple(BlankString,
                                  Furnace(FurnaceNum).HeatingCoilIndex,
                                  Furnace(FurnaceNum).HeatingCoilSensDemand,
                                  Dummy,
                                  FanOpMode,
                                  WSHPRuntimeFrac,
                                  Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                  Furnace(FurnaceNum).HPTimeConstant,
                                  Furnace(FurnaceNum).FanDelayTime,
                                  CompOp,
                                  HeatPartLoadRatio,
                                  FirstHVACIteration); // HeatPartLoadRatio
            //     Simulate the draw-thru fan
            if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
            }
            //     Simulate the supplemental heating coil
            if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat && ReheatCoilLoad > 0.0) {
                SuppHeatingCoilFlag = true; // if true simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, ReheatCoilLoad, FanOpMode, QActual);
            } else {
                SuppHeatingCoilFlag = true; // if true simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, FanOpMode, QActual);
            }
            // Simulate the detailed water-to-air heat pump
        } else if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir &&
                   Furnace(FurnaceNum).WatertoAirHPType == WatertoAir_ParEst) {
            //    Simulate the draw-thru fan
            if (Furnace(FurnaceNum).FanPlace == BlowThru) {
                SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
            }
            //    Simulate the cooling and heating coils
            SimWatertoAirHP(BlankString,
                            Furnace(FurnaceNum).CoolingCoilIndex,
                            Furnace(FurnaceNum).DesignMassFlowRate,
                            FanOpMode,
                            FirstHVACIteration,
                            WSHPRuntimeFrac,
                            Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                            Furnace(FurnaceNum).HPTimeConstant,
                            Furnace(FurnaceNum).FanDelayTime,
                            Furnace(FurnaceNum).InitHeatPump,
                            Furnace(FurnaceNum).CoolingCoilSensDemand,
                            Furnace(FurnaceNum).CoolingCoilLatentDemand,
                            CompOp,
                            CoolPartLoadRatio);
            Dummy = 0.0;
            SimWatertoAirHP(BlankString,
                            Furnace(FurnaceNum).HeatingCoilIndex,
                            Furnace(FurnaceNum).DesignMassFlowRate,
                            FanOpMode,
                            FirstHVACIteration,
                            WSHPRuntimeFrac,
                            Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                            Furnace(FurnaceNum).HPTimeConstant,
                            Furnace(FurnaceNum).FanDelayTime,
                            Furnace(FurnaceNum).InitHeatPump,
                            Furnace(FurnaceNum).HeatingCoilSensDemand,
                            Dummy,
                            CompOp,
                            HeatPartLoadRatio);
            //    Simulate the draw-thru fan
            if (Furnace(FurnaceNum).FanPlace == DrawThru) {
                SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
            }
            //    Simulate the supplemental heating coil
            SimulateHeatingCoilComponents(BlankString, FirstHVACIteration, HeatCoilLoad, Furnace(FurnaceNum).SuppHeatCoilIndex, _, true, FanOpMode);

        } else { // ELSE it's not a heat pump
            //   Simulate blow-thru fan
            if (Furnace(FurnaceNum).FanPlace == BlowThru) {

                SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);

                //     For non-linear coils, simulate coil to update PLF used by the ONOFF Fan
                if (Furnace(FurnaceNum).FanType_Num == FanType_SimpleOnOff) {
                    if (Furnace(FurnaceNum).FurnaceType_Num != UnitarySys_HeatOnly && Furnace(FurnaceNum).FurnaceType_Num != Furnace_HeatOnly) {

                        if (!Furnace(FurnaceNum).CoolingCoilUpstream) {
                            SuppHeatingCoilFlag = false; // if false simulates heating coil
                            CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, FanOpMode, QActual);
                        }

                        if (CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {
                            SimHXAssistedCoolingCoil(BlankString,
                                                     FirstHVACIteration,
                                                     CompOp,
                                                     CoolPartLoadRatio,
                                                     Furnace(FurnaceNum).CoolingCoilIndex,
                                                     FanOpMode,
                                                     HXUnitOn,
                                                     OnOffAirFlowRatio,
                                                     EconomizerFlag);
                        } else {
                            SimDXCoil(BlankString,
                                      CompOp,
                                      FirstHVACIteration,
                                      Furnace(FurnaceNum).CoolingCoilIndex,
                                      FanOpMode,
                                      CoolPartLoadRatio,
                                      OnOffAirFlowRatio,
                                      CoolHeatPLRRat);
                        }
                    }

                    if (Furnace(FurnaceNum).CoolingCoilUpstream) {
                        SuppHeatingCoilFlag = false; // if false simulates heating coil
                        CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, FanOpMode, QActual);
                    }
                    SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
                } // Simple OnOff fan

            } // Blow thru fan

            //   Simulate the cooling and heating coils
            if (Furnace(FurnaceNum).FurnaceType_Num != UnitarySys_HeatOnly && Furnace(FurnaceNum).FurnaceType_Num != Furnace_HeatOnly) {

                if (!Furnace(FurnaceNum).CoolingCoilUpstream) {
                    SuppHeatingCoilFlag = false; // if false simulates heating coil
                    CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, FanOpMode, QActual);
                }

                if (CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {
                    SimHXAssistedCoolingCoil(BlankString,
                                             FirstHVACIteration,
                                             CompOp,
                                             CoolPartLoadRatio,
                                             Furnace(FurnaceNum).CoolingCoilIndex,
                                             FanOpMode,
                                             HXUnitOn,
                                             OnOffAirFlowRatio,
                                             EconomizerFlag);
                } else {
                    SimDXCoil(BlankString,
                              CompOp,
                              FirstHVACIteration,
                              Furnace(FurnaceNum).CoolingCoilIndex,
                              FanOpMode,
                              CoolPartLoadRatio,
                              OnOffAirFlowRatio,
                              CoolHeatPLRRat);
                }
            }

            if (Furnace(FurnaceNum).CoolingCoilUpstream) {
                SuppHeatingCoilFlag = false; // if false simulates heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, FanOpMode, QActual);
            }
            //   Simulate the draw-thru fan
            if (Furnace(FurnaceNum).FanPlace == DrawThru) {
                SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
            }
            if (Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat || Furnace(FurnaceNum).SuppHeatCoilIndex > 0) {
                SuppHeatingCoilFlag = true; // if truee simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, ReheatCoilLoad, FanOpMode, QActual);
            }
        } // IF(Furnace(FurnaceNum)%FurnaceType_Num == UnitarySys_HeatPump_AirToAir)THEN

        // check the DesignMaxOutletTemp and reset if necessary (for Coil:Gas:Heating or Coil:Electric:Heating only)
        if (Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).Temp > Furnace(FurnaceNum).DesignMaxOutletTemp) {
            Wout = Node(FurnaceOutletNode).HumRat;
            Tout = Furnace(FurnaceNum).DesignMaxOutletTemp;
            ModifiedHeatCoilLoad = HeatCoilLoad - (AirMassFlow * PsyCpAirFnWTdb(Wout, Tout) * (Node(FurnaceOutletNode).Temp - Tout));
            Node(FurnaceOutletNode).Temp = Tout;
        }

        // If the fan runs continually do not allow coils to set OnOffFanPartLoadRatio.
        if (FanOpMode == ContFanCycCoil) OnOffFanPartLoadFraction = 1.0;

        // Check delta T (outlet to space), if positive
        // use space HumRat (next line), else outlet humrat (IF) so psyc routine gives good result
        MinHumRatio = Node(Furnace(FurnaceNum).NodeNumOfControlledZone).HumRat;
        if (Node(FurnaceOutletNode).Temp < Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp) MinHumRatio = Node(FurnaceOutletNode).HumRat;

        // Calculate sensible load met (at constant humidity ratio)
        SensibleLoadMet = AirMassFlow * (PsyHFnTdbW(Node(FurnaceOutletNode).Temp, MinHumRatio) -
                                         PsyHFnTdbW(Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp, MinHumRatio)) -
                          Furnace(FurnaceNum).SenLoadLoss;
        Furnace(FurnaceNum).SensibleLoadMet = SensibleLoadMet;

        if (Furnace(FurnaceNum).Humidistat) {
            MaxTemp = Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp;
            // modified, why does switching between furnace outlet and control zone temp
            // cause latent load to change when latent capacity is 0 ?
            //    IF(Node(FurnaceOutletNode)%Temp .GT. Node(Furnace(FurnaceNum)%NodeNumOfControlledZone)%Temp ) &
            //       MaxTemp = Node(FurnaceOutletNode)%Temp

            //   Calculate latent load met (at constant temperature)
            LatentLoadMet = AirMassFlow * (PsyHFnTdbW(MaxTemp, Node(FurnaceOutletNode).HumRat) -
                                           PsyHFnTdbW(MaxTemp, Node(Furnace(FurnaceNum).NodeNumOfControlledZone).HumRat)) -
                            Furnace(FurnaceNum).LatLoadLoss;
        } else {
            LatentLoadMet = 0.0;
        }
        Furnace(FurnaceNum).LatentLoadMet = LatentLoadMet;
    }

    //        End of Update subroutines for the Furnace Module
    // *****************************************************************************

    Real64 CalcFurnaceResidual(Real64 const PartLoadRatio, // DX cooling coil part load ratio
                               Array1<Real64> const &Par   // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   Feb 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for cooling and heating coils

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to call this Function to converge on a solution

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
        //       Par(1)  = REAL(FurnaceNum,r64) ! Index to furnace
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, if 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(OpMode,r64)     ! Fan control, if 1.0 then cycling fan, if 0.0 then continuous fan
        //       Par(4)  = REAL(CompOp,r64)     ! Compressor control, if 1.0 then compressor ON, if 0.0 then compressor OFF
        //       Par(5)  = CoolCoilLoad         ! Sensible or Latent load to be met by furnace
        //       Par(6)  = 1.0                  ! Type of load FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
        //       Par(7)  = 1.0                  ! Output calculation FLAG, 0.0 for latent capacity, 1.0 for sensible capacity
        //       Par(8)  = OnOffAirFlowRatio    ! Ratio of compressor ON air mass flow to AVERAGE air mass flow over time step
        //       Par(9)  = HXUnitOn             ! flag to enable HX, 1=ON and 2=OFF
        //       Par(10) = HeatingCoilPLR       ! used to calculate latent degradation for cycling fan RH control

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FurnaceNum;                // Index to furnace
        bool FirstHVACIteration;       // FirstHVACIteration flag
        int FanOpMode;                 // Cycling fan or constant fan
        int CompOp;                    // Compressor on/off; 1=on, 0=off
        Real64 CoolPartLoadRatio;      // DX cooling coil part load ratio
        Real64 HeatPartLoadRatio;      // DX heating coil part load ratio (0 for other heating coil types)
        Real64 HeatCoilLoad;           // Heating coil load for gas heater
        Real64 SensibleLoadMet;        // Sensible cooling load met (furnace outlet with respect to control zone temp)
        Real64 LatentLoadMet;          // Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
        Real64 LoadToBeMet;            // Sensible or Latent load to be met by furnace
        Real64 OnOffAirFlowRatio;      // Ratio of compressor ON air mass flow to AVERAGE air mass flow over time step
        Real64 RuntimeFrac;            // heat pump runtime fraction
        Real64 CoolingHeatingPLRRatio; // ratio of cooling PLR to heating PLR, used for cycling fan RH control
        bool HXUnitOn;                 // flag to enable HX based on zone moisture load
        bool errFlag;                  // flag denoting error in runtime calculation

        // Convert parameters to usable variables
        FurnaceNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        FanOpMode = int(Par(3));
        CompOp = int(Par(4));
        LoadToBeMet = Par(5);

        if (Par(6) == 1.0) {
            CoolPartLoadRatio = PartLoadRatio;
            HeatPartLoadRatio = 0.0;
            HeatCoilLoad = 0.0;
        } else {
            CoolPartLoadRatio = 0.0;
            HeatPartLoadRatio = PartLoadRatio;

            auto const HeatingCoilType_Num(Furnace(FurnaceNum).HeatingCoilType_Num);
            if (HeatingCoilType_Num == Coil_HeatingGasOrOtherFuel || HeatingCoilType_Num == Coil_HeatingElectric ||
                HeatingCoilType_Num == Coil_HeatingWater || HeatingCoilType_Num == Coil_HeatingSteam) {
                HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity * PartLoadRatio;
            } else {
                HeatCoilLoad = 0.0;
            }
        }

        //  OnOffAirFlowRatio = Par(8)
        if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_WaterToAir) {
            HeatPumpRunFrac(FurnaceNum, PartLoadRatio, errFlag, RuntimeFrac);
            Furnace(FurnaceNum).CompPartLoadRatio = PartLoadRatio;
            Furnace(FurnaceNum).WSHPRuntimeFrac = RuntimeFrac;
        }

        if (Par(9) == 1.0) {
            HXUnitOn = true;
        } else {
            HXUnitOn = false;
        }

        if (Par(10) > 0.0) {
            //    Par(10) = Furnace(FurnaceNum)%HeatPartLoadRatio
            //    FanOpMode = CycFan and Furnace(FurnaceNum)%HeatPartLoadRatio must be > 0 for Part(10) to be greater than 0
            //    This variable used when in heating mode and dehumidification (cooling) is required.
            CoolingHeatingPLRRatio = min(1.0, CoolPartLoadRatio / Furnace(FurnaceNum).HeatPartLoadRatio);
        } else {
            CoolingHeatingPLRRatio = 1.0;
        }

        // Subroutine arguments
        // CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,FanOpMode,CompOp,CoolPartLoadRatio,HeatPartLoadRatio, &
        //                    HeatCoilLoad, ReHeatCoilLoad, SensibleLoadMet, LatentLoadMet, OnOffAirFlowRatio, HXUnitOn)
        CalcFurnaceOutput(FurnaceNum,
                          FirstHVACIteration,
                          FanOpMode,
                          CompOp,
                          CoolPartLoadRatio,
                          HeatPartLoadRatio,
                          HeatCoilLoad,
                          0.0,
                          SensibleLoadMet,
                          LatentLoadMet,
                          OnOffAirFlowRatio,
                          HXUnitOn,
                          CoolingHeatingPLRRatio);

        // Calculate residual based on output calculation flag
        if (Par(7) == 1.0) {
            if (LoadToBeMet == 0.0) {
                Residuum = (SensibleLoadMet - LoadToBeMet) / 100.0;
            } else {
                Residuum = (SensibleLoadMet - LoadToBeMet) / LoadToBeMet;
            }
        } else {
            if (LoadToBeMet == 0.0) {
                Residuum = (LatentLoadMet - LoadToBeMet) / 100.0;
            } else {
                Residuum = (LatentLoadMet - LoadToBeMet) / LoadToBeMet;
            }
        }

        return Residuum;
    }

    Real64 CalcWaterToAirResidual(Real64 const PartLoadRatio, // DX cooling coil part load ratio
                                  Array1<Real64> const &Par   // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   October 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for water to air HP's
        // this is used for parameter estimation WAHPs but not equation fit WAHPs

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to call this Function to converge on a solution

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
        //     Par(1)  = REAL(FurnaceNum,r64) ! Index to furnace
        //     Par(2)  = 0.0                  ! FirstHVACIteration FLAG, if 1.0 then TRUE, if 0.0 then FALSE
        //     Par(3)  = REAL(OpMode,r64)     ! Fan control, if 1.0 then cycling fan, if 0.0 then continuous fan
        //     Par(4)  = REAL(CompOp,r64)     ! Compressor control, if 1.0 then compressor ON, if 0.0 then compressor OFF
        //     Par(5)  = CoolCoilLoad         ! Sensible or Latent load to be met by furnace
        //     Par(6)  = 1.0                  ! Type of load FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
        //     Par(7)  = 1.0                  ! Output calculation FLAG, 0.0 for latent capacity, 1.0 for sensible capacity
        //     Par(8)  = ZoneSensLoadMetFanONCompOFF  ! Output with fan ON compressor OFF

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FurnaceNum;           // Index to furnace
        bool FirstHVACIteration;  // FirstHVACIteration flag
        int FanOpMode;            // Cycling fan or constant fan
        int CompOp;               // Compressor on/off; 1=on, 0=off
        Real64 CoolPartLoadRatio; // DX cooling coil part load ratio
        Real64 HeatPartLoadRatio; // DX heating coil part load ratio (0 for other heating coil types)
        Real64 HeatCoilLoad;      // Heating coil load for gas heater
        Real64 ZoneSensLoadMet;   // Sensible cooling load met (furnace outlet with respect to control zone temp)
        Real64 ZoneLatLoadMet;    // Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
        Real64 LoadToBeMet;       // Sensible or Latent load to be met by furnace
        bool errFlag;
        Real64 RuntimeFrac;
        Real64 Dummy;
        Real64 HPCoilSensDemand;
        Real64 ZoneSensLoadMetFanONCompOFF;
        Real64 OnOffAirFlowRatio;
        bool HXUnitOn; // flag to enable HX based on zone moisture load (not valid for water-to-air HP's

        // Convert parameters to usable variables
        FurnaceNum = int(Par(1));
        if (Par(2) == 1.0) {
            FirstHVACIteration = true;
        } else {
            FirstHVACIteration = false;
        }
        FanOpMode = int(Par(3));
        CompOp = int(Par(4));
        LoadToBeMet = Par(5);

        if (Par(6) == 1.0) {
            CoolPartLoadRatio = PartLoadRatio;
            HeatPartLoadRatio = 0.0;
            HeatCoilLoad = 0.0;
        } else {
            CoolPartLoadRatio = 0.0;
            HeatPartLoadRatio = PartLoadRatio;
        }
        ZoneSensLoadMetFanONCompOFF = Par(8);
        // calculate the run time fraction
        HeatPumpRunFrac(FurnaceNum, PartLoadRatio, errFlag, RuntimeFrac);

        // update the fan part load factor
        // see 'Note' under INITIAL CALCULATIONS
        if (Par(6) == 1.0) {
            if (RuntimeFrac > 0.0) {
                OnOffFanPartLoadFraction = CoolPartLoadRatio / RuntimeFrac;
            } else {
                OnOffFanPartLoadFraction = 1.0;
            }
        } else {
            if (RuntimeFrac > 0.0) {
                OnOffFanPartLoadFraction = PartLoadRatio / RuntimeFrac;
                //   Else IF(RuntimeFrac == 0.0d0)THEN
                //     OnOffFanPartLoadFraction = 0.0
            } else {
                OnOffFanPartLoadFraction = 1.0;
            }
        }
        OnOffFanPartLoadFractionSave = OnOffFanPartLoadFraction;
        // update fan and compressor run times
        Furnace(FurnaceNum).CompPartLoadRatio = PartLoadRatio;
        Furnace(FurnaceNum).WSHPRuntimeFrac = RuntimeFrac;

        // Calculate the heating coil demand as (the zone sensible load - load met by fan heat and mixed air)
        // Note; The load met by fan heat and mixed air is calculated as mdot(zoneinletenthalpy-zoneoutletenthalpy)
        // This accounts for the negative sign in the equation.

        // Calculate the heat coil sensible capacity as the load met by the system with the fan and compressor on less
        // the load met by the system with the compressor off.
        //  HPCoilSensCapacity = ZoneSensLoadMetFanONCompON - ZoneSensLoadMetFanONCompOFF

        // Set input parameters for heat pump coil model
        HPCoilSensDemand = LoadToBeMet - RuntimeFrac * ZoneSensLoadMetFanONCompOFF;
        //  HPCoilSensDemand = LoadToBeMet  - PartLoadRatio*ZoneSensLoadMetFanONCompOFF
        if (Par(6) == 1.0) {
            Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;
            Furnace(FurnaceNum).CoolingCoilSensDemand = std::abs(HPCoilSensDemand);
        } else {
            Furnace(FurnaceNum).HeatingCoilSensDemand = HPCoilSensDemand;
            Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
        }
        Furnace(FurnaceNum).InitHeatPump = false; // initialization call to Calc Furnace

        // Calculate the zone loads met and the new part load ratio and for the specified run time
        Dummy = 0.0;
        OnOffAirFlowRatio = 1.0;
        if (Par(9) == 1.0) {
            HXUnitOn = true;
        } else {
            HXUnitOn = false;
        }

        //  Subroutine arguments
        //  CALL CalcFurnaceOutput(FurnaceNum,FirstHVACIteration,FanOpMode,CompOp,CoolPartLoadRatio,&
        //                         HeatPartLoadRatio, HeatCoilLoad, ReHeatCoilLoad, SensibleLoadMet, LatentLoadMet, HXUnitOn)
        CalcFurnaceOutput(FurnaceNum,
                          FirstHVACIteration,
                          FanOpMode,
                          CompOp,
                          CoolPartLoadRatio,
                          HeatPartLoadRatio,
                          Dummy,
                          Dummy,
                          ZoneSensLoadMet,
                          ZoneLatLoadMet,
                          OnOffAirFlowRatio,
                          HXUnitOn);

        // Calculate residual based on output calculation flag
        if (Par(7) == 1.0) {
            Residuum = (ZoneSensLoadMet - LoadToBeMet) / LoadToBeMet;
        } else {
            Residuum = (ZoneLatLoadMet - LoadToBeMet) / LoadToBeMet;
        }

        return Residuum;
    }

    void SetAverageAirFlow(int const FurnaceNum,       // Unit index
                           Real64 const PartLoadRatio, // unit part load ratio
                           Real64 &OnOffAirFlowRatio   // ratio of compressor ON airflow to AVERAGE airflow over timestep
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   July 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set the average air mass flow rates using the part-load fraction of the HVAC system for this time step
        // Set OnOffAirFlowRatio to be used by DX coils

        // METHODOLOGY EMPLOYED:
        // The air flow rate in cooling, heating, and no cooling or heating can be different.
        // Calculate the air flow rate based on initializations made in InitFurnace.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int InletNode;              // inlet node number for furnace
        Real64 AverageUnitMassFlow; // average supply air mass flow rate over time step

        InletNode = Furnace(FurnaceNum).FurnaceInletNodeNum;

        AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1 - PartLoadRatio) * CompOffMassFlow);
        if (CompOffFlowRatio > 0.0) {
            FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1 - PartLoadRatio) * CompOffFlowRatio);
        } else {
            FanSpeedRatio = CompOnFlowRatio;
        }
        // IF the furnace is scheduled on or nightime cycle overrides fan schedule. Uses same logic as fan.
        if (GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0 &&
            ((GetCurrentScheduleValue(Furnace(FurnaceNum).FanAvailSchedPtr) > 0.0 || TurnFansOn) && !TurnFansOff)) {
            Node(InletNode).MassFlowRate = AverageUnitMassFlow;
            Node(InletNode).MassFlowRateMaxAvail = AverageUnitMassFlow;
            if (AverageUnitMassFlow > 0.0) {
                OnOffAirFlowRatio = CompOnMassFlow / AverageUnitMassFlow;
            } else {
                OnOffAirFlowRatio = 0.0;
            }
        } else {
            Node(InletNode).MassFlowRate = 0.0;
            OnOffAirFlowRatio = 1.0;
        }

        Furnace(FurnaceNum).MdotFurnace = CompOnMassFlow;
        OnOffAirFlowRatioSave = OnOffAirFlowRatio;
    }

    void HeatPumpRunFrac(int const FurnaceNum, // Furnace Index Number
                         Real64 const PLR,     // part load ratio
                         bool &errFlag,        // part load factor out of range flag
                         Real64 &RuntimeFrac   // the required run time fraction to meet part load
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kenneth Tang
        //       DATE WRITTEN   Apr 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the PLF based on the PLR. Parameters required are
        // thermostat cycling rate (Nmax), heat pump time constant (tau), and the fraction
        // of on-cycle power use (pr)

        // METHODOLOGY EMPLOYED:
        // NA

        // REFERENCES:
        // (1) Henderson, H. I., K. Rengarajan.1996. A Model to predict the latent capacity
        // of air conditioners and heat pumps at part-load conditions with constant fan
        // operation. ASHRAE Transactions 102 (1): 266-274

        // (2) Henderson, H.I. Jr., Y.J. Huang and Danny Parker. 1999. Residential Equipment
        // Part Load Curves for Use in DOE-2.  Environmental Energy Technologies Division,
        // Ernest Orlando Lawrence Berkeley National Laboratory.

        // USE STATEMENTS:

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 PartLoadFactor; // Part load factor
        Real64 Nmax;           // Maximum cycling rate [cycles/hr]
        Real64 tau;            // Heat pump time constant [s]
        Real64 pr;             // On-cycle power use fraction [~]
        Real64 error;          // Calculation error
        Real64 PLF1;           // ith term of part load factor
        Real64 PLF2;           // (i+1)th term of part load factor
        Real64 A;              // Variable for simplify equation
        int NumIteration;      // Iteration Counter

        Nmax = Furnace(FurnaceNum).MaxONOFFCyclesperHour;
        tau = Furnace(FurnaceNum).HPTimeConstant;
        pr = Furnace(FurnaceNum).OnCyclePowerFraction;

        // Initialize
        errFlag = false;
        error = 1.0;
        NumIteration = 0;

        // Initial guess for part load fraction
        PLF1 = 1.0;

        // Calculate PLF using successive substitution until convergence is achieved
        while (true) {
            ++NumIteration;

            if (PLR == 1) {
                // Set part load fraction, PLF1=1.0 if PLR=1.0 and exit loop
                PLF1 = 1.0;
                goto LOOPPLF_exit;
            }

            if (NumIteration > 100) {
                // Exit loop if interation exceed 100
                errFlag = true;
                PLF1 = 1.0;
                goto LOOPPLF_exit;
            }

            if (error < 0.00001) {
                // Exit loop if convergence is achieved
                goto LOOPPLF_exit;

            } else {
                // Calculate PLF
                A = 4.0 * tau * (Nmax / 3600.0) * (1 - PLR / PLF1);
                if (A < 1.5e-3) {
                    // A safety check to prevent PLF2 = 1 - A * (1 - Exp(-1 / A))
                    // from "float underflow error". Occurs when PLR is very close to 1.0,
                    // small A value, thus Exp(-1/A) = 0
                    PLF2 = 1 - A;
                } else {
                    PLF2 = 1.0 - A * (1.0 - std::exp(-1.0 / A));
                }
                error = std::abs((PLF2 - PLF1) / PLF1);
                PLF1 = PLF2;
            }
        }
    LOOPPLF_exit:;

        // Adjust PLF for the off cycle power consumption if
        // on-cycle power use is specified by the user
        if (pr > 0.0) {
            PartLoadFactor = PLR / ((PLR / PLF1) + (1 - PLR / PLF1) * pr);
        } else {
            PartLoadFactor = PLF1;
        }

        if (PartLoadFactor <= 0.0) {
            PartLoadFactor = 0.0;
            RuntimeFrac = 0.0;
            errFlag = true;
        } else {
            RuntimeFrac = PLR / PartLoadFactor;
        }

        if (RuntimeFrac > 1.0) {
            RuntimeFrac = 1.0;
        }
    }

    // Beginning of Reporting subroutines for the Furnace Module
    // *****************************************************************************

    void ReportFurnace(int const FurnaceNum, int const AirLoopNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   Feb 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the report variable for the coils.

        // METHODOLOGY EMPLOYED:
        // Update fan part-load ratio based on mass flow rate ratio.
        // Update global variables used by AirflowNetwork module.

        // Using/Aliasing
        using DataAirflowNetwork::AirflowNetworkControlMultiADS;
        using DataAirflowNetwork::AirflowNetworkControlSimpleADS;
        using DataAirflowNetwork::SimulateAirflowNetwork;
        using DataAirLoop::AirLoopAFNInfo;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 ratio;
        Real64 OnOffRatio;

        // Report the Furnace Fan Part Load Ratio
        if (Furnace(FurnaceNum).NumOfSpeedCooling < 1) {
            if (Furnace(FurnaceNum).DesignMassFlowRate > 0.0) {
                Furnace(FurnaceNum).FanPartLoadRatio = Furnace(FurnaceNum).MdotFurnace / Furnace(FurnaceNum).DesignMassFlowRate;
            } else {
                Furnace(FurnaceNum).FanPartLoadRatio = 0.0;
            }
        }

        // Set mass flow rates during on and off cylce using an OnOff fan
        if (SimulateAirflowNetwork == AirflowNetworkControlMultiADS || SimulateAirflowNetwork == AirflowNetworkControlSimpleADS) {
            AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate = CompOnMassFlow;
            AirLoopAFNInfo(AirLoopNum).LoopSystemOffMassFlowrate = CompOffMassFlow;
            AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode = Furnace(FurnaceNum).OpMode;
            AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio = Furnace(FurnaceNum).FanPartLoadRatio;
            OnOffRatio = AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio;
            if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatPump_AirToAir) {
                AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio =
                    max(Furnace(FurnaceNum).FanPartLoadRatio, Furnace(FurnaceNum).HeatPartLoadRatio, Furnace(FurnaceNum).CoolPartLoadRatio);
                AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio = min(1.0, AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio);
            }
            if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool) {
                if (Furnace(FurnaceNum).HeatPartLoadRatio == 0.0 && Furnace(FurnaceNum).CoolPartLoadRatio == 0.0 &&
                    Furnace(FurnaceNum).FanPartLoadRatio > 0.0) {
                    if (CompOnMassFlow < max(Furnace(FurnaceNum).MaxCoolAirMassFlow, Furnace(FurnaceNum).MaxHeatAirMassFlow) &&
                        CompOnMassFlow > 0.0) {
                        ratio = max(Furnace(FurnaceNum).MaxCoolAirMassFlow, Furnace(FurnaceNum).MaxHeatAirMassFlow) / CompOnMassFlow;
                        AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio = AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio * ratio;
                     }
                }
            }
        }
        DataHVACGlobals::OnOffFanPartLoadFraction =
            1.0; // reset to 1 in case blow through fan configuration (fan resets to 1, but for blow thru fans coil sets back down < 1)
    }

    void CalcNonDXHeatingCoils(int const FurnaceNum,           // Furnace Index
                               bool const SuppHeatingCoilFlag, // .TRUE. if supplemental heating coil
                               bool const FirstHVACIteration,  // flag for first HVAC iteration in the time step
                               Real64 const QCoilLoad,         // load met by unit (watts)
                               int const FanMode,              // fan operation mode
                               Real64 &HeatCoilLoadmet         // Heating Load Met
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

        // Using/Aliasing
        using DataHVACGlobals::SmallLoad;
        using General::RoundSigDigits;
        using General::SolveRoot;
        using HeatingCoils::SimulateHeatingCoilComponents;
        using PlantUtilities::SetComponentFlowRate;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const ErrTolerance(0.001); // convergence limit for hotwater coil
        int const SolveMaxIter(50);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 QActual;         // actual heating load
        Real64 mdot;            // heating coil steam or hot water mass flow rate
        Real64 MinWaterFlow;    // coil minimum hot water mass flow rate, kg/s
        Real64 MaxHotWaterFlow; // coil maximum hot water mass flow rate, kg/s
        Real64 HotWaterMdot;    // actual hot water mass flow rate
        Array1D<Real64> Par(4);
        int SolFlag;
        static std::string HeatingCoilName; // name of heating coil
        static std::string HeatingCoilType; // type of heating coil
        static int CoilTypeNum(0);          // heating coil type number
        static int HeatingCoilIndex(0);     // heating coil index
        static int CoilControlNode(0);      // control node for hot water and steam heating coils
        static int CoilOutletNode(0);       // air outlet node of the heatiing coils
        static int LoopNum(0);              // plant loop number
        static int LoopSideNum(0);          // plant loop side number
        static int BranchNum(0);            // plant branch number
        static int CompNum(0);              // Numeric Equivalent for Supplemental Heat Coil Type

        QActual = 0.0;

        if (SuppHeatingCoilFlag) {
            HeatingCoilName = Furnace(FurnaceNum).SuppHeatCoilName;
            HeatingCoilIndex = Furnace(FurnaceNum).SuppHeatCoilIndex;
            CoilControlNode = Furnace(FurnaceNum).SuppCoilControlNode;
            CoilOutletNode = Furnace(FurnaceNum).SuppCoilOutletNode;
            CoilTypeNum = Furnace(FurnaceNum).SuppHeatCoilType_Num;
            LoopNum = Furnace(FurnaceNum).LoopNumSupp;
            LoopSideNum = Furnace(FurnaceNum).LoopSideSupp;
            BranchNum = Furnace(FurnaceNum).BranchNumSupp;
            CompNum = Furnace(FurnaceNum).CompNumSupp;
            MaxHotWaterFlow = Furnace(FurnaceNum).MaxSuppCoilFluidFlow;
        } else {
            HeatingCoilName = Furnace(FurnaceNum).HeatingCoilName;
            HeatingCoilIndex = Furnace(FurnaceNum).HeatingCoilIndex;
            CoilControlNode = Furnace(FurnaceNum).CoilControlNode;
            CoilOutletNode = Furnace(FurnaceNum).CoilOutletNode;
            CoilTypeNum = Furnace(FurnaceNum).HeatingCoilType_Num;
            LoopNum = Furnace(FurnaceNum).LoopNum;
            LoopSideNum = Furnace(FurnaceNum).LoopSide;
            BranchNum = Furnace(FurnaceNum).BranchNum;
            CompNum = Furnace(FurnaceNum).CompNum;
            MaxHotWaterFlow = Furnace(FurnaceNum).MaxHeatCoilFluidFlow;
        }

        {
            auto const SELECT_CASE_var(CoilTypeNum);
            if ((SELECT_CASE_var == Coil_HeatingGasOrOtherFuel) || (SELECT_CASE_var == Coil_HeatingElectric) ||
                (SELECT_CASE_var == Coil_HeatingDesuperheater)) {
                SimulateHeatingCoilComponents(
                    HeatingCoilName, FirstHVACIteration, QCoilLoad, HeatingCoilIndex, QActual, SuppHeatingCoilFlag, FanMode);
            } else if (SELECT_CASE_var == Coil_HeatingWater) {
                if (QCoilLoad > SmallLoad) {
                    SetComponentFlowRate(MaxHotWaterFlow, CoilControlNode, CoilOutletNode, LoopNum, LoopSideNum, BranchNum, CompNum);
                    SimulateWaterCoilComponents(HeatingCoilName, FirstHVACIteration, HeatingCoilIndex, QActual, FanMode);

                    if (QActual > (QCoilLoad + SmallLoad)) {
                        // control water flow to obtain output matching QCoilLoad
                        MinWaterFlow = 0.0;
                        Par(1) = double(FurnaceNum);
                        if (FirstHVACIteration) {
                            Par(2) = 1.0;
                        } else {
                            Par(2) = 0.0;
                        }
                        Par(3) = QCoilLoad;
                        if (SuppHeatingCoilFlag) {
                            Par(4) = 1.0;
                        } else {
                            Par(4) = 0.0;
                        }
                        SolveRoot(ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, MinWaterFlow, MaxHotWaterFlow, Par);
                        if (SolFlag == -1) {
                            if (Furnace(FurnaceNum).HotWaterCoilMaxIterIndex == 0) {
                                ShowWarningMessage("CalcNonDXHeatingCoils: Hot water coil control failed for " +
                                                   cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + "=\"" + Furnace(FurnaceNum).Name + "\"");
                                ShowContinueErrorTimeStamp("");
                                ShowContinueError("  Iteration limit [" + RoundSigDigits(SolveMaxIter) +
                                                  "] exceeded in calculating hot water mass flow rate");
                            }
                            ShowRecurringWarningErrorAtEnd("CalcNonDXHeatingCoils: Hot water coil control failed (iteration limit [" +
                                                               RoundSigDigits(SolveMaxIter) + "]) for " +
                                                               cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + "=\"" + Furnace(FurnaceNum).Name,
                                                           Furnace(FurnaceNum).HotWaterCoilMaxIterIndex);
                        } else if (SolFlag == -2) {
                            if (Furnace(FurnaceNum).HotWaterCoilMaxIterIndex2 == 0) {
                                ShowWarningMessage("CalcNonDXHeatingCoils: Hot water coil control failed (maximum flow limits) for " +
                                                   cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + "=\"" + Furnace(FurnaceNum).Name + "\"");
                                ShowContinueErrorTimeStamp("");
                                ShowContinueError("...Bad hot water maximum flow rate limits");
                                ShowContinueError("...Given minimum water flow rate=" + RoundSigDigits(MinWaterFlow, 3) + " kg/s");
                                ShowContinueError("...Given maximum water flow rate=" + RoundSigDigits(MaxHotWaterFlow, 3) + " kg/s");
                            }
                            ShowRecurringWarningErrorAtEnd("CalcNonDXHeatingCoils: Hot water coil control failed (flow limits) for " +
                                                               cFurnaceTypes(Furnace(FurnaceNum).FurnaceType_Num) + "=\"" + Furnace(FurnaceNum).Name +
                                                               "\"",
                                                           Furnace(FurnaceNum).HotWaterCoilMaxIterIndex2,
                                                           MaxHotWaterFlow,
                                                           MinWaterFlow,
                                                           _,
                                                           "[kg/s]",
                                                           "[kg/s]");
                        }
                    }
                } else {
                    mdot = 0.0;
                    SetComponentFlowRate(mdot, CoilControlNode, CoilOutletNode, LoopNum, LoopSideNum, BranchNum, CompNum);
                }
                // simulate the hot water heating coil
                SimulateWaterCoilComponents(HeatingCoilName, FirstHVACIteration, HeatingCoilIndex, QActual, FanMode);
            } else if (SELECT_CASE_var == Coil_HeatingSteam) {
                if (QCoilLoad > SmallLoad) {
                    SetComponentFlowRate(MaxHotWaterFlow, CoilControlNode, CoilOutletNode, LoopNum, LoopSideNum, BranchNum, CompNum);
                    // simulate the steam heating coil
                    SimulateSteamCoilComponents(HeatingCoilName, FirstHVACIteration, HeatingCoilIndex, QCoilLoad, QActual, FanMode);
                } else {
                    mdot = 0.0;
                    SetComponentFlowRate(mdot, CoilControlNode, CoilOutletNode, LoopNum, LoopSideNum, BranchNum, CompNum);
                    // simulate the steam heating coil
                    SimulateSteamCoilComponents(HeatingCoilName, FirstHVACIteration, HeatingCoilIndex, QCoilLoad, QActual, FanMode);
                }
            }
        }

        HeatCoilLoadmet = QActual;
    }

    Real64 HotWaterCoilResidual(Real64 const HWFlow,      // hot water flow rate in kg/s
                                Array1<Real64> const &Par // Par(5) is the requested coil load
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC/UCF
        //       DATE WRITTEN   January 2011
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (QCoilActual - QCoilRequested) / QCoilRequested
        // the coil actual output depends on the hot water flow rate which is being varied
        // to minimize the residual.

        // METHODOLOGY EMPLOYED:
        // Calls HotWaterCoilResidual, and calculates the residual as defined above.

        // Using/Aliasing
        using PlantUtilities::SetComponentFlowRate;
        using WaterCoils::SimulateWaterCoilComponents;

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int FurnaceNum;
        bool FirstHVACIteration;
        Real64 QCoilActual;    // delivered coild load, W
        Real64 QCoilRequested; // requested coild load, W
        Real64 mdot;
        bool SuppHeatingCoilFlag; // .TRUE. if supplemental heating coil

        FurnaceNum = int(Par(1));
        FirstHVACIteration = (Par(2) > 0.0);
        QCoilRequested = Par(3);
        SuppHeatingCoilFlag = (Par(4) > 0.0);
        QCoilActual = QCoilRequested;
        mdot = HWFlow;
        if (!SuppHeatingCoilFlag) {
            SetComponentFlowRate(mdot,
                                 Furnace(FurnaceNum).CoilControlNode,
                                 Furnace(FurnaceNum).CoilOutletNode,
                                 Furnace(FurnaceNum).LoopNum,
                                 Furnace(FurnaceNum).LoopSide,
                                 Furnace(FurnaceNum).BranchNum,
                                 Furnace(FurnaceNum).CompNum);
            SimulateWaterCoilComponents(Furnace(FurnaceNum).HeatingCoilName,
                                        FirstHVACIteration,
                                        Furnace(FurnaceNum).HeatingCoilIndex,
                                        QCoilActual,
                                        Furnace(FurnaceNum).OpMode);
        } else {
            // supplemental coil
            SetComponentFlowRate(mdot,
                                 Furnace(FurnaceNum).SuppCoilControlNode,
                                 Furnace(FurnaceNum).SuppCoilOutletNode,
                                 Furnace(FurnaceNum).LoopNumSupp,
                                 Furnace(FurnaceNum).LoopSideSupp,
                                 Furnace(FurnaceNum).BranchNumSupp,
                                 Furnace(FurnaceNum).CompNumSupp);
            // simulate the hot water supplemental heating coil
            SimulateWaterCoilComponents(Furnace(FurnaceNum).SuppHeatCoilName,
                                        FirstHVACIteration,
                                        Furnace(FurnaceNum).SuppHeatCoilIndex,
                                        QCoilActual,
                                        Furnace(FurnaceNum).OpMode);
        }
        if (QCoilRequested != 0.0) {
            Residuum = (QCoilActual - QCoilRequested) / QCoilRequested;
        } else { // Autodesk:Return ELSE added to assure return value is set
            Residuum = 0.0;
        }
        return Residuum;
    }

    //        End of Reporting subroutines for the Furnace Module

    //******************************************************************************

    void SimVariableSpeedHP(int const FurnaceNum,          // number of the current engine driven Heat Pump being simulated
                            bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                            int const AirLoopNum,          // index to air loop
                            Real64 const QZnReq,           // required zone load
                            Real64 const QLatReq,          // required latent load
                            Real64 &OnOffAirFlowRatio      // ratio of compressor ON airflow to AVERAGE airflow over timestep
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:CalcMSHeatPump
        //       DATE WRITTEN   March, 2012

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate a multispeed heat pump; adjust its output to match the
        // required system load.

        // METHODOLOGY EMPLOYED:
        // Calls ControlMSHPOutput to obtain the desired unit output

        using namespace DataZoneEnergyDemands;
        using DataAirLoop::AirLoopControlInfo;
        using DataAirLoop::AirToZoneNodeInfo;
        using DataAirSystems::PrimaryAirSystem;
        using DataHVACGlobals::SmallLoad;
        using DataHVACGlobals::SmallMassFlow;
        using DataZoneEquipment::ZoneEquipConfig;
        using IntegratedHeatPump::DecideWorkMode;

        Real64 PartLoadFrac;                 // compressor part load fraction
        Real64 SpeedRatio;                   // compressor speed ratio
        bool UnitOn;                         // TRUE if unit is on
        int OutletNode;                      // MSHP air outlet node
        int InletNode;                       // MSHP air inlet node
        Real64 AirMassFlow;                  // air mass flow rate [kg/s]
        int OpMode;                          // operating mode (fan cycling or continious; DX coil always cycles)
        int ZoneNum;                         // Controlled zone number
        Real64 QTotUnitOut;                  // capacity output
        static int SpeedNum(1);              // Speed number
        static Real64 SupHeaterLoad(0.0);    // supplement heater load
        int CompOp;                          // compressor operation; 1=on, 0=off
        Real64 SaveMassFlowRate;             // saved inlet air mass flow rate [kg/s]
        Real64 QSensUnitOut;                 // sensible capacity output
        Real64 QLatUnitOut;                  // latent capacity output
        static Real64 TotalZoneLatentLoad;   // Total ZONE latent load
        static Real64 TotalZoneSensibleLoad; // Total ZONE sensible load
        Real64 ActualSensibleOutput;         // Actual furnace sensible capacity
        Real64 ReheatCoilLoad;               // reheat coil load due to dehumidification
        static Real64 SystemSensibleLoad;    // Positive value means heating required
        Real64 QToHeatSetPt;                 // Load required to meet heating setpoint temp (>0 is a heating load)
        Real64 NoCompOutput;                 // output when no active compressor [W]
        int TotBranchNum;                    // total exit branch number
        int ZoneSideNodeNum;                 // zone equip supply node
        bool EconoActive;                    // TRUE if Economizer is active

        // to be removed by furnace/unitary system

        // zero DX coils, and supplemental electric heater electricity consumption
        DXElecHeatingPower = 0.0;
        DXElecCoolingPower = 0.0;
        SaveCompressorPLR = 0.0;
        ElecHeatingCoilPower = 0.0;

        SystemSensibleLoad = QZnReq;
        TotalZoneSensibleLoad = QZnReq;
        TotalZoneLatentLoad = QLatReq;

        // initialize local variables
        UnitOn = true;
        OutletNode = Furnace(FurnaceNum).FurnaceOutletNodeNum;
        InletNode = Furnace(FurnaceNum).FurnaceInletNodeNum;
        AirMassFlow = Furnace(FurnaceNum).DesignMassFlowRate;
        OpMode = Furnace(FurnaceNum).OpMode;
        ZoneNum = Furnace(FurnaceNum).ControlZoneNum;
        CompOp = On;

        // Set latent load for heating
        if (HeatingLoad) {
            Furnace(FurnaceNum).HeatCoolMode = HeatingMode;
            // Set latent load for cooling and no sensible load condition
        } else if (CoolingLoad) {
            Furnace(FurnaceNum).HeatCoolMode = CoolingMode;
        } else {
            Furnace(FurnaceNum).HeatCoolMode = 0;
        }

        // set the on/off flags
        if (Furnace(FurnaceNum).OpMode == CycFanCycCoil) {
            // cycling unit only runs if there is a cooling or heating load.
            if (std::abs(QZnReq) < SmallLoad || AirMassFlow < SmallMassFlow || CurDeadBandOrSetback(ZoneNum)) {
                UnitOn = false;
            }
        } else if (Furnace(FurnaceNum).OpMode == ContFanCycCoil) {
            // continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
            if (AirMassFlow < SmallMassFlow) {
                UnitOn = false;
            }
        }

        OnOffFanPartLoadFraction = 1.0;

        if (AirLoopNum != 0) {
            EconoActive = AirLoopControlInfo(AirLoopNum).EconoActive;
        } else {
            EconoActive = false;
        }

        SaveMassFlowRate = Node(InletNode).MassFlowRate;
        // decide current working mode for IHP
        if ((FirstHVACIteration) && (Furnace(FurnaceNum).bIsIHP))
            DecideWorkMode(Furnace(FurnaceNum).CoolingCoilIndex, TotalZoneSensibleLoad, TotalZoneLatentLoad);

        if (!FirstHVACIteration && Furnace(FurnaceNum).OpMode == CycFanCycCoil &&
            (QZnReq < (-1.0 * SmallLoad) || TotalZoneLatentLoad < (-1.0 * SmallLoad)) && EconoActive) {
            // for cycling fan, cooling load, check whether furnace can meet load with compressor off
            CompOp = Off;
            ControlVSHPOutput(FurnaceNum,
                              FirstHVACIteration,
                              CompOp,
                              OpMode,
                              TotalZoneSensibleLoad,
                              TotalZoneLatentLoad,
                              ZoneNum,
                              SpeedNum,
                              SpeedRatio,
                              PartLoadFrac,
                              OnOffAirFlowRatio,
                              SupHeaterLoad);

            TotalZoneSensibleLoad = QZnReq;
            TotalZoneLatentLoad = QLatReq;

            if (SpeedNum == Furnace(FurnaceNum).NumOfSpeedCooling && SpeedRatio == 1.0) {
                // compressor on (reset inlet air mass flow rate to starting value)
                Node(InletNode).MassFlowRate = SaveMassFlowRate;
                CompOp = On;
                ControlVSHPOutput(FurnaceNum,
                                  FirstHVACIteration,
                                  CompOp,
                                  OpMode,
                                  TotalZoneSensibleLoad,
                                  TotalZoneLatentLoad,
                                  ZoneNum,
                                  SpeedNum,
                                  SpeedRatio,
                                  PartLoadFrac,
                                  OnOffAirFlowRatio,
                                  SupHeaterLoad);
            }
        } else {
            // compressor on
            CompOp = On;

            //     if ( QZnReq < -1000.0 .AND. FurnaceNum == 1 ) then
            //       CompOp      = On
            //     end if
            ControlVSHPOutput(FurnaceNum,
                              FirstHVACIteration,
                              CompOp,
                              OpMode,
                              TotalZoneSensibleLoad,
                              TotalZoneLatentLoad,
                              ZoneNum,
                              SpeedNum,
                              SpeedRatio,
                              PartLoadFrac,
                              OnOffAirFlowRatio,
                              SupHeaterLoad);
        }

        if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool) {
            SaveCompressorPLR = PartLoadFrac;
        } else {
            if (SpeedNum > 1) {
                SaveCompressorPLR = 1.0;
            }

            if (PartLoadFrac == 1.0 && SaveCompressorPLR < 1.0) {
                PartLoadFrac = SaveCompressorPLR;
            }
        }

        ReheatCoilLoad = 0.0;
        TotalZoneSensibleLoad = QZnReq;
        TotalZoneLatentLoad = QLatReq;
        //     Calculate the reheat coil output
        if ((GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0 && CoolingLoad) &&
            (Furnace(FurnaceNum).Humidistat && Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat &&
             (QLatReq < 0.0))) { // if a Humidistat is installed and dehumdification control type is CoolReheat
            CalcVarSpeedHeatPump(FurnaceNum,
                                 FirstHVACIteration,
                                 CompOp,
                                 SpeedNum,
                                 SpeedRatio,
                                 PartLoadFrac,
                                 ActualSensibleOutput,
                                 QLatUnitOut,
                                 TotalZoneSensibleLoad,
                                 TotalZoneLatentLoad,
                                 OnOffAirFlowRatio,
                                 ReheatCoilLoad);
            if (Furnace(FurnaceNum).ZoneSequenceHeatingNum > 0) {
                QToHeatSetPt = (ZoneSysEnergyDemand(Furnace(FurnaceNum).ControlZoneNum)
                                    .SequencedOutputRequiredToHeatingSP(Furnace(FurnaceNum).ZoneSequenceHeatingNum) /
                                Furnace(FurnaceNum).ControlZoneMassFlowFrac);
            } else {
                QToHeatSetPt =
                    (ZoneSysEnergyDemand(Furnace(FurnaceNum).ControlZoneNum).OutputRequiredToHeatingSP / Furnace(FurnaceNum).ControlZoneMassFlowFrac);
            }
            //       Cooling mode or floating condition and dehumidification is required
            if (QToHeatSetPt < 0.0) {
                //         Calculate the reheat coil load wrt the heating setpoint temperature. Reheat coil picks up
                //         the entire excess sensible cooling (DX cooling coil and impact of outdoor air).
                ReheatCoilLoad = max(0.0, (QToHeatSetPt - ActualSensibleOutput));
                Furnace(FurnaceNum).DehumidInducedHeatingDemandRate = ReheatCoilLoad;
                //       Heating mode and dehumidification is required
            } else if (QToHeatSetPt >= 0.0) {
                ReheatCoilLoad = max(QToHeatSetPt, QToHeatSetPt - ActualSensibleOutput);
                Furnace(FurnaceNum).DehumidInducedHeatingDemandRate = max(0.0, ActualSensibleOutput * (-1.0));
            } else {
                ReheatCoilLoad = 0.0;
            }

            SupHeaterLoad = 0.0;
            CalcVarSpeedHeatPump(
                FurnaceNum, FirstHVACIteration, CompOp, 1, 0.0, 0.0, NoCompOutput, QLatUnitOut, 0.0, 0.0, OnOffAirFlowRatio, SupHeaterLoad);

            if (NoCompOutput > SystemSensibleLoad && SystemSensibleLoad > 0.0 && ReheatCoilLoad > 0.0) {
                // Reduce reheat coil load if you are controlling high humidity but outside air
                // and/or the supply air fan is providing enough heat to meet the system sensible load.
                // This will bring the zone temp closer to the heating setpoint temp.
                ReheatCoilLoad = max(0.0, ReheatCoilLoad - (NoCompOutput - SystemSensibleLoad));
            }
        } else {
            //       No humidistat installed
            ReheatCoilLoad = 0.0;
        }

        TotalZoneSensibleLoad = QZnReq;
        TotalZoneLatentLoad = QLatReq;
        if (ReheatCoilLoad > 0.0) {
            CalcVarSpeedHeatPump(FurnaceNum,
                                 FirstHVACIteration,
                                 CompOp,
                                 SpeedNum,
                                 SpeedRatio,
                                 PartLoadFrac,
                                 QSensUnitOut,
                                 QLatUnitOut,
                                 TotalZoneSensibleLoad,
                                 TotalZoneLatentLoad,
                                 OnOffAirFlowRatio,
                                 ReheatCoilLoad);
        } else {
            CalcVarSpeedHeatPump(FurnaceNum,
                                 FirstHVACIteration,
                                 CompOp,
                                 SpeedNum,
                                 SpeedRatio,
                                 PartLoadFrac,
                                 QSensUnitOut,
                                 QLatUnitOut,
                                 TotalZoneSensibleLoad,
                                 TotalZoneLatentLoad,
                                 OnOffAirFlowRatio,
                                 SupHeaterLoad);
        }

        // calculate delivered capacity
        AirMassFlow = Node(InletNode).MassFlowRate;

        Furnace(FurnaceNum).MdotFurnace = AirMassFlow;

        QTotUnitOut = AirMassFlow * (Node(OutletNode).Enthalpy - Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Enthalpy);

        Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;
        Node(OutletNode).MassFlowRateMaxAvail = AirMassFlow;

        if (!FirstHVACIteration && AirMassFlow > 0.0 && AirLoopNum > 0) {
            TotBranchNum = PrimaryAirSystem(AirLoopNum).NumOutletBranches;
            if (TotBranchNum == 1) {
                ZoneSideNodeNum = AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(1);
                // THE MASS FLOW PRECISION of the system solver is not enough for some small air flow rate iterations , BY DEBUGGING
                // it may cause mass flow rate occilations between airloop and zoneequip
                // specify the air flow rate directly for one-to-one system, when the iteration deviation is closing the solver precision level
                // 0.02 is 2 * HVACFlowRateToler, in order to accomodate the system solver precision level
                if (std::abs(AirMassFlow - Node(ZoneSideNodeNum).MassFlowRate) < 0.02) Node(ZoneSideNodeNum).MassFlowRateMaxAvail = AirMassFlow;
                Node(ZoneSideNodeNum).MassFlowRate = AirMassFlow;
            }

            // the below might be useful if more divergences occur
            // Node(PrimaryAirSystem(AirLoopNumber)%Branch(1)%NodeNumIn)%MassFlowRateMaxAvail = AirMassFlow
            // Node(PrimaryAirSystem(AirLoopNumber)%Branch(1)%NodeNumIn)%MassFlowRate = AirMassFlow
        }

        // report variables
        Furnace(FurnaceNum).DehumidInducedHeatingDemandRate = ReheatCoilLoad;
        if (QZnReq > SmallLoad) { // HEATING LOAD
            Furnace(FurnaceNum).CoolingCoilSensDemand = 0.0;
            Furnace(FurnaceNum).HeatingCoilSensDemand = QZnReq;
        } else {
            Furnace(FurnaceNum).CoolingCoilSensDemand = std::abs(QZnReq);
            Furnace(FurnaceNum).HeatingCoilSensDemand = 0.0;
        }

        Furnace(FurnaceNum).CompPartLoadRatio = SaveCompressorPLR;
        if (Furnace(FurnaceNum).OpMode == CycFanCycCoil) {
            if (SupHeaterLoad > 0.0) {
                Furnace(FurnaceNum).FanPartLoadRatio = 1.0;
            } else {
                if (SpeedNum < 2) {
                    Furnace(FurnaceNum).FanPartLoadRatio = PartLoadFrac;
                } else {
                    Furnace(FurnaceNum).FanPartLoadRatio = 1.0;
                }
            }
        } else {
            if (UnitOn) {
                Furnace(FurnaceNum).FanPartLoadRatio = 1.0;
            } else {
                if (SpeedNum < 2) {
                    Furnace(FurnaceNum).FanPartLoadRatio = PartLoadFrac;
                } else {
                    Furnace(FurnaceNum).FanPartLoadRatio = 1.0;
                }
            }
        }
    }

    //******************************************************************************

    void ControlVSHPOutput(int const FurnaceNum,          // Unit index of engine driven heat pump
                           bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
                           int const CompOp,              // compressor operation; 1=on, 0=off
                           int const OpMode,              // operating mode: CycFanCycCoil | ContFanCycCoil
                           Real64 &QZnReq,                // cooling or heating output needed by zone [W]
                           Real64 &QLatReq,               // latent cooling output needed by zone [W]
                           int const ZoneNum,             // Index to zone number
                           int &SpeedNum,                 // Speed number
                           Real64 &SpeedRatio,            // unit speed ratio for DX coils
                           Real64 &PartLoadFrac,          // unit part load fraction
                           Real64 &OnOffAirFlowRatio,     // ratio of compressor ON airflow to AVERAGE airflow over timestep
                           Real64 &SupHeaterLoad          // Supplemental heater load [W]
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:ControlMSHPOutput
        //       DATE WRITTEN   March,  2012
        //       MODIFIED       na
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        // Determine the part load fraction at low speed, and speed ratio at high speed for this time step.

        // METHODOLOGY EMPLOYED:
        // Use RegulaFalsi technique to iterate on part-load ratio until convergence is achieved.

        // Using/Aliasing
        using DataGlobals::WarmupFlag;
        using General::RoundSigDigits;
        using General::SolveRoot;
        using General::TrimSigDigits;
        using HeatingCoils::SimulateHeatingCoilComponents;
        using IntegratedHeatPump::GetCurWorkMode;
        using IntegratedHeatPump::GetMaxSpeedNumIHP;
        using IntegratedHeatPump::IHPOperationMode;
        using IntegratedHeatPump::IntegratedHeatPumps;
        using Psychrometrics::PsyCpAirFnWTdb;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500); // maximum number of iterations

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FullOutput;         // unit full output when compressor is operating [W]
        Real64 LowOutput;          // unit full output at low speed [W]
        Real64 TempOutput;         // unit output when iteration limit exceeded [W]
        Real64 NoCompOutput;       // output when no active compressor [W]
        Real64 LatOutput;          // latent capacity output
        Real64 ErrorToler;         // error tolerance
        int SolFla;                // Flag of RegulaFalsi solver
        Array1D<Real64> Par(10);   // Parameters passed to RegulaFalsi
        Real64 CpAir;              // air specific heat
        Real64 QCoilActual;        // coil load actually delivered returned to calling component
        int i;                     // Speed index
        static int ErrCountCyc(0); // Counter used to minimize the occurrence of output warnings
        static int ErrCountVar(0); // Counter used to minimize the occurrence of output warnings
        IHPOperationMode IHPMode(IHPOperationMode::IdleMode);

        // FLOW
        SupHeaterLoad = 0.0;
        PartLoadFrac = 0.0;
        SpeedRatio = 0.0;
        SpeedNum = 1;
        LatOutput = 0.0;
        ErrorToler = 0.001; // Error tolerance for convergence from input deck

        // dehumidification load has the priority
        if ((GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) > 0.0 && CoolingLoad) &&
            (Furnace(FurnaceNum).Humidistat && Furnace(FurnaceNum).DehumidControlType_Num == DehumidControl_CoolReheat && (QLatReq < 0.0))) {
            QZnReq = 0.0;
        } else {
            QLatReq = 0.0;
        }

        if (GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) == 0.0) return;

        // Get result when DX coil is off
        SupHeaterLoad = 0.0;
        CalcVarSpeedHeatPump(FurnaceNum,
                             FirstHVACIteration,
                             CompOp,
                             SpeedNum,
                             SpeedRatio,
                             PartLoadFrac,
                             NoCompOutput,
                             LatOutput,
                             0.0,
                             0.0,
                             OnOffAirFlowRatio,
                             SupHeaterLoad);

        if (Furnace(FurnaceNum).bIsIHP) {
            IHPMode = GetCurWorkMode(Furnace(FurnaceNum).CoolingCoilIndex);
            if ((IHPOperationMode::DWHMode == IHPMode) || (IHPOperationMode::SCWHMatchWHMode == IHPMode)) { // cooling capacity is a resultant
                return;
            }
        }

        // If cooling and NoCompOutput < QZnReq, the coil needs to be off
        // If heating and NoCompOutput > QZnReq, the coil needs to be off
        if ((QZnReq < (-1.0 * SmallLoad) && NoCompOutput < QZnReq) || (QZnReq > SmallLoad && NoCompOutput > QZnReq) ||
            ((std::abs(QZnReq) <= SmallLoad) && (std::abs(QLatReq) <= SmallLoad)) || (QLatReq < (-1.0 * SmallLoad) && LatOutput < QLatReq)) {
            return;
        }

        // Get full load result
        PartLoadFrac = 1.0;
        SpeedRatio = 1.0;
        if (Furnace(FurnaceNum).HeatCoolMode == HeatingMode) {
            if (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool) {
                SpeedNum = Furnace(FurnaceNum).NumOfSpeedCooling;
            } else {
                SpeedNum = Furnace(FurnaceNum).NumOfSpeedHeating;
            }
        } else if (Furnace(FurnaceNum).HeatCoolMode == CoolingMode) {
            SpeedNum = Furnace(FurnaceNum).NumOfSpeedCooling;
        } else {
            SpeedNum = 1;
            PartLoadFrac = 0.0;
        }

        if (Furnace(FurnaceNum).bIsIHP) SpeedNum = GetMaxSpeedNumIHP(Furnace(FurnaceNum).CoolingCoilIndex);

        CalcVarSpeedHeatPump(FurnaceNum,
                             FirstHVACIteration,
                             CompOp,
                             SpeedNum,
                             SpeedRatio,
                             PartLoadFrac,
                             FullOutput,
                             LatOutput,
                             QZnReq,
                             QLatReq,
                             OnOffAirFlowRatio,
                             SupHeaterLoad);

        if (QLatReq < (-1.0 * SmallLoad)) { // dehumidification mode
            //  ! If the QLatReq <= LatOutput the unit needs to run full out
            if (QLatReq <= LatOutput) {
                PartLoadFrac = 1.0;
                SpeedRatio = 1.0;
                Furnace(FurnaceNum).CompPartLoadRatio = PartLoadFrac;
                Furnace(FurnaceNum).CompSpeedRatio = SpeedRatio;
                Furnace(FurnaceNum).CompSpeedNum = SpeedNum;
                return;
            }
            ErrorToler = 0.001; // Error tolerance for convergence from input deck
        } else if (QZnReq < (-1.0 * SmallLoad)) {
            // Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCompOutput
            // Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
            if (FullOutput >= 0.0 || FullOutput >= NoCompOutput) {
                PartLoadFrac = 0.0;
                SpeedRatio = 0.0;
                SpeedNum = 1;
                return;
            }
            //  ! If the QZnReq <= FullOutput the unit needs to run full out
            if (QZnReq <= FullOutput) {
                PartLoadFrac = 1.0;
                SpeedRatio = 1.0;
                Furnace(FurnaceNum).CompPartLoadRatio = PartLoadFrac;
                Furnace(FurnaceNum).CompSpeedRatio = SpeedRatio;
                Furnace(FurnaceNum).CompSpeedNum = SpeedNum;
                return;
            }
            ErrorToler = 0.001; // Error tolerance for convergence from input deck
        } else {
            // Since we are heating, we expect FullOutput to be > 0 and FullOutput > NoCompOutput
            // Check that this is the case; if not set PartLoadFrac = 0.0 (off)
            if (FullOutput <= 0.0 || FullOutput <= NoCompOutput) {
                PartLoadFrac = 0.0;
                SpeedRatio = 0.0;
                SpeedNum = 1;
                // may need supplemental heating so don't return in heating mode
            }
            if (QZnReq >= FullOutput) {
                PartLoadFrac = 1.0;
                SpeedRatio = 1.0;
                // may need supplemental heating so don't return in heating mode
            }
            ErrorToler = 0.001; // Error tolerance for convergence from input deck
        }

        if ((QZnReq > SmallLoad && QZnReq < FullOutput) || (QZnReq < (-1.0 * SmallLoad) && QZnReq > FullOutput) || (QLatReq < (-1.0 * SmallLoad))) {

            Par(1) = FurnaceNum;
            Par(2) = ZoneNum;
            if (FirstHVACIteration) {
                Par(3) = 1.0;
            } else {
                Par(3) = 0.0;
            }
            Par(4) = OpMode;
            Par(5) = QZnReq;
            Par(6) = OnOffAirFlowRatio;
            Par(7) = SupHeaterLoad;
            Par(9) = CompOp;
            Par(10) = 1.0;
            // Check whether the low speed coil can meet the load or not
            CalcVarSpeedHeatPump(
                FurnaceNum, FirstHVACIteration, CompOp, 1, 0.0, 1.0, LowOutput, LatOutput, QZnReq, QLatReq, OnOffAirFlowRatio, SupHeaterLoad);
            if ((QZnReq > SmallLoad && QZnReq <= LowOutput) || (QZnReq < (-1.0 * SmallLoad) && QZnReq >= LowOutput) ||
                (QLatReq < (-1.0 * SmallLoad) && QLatReq > LatOutput)) {
                // Calculate the part load fraction
                SpeedRatio = 0.0;
                SpeedNum = 1;

                if (QLatReq < 0.0) { // calculate latent heat residual
                    Par(10) = 0.0;
                    Par(5) = QLatReq;
                }

                SolveRoot(ErrorToler, MaxIte, SolFla, PartLoadFrac, VSHPCyclingResidual, 0.0, 1.0, Par);
                if (SolFla == -1) {
                    if (!WarmupFlag) {
                        if (ErrCountCyc == 0) {
                            ++ErrCountCyc;
                            ShowWarningError("Iteration limit exceeded calculating VS WSHP unit cycling ratio, for unit=" + Furnace(FurnaceNum).Name);
                            ShowContinueErrorTimeStamp("Cycling ratio returned=" + RoundSigDigits(PartLoadFrac, 2));
                        } else {
                            ++ErrCountCyc;
                            ShowRecurringWarningErrorAtEnd(
                                Furnace(FurnaceNum).Name + "\": Iteration limit warning exceeding calculating DX unit cycling ratio  continues...",
                                Furnace(FurnaceNum).ErrIndexCyc,
                                PartLoadFrac,
                                PartLoadFrac);
                        }
                    }
                } else if (SolFla == -2) {
                    ShowFatalError("VS WSHP unit cycling ratio calculation failed: cycling limits exceeded, for unit=" + Furnace(FurnaceNum).Name);
                }
            } else {
                // Check to see which speed to meet the load
                PartLoadFrac = 1.0;
                SpeedRatio = 1.0;
                if ((QZnReq < (-1.0 * SmallLoad)) || (QLatReq < (-1.0 * SmallLoad))) { // Cooling
                    for (i = 2; i <= Furnace(FurnaceNum).NumOfSpeedCooling; ++i) {
                        CalcVarSpeedHeatPump(FurnaceNum,
                                             FirstHVACIteration,
                                             CompOp,
                                             i,
                                             SpeedRatio,
                                             PartLoadFrac,
                                             TempOutput,
                                             LatOutput,
                                             QZnReq,
                                             QLatReq,
                                             OnOffAirFlowRatio,
                                             SupHeaterLoad);

                        if (QLatReq < (-1.0 * SmallLoad)) {
                            if (QLatReq > LatOutput) {
                                SpeedNum = i;
                                break;
                            }
                        } else if (QZnReq >= TempOutput) {
                            SpeedNum = i;
                            break;
                        }
                    }
                } else {
                    for (i = 2; i <= Furnace(FurnaceNum).NumOfSpeedHeating; ++i) {
                        CalcVarSpeedHeatPump(FurnaceNum,
                                             FirstHVACIteration,
                                             CompOp,
                                             i,
                                             SpeedRatio,
                                             PartLoadFrac,
                                             TempOutput,
                                             LatOutput,
                                             QZnReq,
                                             QLatReq,
                                             OnOffAirFlowRatio,
                                             SupHeaterLoad);
                        if (QZnReq <= TempOutput) {
                            SpeedNum = i;
                            break;
                        }
                    }
                }
                Par(8) = SpeedNum;

                if (QLatReq < (-1.0 * SmallLoad)) { // calculate latent heat residual
                    Par(10) = 0.0;
                    Par(5) = QLatReq;
                }

                SolveRoot(ErrorToler, MaxIte, SolFla, SpeedRatio, VSHPSpeedResidual, 1.0e-10, 1.0, Par);
                if (SolFla == -1) {
                    if (!WarmupFlag) {
                        if (ErrCountVar == 0) {
                            ++ErrCountVar;
                            ShowWarningError("Iteration limit exceeded calculating VS WSHP unit speed ratio, for unit=" + Furnace(FurnaceNum).Name);
                            ShowContinueErrorTimeStamp("Speed ratio returned=[" + RoundSigDigits(SpeedRatio, 2) +
                                                       "], Speed number =" + RoundSigDigits(SpeedNum));
                        } else {
                            ++ErrCountVar;
                            ShowRecurringWarningErrorAtEnd(Furnace(FurnaceNum).Name +
                                                               "\": Iteration limit warning exceeding calculating DX unit speed ratio continues...",
                                                           Furnace(FurnaceNum).ErrIndexVar,
                                                           SpeedRatio,
                                                           SpeedRatio);
                        }
                    }
                } else if (SolFla == -2) {
                    ShowFatalError("VS WSHP unit compressor speed calculation failed: speed limits exceeded, for unit=" + Furnace(FurnaceNum).Name);
                }
            }
        }

        // if the heating coil cannot meet the load, trim with supplemental heater
        // occurs with constant fan mode when compressor is on or off
        // occurs with cycling fan mode when compressor PLR is equal to 1
        if ((QZnReq > SmallLoad && QZnReq > FullOutput) && (Furnace(FurnaceNum).SuppHeatCoilIndex != 0)) {
            PartLoadFrac = 1.0;
            SpeedRatio = 1.0;
            if (Furnace(FurnaceNum).NumOfSpeedHeating > 0)
                SpeedNum = Furnace(FurnaceNum).NumOfSpeedHeating; // maximum heating speed, avoid zero for cooling only mode

            if (OutDryBulbTemp <= Furnace(FurnaceNum).MaxOATSuppHeat) {
                SupHeaterLoad = QZnReq - FullOutput;
            } else {
                SupHeaterLoad = 0.0;
            }
            CalcVarSpeedHeatPump(FurnaceNum,
                                 FirstHVACIteration,
                                 CompOp,
                                 SpeedNum,
                                 SpeedRatio,
                                 PartLoadFrac,
                                 TempOutput,
                                 LatOutput,
                                 QZnReq,
                                 QLatReq,
                                 OnOffAirFlowRatio,
                                 SupHeaterLoad);
        }

        // check the outlet of the supplemental heater to be lower than the maximum supplemental heater supply air temperature
        if (Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).Temp > Furnace(FurnaceNum).DesignMaxOutletTemp && SupHeaterLoad > 0.0) {

            //   If the supply air temperature is to high, turn off the supplemental heater to recalculate the outlet temperature
            CalcNonDXHeatingCoils(FurnaceNum, true, FirstHVACIteration, 0.0, OpMode, QCoilActual);
            //   If the outlet temperature is below the maximum supplemental heater supply air temperature, reduce the load passed to
            //   the supplemental heater, otherwise leave the supplemental heater off. If the supplemental heater is to be turned on,
            //   use the outlet conditions when the supplemental heater was off (CALL above) as the inlet conditions for the calculation
            //   of supplemental heater load to just meet the maximum supply air temperature from the supplemental heater.
            if (Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).Temp < Furnace(FurnaceNum).DesignMaxOutletTemp) {
                CpAir = PsyCpAirFnWTdb(Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).HumRat, Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).Temp);
                SupHeaterLoad = Node(Furnace(FurnaceNum).FurnaceInletNodeNum).MassFlowRate * CpAir *
                                (Furnace(FurnaceNum).DesignMaxOutletTemp - Node(Furnace(FurnaceNum).FurnaceOutletNodeNum).Temp);

            } else {
                SupHeaterLoad = 0.0;
            }
        }

        // prepare module level output
        Furnace(FurnaceNum).CompPartLoadRatio = PartLoadFrac;
        Furnace(FurnaceNum).CompSpeedRatio = SpeedRatio;
        Furnace(FurnaceNum).CompSpeedNum = SpeedNum;
        Furnace(FurnaceNum).CoolingCoilLatentDemand = std::abs(QLatReq);

        if (Furnace(FurnaceNum).OpMode == ContFanCycCoil) {
            Furnace(FurnaceNum).FanPartLoadRatio = 1.0;
        } else {
            Furnace(FurnaceNum).FanPartLoadRatio = PartLoadFrac;
        }
    }

    //******************************************************************************

    void CalcVarSpeedHeatPump(int const FurnaceNum,          // Variable speed heat pump number
                              bool const FirstHVACIteration, // Flag for 1st HVAC iteration
                              int const CompOp,              // Compressor on/off; 1=on, 0=off
                              int const SpeedNum,            // Speed number
                              Real64 const SpeedRatio,       // Compressor speed ratio
                              Real64 const PartLoadFrac,     // Compressor part load fraction
                              Real64 &SensibleLoadMet,       // Sensible cooling load met (furnace outlet with respect to control zone temp)
                              Real64 &LatentLoadMet,         // Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
                              Real64 const QZnReq,           // Zone load (W)
                              Real64 const QLatReq,          // Zone latent load []
                              Real64 &OnOffAirFlowRatio,     // Ratio of compressor ON airflow to AVERAGE airflow over timestep
                              Real64 &SupHeaterLoad          // supplemental heater load (W)
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Bo Shen, based on HVACMultiSpeedHeatPump:CalcMSHeatPump
        //       DATE WRITTEN:    March 2012
        //       MODIFIED         na
        //       RE-ENGINEERED    na

        // PURPOSE OF THIS SUBROUTINE:
        //  This routine will calcultes MSHP performance based on given system load

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES: na

        // Using/Aliasing
        using Fans::SimulateFanComponents;
        using IntegratedHeatPump::SimIHP;
        using VariableSpeedCoils::SimVariableSpeedCoils;
        using VariableSpeedCoils::VarSpeedCoil;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        //  INTEGER, PARAMETER  ::   On  = 1           ! Compressor on flag
        //  INTEGER, PARAMETER  ::   Off = 2           ! Compressor off flag

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVMS TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int OutletNode;           // MSHP air outlet node
        int InletNode;            // MSHP air inlet node
        Real64 MinHumRat;         // Minimum humidity ratio for sensible capacity calculation (kg/kg)
        Real64 AirMassFlow;       // Air mass flow rate [kg/s]
        Real64 SavePartloadRatio; // part-load ratio
        Real64 SaveSpeedRatio;    // speed ratio
        Real64 QCoilActual;       // coil load actually delivered returned to calling component
        Real64 ErrorToler;        // supplemental heating coil convergence tollerance
        bool SuppHeatingCoilFlag; // whether to turn on the supplemental heater
        Real64 MaxTemp;           // Maximum temperature for calculating latent load at a constant temperature
        Real64 HeatCoilLoad;      // REQUIRED HEAT COIL LOAD

        // FLOW
        InletNode = Furnace(FurnaceNum).FurnaceInletNodeNum;
        OutletNode = Furnace(FurnaceNum).FurnaceOutletNodeNum;

        HeatCoilLoad = 0.0;
        SaveCompressorPLR = 0.0;
        SavePartloadRatio = 0.0;
        ErrorToler = 0.001;

        // Set inlet air mass flow rate based on PLR and compressor on/off air flow rates
        SetVSHPAirFlow(FurnaceNum, PartLoadFrac, OnOffAirFlowRatio, SpeedNum, SpeedRatio);

        if ((SupHeaterLoad > 1.0e-10) && (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool) &&
            (Furnace(FurnaceNum).SuppHeatCoilIndex == 0)) {
            // ONLY HEATING COIL, NO SUPPLEMENTAL COIL, USED FOR REHEAT DURING DUHMI
            HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity * PartLoadFrac; // REHEAT IN FAN ON TIME

            if (HeatCoilLoad > SupHeaterLoad) HeatCoilLoad = SupHeaterLoad; // HEATING COIL RUN TIME < FAN ON TIME

        } else if ((QZnReq > SmallLoad) && (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool)) {
            HeatCoilLoad = Furnace(FurnaceNum).DesignHeatingCapacity * PartLoadFrac;
        } else {
            HeatCoilLoad = 0.0;
        }

        AirMassFlow = Node(InletNode).MassFlowRate;
        // if blow through, simulate fan then coils
        if (Furnace(FurnaceNum).FanPlace == BlowThru) {
            SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);

            if ((!Furnace(FurnaceNum).CoolingCoilUpstream) && (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool)) {
                // simulate furnace heating coil
                SuppHeatingCoilFlag = false; // if true simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, Furnace(FurnaceNum).OpMode, QCoilActual);
            }

            if ((QZnReq < (-1.0 * SmallLoad) || (QLatReq < (-1.0 * SmallLoad))) &&
                (OutDryBulbTemp >= Furnace(FurnaceNum).MinOATCompressorCooling)) { // COOLING MODE or dehumidification mode

                if (Furnace(FurnaceNum).bIsIHP) {
                    SimIHP(BlankString,
                           Furnace(FurnaceNum).CoolingCoilIndex,
                           Furnace(FurnaceNum).OpMode,
                           Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                           Furnace(FurnaceNum).HPTimeConstant,
                           Furnace(FurnaceNum).FanDelayTime,
                           CompOp,
                           PartLoadFrac,
                           SpeedNum,
                           SpeedRatio,
                           QZnReq,
                           QLatReq,
                           false,
                           false,
                           OnOffAirFlowRatio);
                } else {
                    SimVariableSpeedCoils(BlankString,
                                          Furnace(FurnaceNum).CoolingCoilIndex,
                                          Furnace(FurnaceNum).OpMode,
                                          Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                          Furnace(FurnaceNum).HPTimeConstant,
                                          Furnace(FurnaceNum).FanDelayTime,
                                          CompOp,
                                          PartLoadFrac,
                                          SpeedNum,
                                          SpeedRatio,
                                          QZnReq,
                                          QLatReq,
                                          OnOffAirFlowRatio);
                }

                SavePartloadRatio = PartLoadFrac;
                SaveSpeedRatio = SpeedRatio;

                SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum).CoolingCoilIndex).PartLoadRatio;
            } else {
                if (Furnace(FurnaceNum).bIsIHP) {
                    SimIHP(BlankString,
                           Furnace(FurnaceNum).CoolingCoilIndex,
                           Furnace(FurnaceNum).OpMode,
                           Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                           Furnace(FurnaceNum).HPTimeConstant,
                           Furnace(FurnaceNum).FanDelayTime,
                           CompOp,
                           PartLoadFrac,
                           SpeedNum,
                           SpeedRatio,
                           QZnReq,
                           QLatReq,
                           false,
                           false,
                           OnOffAirFlowRatio);
                } else {
                    SimVariableSpeedCoils(BlankString,
                                          Furnace(FurnaceNum).CoolingCoilIndex,
                                          Furnace(FurnaceNum).OpMode,
                                          Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                          Furnace(FurnaceNum).HPTimeConstant,
                                          Furnace(FurnaceNum).FanDelayTime,
                                          CompOp,
                                          0.0,
                                          1,
                                          0.0,
                                          0.0,
                                          0.0,
                                          OnOffAirFlowRatio);
                }
            }

            if (Furnace(FurnaceNum).FurnaceType_Num != UnitarySys_HeatCool) {
                if (QZnReq > SmallLoad) {
                    if (Furnace(FurnaceNum).bIsIHP) {
                        SimIHP(BlankString,
                               Furnace(FurnaceNum).HeatingCoilIndex,
                               Furnace(FurnaceNum).OpMode,
                               Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                               Furnace(FurnaceNum).HPTimeConstant,
                               Furnace(FurnaceNum).FanDelayTime,
                               CompOp,
                               PartLoadFrac,
                               SpeedNum,
                               SpeedRatio,
                               QZnReq,
                               QLatReq,
                               false,
                               false,
                               OnOffAirFlowRatio);
                    } else {
                        SimVariableSpeedCoils(BlankString,
                                              Furnace(FurnaceNum).HeatingCoilIndex,
                                              Furnace(FurnaceNum).OpMode,
                                              Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                              Furnace(FurnaceNum).HPTimeConstant,
                                              Furnace(FurnaceNum).FanDelayTime,
                                              CompOp,
                                              PartLoadFrac,
                                              SpeedNum,
                                              SpeedRatio,
                                              QZnReq,
                                              QLatReq,
                                              OnOffAirFlowRatio);
                    }

                    SavePartloadRatio = PartLoadFrac;
                    SaveSpeedRatio = SpeedRatio;

                    SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum).HeatingCoilIndex).PartLoadRatio;
                } else {
                    if (Furnace(FurnaceNum).bIsIHP) {
                        SimIHP(BlankString,
                               Furnace(FurnaceNum).CoolingCoilIndex,
                               Furnace(FurnaceNum).OpMode,
                               Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                               Furnace(FurnaceNum).HPTimeConstant,
                               Furnace(FurnaceNum).FanDelayTime,
                               CompOp,
                               PartLoadFrac,
                               SpeedNum,
                               SpeedRatio,
                               QZnReq,
                               QLatReq,
                               false,
                               false,
                               OnOffAirFlowRatio);
                    } else {
                        SimVariableSpeedCoils(BlankString,
                                              Furnace(FurnaceNum).HeatingCoilIndex,
                                              Furnace(FurnaceNum).OpMode,
                                              Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                              Furnace(FurnaceNum).HPTimeConstant,
                                              Furnace(FurnaceNum).FanDelayTime,
                                              CompOp,
                                              0.0,
                                              1,
                                              0.0,
                                              0.0,
                                              0.0,
                                              OnOffAirFlowRatio);
                    }
                }
            } else if (Furnace(FurnaceNum).CoolingCoilUpstream && (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool)) {
                // simulate furnace heating coil
                SuppHeatingCoilFlag = false; // if true simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, Furnace(FurnaceNum).OpMode, QCoilActual);
            }

            // Call twice to ensure the fan outlet conditions are updated
            SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);

            if ((!Furnace(FurnaceNum).CoolingCoilUpstream) && (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool)) {
                // simulate furnace heating coil
                SuppHeatingCoilFlag = false; // if true simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, Furnace(FurnaceNum).OpMode, QCoilActual);
            }

            if ((QZnReq < (-1.0 * SmallLoad) || (QLatReq < (-1.0 * SmallLoad))) && (OutDryBulbTemp >= Furnace(FurnaceNum).MinOATCompressorCooling)) {

                if (Furnace(FurnaceNum).bIsIHP) {
                    SimIHP(BlankString,
                           Furnace(FurnaceNum).CoolingCoilIndex,
                           Furnace(FurnaceNum).OpMode,
                           Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                           Furnace(FurnaceNum).HPTimeConstant,
                           Furnace(FurnaceNum).FanDelayTime,
                           CompOp,
                           PartLoadFrac,
                           SpeedNum,
                           SpeedRatio,
                           QZnReq,
                           QLatReq,
                           false,
                           false,
                           OnOffAirFlowRatio);
                } else {
                    SimVariableSpeedCoils(BlankString,
                                          Furnace(FurnaceNum).CoolingCoilIndex,
                                          Furnace(FurnaceNum).OpMode,
                                          Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                          Furnace(FurnaceNum).HPTimeConstant,
                                          Furnace(FurnaceNum).FanDelayTime,
                                          CompOp,
                                          PartLoadFrac,
                                          SpeedNum,
                                          SpeedRatio,
                                          QZnReq,
                                          QLatReq,
                                          OnOffAirFlowRatio);
                }

                SavePartloadRatio = PartLoadFrac;
                SaveSpeedRatio = SpeedRatio;
                SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum).CoolingCoilIndex).PartLoadRatio;
            } else {

                if (Furnace(FurnaceNum).bIsIHP) {
                    SimIHP(BlankString,
                           Furnace(FurnaceNum).CoolingCoilIndex,
                           Furnace(FurnaceNum).OpMode,
                           Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                           Furnace(FurnaceNum).HPTimeConstant,
                           Furnace(FurnaceNum).FanDelayTime,
                           CompOp,
                           PartLoadFrac,
                           SpeedNum,
                           SpeedRatio,
                           QZnReq,
                           QLatReq,
                           false,
                           false,
                           OnOffAirFlowRatio);
                } else {
                    SimVariableSpeedCoils(BlankString,
                                          Furnace(FurnaceNum).CoolingCoilIndex,
                                          Furnace(FurnaceNum).OpMode,
                                          Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                          Furnace(FurnaceNum).HPTimeConstant,
                                          Furnace(FurnaceNum).FanDelayTime,
                                          CompOp,
                                          0.0,
                                          1,
                                          0.0,
                                          0.0,
                                          0.0,
                                          OnOffAirFlowRatio);
                }
            }

            if (Furnace(FurnaceNum).FurnaceType_Num != UnitarySys_HeatCool) {
                if (QZnReq > SmallLoad) {
                    if (Furnace(FurnaceNum).bIsIHP) {
                        SimIHP(BlankString,
                               Furnace(FurnaceNum).HeatingCoilIndex,
                               Furnace(FurnaceNum).OpMode,
                               Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                               Furnace(FurnaceNum).HPTimeConstant,
                               Furnace(FurnaceNum).FanDelayTime,
                               CompOp,
                               PartLoadFrac,
                               SpeedNum,
                               SpeedRatio,
                               QZnReq,
                               QLatReq,
                               false,
                               false,
                               OnOffAirFlowRatio);
                    } else {
                        SimVariableSpeedCoils(BlankString,
                                              Furnace(FurnaceNum).HeatingCoilIndex,
                                              Furnace(FurnaceNum).OpMode,
                                              Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                              Furnace(FurnaceNum).HPTimeConstant,
                                              Furnace(FurnaceNum).FanDelayTime,
                                              CompOp,
                                              PartLoadFrac,
                                              SpeedNum,
                                              SpeedRatio,
                                              QZnReq,
                                              QLatReq,
                                              OnOffAirFlowRatio);
                    }

                    SavePartloadRatio = PartLoadFrac;
                    SaveSpeedRatio = SpeedRatio;
                    SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum).HeatingCoilIndex).PartLoadRatio;
                } else {
                    if (Furnace(FurnaceNum).bIsIHP) {
                        SimIHP(BlankString,
                               Furnace(FurnaceNum).CoolingCoilIndex,
                               Furnace(FurnaceNum).OpMode,
                               Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                               Furnace(FurnaceNum).HPTimeConstant,
                               Furnace(FurnaceNum).FanDelayTime,
                               CompOp,
                               PartLoadFrac,
                               SpeedNum,
                               SpeedRatio,
                               QZnReq,
                               QLatReq,
                               false,
                               false,
                               OnOffAirFlowRatio);
                    } else {
                        SimVariableSpeedCoils(BlankString,
                                              Furnace(FurnaceNum).HeatingCoilIndex,
                                              Furnace(FurnaceNum).OpMode,
                                              Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                              Furnace(FurnaceNum).HPTimeConstant,
                                              Furnace(FurnaceNum).FanDelayTime,
                                              CompOp,
                                              0.0,
                                              1,
                                              0.0,
                                              0.0,
                                              0.0,
                                              OnOffAirFlowRatio);
                    }
                }
            } else if (Furnace(FurnaceNum).CoolingCoilUpstream && (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool)) {
                // simulate furnace heating coil
                SuppHeatingCoilFlag = false; // if true simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, Furnace(FurnaceNum).OpMode, QCoilActual);
            }

            //  Simulate supplemental heating coil for blow through fan
            if (Furnace(FurnaceNum).SuppHeatCoilIndex > 0) {
                SuppHeatingCoilFlag = true; // if true simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, SupHeaterLoad, Furnace(FurnaceNum).OpMode, QCoilActual);
            }
        } else { // otherwise simulate DX coils then fan then supplemental heater

            if ((!Furnace(FurnaceNum).CoolingCoilUpstream) && (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool)) {
                // simulate furnace heating coil
                SuppHeatingCoilFlag = false; // if true simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, Furnace(FurnaceNum).OpMode, QCoilActual);
            }

            if ((QZnReq < (-1.0 * SmallLoad) || (QLatReq < (-1.0 * SmallLoad))) && (OutDryBulbTemp >= Furnace(FurnaceNum).MinOATCompressorCooling)) {

                if (Furnace(FurnaceNum).bIsIHP) {
                    SimIHP(BlankString,
                           Furnace(FurnaceNum).CoolingCoilIndex,
                           Furnace(FurnaceNum).OpMode,
                           Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                           Furnace(FurnaceNum).HPTimeConstant,
                           Furnace(FurnaceNum).FanDelayTime,
                           CompOp,
                           PartLoadFrac,
                           SpeedNum,
                           SpeedRatio,
                           QZnReq,
                           QLatReq,
                           false,
                           false,
                           OnOffAirFlowRatio);
                } else {
                    SimVariableSpeedCoils(BlankString,
                                          Furnace(FurnaceNum).CoolingCoilIndex,
                                          Furnace(FurnaceNum).OpMode,
                                          Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                          Furnace(FurnaceNum).HPTimeConstant,
                                          Furnace(FurnaceNum).FanDelayTime,
                                          CompOp,
                                          PartLoadFrac,
                                          SpeedNum,
                                          SpeedRatio,
                                          QZnReq,
                                          QLatReq,
                                          OnOffAirFlowRatio);
                }

                SavePartloadRatio = PartLoadFrac;
                SaveSpeedRatio = SpeedRatio;

                SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum).CoolingCoilIndex).PartLoadRatio;
            } else {
                if (Furnace(FurnaceNum).bIsIHP) {
                    SimIHP(BlankString,
                           Furnace(FurnaceNum).CoolingCoilIndex,
                           Furnace(FurnaceNum).OpMode,
                           Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                           Furnace(FurnaceNum).HPTimeConstant,
                           Furnace(FurnaceNum).FanDelayTime,
                           CompOp,
                           PartLoadFrac,
                           SpeedNum,
                           SpeedRatio,
                           QZnReq,
                           QLatReq,
                           false,
                           false,
                           OnOffAirFlowRatio);
                } else {
                    SimVariableSpeedCoils(BlankString,
                                          Furnace(FurnaceNum).CoolingCoilIndex,
                                          Furnace(FurnaceNum).OpMode,
                                          Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                          Furnace(FurnaceNum).HPTimeConstant,
                                          Furnace(FurnaceNum).FanDelayTime,
                                          CompOp,
                                          0.0,
                                          1,
                                          0.0,
                                          0.0,
                                          0.0,
                                          OnOffAirFlowRatio);
                }
            }

            if (Furnace(FurnaceNum).FurnaceType_Num != UnitarySys_HeatCool) {
                if (QZnReq > SmallLoad && (OutDryBulbTemp >= Furnace(FurnaceNum).MinOATCompressorCooling)) {

                    if (Furnace(FurnaceNum).bIsIHP) {
                        SimIHP(BlankString,
                               Furnace(FurnaceNum).HeatingCoilIndex,
                               Furnace(FurnaceNum).OpMode,
                               Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                               Furnace(FurnaceNum).HPTimeConstant,
                               Furnace(FurnaceNum).FanDelayTime,
                               CompOp,
                               PartLoadFrac,
                               SpeedNum,
                               SpeedRatio,
                               QZnReq,
                               QLatReq,
                               false,
                               false,
                               OnOffAirFlowRatio);
                    } else {
                        SimVariableSpeedCoils(BlankString,
                                              Furnace(FurnaceNum).HeatingCoilIndex,
                                              Furnace(FurnaceNum).OpMode,
                                              Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                              Furnace(FurnaceNum).HPTimeConstant,
                                              Furnace(FurnaceNum).FanDelayTime,
                                              CompOp,
                                              PartLoadFrac,
                                              SpeedNum,
                                              SpeedRatio,
                                              QZnReq,
                                              QLatReq,
                                              OnOffAirFlowRatio);
                    }

                    SavePartloadRatio = PartLoadFrac;
                    SaveSpeedRatio = SpeedRatio;
                    SaveCompressorPLR = VarSpeedCoil(Furnace(FurnaceNum).HeatingCoilIndex).PartLoadRatio;
                } else {
                    if (Furnace(FurnaceNum).bIsIHP) {
                        SimIHP(BlankString,
                               Furnace(FurnaceNum).CoolingCoilIndex,
                               Furnace(FurnaceNum).OpMode,
                               Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                               Furnace(FurnaceNum).HPTimeConstant,
                               Furnace(FurnaceNum).FanDelayTime,
                               CompOp,
                               PartLoadFrac,
                               SpeedNum,
                               SpeedRatio,
                               QZnReq,
                               QLatReq,
                               false,
                               false,
                               OnOffAirFlowRatio);
                    } else {
                        SimVariableSpeedCoils(BlankString,
                                              Furnace(FurnaceNum).HeatingCoilIndex,
                                              Furnace(FurnaceNum).OpMode,
                                              Furnace(FurnaceNum).MaxONOFFCyclesperHour,
                                              Furnace(FurnaceNum).HPTimeConstant,
                                              Furnace(FurnaceNum).FanDelayTime,
                                              CompOp,
                                              0.0,
                                              1,
                                              0.0,
                                              0.0,
                                              0.0,
                                              OnOffAirFlowRatio);
                    }
                }
            } else if (Furnace(FurnaceNum).CoolingCoilUpstream && (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool)) {
                // simulate furnace heating coil
                SuppHeatingCoilFlag = false; // if true simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, HeatCoilLoad, Furnace(FurnaceNum).OpMode, QCoilActual);
            }

            SimulateFanComponents(BlankString, FirstHVACIteration, Furnace(FurnaceNum).FanIndex, FanSpeedRatio);
            //  Simulate supplemental heating coil for draw through fan
            if (Furnace(FurnaceNum).SuppHeatCoilIndex > 0) {
                SuppHeatingCoilFlag = true; // if true simulates supplemental heating coil
                CalcNonDXHeatingCoils(FurnaceNum, SuppHeatingCoilFlag, FirstHVACIteration, SupHeaterLoad, Furnace(FurnaceNum).OpMode, QCoilActual);
            }
        }

        // If the fan runs continually do not allow coils to set OnOffFanPartLoadRatio.
        if (Furnace(FurnaceNum).OpMode == ContFanCycCoil) OnOffFanPartLoadFraction = 1.0;

        // Check delta T (outlet to space), if positive
        // use space HumRat (next line), else outlet humrat (IF) so psyc routine gives good result
        MinHumRat = Node(Furnace(FurnaceNum).NodeNumOfControlledZone).HumRat;
        if (Node(OutletNode).Temp < Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp) MinHumRat = Node(OutletNode).HumRat;

        // Calculate sensible load met (at constant humidity ratio)
        SensibleLoadMet = AirMassFlow * (PsyHFnTdbW(Node(OutletNode).Temp, MinHumRat) -
                                         PsyHFnTdbW(Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp, MinHumRat)) -
                          Furnace(FurnaceNum).SenLoadLoss;
        Furnace(FurnaceNum).SensibleLoadMet = SensibleLoadMet;

        if (Furnace(FurnaceNum).Humidistat) {
            MaxTemp = Node(Furnace(FurnaceNum).NodeNumOfControlledZone).Temp;
            // modified, why does switching between furnace outlet and control zone temp
            // cause latent load to change when latent capacity is 0 ?
            //    IF(Node(FurnaceOutletNode)%Temp .GT. Node(Furnace(FurnaceNum)%NodeNumOfControlledZone)%Temp ) &
            //       MaxTemp = Node(FurnaceOutletNode)%Temp
            //   Calculate latent load met (at constant temperature)
            LatentLoadMet = AirMassFlow * (PsyHFnTdbW(MaxTemp, Node(OutletNode).HumRat) -
                                           PsyHFnTdbW(MaxTemp, Node(Furnace(FurnaceNum).NodeNumOfControlledZone).HumRat)) -
                            Furnace(FurnaceNum).LatLoadLoss;
        } else {
            LatentLoadMet = 0.0;
        }
        Furnace(FurnaceNum).LatentLoadMet = LatentLoadMet;
    }

    //******************************************************************************

    Real64 VSHPCyclingResidual(Real64 const PartLoadFrac, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                               Array1<Real64> const &Par  // par(1) = FurnaceNum
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:MSHPCyclingResidual
        //       DATE WRITTEN   March, 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
        //  MSHP output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        //  Calls CalcMSHeatPump to get ActualOutput at the given part load ratio
        //  and calculates the residual as defined above

        // REFERENCES:

        // USE STATEMENTS:
        // na

        // Return value
        Real64 VSHPCyclingResidual;

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = Zone Num
        // par(3) = FirstHVACIteration
        // par(4) = OpMode
        // par(5) = QZnReq, load to be met
        // par(6) = OnOffAirFlowRatio
        // par(7) = SupHeaterLoad

        // par(9) = CompOp
        // par(10) = 1.0 to meet sensible load

        // FUNCTION PARAMETER DEFINITIONS:
        //  na

        // INTERFACE BLOCK SPECIFICATIONS
        //  na

        // DERIVED TYPE DEFINITIONS
        //  na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int FurnaceNum;           // MSHP index
        int ZoneNum;              // Zone index
        bool FirstHVACIteration;  // FirstHVACIteration flag
        int OpMode;               // Compressor operating mode
        Real64 QZnReq;            // zone sensible load (W)
        Real64 QZnLat;            // zone latent load (W)
        Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
        Real64 ZoneSensLoadMet;   // delivered sensible capacity of MSHP
        Real64 ZoneLatLoadMet;    // delivered latent capacity of MSHP
        Real64 LoadToBeMet;       // sensible or latent load to be met
        Real64 SupHeaterLoad;     // Supplemental heater load
        Real64 ResScale;          // Residual scale
        int CompOp;               // compressor operation; 1=on, 0=off

        FurnaceNum = int(Par(1));
        ZoneNum = int(Par(2));
        // FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
        FirstHVACIteration = (Par(3) == 1.0);
        OpMode = int(Par(4));

        QZnReq = 0.0;
        QZnLat = 0.0;

        LoadToBeMet = Par(5);
        if (Par(10) == 1.0) {
            QZnReq = Par(5);
        } else {
            QZnLat = Par(5);
        }

        OnOffAirFlowRatio = Par(6);
        SupHeaterLoad = Par(7);
        CompOp = int(Par(9));

        CalcVarSpeedHeatPump(FurnaceNum,
                             FirstHVACIteration,
                             CompOp,
                             1,
                             0.0,
                             PartLoadFrac,
                             ZoneSensLoadMet,
                             ZoneLatLoadMet,
                             QZnReq,
                             QZnLat,
                             OnOffAirFlowRatio,
                             SupHeaterLoad);

        ResScale = std::abs(LoadToBeMet);
        if (ResScale < 100.0) {
            ResScale = 100.0;
        } else {
            ResScale = LoadToBeMet;
        }

        // Calculate residual based on output calculation flag
        if (Par(10) == 1.0) {
            VSHPCyclingResidual = (ZoneSensLoadMet - LoadToBeMet) / ResScale;
        } else {
            VSHPCyclingResidual = (ZoneLatLoadMet - LoadToBeMet) / ResScale;
        }

        return VSHPCyclingResidual;
    }

    //******************************************************************************

    Real64 VSHPSpeedResidual(Real64 const SpeedRatio,  // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                             Array1<Real64> const &Par // par(1) = MSHPNum
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Bo Shen, , based on HVACMultiSpeedHeatPump:MSHPVarSpeedgResidual
        //       DATE WRITTEN   March, 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function ((ActualOutput - QZnReq)/QZnReq)
        //  MSHP output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        //  Calls CalcMSHeatPump to get ActualOutput at the given speed ratio (partload ratio for high speed)
        //  and calculates the residual as defined above

        // REFERENCES:

        // USE STATEMENTS:
        // na

        // Return value
        Real64 VSHPSpeedResidual;

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = Zone Num
        // par(3) = FirstHVACIteration
        // par(4) = OpMode
        // par(5) = QZnReq
        // par(6) = OnOffAirFlowRatio
        // par(7) = SupHeaterLoad
        // par(8) = SpeedNum
        // par(9) = CompOp
        // par(10) = 1.0 to meet sensible load

        // FUNCTION PARAMETER DEFINITIONS:
        //  na

        // INTERFACE BLOCK SPECIFICATIONS
        //  na

        // DERIVED TYPE DEFINITIONS
        //  na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int FurnaceNum;           // MSHP index
        int ZoneNum;              // Zone index
        bool FirstHVACIteration;  // FirstHVACIteration flag
        int OpMode;               // Compressor operating mode
        Real64 QZnReq;            // zone load (W)
        Real64 QZnLat;            // zone latent load (W)
        Real64 OnOffAirFlowRatio; // ratio of compressor ON airflow to average airflow over timestep
        Real64 ZoneSensLoadMet;   // delivered sensible capacity of MSHP
        Real64 ZoneLatLoadMet;    // delivered latent capacity of MSHP
        Real64 LoadToBeMet;       // sensible or latent load to be met
        Real64 SupHeaterLoad;     // Supplemental heater load
        Real64 ResScale;          // Residual scale
        int SpeedNum;             // Speed number
        int CompOp;               // compressor operation; 1=on, 0=off

        FurnaceNum = int(Par(1));
        ZoneNum = int(Par(2));
        // FirstHVACIteration is a logical, Par is REAL(r64), so make 1.0=TRUE and 0.0=FALSE
        FirstHVACIteration = (Par(3) == 1.0);
        OpMode = int(Par(4));

        QZnReq = 0.0;
        QZnLat = 0.0;

        LoadToBeMet = Par(5);
        if (Par(10) == 1.0) {
            QZnReq = Par(5);
        } else {
            QZnLat = Par(5);
        }

        OnOffAirFlowRatio = Par(6);
        SupHeaterLoad = Par(7);
        SpeedNum = int(Par(8));
        CompOp = int(Par(9));

        CalcVarSpeedHeatPump(FurnaceNum,
                             FirstHVACIteration,
                             CompOp,
                             SpeedNum,
                             SpeedRatio,
                             1.0,
                             ZoneSensLoadMet,
                             ZoneLatLoadMet,
                             QZnReq,
                             QZnLat,
                             OnOffAirFlowRatio,
                             SupHeaterLoad);

        ResScale = std::abs(LoadToBeMet);
        if (ResScale < 100.0) {
            ResScale = 100.0;
        } else {
            ResScale = LoadToBeMet;
        }

        // Calculate residual based on output calculation flag
        if (Par(10) == 1.0) {
            VSHPSpeedResidual = (ZoneSensLoadMet - LoadToBeMet) / ResScale;
        } else {
            VSHPSpeedResidual = (ZoneLatLoadMet - LoadToBeMet) / ResScale;
        }

        return VSHPSpeedResidual;
    }

    void SetVSHPAirFlow(int const FurnaceNum,             // Unit index
                        Real64 const PartLoadRatio,       // unit part load ratio
                        Real64 &OnOffAirFlowRatio,        // ratio of compressor ON airflow to average airflow over timestep
                        Optional_int_const SpeedNum,      // Speed number
                        Optional<Real64 const> SpeedRatio // Speed ratio
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bo Shen, based on HVACMultiSpeedHeatPump:SetAverageAirFlow
        //       DATE WRITTEN   March, 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na
        // PURPOSE OF THIS SUBROUTINE:
        // Set the average air mass flow rates using the part load fraction of the heat pump for this time step
        // Set OnOffAirFlowRatio to be used by DX coils

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataHVACGlobals::MSHPMassFlowRateHigh;
        using DataHVACGlobals::MSHPMassFlowRateLow;
        using DataZoneEnergyDemands::CurDeadBandOrSetback;
        using IntegratedHeatPump::GetAirMassFlowRateIHP;
        using IntegratedHeatPump::GetMaxSpeedNumIHP;
        using IntegratedHeatPump::IHPOperationMode;
        using IntegratedHeatPump::IntegratedHeatPumps;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVMS TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int InletNode;              // inlet node number for PTHPNum
        Real64 AverageUnitMassFlow; // average supply air mass flow rate over time step
        int OutNode;                // Outlet node number in MSHP loop

        InletNode = Furnace(FurnaceNum).FurnaceInletNodeNum;
        OutNode = Furnace(FurnaceNum).FurnaceOutletNodeNum;

        MSHPMassFlowRateLow = 0.0;  // Mass flow rate at low speed
        MSHPMassFlowRateHigh = 0.0; // Mass flow rate at high speed

        if (Furnace(FurnaceNum).OpMode == ContFanCycCoil) {
            CompOffMassFlow = Furnace(FurnaceNum).IdleMassFlowRate;
            CompOffFlowRatio = Furnace(FurnaceNum).IdleSpeedRatio;
        } else {
            CompOffMassFlow = 0.0;
            CompOffFlowRatio = 0.0;
        }

        if (CoolingLoad && (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool)) {
            if (Furnace(FurnaceNum).NumOfSpeedCooling > 0) {
                CompOnMassFlow = Furnace(FurnaceNum).CoolMassFlowRate(Furnace(FurnaceNum).NumOfSpeedCooling);
                CompOnFlowRatio = Furnace(FurnaceNum).MSCoolingSpeedRatio(Furnace(FurnaceNum).NumOfSpeedCooling);
                MSHPMassFlowRateLow = Furnace(FurnaceNum).CoolMassFlowRate(Furnace(FurnaceNum).NumOfSpeedCooling);
                MSHPMassFlowRateHigh = Furnace(FurnaceNum).CoolMassFlowRate(Furnace(FurnaceNum).NumOfSpeedCooling);
            } else {
                CompOnMassFlow = Furnace(FurnaceNum).MaxCoolAirMassFlow;
                CompOnFlowRatio = Furnace(FurnaceNum).CoolingSpeedRatio;
            }
            AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1 - PartLoadRatio) * CompOffMassFlow);
            if (CompOffFlowRatio > 0.0) {
                FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1 - PartLoadRatio) * CompOffFlowRatio);
            } else {
                FanSpeedRatio = CompOnFlowRatio;
            }
        } else if (HeatingLoad && (Furnace(FurnaceNum).FurnaceType_Num == UnitarySys_HeatCool)) {
            if (Furnace(FurnaceNum).NumOfSpeedHeating > 0) {
                CompOnMassFlow = Furnace(FurnaceNum).HeatMassFlowRate(Furnace(FurnaceNum).NumOfSpeedHeating);
                CompOnFlowRatio = Furnace(FurnaceNum).MSHeatingSpeedRatio(Furnace(FurnaceNum).NumOfSpeedHeating);
                MSHPMassFlowRateLow = Furnace(FurnaceNum).HeatMassFlowRate(Furnace(FurnaceNum).NumOfSpeedHeating);
                MSHPMassFlowRateHigh = Furnace(FurnaceNum).HeatMassFlowRate(Furnace(FurnaceNum).NumOfSpeedHeating);
            } else {
                CompOnMassFlow = Furnace(FurnaceNum).MaxHeatAirMassFlow;
                CompOnFlowRatio = Furnace(FurnaceNum).HeatingSpeedRatio;
            }
            AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1 - PartLoadRatio) * CompOffMassFlow);
            if (CompOffFlowRatio > 0.0) {
                FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1 - PartLoadRatio) * CompOffFlowRatio);
            } else {
                FanSpeedRatio = CompOnFlowRatio;
            }
        } else if (Furnace(FurnaceNum).bIsIHP) {
            if (!CurDeadBandOrSetback(Furnace(FurnaceNum).ControlZoneNum) && present(SpeedNum)) {
                // if(present(SpeedNum)) {
                CompOnMassFlow = GetAirMassFlowRateIHP(Furnace(FurnaceNum).CoolingCoilIndex, SpeedNum, SpeedRatio, false);
                CompOnFlowRatio =
                    CompOnMassFlow /
                    GetAirMassFlowRateIHP(Furnace(FurnaceNum).CoolingCoilIndex, GetMaxSpeedNumIHP(Furnace(FurnaceNum).CoolingCoilIndex), 1.0, false);
                MSHPMassFlowRateLow = GetAirMassFlowRateIHP(Furnace(FurnaceNum).CoolingCoilIndex, SpeedNum, 0.0, false);
                MSHPMassFlowRateHigh = GetAirMassFlowRateIHP(Furnace(FurnaceNum).CoolingCoilIndex, SpeedNum, 1.0, false);
            }

            // Set up fan flow rate during compressor off time
            if (Furnace(FurnaceNum).OpMode == ContFanCycCoil && present(SpeedNum)) {
                if (Furnace(FurnaceNum).AirFlowControl == UseCompressorOnFlow && CompOnMassFlow > 0.0) {
                    CompOffMassFlow = GetAirMassFlowRateIHP(Furnace(FurnaceNum).CoolingCoilIndex, SpeedNum, 1.0, false);
                    CompOffFlowRatio = CompOffMassFlow /
                                       GetAirMassFlowRateIHP(
                                           Furnace(FurnaceNum).CoolingCoilIndex, GetMaxSpeedNumIHP(Furnace(FurnaceNum).CoolingCoilIndex), 1.0, false);
                }
            }

            if (present(SpeedNum)) {
                if (SpeedNum > 1) {
                    AverageUnitMassFlow = CompOnMassFlow;
                    FanSpeedRatio = CompOnFlowRatio;
                } else {
                    AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1 - PartLoadRatio) * CompOffMassFlow);
                    if (CompOffFlowRatio > 0.0) {
                        FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1 - PartLoadRatio) * CompOffFlowRatio);
                    } else {
                        FanSpeedRatio = CompOnFlowRatio;
                    }
                }
            } else {
                AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1 - PartLoadRatio) * CompOffMassFlow);
                if (CompOffFlowRatio > 0.0) {
                    FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1 - PartLoadRatio) * CompOffFlowRatio);
                } else {
                    FanSpeedRatio = CompOnFlowRatio;
                }
            }

            if (IHPOperationMode::SCWHMatchWHMode == IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).CurMode) {
                CompOnMassFlow = GetAirMassFlowRateIHP(Furnace(FurnaceNum).CoolingCoilIndex, SpeedNum, SpeedRatio, false);
                AverageUnitMassFlow = CompOnMassFlow;
            };
        } else {
            if (!CurDeadBandOrSetback(Furnace(FurnaceNum).ControlZoneNum) && present(SpeedNum)) {
                if (Furnace(FurnaceNum).HeatCoolMode == HeatingMode) {
                    if (SpeedNum == 1) {
                        CompOnMassFlow = Furnace(FurnaceNum).HeatMassFlowRate(SpeedNum);
                        CompOnFlowRatio = Furnace(FurnaceNum).MSHeatingSpeedRatio(SpeedNum);
                        MSHPMassFlowRateLow = Furnace(FurnaceNum).HeatMassFlowRate(1);
                        MSHPMassFlowRateHigh = Furnace(FurnaceNum).HeatMassFlowRate(1);
                    } else if (SpeedNum > 1) {
                        CompOnMassFlow = SpeedRatio * Furnace(FurnaceNum).HeatMassFlowRate(SpeedNum) +
                                         (1.0 - SpeedRatio) * Furnace(FurnaceNum).HeatMassFlowRate(SpeedNum - 1);
                        CompOnFlowRatio = SpeedRatio * Furnace(FurnaceNum).MSHeatingSpeedRatio(SpeedNum) +
                                          (1.0 - SpeedRatio) * Furnace(FurnaceNum).MSHeatingSpeedRatio(SpeedNum - 1);
                        MSHPMassFlowRateLow = Furnace(FurnaceNum).HeatMassFlowRate(SpeedNum - 1);
                        MSHPMassFlowRateHigh = Furnace(FurnaceNum).HeatMassFlowRate(SpeedNum);
                    }
                } else if (Furnace(FurnaceNum).HeatCoolMode == CoolingMode) {
                    if (SpeedNum == 1) {
                        CompOnMassFlow = Furnace(FurnaceNum).CoolMassFlowRate(SpeedNum);
                        CompOnFlowRatio = Furnace(FurnaceNum).MSCoolingSpeedRatio(SpeedNum);
                        MSHPMassFlowRateLow = Furnace(FurnaceNum).CoolMassFlowRate(1);
                        MSHPMassFlowRateHigh = Furnace(FurnaceNum).CoolMassFlowRate(1);
                    } else if (SpeedNum > 1) {
                        CompOnMassFlow = SpeedRatio * Furnace(FurnaceNum).CoolMassFlowRate(SpeedNum) +
                                         (1.0 - SpeedRatio) * Furnace(FurnaceNum).CoolMassFlowRate(SpeedNum - 1);
                        CompOnFlowRatio = SpeedRatio * Furnace(FurnaceNum).MSCoolingSpeedRatio(SpeedNum) +
                                          (1.0 - SpeedRatio) * Furnace(FurnaceNum).MSCoolingSpeedRatio(SpeedNum - 1);
                        MSHPMassFlowRateLow = Furnace(FurnaceNum).CoolMassFlowRate(SpeedNum - 1);
                        MSHPMassFlowRateHigh = Furnace(FurnaceNum).CoolMassFlowRate(SpeedNum);
                    }
                }
            }

            // Set up fan flow rate during compressor off time
            if (Furnace(FurnaceNum).OpMode == ContFanCycCoil && present(SpeedNum)) {
                if (Furnace(FurnaceNum).AirFlowControl == UseCompressorOnFlow && CompOnMassFlow > 0.0) {
                    if (SpeedNum == 1) { // LOWEST SPEED USE IDLE FLOW
                        CompOffMassFlow = Furnace(FurnaceNum).IdleMassFlowRate;
                        CompOffFlowRatio = Furnace(FurnaceNum).IdleSpeedRatio;
                    } else if (Furnace(FurnaceNum).LastMode == HeatingMode) {
                        CompOffMassFlow = Furnace(FurnaceNum).HeatMassFlowRate(SpeedNum);
                        CompOffFlowRatio = Furnace(FurnaceNum).MSHeatingSpeedRatio(SpeedNum);
                    } else {
                        CompOffMassFlow = Furnace(FurnaceNum).CoolMassFlowRate(SpeedNum);
                        CompOffFlowRatio = Furnace(FurnaceNum).MSCoolingSpeedRatio(SpeedNum);
                    }
                }
            }

            if (present(SpeedNum)) {
                if (SpeedNum > 1) {
                    AverageUnitMassFlow = CompOnMassFlow;
                    FanSpeedRatio = CompOnFlowRatio;
                } else {
                    AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1 - PartLoadRatio) * CompOffMassFlow);
                    if (CompOffFlowRatio > 0.0) {
                        FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1 - PartLoadRatio) * CompOffFlowRatio);
                    } else {
                        FanSpeedRatio = CompOnFlowRatio;
                    }
                }
            } else {
                AverageUnitMassFlow = (PartLoadRatio * CompOnMassFlow) + ((1 - PartLoadRatio) * CompOffMassFlow);
                if (CompOffFlowRatio > 0.0) {
                    FanSpeedRatio = (PartLoadRatio * CompOnFlowRatio) + ((1 - PartLoadRatio) * CompOffFlowRatio);
                } else {
                    FanSpeedRatio = CompOnFlowRatio;
                }
            }
        }

        if (GetCurrentScheduleValue(Furnace(FurnaceNum).SchedPtr) == 0.0) {
            Node(InletNode).MassFlowRate = 0.0;
            OnOffAirFlowRatio = 0.0;
        } else {
            Node(InletNode).MassFlowRate = AverageUnitMassFlow;
            Node(InletNode).MassFlowRateMaxAvail = AverageUnitMassFlow;
            if (AverageUnitMassFlow > 0.0) {
                OnOffAirFlowRatio = CompOnMassFlow / AverageUnitMassFlow;
            } else {
                OnOffAirFlowRatio = 0.0;
            }
        }

        Node(OutNode).MassFlowRate = Node(InletNode).MassFlowRate;

        //  IF(ABS(Node(OutNode)%MassFlowRate - 0.435)  < 0.001) THEN
        //    Node(OutNode)%MassFlowRate  = Node(InletNode)%MassFlowRate
        //  END IF
    }

    void SetOnOffMassFlowRateVSCoil(int const FurnaceNum,                 // index to furnace
                                    int const ZoneNum,                    // index to zone
                                    bool const FirstHVACIteration,        // Flag for 1st HVAC iteration
                                    int const EP_UNUSED(AirLoopNum),      // index to air loop !unused1208
                                    Real64 &OnOffAirFlowRatio,            // ratio of coil on to coil off air flow rate
                                    int const EP_UNUSED(OpMode),          // fan operating mode
                                    Real64 const EP_UNUSED(QZnReq),       // sensible load to be met (W) !unused1208
                                    Real64 const EP_UNUSED(MoistureLoad), // moisture load to be met (W)
                                    Real64 &PartLoadRatio                 // coil part-load ratio
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bo Shen
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Furnace Components.

        // METHODOLOGY EMPLOYED:
        // The HeatCool furnace/unitarysystem and air-to-air heat pump may have alternate air flow rates
        // in cooling, heating, and when no cooling or heating is needed. Set up the coil (comp) ON and OFF
        // air flow rates. Use these flow rates during the Calc routines to set the average mass flow rates
        // based on PLR.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataZoneEnergyDemands::CurDeadBandOrSetback;
        using General::RoundSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int InNode;  // Inlet node number in MSHP loop
        int OutNode; // Outlet node number in MSHP loop

        InNode = Furnace(FurnaceNum).FurnaceInletNodeNum;
        OutNode = Furnace(FurnaceNum).FurnaceOutletNodeNum;

        // FLOW:

        if (CoolingLoad) {
            Furnace(FurnaceNum).HeatCoolMode = CoolingMode;
        } else if (HeatingLoad) {
            Furnace(FurnaceNum).HeatCoolMode = HeatingMode;
        } else {
            Furnace(FurnaceNum).HeatCoolMode = 0;
        }

        // Set the inlet node mass flow rate
        if (Furnace(FurnaceNum).OpMode == ContFanCycCoil) {
            // constant fan mode
            if ((Furnace(FurnaceNum).HeatCoolMode == HeatingMode) && !CurDeadBandOrSetback(ZoneNum)) {
                CompOnMassFlow = Furnace(FurnaceNum).HeatMassFlowRate(1);
                CompOnFlowRatio = Furnace(FurnaceNum).MSHeatingSpeedRatio(1);
                Furnace(FurnaceNum).LastMode = HeatingMode;
            } else if ((Furnace(FurnaceNum).HeatCoolMode == CoolingMode) && !CurDeadBandOrSetback(ZoneNum)) {
                CompOnMassFlow = Furnace(FurnaceNum).CoolMassFlowRate(1);
                CompOnFlowRatio = Furnace(FurnaceNum).MSCoolingSpeedRatio(1);
                Furnace(FurnaceNum).LastMode = CoolingMode;
            } else {
                CompOnMassFlow = Furnace(FurnaceNum).IdleMassFlowRate;
                CompOnFlowRatio = Furnace(FurnaceNum).IdleSpeedRatio;
            }
            CompOffMassFlow = Furnace(FurnaceNum).IdleMassFlowRate;
            CompOffFlowRatio = Furnace(FurnaceNum).IdleSpeedRatio;
        } else {
            // cycling fan mode
            if ((Furnace(FurnaceNum).HeatCoolMode == HeatingMode) && !CurDeadBandOrSetback(ZoneNum)) {
                CompOnMassFlow = Furnace(FurnaceNum).HeatMassFlowRate(1);
                CompOnFlowRatio = Furnace(FurnaceNum).MSHeatingSpeedRatio(1);
            } else if ((Furnace(FurnaceNum).HeatCoolMode == CoolingMode) && !CurDeadBandOrSetback(ZoneNum)) {
                CompOnMassFlow = Furnace(FurnaceNum).CoolMassFlowRate(1);
                CompOnFlowRatio = Furnace(FurnaceNum).MSCoolingSpeedRatio(1);
            } else {
                CompOnMassFlow = 0.0;
                CompOnFlowRatio = 0.0;
            }
            CompOffMassFlow = 0.0;
            CompOffFlowRatio = 0.0;
        }

        // Set the inlet node mass flow rate
        if (GetCurrentScheduleValue(Furnace(FurnaceNum).FanAvailSchedPtr) > 0.0 && CompOnMassFlow != 0.0) {
            OnOffAirFlowRatio = 1.0;
            if (FirstHVACIteration) {
                Node(InNode).MassFlowRate = CompOnMassFlow;
                PartLoadRatio = 0.0;
            } else {
                if (Furnace(FurnaceNum).HeatCoolMode != 0) {
                    PartLoadRatio = 1.0;
                } else {
                    PartLoadRatio = 0.0;
                }
            }
        } else {
            PartLoadRatio = 0.0;
            Node(InNode).MassFlowRate = 0.0;
            Node(OutNode).MassFlowRate = 0.0;
            Node(OutNode).MassFlowRateMaxAvail = 0.0;
            OnOffAirFlowRatio = 1.0;
        }

        // Set the system mass flow rates
        SetVSHPAirFlow(FurnaceNum, PartLoadRatio, OnOffAirFlowRatio);
    }

    void SetMinOATCompressor(int const FurnaceNum,                   // index to furnace
                             std::string const FurnaceName,          // name of furnace
                             std::string const cCurrentModuleObject, // type of furnace
                             std::string const CoolingCoilType,      // type of cooling coil
                             std::string const CoolingCoilName,      // name of cooling coil
                             std::string const HeatingCoilType,      // type of heating coil
                             std::string const HeatingCoilName,      // name of heating coil
                             bool &ErrorsFound                       // GetInput logical that errors were found
    )
    {
        // Using/Aliasing
        auto &GetMinOATDXCoilCompressor(DXCoils::GetMinOATCompressor);
        using DXCoils::GetMinOATCompressorUsingIndex;
        using HVACHXAssistedCoolingCoil::GetActualDXCoilIndex;
        using IntegratedHeatPump::IntegratedHeatPumps;
        using VariableSpeedCoils::GetVSCoilMinOATCompressor;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool errFlag;
        std::string IHPCoilName;
        int DXCoilIndex;

        // Set minimum OAT for heat pump compressor operation in heating mode
        errFlag = false;
        if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) {
            Furnace(FurnaceNum).MinOATCompressorCooling = GetMinOATDXCoilCompressor(CoolingCoilType, CoolingCoilName, errFlag);
        } else if (Furnace(FurnaceNum).CoolingCoilType_Num == CoilDX_CoolingHXAssisted) {
            DXCoilIndex = GetActualDXCoilIndex(CoolingCoilType, CoolingCoilName, ErrorsFound);
            Furnace(FurnaceNum).MinOATCompressorCooling = GetMinOATCompressorUsingIndex(DXCoilIndex, errFlag);
        } else if (Furnace(FurnaceNum).CoolingCoilType_Num == Coil_CoolingAirToAirVariableSpeed) {
            if (Furnace(FurnaceNum).bIsIHP) {
                IHPCoilName = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SCCoilName;
                Furnace(FurnaceNum).MinOATCompressorHeating = GetVSCoilMinOATCompressor(IHPCoilName, errFlag);
            } else {
                Furnace(FurnaceNum).MinOATCompressorHeating = GetVSCoilMinOATCompressor(CoolingCoilName, errFlag);
            }
        } else {
            Furnace(FurnaceNum).MinOATCompressorCooling = -1000.0;
        }
        if (errFlag) {
            ShowContinueError("...occurs in " + cCurrentModuleObject + " = " + FurnaceName);
            ErrorsFound = true;
        }

        // Set minimum OAT for heat pump compressor operation in heating mode
        errFlag = false;
        if (Furnace(FurnaceNum).HeatingCoilType_Num == Coil_HeatingAirToAirVariableSpeed) {
            if (Furnace(FurnaceNum).bIsIHP) {
                IHPCoilName = IntegratedHeatPumps(Furnace(FurnaceNum).CoolingCoilIndex).SHCoilName;
                Furnace(FurnaceNum).MinOATCompressorHeating = GetVSCoilMinOATCompressor(IHPCoilName, errFlag);
            } else {
                Furnace(FurnaceNum).MinOATCompressorHeating = GetVSCoilMinOATCompressor(HeatingCoilName, errFlag);
            }
        } else if (Furnace(FurnaceNum).HeatingCoilType_Num == CoilDX_HeatingEmpirical) {
            Furnace(FurnaceNum).MinOATCompressorHeating = GetMinOATDXCoilCompressor(HeatingCoilType, HeatingCoilName, errFlag);
        } else {
            Furnace(FurnaceNum).MinOATCompressorHeating = -1000.0;
        }
        if (errFlag) {
            ShowContinueError("...occurs in " + cCurrentModuleObject + " = " + FurnaceName);
            ErrorsFound = true;
        }
    }

} // namespace Furnaces

} // namespace EnergyPlus
