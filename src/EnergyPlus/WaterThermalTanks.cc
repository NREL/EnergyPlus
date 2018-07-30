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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Optional.hh>
#include <ObjexxFCL/floops.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DXCoils.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEquipment.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <HVACFan.hh>
#include <HeatBalanceInternalHeatGains.hh>
#include <InputProcessing/InputProcessor.hh>
#include <IntegratedHeatPump.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <RefrigeratedCase.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SolarCollectors.hh>
#include <VariableSpeedCoils.hh>
#include <WaterThermalTanks.hh>

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

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using DataGlobals::NumOfTimeStepInHour;
    using DataGlobals::OutputFileInits;
    using DataGlobals::SecInHour;
    using DataHeatBalance::HeatReclaimDXCoil;
    using DataHeatBalance::HeatReclaimRefrigCondenser;
    using DataHeatBalance::HeatReclaimRefrigeratedRack;
    using DataHeatBalance::NumRefrigCondensers;
    using DataHeatBalance::NumRefrigeratedRacks;
    using namespace DataPlant;
    using General::TrimSigDigits;
    using ReportSizingManager::ReportSizingOutput;
    using VariableSpeedCoils::MaxSpedLevels;

    // Data
    // MODULE PARAMETER DEFINITIONS:
    std::string const cMixedWHModuleObj("WaterHeater:Mixed");
    std::string const cStratifiedWHModuleObj("WaterHeater:Stratified");
    std::string const cMixedCWTankModuleObj("ThermalStorage:ChilledWater:Mixed");
    std::string const cStratifiedCWTankModuleObj("ThermalStorage:ChilledWater:Stratified");
    std::string const cHPWHPumpedCondenser("WaterHeater:HeatPump:PumpedCondenser");
    std::string const cHPWHWrappedCondenser("WaterHeater:HeatPump:WrappedCondenser");
    static std::string const BlankString;

    int const HeatMode(1);  // heating source is on, source will not turn off until setpoint temp is reached
    int const FloatMode(0); // heating source is off, source will not turn on until cutin temp is reached
    int const VentMode(-1); // tank temp is above maximum temperature and water is venting
    int const CoolMode(2);  // cooling source is on, source will not turn off until setpoint temp is reached

    int const AmbientTempSchedule(1);   // ambient temperature around tank (or HPWH inlet air) is scheduled
    int const AmbientTempZone(2);       // tank is located in a zone or HPWH inlet air is zone air only
    int const AmbientTempOutsideAir(3); // tank is located outdoors or HPWH inlet air is outdoor air only
    int const AmbientTempZoneAndOA(4);  // applicable to HPWH only, inlet air is mixture of OA and zone air

    int const CrankcaseTempSchedule(1); // temperature controlling compressor crankcase heater is scheduled
    int const CrankcaseTempZone(2);     // temperature controlling compressor crankcase heater is zone air
    int const CrankcaseTempExterior(3); // temperature controlling compressor crankcase heater is outdoor air

    int const ControlTypeCycle(1);    // water heater only, cycling heating source control
    int const ControlTypeModulate(2); // water heater only, modulating heating source control

    int const TankShapeVertCylinder(1);  // tank shape is a vertical cylinder
    int const TankShapeHorizCylinder(2); // tank shape is a horizontal cylinder
    int const TankShapeOther(3);         // tank shape has an arbitrary perimeter shape

    int const PriorityMasterSlave(1);  // water heater only, master-slave priority control of heater elements
    int const PrioritySimultaneous(2); // water heater only, simultaneous control of heater elements

    int const InletModeFixed(1);   // water heater only, inlet water always enters at the user-specified height
    int const InletModeSeeking(2); // water heater only, inlet water seeks out the node with the closest temperature

    // integer parameter for water heater
    int const MixedWaterHeater(TypeOf_WtrHeaterMixed);           // WaterHeater:Mixed
    int const StratifiedWaterHeater(TypeOf_WtrHeaterStratified); // WaterHeater:Stratified
    // stovall, next line never used because all desuperheater coils used in mixed water heater types
    int const CoilWaterDesuperHeater(4);                                        // Coil:WaterHeating:Desuperheater
    int const MixedChilledWaterStorage(TypeOf_ChilledWaterTankMixed);           // 'ThermalStorage:ChilledWater:Mixed'
    int const StratifiedChilledWaterStorage(TypeOf_ChilledWaterTankStratified); // 'ThermalStorage:ChilledWater:Stratified'

    // reclaim heat object types for Coil:WaterHeating:Desuperheater object
    int const COMPRESSORRACK_REFRIGERATEDCASE(1); // reclaim heating source is refrigerated case compressor rack
    int const COIL_DX_COOLING(2);                 // reclaim heating source is DX cooling coil
    int const COIL_DX_MULTISPEED(3);              // reclaim heating source is DX multispeed coil
    int const COIL_DX_MULTIMODE(4);               // reclaim heating source is DX multimode coil
    int const CONDENSER_REFRIGERATION(5);         // reclaim heating source is detailed refrigeration system condenser
    int const COIL_DX_VARIABLE_COOLING(6);        // reclaim heating source is Variable Speed DX cooling coil

    int const UseSide(101);    // Indicates Use side of water heater
    int const SourceSide(102); // Indicates Source side of water heater

    int const SizeNotSet(200);
    int const SizePeakDraw(201);
    int const SizeResidentialMin(202);
    int const SizePerPerson(203);
    int const SizePerFloorArea(204);
    int const SizePerUnit(205);
    int const SizePerSolarColArea(206);

    int const HPWHControlNotSet(500);
    int const Heater1HPWHControl(501);
    int const Heater2HPWHControl(502);
    int const SourceInletHPWHControl(503);
    int const SourceOutletHPWHControl(504);
    int const UseInletHPWHControl(505);
    int const UseOutletHPWHControl(506);

    int const SourceSideStorageTank(600);
    int const SourceSideIndirectHeatPrimarySetpoint(601);
    int const SourceSideIndirectHeatAltSetpoint(602);

    static std::string const fluidNameWater("WATER");

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE TYPE DECLARATIONS:
    Array1D_bool ValidSourceType; // Used to determine if a source for a desuperheater heating coil is valid
    Array1D_bool MyHPSizeFlag;    // Used to report autosize info in Init
    Array1D_bool CheckWTTEquipName;
    Array1D_bool CheckHPWHEquipName;

    // MODULE VARIABLE DECLARATIONS:
    int NumChilledWaterMixed(0);        // number of mixed chilled water tanks
    int NumChilledWaterStratified(0);   // number of stratified chilled water tanks
    int NumWaterHeaterMixed(0);         // number of mixed water heaters
    int NumWaterHeaterStratified(0);    // number of stratified water heaters
    int NumWaterThermalTank(0);         // total number of water thermal tanks, hot and cold (MIXED + STRATIFIED)
    int NumWaterHeaterDesuperheater(0); // number of desuperheater heating coils
    int NumHeatPumpWaterHeater(0);      // number of heat pump water heaters
    // INTEGER :: MaxCyclesErrorCount           =0 ! error counter for water heater that cycles more than max during time step

    Real64 HPPartLoadRatio(0.0);             // part load ratio of HPWH
    bool GetWaterThermalTankInputFlag(true); // Calls to Water Heater from multiple places in code
    Real64 MixerInletAirSchedule(0.0);       // output of inlet air mixer node schedule
    Real64 MdotAir(0.0);                     // mass flow rate of evaporator air, kg/s
    int NumWaterHeaterSizing(0);             // Number of sizing/design objects for water heaters.
    Array1D_bool AlreadyRated;               // control so we don't repeat again

    namespace {
        // These were static variables within different functions. They were pulled out into the namespace
        // to facilitate easier unit testing of those functions.
        // These are purposefully not in the header file as an extern variable. No one outside of this should
        // use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
        // This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
        bool InitWaterThermalTanksOnce(true); // flag for 1 time initialization
        bool SimWaterThermalTank_OneTimeSetupFlag(true);
        bool CalcWaterThermalTankZoneGains_MyEnvrnFlag(true);
    } // namespace

    // SUBROUTINE SPECIFICATIONS:

    // Object Data
    Array1D<WaterThermalTankData> WaterThermalTank;
    std::unordered_map<std::string, std::string> UniqueWaterThermalTankNames;
    Array1D<HeatPumpWaterHeaterData> HPWaterHeater;
    Array1D<WaterHeaterDesuperheaterData> WaterHeaterDesuperheater;

    static gio::Fmt fmtLD("*");

    // MODULE SUBROUTINES:

    // Functions

    // Default Constructor
    HeatPumpWaterHeaterData::HeatPumpWaterHeaterData()
        : TypeNum(0), TankTypeNum(0), StandAlone(false), AvailSchedPtr(0), SetPointTempSchedule(0), DeadBandTempDiff(0.0), Capacity(0.0),
          BackupElementCapacity(0.0), BackupElementEfficiency(0.0), WHOnCycParaLoad(0.0), WHOffCycParaLoad(0.0), WHOnCycParaFracToTank(0.0),
          WHOffCycParaFracToTank(0.0), WHPLFCurve(0), OperatingAirFlowRate(0.0), OperatingWaterFlowRate(0.0), COP(0.0), SHR(0.0),
          RatedInletDBTemp(0.0), RatedInletWBTemp(0.0), RatedInletWaterTemp(0.0), FoundTank(false), HeatPumpAirInletNode(0), HeatPumpAirOutletNode(0),
          OutsideAirNode(0), ExhaustAirNode(0), CondWaterInletNode(0), CondWaterOutletNode(0), WHUseInletNode(0), WHUseOutletNode(0),
          WHUseSidePlantLoopNum(0), DXCoilNum(0), DXCoilTypeNum(0), DXCoilAirInletNode(0), DXCoilPLFFPLR(0), FanType_Num(0), FanNum(0),
          FanPlacement(0), FanOutletNode(0), WaterHeaterTankNum(0), OutletAirSplitterSchPtr(0), InletAirMixerSchPtr(0), Mode(0), SaveMode(0),
          SaveWHMode(0), Power(0.0), Energy(0.0), HeatingPLR(0.0), SetPointTemp(0.0), MinAirTempForHPOperation(5.0),
          MaxAirTempForHPOperation(48.8888888889), InletAirMixerNode(0), OutletAirSplitterNode(0), SourceMassFlowRate(0.0), InletAirConfiguration(0),
          AmbientTempSchedule(0), AmbientRHSchedule(0), AmbientTempZone(0), CrankcaseTempIndicator(0), CrankcaseTempSchedule(0), CrankcaseTempZone(0),
          OffCycParaLoad(0.0), OnCycParaLoad(0.0), ParasiticTempIndicator(0), OffCycParaFuelRate(0.0), OnCycParaFuelRate(0.0),
          OffCycParaFuelEnergy(0.0), OnCycParaFuelEnergy(0.0), AirFlowRateAutoSized(false), WaterFlowRateAutoSized(false), HPSetPointError(0),
          HPSetPointErrIndex1(0), IterLimitErrIndex1(0), IterLimitExceededNum1(0), RegulaFalsiFailedIndex1(0), RegulaFalsiFailedNum1(0),
          IterLimitErrIndex2(0), IterLimitExceededNum2(0), RegulaFalsiFailedIndex2(0), RegulaFalsiFailedNum2(0), FirstTimeThroughFlag(true),
          ShowSetPointWarning(true), HPWaterHeaterSensibleCapacity(0.0), HPWaterHeaterLatentCapacity(0.0), WrappedCondenserBottomLocation(0.0),
          WrappedCondenserTopLocation(0.0), ControlSensor1Height(-1.0), ControlSensor1Node(1), ControlSensor1Weight(1.0), ControlSensor2Height(-1.0),
          ControlSensor2Node(2), ControlSensor2Weight(0.0), ControlTempAvg(0.0), ControlTempFinal(0.0),
          AllowHeatingElementAndHeatPumpToRunAtSameTime(true), NumofSpeed(0), HPWHAirVolFlowRate(MaxSpedLevels, 0.0),
          HPWHAirMassFlowRate(MaxSpedLevels, 0.0), HPWHWaterVolFlowRate(MaxSpedLevels, 0.0), HPWHWaterMassFlowRate(MaxSpedLevels, 0.0),
          MSAirSpeedRatio(MaxSpedLevels, 0.0), MSWaterSpeedRatio(MaxSpedLevels, 0.0), bIsIHP(false)
    {
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

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology. Subroutine is called from PlantLoopEquipments

        // Using/Aliasing
        using DataGlobals::KickOffSimulation;
        using DataSizing::DataNonZoneNonAirloopValue;
        using IntegratedHeatPump::GetCurWorkMode;
        using IntegratedHeatPump::IHPOperationMode;
        using IntegratedHeatPump::IntegratedHeatPumps;
        using VariableSpeedCoils::GetCoilInletNodeVariableSpeed;
        using VariableSpeedCoils::GetCoilOutletNodeVariableSpeed;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static Array1D_bool MyOneTimeFlagWH; // first pass log
        static Array1D_bool MyTwoTimeFlagWH; // second pass do input check
        static Array1D_bool MyOneTimeFlagHP; // first pass log
        static Array1D_bool MyTwoTimeFlagHP; // second pass do input check
        int tmpLoopNum;
        int tmpLoopSideNum;
        int CompNum;
        int TankNum;
        int InletNodeSav(0);
        int OutletNodeSav(0);
        int DXINletNodeSav(0);
        IHPOperationMode IHPMode(IHPOperationMode::IdleMode);
        bool bDWHCoilReading(false);
        std::string IHPFanNameSave("");
        int IHPFanIndexSav;
        int IHPFanplaceSav;

        // FLOW:
        if (GetWaterThermalTankInputFlag) {
            GetWaterThermalTankInput();
            GetWaterThermalTankInputFlag = false;
        }

        if (SimWaterThermalTank_OneTimeSetupFlag) {
            MyOneTimeFlagWH.allocate(NumWaterThermalTank);
            MyTwoTimeFlagWH.allocate(NumWaterThermalTank);
            MyOneTimeFlagHP.allocate(NumHeatPumpWaterHeater);
            MyTwoTimeFlagHP.allocate(NumHeatPumpWaterHeater);
            MyOneTimeFlagWH = true;
            MyTwoTimeFlagWH = true;
            MyOneTimeFlagHP = true;
            MyTwoTimeFlagHP = true;
            SimWaterThermalTank_OneTimeSetupFlag = false;
        }

        // Find the correct Equipment
        if (CompType != TypeOf_HeatPumpWtrHeaterPumped && CompType != TypeOf_HeatPumpWtrHeaterWrapped) {
            if (CompIndex == 0) {
                CompNum = UtilityRoutines::FindItem(CompName, WaterThermalTank);
                if (CompNum == 0) {
                    ShowFatalError("SimWaterThermalTank:  Unit not found=" + CompName);
                }
                CompIndex = CompNum;
            } else {
                CompNum = CompIndex;
                if (CompNum > NumWaterThermalTank || CompNum < 1) {
                    ShowFatalError("SimWaterThermalTank:  Invalid CompIndex passed=" + TrimSigDigits(CompNum) +
                                   ", Number of Units=" + TrimSigDigits(NumWaterThermalTank) + ", Entered Unit name=" + CompName);
                }
                if (CheckWTTEquipName(CompNum)) {
                    if (CompName != WaterThermalTank(CompNum).Name) {
                        ShowFatalError("SimWaterThermalTank: Invalid CompIndex passed=" + TrimSigDigits(CompNum) + ", Unit name=" + CompName +
                                       ", stored Unit Name for that index=" + WaterThermalTank(CompNum).Name);
                    }
                    CheckWTTEquipName(CompNum) = false;
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
                    ShowFatalError("SimWaterThermalTank:  Invalid CompIndex passed=" + TrimSigDigits(CompNum) +
                                   ", Number of Units=" + TrimSigDigits(NumHeatPumpWaterHeater) + ", Entered Unit name=" + CompName);
                }
                if (CheckHPWHEquipName(CompNum)) {
                    if (CompName != HPWaterHeater(CompNum).Name) {
                        ShowFatalError("SimWaterThermalTank: Invalid CompIndex passed=" + TrimSigDigits(CompNum) + ", Unit name=" + CompName +
                                       ", stored Unit Name for that index=" + HPWaterHeater(CompNum).Name);
                    }
                    CheckHPWHEquipName(CompNum) = false;
                }
            }
        }

        // this case statement needs integerization.
        {
            auto const SELECT_CASE_var(CompType);

            // string comparisons to remove here.
            // =========================  Water Heater and Chilled Water Storage
            if ((SELECT_CASE_var == TypeOf_WtrHeaterMixed) || (SELECT_CASE_var == TypeOf_WtrHeaterStratified) ||
                (SELECT_CASE_var == TypeOf_ChilledWaterTankMixed) || (SELECT_CASE_var == TypeOf_ChilledWaterTankStratified)) {

                if (InitLoopEquip) {
                    if (present(LoopNum)) {
                        InitWaterThermalTank(CompNum, FirstHVACIteration, LoopNum, LoopSideNum);
                    } else {
                        InitWaterThermalTank(CompNum, FirstHVACIteration);
                    }
                    MinePlantStructForInfo(CompNum);
                    if (present(LoopNum)) {
                        if (((WaterThermalTank(CompNum).SourceSidePlantLoopNum == LoopNum) &&
                             (WaterThermalTank(CompNum).SourceSidePlantLoopSide == LoopSideNum)) ||
                            ((WaterThermalTank(CompNum).UseSidePlantLoopNum == LoopNum) &&
                             (WaterThermalTank(CompNum).UseSidePlantLoopSide == LoopSideNum))) {

                            SizeTankForDemandSide(CompNum);
                            SizeDemandSidePlantConnections(CompNum);
                            SizeSupplySidePlantConnections(CompNum, LoopNum, LoopSideNum);
                            SizeTankForSupplySide(CompNum);
                        } else {
                            return;
                        }
                    } else {
                        SizeTankForDemandSide(CompNum);
                        SizeDemandSidePlantConnections(CompNum);
                        SizeSupplySidePlantConnections(CompNum);
                        SizeTankForSupplySide(CompNum);
                    }

                    // Calculate and report water heater standard ratings to EIO file (now that sizing is done)
                    if (PlantFirstSizesOkayToFinalize) {
                        if (!WaterThermalTank(CompNum).IsChilledWaterTank) {
                            CalcStandardRatings(CompNum);
                        } else {
                            ReportCWTankInits(CompNum);
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

                if (MyOneTimeFlagWH(CompNum)) {
                    MyOneTimeFlagWH(CompNum) = false;
                } else {
                    if (MyTwoTimeFlagWH(CompNum)) {
                        MinePlantStructForInfo(CompNum); // call it again to get control types filled out
                        MyTwoTimeFlagWH(CompNum) = false;
                    }
                }
                WaterThermalTank(CompNum).UseSideLoadRequested = std::abs(MyLoad);
                tmpLoopNum = WaterThermalTank(CompNum).UseSidePlantLoopNum;
                tmpLoopSideNum = WaterThermalTank(CompNum).UseSidePlantLoopSide;
                if (tmpLoopNum > 0 && tmpLoopSideNum > 0 && !KickOffSimulation) {
                    WaterThermalTank(CompNum).UseCurrentFlowLock = PlantLoop(tmpLoopNum).LoopSide(LoopSideNum).FlowLock;
                } else {
                    WaterThermalTank(CompNum).UseCurrentFlowLock = 1;
                }
                tmpLoopNum = WaterThermalTank(CompNum).SourceSidePlantLoopNum;
                tmpLoopSideNum = WaterThermalTank(CompNum).SourceSidePlantLoopSide;
                if (tmpLoopNum > 0 && tmpLoopSideNum > 0 && !KickOffSimulation) {
                    WaterThermalTank(CompNum).SourceCurrentFlowLock = PlantLoop(tmpLoopNum).LoopSide(LoopSideNum).FlowLock;
                } else {
                    WaterThermalTank(CompNum).SourceCurrentFlowLock = 1;
                }
                InitWaterThermalTank(CompNum, FirstHVACIteration);
                //       Plant connected water heaters may have a desuperheater heating coil attached
                if (WaterThermalTank(CompNum).DesuperheaterNum == 0) {
                    if ((WaterThermalTank(CompNum).TypeNum == MixedWaterHeater) || (WaterThermalTank(CompNum).TypeNum == MixedChilledWaterStorage)) {
                        CalcWaterThermalTankMixed(CompNum);
                    } else if ((WaterThermalTank(CompNum).TypeNum == StratifiedWaterHeater) ||
                               (WaterThermalTank(CompNum).TypeNum == StratifiedChilledWaterStorage)) {
                        CalcWaterThermalTankStratified(CompNum);
                    }
                } else if (WaterThermalTank(CompNum).DesuperheaterNum > 0) {
                    CalcDesuperheaterWaterHeater(CompNum, FirstHVACIteration);
                }
                UpdateWaterThermalTank(CompNum);
                ReportWaterThermalTank(CompNum);

                // =========================  Heat Pump Water Heater
            } else if (SELECT_CASE_var == TypeOf_HeatPumpWtrHeaterPumped || SELECT_CASE_var == TypeOf_HeatPumpWtrHeaterWrapped) {
                if (InitLoopEquip) {
                    // CompNum is index to heatpump model, not tank so get the tank index
                    TankNum = HPWaterHeater(CompNum).WaterHeaterTankNum;
                    if (present(LoopNum)) {
                        InitWaterThermalTank(TankNum, FirstHVACIteration, LoopNum, LoopSideNum);
                    } else {
                        InitWaterThermalTank(TankNum, FirstHVACIteration);
                    }
                    MinePlantStructForInfo(TankNum);
                    if (present(LoopNum)) {
                        if (((WaterThermalTank(TankNum).SourceSidePlantLoopNum == LoopNum) &&
                             (WaterThermalTank(TankNum).SourceSidePlantLoopSide == LoopSideNum)) ||
                            ((WaterThermalTank(TankNum).UseSidePlantLoopNum == LoopNum) &&
                             (WaterThermalTank(TankNum).UseSidePlantLoopSide == LoopSideNum))) {
                            SizeTankForDemandSide(CompNum);
                            SizeDemandSidePlantConnections(CompNum);
                            SizeSupplySidePlantConnections(TankNum, LoopNum, LoopSideNum);
                            SizeTankForSupplySide(TankNum);
                        } else {
                            return;
                        }
                    } else {
                        SizeTankForDemandSide(CompNum);
                        SizeDemandSidePlantConnections(CompNum);
                        SizeSupplySidePlantConnections(TankNum);
                        SizeTankForSupplySide(TankNum);
                    }

                    if (PlantFirstSizesOkayToFinalize) {
                        CalcStandardRatings(TankNum);
                        DataNonZoneNonAirloopValue = 0.0;
                    }
                    MinCap = 0.0;
                    MaxCap = HPWaterHeater(CompNum).Capacity;
                    OptCap = HPWaterHeater(CompNum).Capacity;

                    return;
                }

                if (MyOneTimeFlagHP(CompNum)) {
                    MyOneTimeFlagHP(CompNum) = false;
                } else {
                    if (MyTwoTimeFlagHP(CompNum)) {
                        MinePlantStructForInfo(HPWaterHeater(CompNum).WaterHeaterTankNum); // call it again to get control types filled out
                        MyTwoTimeFlagHP(CompNum) = false;
                    }
                }
                WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).UseSideLoadRequested = std::abs(MyLoad);
                tmpLoopNum = WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).UseSidePlantLoopNum;
                tmpLoopSideNum = WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).UseSidePlantLoopSide;
                if (tmpLoopNum > 0 && tmpLoopSideNum > 0 && !KickOffSimulation) {
                    WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).UseCurrentFlowLock =
                        PlantLoop(tmpLoopNum).LoopSide(LoopSideNum).FlowLock;
                } else {
                    WaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum).UseCurrentFlowLock = 1;
                }
                if (present(LoopNum)) {
                    InitWaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum, FirstHVACIteration, LoopNum, LoopSideNum);
                } else {
                    InitWaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum, FirstHVACIteration);
                }

                InletNodeSav = HPWaterHeater(CompNum).HeatPumpAirInletNode;
                OutletNodeSav = HPWaterHeater(CompNum).HeatPumpAirOutletNode;
                DXINletNodeSav = HPWaterHeater(CompNum).DXCoilAirInletNode;
                IHPFanIndexSav = HPWaterHeater(CompNum).FanNum;
                IHPFanNameSave = HPWaterHeater(CompNum).FanName;
                IHPFanplaceSav = HPWaterHeater(CompNum).FanPlacement;

                if (HPWaterHeater(CompNum).bIsIHP) // pass the tank indexes to the IHP object
                {
                    IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).WHtankType = CompType;
                    IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).WHtankName = CompName;
                    IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).WHtankID = CompIndex;
                    if (present(LoopNum)) {
                        IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).LoopNum = LoopNum;
                        IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).LoopSideNum = LoopSideNum;
                    }

                    IHPMode = GetCurWorkMode(HPWaterHeater(CompNum).DXCoilNum);
                    if ((IHPOperationMode::DWHMode == IHPMode) || (IHPOperationMode::SCDWHMode == IHPMode) ||
                        (IHPOperationMode::SHDWHElecHeatOffMode == IHPMode) ||
                        (IHPOperationMode::SHDWHElecHeatOnMode == IHPMode)) { // default is to specify the air nodes for SCWH mode
                        HPWaterHeater(CompNum).HeatPumpAirInletNode =
                            GetCoilInletNodeVariableSpeed("COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED",
                                                          IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).DWHCoilName,
                                                          bDWHCoilReading);
                        HPWaterHeater(CompNum).HeatPumpAirOutletNode =
                            GetCoilOutletNodeVariableSpeed("COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED",
                                                           IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).DWHCoilName,
                                                           bDWHCoilReading);
                        HPWaterHeater(CompNum).DXCoilAirInletNode = HPWaterHeater(CompNum).HeatPumpAirInletNode;
                    } else // default is to input outdoor fan to the the HPWH
                    {
                        HPWaterHeater(CompNum).FanNum = IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).IDFanID;
                        HPWaterHeater(CompNum).FanName = IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).IDFanName;
                        HPWaterHeater(CompNum).FanPlacement = IntegratedHeatPumps(HPWaterHeater(CompNum).DXCoilNum).IDFanPlace;
                    }
                };

                CalcHeatPumpWaterHeater(HPWaterHeater(CompNum).WaterHeaterTankNum, FirstHVACIteration);
                UpdateWaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum);
                ReportWaterThermalTank(HPWaterHeater(CompNum).WaterHeaterTankNum);

                HPWaterHeater(CompNum).HeatPumpAirInletNode = InletNodeSav;
                HPWaterHeater(CompNum).HeatPumpAirOutletNode = OutletNodeSav;
                HPWaterHeater(CompNum).DXCoilAirInletNode = DXINletNodeSav;
                HPWaterHeater(CompNum).FanNum = IHPFanIndexSav;
                HPWaterHeater(CompNum).FanName = IHPFanNameSave;
                HPWaterHeater(CompNum).FanPlacement = IHPFanplaceSav;

            } else {
                ShowSevereError("SimWaterThermalTank: Invalid Water Thermal Tank Equipment Type=" + TrimSigDigits(CompType));
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

        // Using/Aliasing
        using namespace DataPlant;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool LocalRunFlag; // local variables of similar name as others used in Sim modules
        bool LocalInitLoopEquip;
        Real64 MyLoad;
        Real64 MinCap;
        Real64 MaxCap;
        Real64 OptCap;
        int TestNum;

        // FLOW:
        if (GetWaterThermalTankInputFlag) {
            GetWaterThermalTankInput();
            GetWaterThermalTankInputFlag = false;
        }

        // Only simulate stand-alone water heaters here.  Plant connected water heaters are called by the PlantLoopEquipments.
        if (WaterThermalTank(WaterHeaterNum).StandAlone) {
            LocalRunFlag = true;
            LocalInitLoopEquip = false;
            TestNum = WaterHeaterNum;
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
                ShowFatalError("SimulateWaterHeaterStandAlone: Input WaterHeater Num [" + TrimSigDigits(WaterHeaterNum) +
                               "] does not match returned WaterHeater Num[" + TrimSigDigits(TestNum) + "] Name=\"" +
                               WaterThermalTank(WaterHeaterNum).Name + "\".");
            }

            // HPWHs with inlet air from a zone and not connected to a plant loop are simulated through a CALL from ZoneEquipmentManager.
            // HPWHs that are plant connected are always simulated through a CALL from PlantLoopEquipments directly to SimWaterThermalTank.

            // NOTE: HPWHs with inlet air from a zone AND plant connected are not stand alone and are simulated in PlantLoopEquipments
        } else if (WaterThermalTank(WaterHeaterNum).HeatPumpNum > 0) {
            //   Only HPWHs with inlet air from outdoors or scheduled HPWHs (not connected to a plant loop) are simulated here.
            if (HPWaterHeater(WaterThermalTank(WaterHeaterNum).HeatPumpNum).StandAlone &&
                (HPWaterHeater(WaterThermalTank(WaterHeaterNum).HeatPumpNum).InletAirConfiguration == AmbientTempOutsideAir ||
                 HPWaterHeater(WaterThermalTank(WaterHeaterNum).HeatPumpNum).InletAirConfiguration == AmbientTempSchedule)) {
                LocalRunFlag = true;
                LocalInitLoopEquip = false;
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
                LocalRunFlag = true;
                LocalInitLoopEquip = false;
                TestNum = WaterHeaterNum;
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
                    ShowFatalError("SimulateWaterHeaterStandAlone: Input WaterHeater Num [" + TrimSigDigits(WaterHeaterNum) +
                                   "] does not match returned WaterHeater Num[" + TrimSigDigits(TestNum) + "] Name=\"" +
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

        // Using/Aliasing
        using DataGlobals::DoingSizing;
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool LocalRunFlag; // local variables of similar name as others used in Sim modules
        bool LocalInitLoopEquip;
        int LocalFlowLock; // local variables of similar name as others used in sim modules
        int HeatPumpNum;
        Real64 MyLoad;
        Real64 MinCap;
        Real64 MaxCap;
        Real64 OptCap;

        // FLOW:
        if (GetWaterThermalTankInputFlag) {
            GetWaterThermalTankInput();
            GetWaterThermalTankInputFlag = false;
        }

        // Find the correct Heat Pump Water Heater
        if (CompIndex == 0) {
            HeatPumpNum = UtilityRoutines::FindItemInList(CompName, HPWaterHeater);
            if (HeatPumpNum == 0) {
                ShowFatalError("SimHeatPumpWaterHeater: Unit not found=" + CompName);
            }
            CompIndex = HeatPumpNum;
        } else {
            HeatPumpNum = CompIndex;
            if (HeatPumpNum > NumHeatPumpWaterHeater || HeatPumpNum < 1) {
                ShowFatalError("SimHeatPumpWaterHeater:  Invalid CompIndex passed=" + TrimSigDigits(HeatPumpNum) +
                               ", Number of Units=" + TrimSigDigits(NumHeatPumpWaterHeater) + ", Entered Unit name=" + CompName);
            }
        }

        // Only simulate HPWHs specified as zone equipment and not connected to a plant loop.
        // HPWHs not defined as zone equipment with no plant connections are simulated in NonZoneEquipmentManager.
        // Plant connected HPWHs are called by PlantLoopEquipments (but only those on supply side ).
        SensLoadMet = 0.0;
        LatLoadMet = 0.0;

        LocalRunFlag = true;
        LocalFlowLock = 1; // .TRUE.
        LocalInitLoopEquip = false;

        // HPWH will not be included in sizing calculations, fan is initialized only during BeginEnvrnFlag (FALSE during sizing)
        // (fan will be turned off during Standard Ratings procedure yielding incorrect results)
        if (DoingSizing) return;

        // For HPWHs, StandAlone means not connected to a plant loop (use nodes are not used, source nodes are connected to a HPWH)
        if (HPWaterHeater(HeatPumpNum).StandAlone) {
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

        // Using/Aliasing
        using DataGlobals::BeginEnvrnFlag;
        using DataGlobals::DoingSizing;
        using DataHeatBalFanSys::MAT;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int WaterThermalTankNum;
        int ZoneNum;
        Real64 TankTemp;
        Real64 QLossToZone;
        int SchIndex;

        // FLOW:
        if (NumWaterThermalTank == 0) {

            if (!DoingSizing) {
                return;
            } else {
                if (GetWaterThermalTankInputFlag) {
                    GetWaterThermalTankInput();
                    GetWaterThermalTankInputFlag = false;
                }
                if (NumWaterThermalTank == 0) return;
            }
        }

        if (BeginEnvrnFlag && CalcWaterThermalTankZoneGains_MyEnvrnFlag) {
            for (auto &e : WaterThermalTank) {
                e.AmbientZoneGain = 0.0;
                e.FuelEnergy = 0.0;
                e.OffCycParaFuelEnergy = 0.0;
                e.OnCycParaFuelEnergy = 0.0;
            }
            CalcWaterThermalTankZoneGains_MyEnvrnFlag = false;
        }

        if (!BeginEnvrnFlag) CalcWaterThermalTankZoneGains_MyEnvrnFlag = true;

        for (WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum) {
            if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone == 0) continue;
            ZoneNum = WaterThermalTank(WaterThermalTankNum).AmbientTempZone;
            if (DoingSizing) {
                // Initialize tank temperature to setpoint
                // (use HPWH or Desuperheater heating coil set point if applicable)
                if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                    SchIndex = HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTempSchedule;
                } else if (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0) {
                    SchIndex = WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).SetPointTempSchedule;
                } else {
                    SchIndex = WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule;
                }

                if (SchIndex > 0) {
                    TankTemp = GetCurrentScheduleValue(SchIndex);
                } else {
                    TankTemp = 20.0;
                }
                {
                    auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).TypeNum);
                    if (SELECT_CASE_var == MixedWaterHeater) {
                        QLossToZone =
                            max(WaterThermalTank(WaterThermalTankNum).OnCycLossCoeff * WaterThermalTank(WaterThermalTankNum).OnCycLossFracToZone,
                                WaterThermalTank(WaterThermalTankNum).OffCycLossCoeff * WaterThermalTank(WaterThermalTankNum).OffCycLossFracToZone) *
                            (TankTemp - MAT(WaterThermalTank(WaterThermalTankNum).AmbientTempZone));
                    } else if (SELECT_CASE_var == StratifiedWaterHeater) {
                        QLossToZone = max(WaterThermalTank(WaterThermalTankNum).Node(1).OnCycLossCoeff *
                                              WaterThermalTank(WaterThermalTankNum).SkinLossFracToZone,
                                          WaterThermalTank(WaterThermalTankNum).Node(1).OffCycLossCoeff *
                                              WaterThermalTank(WaterThermalTankNum).SkinLossFracToZone) *
                                      (TankTemp - MAT(WaterThermalTank(WaterThermalTankNum).AmbientTempZone));
                    } else if (SELECT_CASE_var == MixedChilledWaterStorage) {
                        QLossToZone = WaterThermalTank(WaterThermalTankNum).OffCycLossCoeff *
                                      WaterThermalTank(WaterThermalTankNum).OffCycLossFracToZone *
                                      (TankTemp - MAT(WaterThermalTank(WaterThermalTankNum).AmbientTempZone));
                    } else if (SELECT_CASE_var == StratifiedChilledWaterStorage) {
                        QLossToZone = WaterThermalTank(WaterThermalTankNum).Node(1).OffCycLossCoeff *
                                      WaterThermalTank(WaterThermalTankNum).SkinLossFracToZone *
                                      (TankTemp - MAT(WaterThermalTank(WaterThermalTankNum).AmbientTempZone));
                    }
                }
                WaterThermalTank(WaterThermalTankNum).AmbientZoneGain = QLossToZone;
            }
        }
    }

    bool GetWaterThermalTankInput()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   July 2016
        //
        // PURPOSE OF THIS SUBROUTINE:
        // Manages GetInput processing and program termination

        static std::string const RoutineName("GetWaterThermalTankInput: "); // include trailing blank space
        static bool ErrorsFound(false);                                     // true if errors detected in GetUnitarySystemInputData

        GetWaterThermalTankInputData(ErrorsFound);

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in getting thermal storage input. Preceding condition(s) causes termination.");
        }

        return ErrorsFound;
    }

    bool GetWaterThermalTankInputData(bool &ErrorsFound)
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

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        using DataGlobals::AutoCalculate;
        using DataGlobals::NumOfZones;
        using DataGlobals::ScheduleAlwaysOn;

        using namespace DataIPShortCuts;
        using BranchNodeConnections::SetUpCompSets;
        using BranchNodeConnections::TestCompSet;
        using CurveManager::CurveValue;
        using CurveManager::GetCurveIndex;
        using CurveManager::GetCurveType;
        using DataEnvironment::OutBaroPress;
        using DataHeatBalance::IntGainTypeOf_ThermalStorageChilledWaterMixed;
        using DataHeatBalance::IntGainTypeOf_ThermalStorageChilledWaterStratified;
        using DataHeatBalance::IntGainTypeOf_WaterHeaterMixed;
        using DataHeatBalance::IntGainTypeOf_WaterHeaterStratified;
        using DataHeatBalance::Zone;
        using DataHVACGlobals::BlowThru;
        using DataHVACGlobals::DrawThru;
        using DataHVACGlobals::FanType_SimpleOnOff;
        using DataLoopNode::Node; // ,NodeConnectionType_Internal
        using DataLoopNode::NodeConnectionType_Inlet;
        using DataLoopNode::NodeConnectionType_Outlet;
        using DataLoopNode::NodeConnectionType_OutsideAirReference;
        using DataLoopNode::NodeConnectionType_ReliefAir;
        using DataLoopNode::NodeID;
        using DataLoopNode::NodeType_Air;
        using DataLoopNode::NodeType_Water;
        using DataLoopNode::ObjectIsNotParent;
        using DataLoopNode::ObjectIsParent;
        using DataSizing::AutoSize;
        using DataZoneEquipment::GetZoneEquipmentData;
        using DataZoneEquipment::ZoneEquipConfig;
        using DataZoneEquipment::ZoneEquipInputsFilled;
        using DataZoneEquipment::ZoneEquipList;
        using DXCoils::DXCoil;
        using DXCoils::GetDXCoilIndex;
        using DXCoils::NumDXCoils;
        using Fans::GetFanIndex;
        using Fans::GetFanInletNode;
        using Fans::GetFanOutletNode;
        using Fans::GetFanType;
        using Fans::GetFanVolFlow;
        using FluidProperties::GetDensityGlycol;
        using General::RoundSigDigits;
        using General::TrimSigDigits;
        using GlobalNames::VerifyUniqueCoilName;
        using IntegratedHeatPump::GetCoilIndexIHP;
        using IntegratedHeatPump::GetCoilInletNodeIHP;
        using IntegratedHeatPump::GetDWHCoilCapacityIHP;
        using IntegratedHeatPump::GetDWHCoilInletNodeIHP;
        using IntegratedHeatPump::GetDWHCoilOutletNodeIHP;
        using IntegratedHeatPump::GetIHPDWHCoilPLFFPLR;
        using IntegratedHeatPump::IHPOperationMode;
        using IntegratedHeatPump::IntegratedHeatPumps;
        using NodeInputManager::GetOnlySingleNode;
        using OutAirNodeManager::CheckAndAddAirNodeNumber;
        using OutAirNodeManager::CheckOutAirNodeNumber;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using RefrigeratedCase::CheckRefrigerationInput;
        using ReportSizingManager::ReportSizingOutput;
        using ScheduleManager::CheckScheduleValueMinMax;
        using ScheduleManager::GetScheduleIndex;
        using VariableSpeedCoils::GetCoilCapacityVariableSpeed;
        using VariableSpeedCoils::GetCoilIndexVariableSpeed;
        using VariableSpeedCoils::GetCoilInletNodeVariableSpeed;
        using VariableSpeedCoils::GetCoilOutletNodeVariableSpeed;
        using VariableSpeedCoils::GetVSCoilPLFFPLR;
        using VariableSpeedCoils::VarSpeedCoil;

        static std::string const RoutineName("GetWaterThermalTankInput: ");
        static std::string const RoutineNameNoColon("GetWaterThermalTankInput");

        int WaterThermalTankNum; // Index to WATER HEATER:*
        int WHsizingNum;         // Index to Water Heater:Sizing, for the IDF objects--not data storage
        int NodeNum;             // Index to a stratified thermal node
        int CheckWaterHeaterNum; // Used to search WATER HEATER:* to find association with HP Water Heater
        int DesuperheaterNum;    // Index to Coil:WaterHeating:Desuperheater
        int HPWaterHeaterNum;    // Index to HEAT PUMP:WATER HEATER
        int HeatingSourceNum;    // Index to DX cooling coil (heat source for desuperheater)
        int NumAlphas;           // Number of elements in the alpha array
        int NumNums;             // Number of elements in the numeric array
        // unused1208  INTEGER                     :: NumArgs                 ! Number of elements in the object (alpha + numeric)
        int RackNum;                              // Index to refrigrated display case rack
        int CondNum;                              // Index to refrigration condenser
        int DXCoilNum;                            // Index to DX coils
        int IOStat;                               // IO Status when calling get input subroutine
        bool IsValid;                             // Flag for validating PLF curve, OutsideAirNode
        static std::string FanInletNode;          // Used to set up comp set
        static std::string FanOutletNode;         // Used to set up comp set
        static std::string CoilInletNode;         // Used to set up comp set
        static std::string CoilOutletNode;        // Used to set up comp set
        static int SupAirIn(0);                   // Used for error checking HPWHs
        static int ExhAirOut(0);                  // Used for error checking HPWHs
        static bool FoundInletNode(false);        // Used for error checking HPWHs
        static bool FoundOutletNode(false);       // Used for error checking HPWHs
        static int ZoneNum(0);                    // Used for error checking HPWHs
        static bool ValidScheduleValue(false);    // Used for error checking HPWH's inlet air mixer schedule
        static int ZoneEquipConfigNum(0);         // Used to determine if HPWH tank is in a Zone Equipment List (ZEL)
        static int ZoneEquipListNum(0);           // Used to determine if HPWH tank is in a Zone Equipment List
        static int EquipmentTypeNum(0);           // Used to determine if HPWH tank is in a Zone Equipment List
        static bool FoundTankInList(false);       // Used to determine if HPWH tank is listed in a Zone Equipment List
        static bool TankNotLowestPriority(false); // Used to determine if HPWH tank is prioritized correctly in ZEL
        static int TankCoolingPriority(0);        // Used to determine if a HPWH tank is prioritized correctly in ZEL
        static int TankHeatingPriority(0);        // Used to determine if a HPWH tank is prioritized correctly in ZEL
        static bool DXCoilErrFlag(false);         // Used for error checking DX coils used with HPWHs
        static Real64 FanVolFlow(0.0);            // Used for error checking fans used with HPWHs
        static bool errFlag(false);               // Used for error checking used with HPWHs
        static Real64 HEffFTemp(0.0);             // Used for error checking desuperheater heating coils
        bool Okay;
        bool bIsVScoil(false); // indicate if the heat pump WH coil is a variable-speed coil
        int IHPIndex(0);       // coil No for integrated heat pump
        std::string IHPCoilName("");

        // Following allow for temporary storage of character strings but not saved in main structure
        Real64 rho; // local fluid density
        static int DummyWaterIndex(1);

        struct WaterHeaterSaveNodes
        {
            // Members
            std::string InletNodeName1;
            std::string OutletNodeName1;
            std::string InletNodeName2;
            std::string OutletNodeName2;

            // Default Constructor
            WaterHeaterSaveNodes()
            {
            }
        };

        // Object Data
        Array1D<WaterHeaterSaveNodes> HPWHSaveNodeNames; // temporary for HPWH node names used in later checks
        Array1D<WaterHeaterSaveNodes> WHSaveNodeNames;   // temporary for WH node names used in later checks
        Array1D<WaterHeaterSaveNodes> CoilSaveNodeNames; // temporary for coil node names used in later checks

        // Formats
        static gio::Fmt Format_720("('! <Water Heater Information>,Type,Name,Volume {m3},Maximum Capacity {W},Standard Rated Recovery Efficiency, "
                                   "','Standard Rated Energy Factor')");
        static gio::Fmt Format_721("('! <Heat Pump Water Heater Information>,Type,Name,Volume {m3},Maximum Capacity {W},','Standard Rated Recovery "
                                   "Efficiency,Standard Rated Energy Factor,\"DX Coil Total Cooling Rate {W, HPWH Only}\"')");
        static gio::Fmt Format_722("('! <Water Heater Stratified Node Information>,Node Number,Height {m},Volume {m3},Maximum Capacity "
                                   "{W},','Off-Cycle UA {W/K},On-Cycle UA {W/K},Number Of Inlets,Number Of Outlets')");
        static gio::Fmt Format_725(
            "('! <Chilled Water Tank Information>,Type,Name,Volume {m3},Use Side Design Flow Rate {m3/s}, ','Source Side Design Flow Rate {m3/s}')");
        static gio::Fmt Format_726("('! <Chilled Water Tank Stratified Node Information>,Node Number,Height {m},Volume {m3},','UA {W/K},Number Of "
                                   "Inlets,Number Of Outlets')");
        static gio::Fmt Format_723("('Water Heater Stratified Node Information',8(',',A))");
        static gio::Fmt Format_724("('Chilled Water Tank Stratified Node Information',6(',',A))");

        // FLOW:

        // Make sure refrigeration input is gotten before this input
        CheckRefrigerationInput();

        if (GetWaterThermalTankInputFlag) {
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
                if ((NumWaterHeaterMixed > 0) || (NumWaterHeaterStratified > 0)) gio::write(OutputFileInits, Format_720);
                if (NumHeatPumpWaterHeater > 0) gio::write(OutputFileInits, Format_721);
                if (NumWaterHeaterStratified > 0) gio::write(OutputFileInits, Format_722);
                if (NumChilledWaterMixed > 0) gio::write(OutputFileInits, Format_725);
                if (NumChilledWaterStratified > 0) gio::write(OutputFileInits, Format_726);
            }

            if (NumWaterThermalTank > 0) {
                WaterThermalTank.allocate(NumWaterThermalTank);
                UniqueWaterThermalTankNames.reserve(static_cast<unsigned>(NumWaterThermalTank));
                WHSaveNodeNames.allocate(NumWaterThermalTank);
                CheckWTTEquipName.dimension(NumWaterThermalTank, true);
            }
            if (NumHeatPumpWaterHeater > 0) {
                HPWaterHeater.allocate(NumHeatPumpWaterHeater);
                MyHPSizeFlag.dimension(NumHeatPumpWaterHeater, true);
                CheckHPWHEquipName.dimension(NumHeatPumpWaterHeater, true);
                HPWHSaveNodeNames.allocate(NumHeatPumpWaterHeater);

                for (IHPIndex = 1; IHPIndex <= NumHeatPumpWaterHeater; ++IHPIndex)
                    HPWaterHeater(IHPIndex).bIsIHP = false; // clear the IHP flag
                IHPIndex = 0;
            }

            if (NumWaterHeaterDesuperheater > 0) {
                WaterHeaterDesuperheater.allocate(NumWaterHeaterDesuperheater);
                ValidSourceType.dimension(NumWaterHeaterDesuperheater, false);
                CoilSaveNodeNames.allocate(NumWaterHeaterDesuperheater);
            }

            //!!=======   Get Coil:WaterHeating:Desuperheater ======================================================================
            if (NumWaterHeaterDesuperheater > 0) {
                cCurrentModuleObject = "Coil:WaterHeating:Desuperheater";
                for (DesuperheaterNum = 1; DesuperheaterNum <= NumWaterHeaterDesuperheater; ++DesuperheaterNum) {

                    inputProcessor->getObjectItem(cCurrentModuleObject,
                                                  DesuperheaterNum,
                                                  cAlphaArgs,
                                                  NumAlphas,
                                                  rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  lNumericFieldBlanks,
                                                  lAlphaFieldBlanks,
                                                  cAlphaFieldNames,
                                                  cNumericFieldNames);
                    UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                    VerifyUniqueCoilName(cCurrentModuleObject, cAlphaArgs(1), errFlag, cCurrentModuleObject + " Name");
                    if (errFlag) {
                        ErrorsFound = true;
                    }
                    WaterHeaterDesuperheater(DesuperheaterNum).Name = cAlphaArgs(1);
                    WaterHeaterDesuperheater(DesuperheaterNum).Type = cCurrentModuleObject;

                    //       convert availability schedule name to pointer
                    if (!lAlphaFieldBlanks(2)) {
                        WaterHeaterDesuperheater(DesuperheaterNum).AvailSchedPtr = GetScheduleIndex(cAlphaArgs(2));
                        if (WaterHeaterDesuperheater(DesuperheaterNum).AvailSchedPtr == 0) {
                            ShowSevereError("Invalid, " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                            ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                    } else {
                        WaterHeaterDesuperheater(DesuperheaterNum).AvailSchedPtr = ScheduleAlwaysOn;
                    }

                    //       convert schedule name to pointer
                    WaterHeaterDesuperheater(DesuperheaterNum).SetPointTempSchedule = GetScheduleIndex(cAlphaArgs(3));
                    if (WaterHeaterDesuperheater(DesuperheaterNum).SetPointTempSchedule == 0) {
                        ShowSevereError("Invalid, " + cAlphaFieldNames(3) + " = " + cAlphaArgs(3));
                        ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));
                        ErrorsFound = true;
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).DeadBandTempDiff = rNumericArgs(1);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).DeadBandTempDiff <= 0.0 ||
                        WaterHeaterDesuperheater(DesuperheaterNum).DeadBandTempDiff > 20.0) {
                        ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        cNumericFieldNames(1) + " must be > 0 and <= 20. " + cNumericFieldNames(1) + " = " +
                                        TrimSigDigits(rNumericArgs(1), 1));
                        ErrorsFound = true;
                    }

                    // WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff       = rNumericArgs(2)
                    // Error limits on heat reclaim efficiency applied after source type identified

                    WaterHeaterDesuperheater(DesuperheaterNum).RatedInletWaterTemp = rNumericArgs(3);
                    WaterHeaterDesuperheater(DesuperheaterNum).RatedOutdoorAirTemp = rNumericArgs(4);
                    WaterHeaterDesuperheater(DesuperheaterNum).MaxInletWaterTemp = rNumericArgs(5);

                    if (!lAlphaFieldBlanks(4)) {
                        WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp = GetCurveIndex(cAlphaArgs(4));
                        if (WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ":  " +
                                            cAlphaFieldNames(4) + " not found = " + cAlphaArgs(4));
                            ErrorsFound = true;
                        } else {
                            // Verify Curve Object, only legal type is Quadratic
                            {
                                auto const SELECT_CASE_var(GetCurveType(WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp));

                                if (SELECT_CASE_var == "BIQUADRATIC") {

                                    if (WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp > 0) {
                                        HEffFTemp = min(1.0,
                                                        max(0.0,
                                                            CurveValue(WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp,
                                                                       WaterHeaterDesuperheater(DesuperheaterNum).RatedInletWaterTemp,
                                                                       WaterHeaterDesuperheater(DesuperheaterNum).RatedOutdoorAirTemp)));
                                        if (std::abs(HEffFTemp - 1.0) > 0.05) {
                                            ShowWarningError(cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name + "\":");
                                            ShowContinueError("The " + cAlphaFieldNames(4) + " should be normalized ");
                                            ShowContinueError(" to 1.0 at the rating point. Curve output at the rating point = " +
                                                              TrimSigDigits(HEffFTemp, 3));
                                            ShowContinueError(" The simulation continues using the user-specified curve.");
                                        }
                                    }

                                } else {
                                    ShowSevereError(cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name + "\" illegal " +
                                                    cAlphaFieldNames(4) +
                                                    " type for this object = " + GetCurveType(WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp));
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).WaterInletNode = GetOnlySingleNode(
                        cAlphaArgs(5), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsParent);

                    WaterHeaterDesuperheater(DesuperheaterNum).WaterOutletNode = GetOnlySingleNode(cAlphaArgs(6),
                                                                                                   ErrorsFound,
                                                                                                   cCurrentModuleObject,
                                                                                                   cAlphaArgs(1),
                                                                                                   NodeType_Water,
                                                                                                   NodeConnectionType_Outlet,
                                                                                                   1,
                                                                                                   ObjectIsParent);

                    CoilSaveNodeNames(DesuperheaterNum).InletNodeName1 = cAlphaArgs(5);
                    CoilSaveNodeNames(DesuperheaterNum).OutletNodeName1 = cAlphaArgs(6);

                    WaterHeaterDesuperheater(DesuperheaterNum).TankType = cAlphaArgs(7);

                    if (!UtilityRoutines::SameString(WaterHeaterDesuperheater(DesuperheaterNum).TankType, cMixedWHModuleObj) &&
                        !UtilityRoutines::SameString(WaterHeaterDesuperheater(DesuperheaterNum).TankType, cStratifiedWHModuleObj)) {

                        ShowSevereError(cCurrentModuleObject + " = " + HPWaterHeater(DesuperheaterNum).Name + ':');
                        ShowContinueError("Desuperheater can only be used with " + cMixedWHModuleObj + " or " + cStratifiedWHModuleObj + '.');
                        ErrorsFound = true;
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).TankName = cAlphaArgs(8);

                    //       get heat reclaim object
                    if (UtilityRoutines::SameString(cAlphaArgs(9), "Coil:Cooling:DX:SingleSpeed") ||
                        UtilityRoutines::SameString(cAlphaArgs(9), "Coil:Cooling:DX:TwoSpeed") ||
                        UtilityRoutines::SameString(cAlphaArgs(9), "Coil:Cooling:DX:TwoStageWithHumidityControlMode")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceType = cAlphaArgs(9);
                        WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName = cAlphaArgs(10);
                        //         load DX coil structure for connection to desuperheater heating coil (refrigerated rack have been loaded)
                        errFlag = false;
                        GetDXCoilIndex(WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName, HeatingSourceNum, errFlag, cCurrentModuleObject);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + cCurrentModuleObject + '=' + WaterHeaterDesuperheater(DesuperheaterNum).Name);
                            ErrorsFound = true;
                        }
                    } else if (UtilityRoutines::SameString(cAlphaArgs(9), "Coil:Cooling:DX:VariableSpeed")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceType = cAlphaArgs(9);
                        WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName = cAlphaArgs(10);
                        errFlag = false;
                        HeatingSourceNum = VariableSpeedCoils::GetCoilIndexVariableSpeed(cAlphaArgs(9), cAlphaArgs(10), errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in " + cCurrentModuleObject + '=' + WaterHeaterDesuperheater(DesuperheaterNum).Name);
                            ErrorsFound = true;
                        }
                    } else if ((UtilityRoutines::SameString(cAlphaArgs(9), "Refrigeration:CompressorRack")) ||
                               (UtilityRoutines::SameString(cAlphaArgs(9), "Refrigeration:Condenser:AirCooled")) ||
                               (UtilityRoutines::SameString(cAlphaArgs(9), "Refrigeration:Condenser:EvaporativeCooled")) ||
                               (UtilityRoutines::SameString(cAlphaArgs(9), "Refrigeration:Condenser:WaterCooled"))) {
                        WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceType = cAlphaArgs(9);
                        WaterHeaterDesuperheater(DesuperheaterNum).HeatingSourceName = cAlphaArgs(10);
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ':');
                        ShowContinueError(" desuperheater can only be used with Coil:Cooling:DX:SingleSpeed, ");
                        ShowContinueError(
                            " Coil:Cooling:DX:TwoSpeed, Coil:Cooling:DX:TwoStageWithHumidityControlMode, Refrigeration:CompressorRack,");
                        ShowContinueError(" Refrigeration:Condenser:AirCooled ,Refrigeration:Condenser:EvaporativeCooled, ");
                        ShowContinueError(" or Refrigeration:Condenser:WaterCooled.");
                        ErrorsFound = true;
                    }

                    //       Set up comp set for water side nodes (reverse inlet/outlet for water heater)
                    SetUpCompSets(WaterHeaterDesuperheater(DesuperheaterNum).Type,
                                  WaterHeaterDesuperheater(DesuperheaterNum).Name,
                                  WaterHeaterDesuperheater(DesuperheaterNum).TankType,
                                  WaterHeaterDesuperheater(DesuperheaterNum).TankName,
                                  cAlphaArgs(6),
                                  cAlphaArgs(5));

                    //       Find the DX equipment index associated with the desuperheater heating coil.
                    if (UtilityRoutines::SameString(cAlphaArgs(9), "Refrigeration:CompressorRack")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = COMPRESSORRACK_REFRIGERATEDCASE;
                        for (RackNum = 1; RackNum <= NumRefrigeratedRacks; ++RackNum) {
                            if (!UtilityRoutines::SameString(HeatReclaimRefrigeratedRack(RackNum).Name, cAlphaArgs(10))) continue;
                            WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = RackNum;
                            if (allocated(HeatReclaimRefrigeratedRack)) ValidSourceType(DesuperheaterNum) = true;
                            break;
                        }
                        if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum == 0) {
                            ShowSevereError(cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                            "\" desuperheater heat source object not found: " + cAlphaArgs(9) + " \"" + cAlphaArgs(10) + "\"");
                            ErrorsFound = true;
                        }
                    } else if ((UtilityRoutines::SameString(cAlphaArgs(9), "Refrigeration:Condenser:AirCooled")) ||
                               (UtilityRoutines::SameString(cAlphaArgs(9), "Refrigeration:Condenser:EvaporativeCooled")) ||
                               (UtilityRoutines::SameString(cAlphaArgs(9), "Refrigeration:Condenser:WaterCooled"))) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = CONDENSER_REFRIGERATION;
                        for (CondNum = 1; CondNum <= NumRefrigCondensers; ++CondNum) {
                            if (!UtilityRoutines::SameString(HeatReclaimRefrigCondenser(CondNum).Name, cAlphaArgs(10))) continue;
                            WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = CondNum;
                            if (allocated(HeatReclaimRefrigCondenser)) ValidSourceType(DesuperheaterNum) = true;
                            break;
                        }
                        if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum == 0) {
                            ShowSevereError(cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                            "\" desuperheater heat source object not found: " + cAlphaArgs(9) + " \"" + cAlphaArgs(10) + "\"");
                            ErrorsFound = true;
                        }
                    } else if (UtilityRoutines::SameString(cAlphaArgs(9), "Coil:Cooling:DX:SingleSpeed")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = COIL_DX_COOLING;
                        for (DXCoilNum = 1; DXCoilNum <= NumDXCoils; ++DXCoilNum) {
                            if (!UtilityRoutines::SameString(HeatReclaimDXCoil(DXCoilNum).Name, cAlphaArgs(10))) continue;
                            WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = DXCoilNum;
                            if (allocated(HeatReclaimDXCoil)) ValidSourceType(DesuperheaterNum) = true;
                            break;
                        }
                        if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum == 0) {
                            ShowSevereError(cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                            "\" desuperheater heat source object not found: " + cAlphaArgs(9) + " \"" + cAlphaArgs(10) + "\"");
                            ErrorsFound = true;
                        }
                    } else if (UtilityRoutines::SameString(cAlphaArgs(9), "Coil:Cooling:DX:TwoSpeed")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = COIL_DX_MULTISPEED;
                        for (DXCoilNum = 1; DXCoilNum <= NumDXCoils; ++DXCoilNum) {
                            if (!UtilityRoutines::SameString(HeatReclaimDXCoil(DXCoilNum).Name, cAlphaArgs(10))) continue;
                            WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = DXCoilNum;
                            if (allocated(HeatReclaimDXCoil)) ValidSourceType(DesuperheaterNum) = true;
                            break;
                        }
                        if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum == 0) {
                            ShowSevereError(cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                            "\" desuperheater heat source object not found: " + cAlphaArgs(9) + " \"" + cAlphaArgs(10) + "\"");
                            ErrorsFound = true;
                        }
                    } else if (UtilityRoutines::SameString(cAlphaArgs(9), "Coil:Cooling:DX:TwoStageWithHumidityControlMode")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = COIL_DX_MULTIMODE;
                        for (DXCoilNum = 1; DXCoilNum <= NumDXCoils; ++DXCoilNum) {
                            if (!UtilityRoutines::SameString(HeatReclaimDXCoil(DXCoilNum).Name, cAlphaArgs(10))) continue;
                            WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = DXCoilNum;
                            if (allocated(HeatReclaimDXCoil)) ValidSourceType(DesuperheaterNum) = true;
                            break;
                        }
                        if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum == 0) {
                            ShowSevereError(cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                            "\" desuperheater heat source object not found: " + cAlphaArgs(9) + " \"" + cAlphaArgs(10) + "\"");
                            ErrorsFound = true;
                        }
                    } else if (UtilityRoutines::SameString(cAlphaArgs(9), "Coil:Cooling:DX:VariableSpeed")) {
                        WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource = COIL_DX_VARIABLE_COOLING;
                        for (DXCoilNum = 1; DXCoilNum <= VariableSpeedCoils::NumVarSpeedCoils; ++DXCoilNum) {
                            if (!UtilityRoutines::SameString(DataHeatBalance::HeatReclaimVS_DXCoil(DXCoilNum).Name, cAlphaArgs(10))) continue;
                            WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum = DXCoilNum;
                            if (allocated(DataHeatBalance::HeatReclaimVS_DXCoil)) ValidSourceType(DesuperheaterNum) = true;
                            break;
                        }
                        if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum == 0) {
                            ShowSevereError(cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                            "\" desuperheater heat source object not found: " + cAlphaArgs(9) + " \"" + cAlphaArgs(10) + "\"");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + ", \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                        "\" invalid desuperheater heat source object: " + cAlphaArgs(9) + " \"" + cAlphaArgs(10) + "\"");
                        ErrorsFound = true;
                    }

                    // Now have source type, so set limits on heat recovery efficiency
                    if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == CONDENSER_REFRIGERATION) {
                        if (lNumericFieldBlanks(2)) {
                            WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff = 0.8;
                        } else {
                            WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff = rNumericArgs(2);
                            if (WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff <= 0.0 ||
                                WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff > 0.9) {
                                ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                                cNumericFieldNames(2) + " must be > 0.0 and <= 0.9, Efficiency = " +
                                                TrimSigDigits(WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff, 3));
                                ErrorsFound = true;
                            }
                        }    // Blank Num(2)
                    } else { // max is 0.3 for all other sources
                        if (lNumericFieldBlanks(2)) {
                            WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff = 0.25;
                        } else {
                            WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff = rNumericArgs(2);
                            if (WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff <= 0.0 ||
                                WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff > 0.3) {
                                ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                                cNumericFieldNames(2) + " must be > 0.0 and <= 0.3, " + cNumericFieldNames(2) + " = " +
                                                TrimSigDigits(WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff, 3));
                                ErrorsFound = true;
                            }
                        } // Blank Num(2)
                    }     // setting limits on heat recovery efficiency

                    WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate = rNumericArgs(6);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate <= 0.0) {
                        ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        cNumericFieldNames(6) + " must be greater than 0. " + cNumericFieldNames(6) + " = " +
                                        TrimSigDigits(rNumericArgs(6), 6));
                        ErrorsFound = true;
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower = rNumericArgs(7);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower < 0.0) {
                        ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        cNumericFieldNames(7) + " must be >= 0. " + cNumericFieldNames(7) + " = " +
                                        TrimSigDigits(rNumericArgs(7), 2));
                        ErrorsFound = true;
                    }

                    if ((WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower /
                         WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate) > 7.9264e6) {
                        ShowWarningError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                         cNumericFieldNames(7) + " to " + cNumericFieldNames(6) + " ratio > 7.9264E6. " + cNumericFieldNames(7) +
                                         " to " + cNumericFieldNames(6) + " = " +
                                         TrimSigDigits((WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower /
                                                        WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate),
                                                       3));
                        ShowContinueError(" Suggest reducing " + cNumericFieldNames(7) + " or increasing " + cNumericFieldNames(6) + '.');
                        ShowContinueError(" The simulation will continue using the user defined values.");
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).PumpFracToWater = rNumericArgs(8);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).PumpFracToWater < 0.0 ||
                        WaterHeaterDesuperheater(DesuperheaterNum).PumpFracToWater > 1.0) {
                        ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        cNumericFieldNames(8) + " must be >= 0 or <= 1. " + cNumericFieldNames(8) + " = " +
                                        TrimSigDigits(rNumericArgs(8), 3));
                        ErrorsFound = true;
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaLoad = rNumericArgs(9);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaLoad < 0.0) {
                        ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        cNumericFieldNames(9) + " must be >= 0. " + cNumericFieldNames(9) + " = " +
                                        TrimSigDigits(rNumericArgs(9), 2));
                        ErrorsFound = true;
                    }

                    WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaLoad = rNumericArgs(10);
                    if (WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaLoad < 0.0) {
                        ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ": " +
                                        cNumericFieldNames(10) + " must be >= 0. " + cNumericFieldNames(10) + " = " +
                                        TrimSigDigits(rNumericArgs(10), 2));
                        ErrorsFound = true;
                    }
                }

                if (ErrorsFound) {
                    ShowFatalError("Errors found in getting " + cCurrentModuleObject + " input. Preceding condition causes termination.");
                }
            }

            //!!=======   Get HEAT PUMP:WATER HEATER ===============================================================================

            //   get input for heat pump water heater object
            if (NumHeatPumpWaterHeater > 0) {
                int const NumPumpedCondenser =
                    inputProcessor->getNumObjectsFound(cHPWHPumpedCondenser); // number of WaterHeater:HeatPump:PumpedCondenser objects
                int nAlphaOffset;            // the difference of array location between alpha items between pumped and wrapped condensers
                int nNumericOffset;          // the difference of array location between numeric items between pumped and wrapped condensers
                int nNumPossibleNumericArgs; // the number of possible numeric arguments in the idd
                int nNumPossibleAlphaArgs;   // the number of possible numeric arguments in the idd

                for (HPWaterHeaterNum = 1; HPWaterHeaterNum <= NumHeatPumpWaterHeater; ++HPWaterHeaterNum) {

                    // Create reference to current HPWH object in array.
                    HeatPumpWaterHeaterData &HPWH = HPWaterHeater(HPWaterHeaterNum);
                    WaterHeaterSaveNodes &HPWHSaveNode = HPWHSaveNodeNames(HPWaterHeaterNum);

                    // Initialize the offsets to zero
                    nAlphaOffset = 0;
                    nNumericOffset = 0;
                    if (HPWaterHeaterNum <= NumPumpedCondenser) {
                        // Pumped Condenser
                        cCurrentModuleObject = cHPWHPumpedCondenser;
                        HPWH.TypeNum = TypeOf_HeatPumpWtrHeaterPumped;
                        nNumPossibleAlphaArgs = 29;
                        nNumPossibleNumericArgs = 9;
                    } else {
                        // Wrapped Condenser
                        cCurrentModuleObject = cHPWHWrappedCondenser;
                        HPWH.TypeNum = TypeOf_HeatPumpWtrHeaterWrapped;
                        nNumPossibleAlphaArgs = 27;
                        nNumPossibleNumericArgs = 10;
                    }

                    // Get the lists of IDF arguments
                    inputProcessor->getObjectItem(cCurrentModuleObject,
                                                  HPWaterHeaterNum,
                                                  cAlphaArgs,
                                                  NumAlphas,
                                                  rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  lNumericFieldBlanks,
                                                  lAlphaFieldBlanks,
                                                  cAlphaFieldNames,
                                                  cNumericFieldNames);

                    // Copy those lists into C++ std::maps
                    std::map<int, std::string> hpwhAlpha;
                    std::map<int, Real64> hpwhNumeric;
                    std::map<int, bool> hpwhAlphaBlank;
                    std::map<int, bool> hpwhNumericBlank;
                    std::map<int, std::string> hpwhAlphaFieldNames;
                    std::map<int, std::string> hpwhNumericFieldNames;
                    for (int i = 1; i <= NumNums; ++i) {
                        hpwhNumeric[i] = rNumericArgs(i);
                        hpwhNumericBlank[i] = lNumericFieldBlanks(i);
                        hpwhNumericFieldNames[i] = cNumericFieldNames(i);
                    }
                    for (int i = NumNums + 1; i <= nNumPossibleNumericArgs; ++i) {
                        hpwhNumericBlank[i] = true;
                    }
                    for (int i = 1; i <= NumAlphas; ++i) {
                        hpwhAlpha[i] = cAlphaArgs(i);
                        hpwhAlphaBlank[i] = lAlphaFieldBlanks(i);
                        hpwhAlphaFieldNames[i] = cAlphaFieldNames(i);
                    }
                    for (int i = NumAlphas + 1; i <= nNumPossibleAlphaArgs; ++i) {
                        hpwhAlphaBlank[i] = true;
                    }
                    UtilityRoutines::IsNameEmpty(hpwhAlpha[1], cCurrentModuleObject, ErrorsFound);

                    // Name and type
                    HPWH.Name = hpwhAlpha[1];
                    HPWH.Type = cCurrentModuleObject;

                    // Availability Schedule
                    // convert schedule name to pointer
                    if (!hpwhAlphaBlank[2]) {
                        HPWH.AvailSchedPtr = GetScheduleIndex(hpwhAlpha[2]);
                        if (HPWH.AvailSchedPtr == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                            ShowContinueError(hpwhAlphaFieldNames[2] + "=\"" + hpwhAlpha[2] + "\".");
                            ErrorsFound = true;
                        }
                    } else {
                        HPWH.AvailSchedPtr = ScheduleAlwaysOn;
                    }

                    // Compressor Setpoint Temperature Schedule
                    // convert schedule name to pointer
                    if (!hpwhAlphaBlank[3]) {
                        HPWH.SetPointTempSchedule = GetScheduleIndex(hpwhAlpha[3]);
                        if (HPWH.SetPointTempSchedule == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                            ShowContinueError(hpwhAlphaFieldNames[3] + "=\"" + hpwhAlpha[3] + "\".");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                        ShowContinueError("required " + hpwhAlphaFieldNames[3] + " is blank.");
                        ErrorsFound = true;
                    }

                    // Dead Band Temperature Difference
                    HPWH.DeadBandTempDiff = hpwhNumeric[1 + nNumericOffset];
                    if (HPWH.DeadBandTempDiff <= 0.0 || HPWH.DeadBandTempDiff > 20.0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                        ShowContinueError(hpwhNumericFieldNames[1 + nNumericOffset] +
                                          " difference must be > 0 and <= 20. Dead band = " + TrimSigDigits(hpwhNumeric[1 + nNumericOffset], 1));
                        ErrorsFound = true;
                    }

                    if (HPWH.TypeNum == TypeOf_HeatPumpWtrHeaterPumped) {

                        // Condenser Inlet/Outlet Nodes
                        HPWH.CondWaterInletNode = GetOnlySingleNode(
                            hpwhAlpha[4], ErrorsFound, cCurrentModuleObject, HPWH.Name, NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsParent);
                        HPWHSaveNode.InletNodeName1 = hpwhAlpha[4];
                        HPWH.CondWaterOutletNode = GetOnlySingleNode(
                            hpwhAlpha[5], ErrorsFound, cCurrentModuleObject, HPWH.Name, NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsParent);
                        HPWHSaveNode.OutletNodeName1 = hpwhAlpha[5];

                        // Condenser Water Flow Rate
                        HPWH.OperatingWaterFlowRate = hpwhNumeric[2];
                        if (HPWH.OperatingWaterFlowRate <= 0.0 && hpwhNumeric[2] != AutoCalculate) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                            ShowContinueError(hpwhNumericFieldNames[2] +
                                              " must be greater than 0. Condenser water flow rate = " + TrimSigDigits(hpwhNumeric[2], 6));
                            ErrorsFound = true;
                        }

                    } else if (HPWH.TypeNum == TypeOf_HeatPumpWtrHeaterWrapped) {

                        // Wrapped Condenser Location
                        HPWH.WrappedCondenserBottomLocation = hpwhNumeric[2 + nNumericOffset];
                        HPWH.WrappedCondenserTopLocation = hpwhNumeric[3 + nNumericOffset];

                        if (HPWH.WrappedCondenserBottomLocation < 0.0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                            ShowContinueError(hpwhNumericFieldNames[2] + " must be greater than 0. Condenser bottom location = " +
                                              TrimSigDigits(HPWH.WrappedCondenserBottomLocation, 6));
                            ErrorsFound = true;
                        }

                        if (HPWH.WrappedCondenserBottomLocation >= HPWH.WrappedCondenserTopLocation) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                            ShowContinueError(hpwhNumericFieldNames[3] + " (" + TrimSigDigits(HPWH.WrappedCondenserTopLocation, 6) +
                                              ") must be greater than " + hpwhNumericFieldNames[2] + " (" +
                                              TrimSigDigits(HPWH.WrappedCondenserBottomLocation, 6) + ").");
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
                    if (HPWH.OperatingAirFlowRate <= 0.0 && hpwhNumeric[3 + nNumericOffset] != AutoCalculate) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                        ShowContinueError(hpwhNumericFieldNames[3 + nNumericOffset] +
                                          " must be greater than 0. Evaporator air flow rate = " + TrimSigDigits(hpwhNumeric[3 + nNumericOffset], 6));
                        ErrorsFound = true;
                    }

                    // Inlet Air Configuration
                    {
                        auto const SELECT_CASE_var(hpwhAlpha[6 + nAlphaOffset]);

                        if (SELECT_CASE_var == "SCHEDULE") {
                            HPWH.InletAirConfiguration = AmbientTempSchedule;

                            // Inlet Air Temperature Schedule
                            if (!hpwhAlphaBlank[11 + nAlphaOffset]) {
                                HPWH.AmbientTempSchedule = GetScheduleIndex(hpwhAlpha[11 + nAlphaOffset]);
                                if (HPWH.AmbientTempSchedule == 0) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                    ShowContinueError(hpwhAlphaFieldNames[11 + nAlphaOffset] + "=\"" + hpwhAlpha[11 + nAlphaOffset] + "\".");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                                ShowContinueError("required " + hpwhAlphaFieldNames[11 + nAlphaOffset] + " is blank.");
                                ErrorsFound = true;
                            }

                            // Inlet Air Humidity Schedule
                            if (!hpwhAlphaBlank[12 + nAlphaOffset]) {
                                HPWH.AmbientRHSchedule = GetScheduleIndex(hpwhAlpha[12 + nAlphaOffset]);
                                if (HPWH.AmbientRHSchedule == 0) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                    ShowContinueError(hpwhAlphaFieldNames[12 + nAlphaOffset] + "=\"" + hpwhAlpha[12 + nAlphaOffset] + "\".");
                                    ErrorsFound = true;
                                } else {
                                    if (!CheckScheduleValueMinMax(HPWH.AmbientRHSchedule, ">=", 0.0, "<=", 1.0)) {
                                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", invalid values");
                                        ShowContinueError(hpwhAlphaFieldNames[12 + nAlphaOffset] + "=\"" + hpwhAlpha[12 + nAlphaOffset] +
                                                          "\", schedule values must be (>=0., <=1.)");
                                        ErrorsFound = true;
                                    }
                                }
                            } else {
                                ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                                ShowContinueError("required " + hpwhAlphaFieldNames[12 + nAlphaOffset] + " is blank.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONEAIRONLY") {
                            HPWH.InletAirConfiguration = AmbientTempZone;

                            // Inlet Air Zone
                            if (!hpwhAlphaBlank[13 + nAlphaOffset]) {
                                HPWH.AmbientTempZone = UtilityRoutines::FindItemInList(hpwhAlpha[13 + nAlphaOffset], Zone);
                                if (HPWH.AmbientTempZone == 0) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                    ShowContinueError(hpwhAlphaFieldNames[13 + nAlphaOffset] + "=\"" + hpwhAlpha[13 + nAlphaOffset] + "\".");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                                ShowContinueError("required " + hpwhAlphaFieldNames[13 + nAlphaOffset] + " is blank.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "OUTDOORAIRONLY") {
                            HPWH.InletAirConfiguration = AmbientTempOutsideAir;

                        } else if (SELECT_CASE_var == "ZONEANDOUTDOORAIR") {
                            HPWH.InletAirConfiguration = AmbientTempZoneAndOA;

                            // Inlet Air Zone
                            if (!hpwhAlphaBlank[13 + nAlphaOffset]) {
                                HPWH.AmbientTempZone = UtilityRoutines::FindItemInList(hpwhAlpha[13 + nAlphaOffset], Zone);
                                if (HPWH.AmbientTempZone == 0) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                    ShowContinueError(hpwhAlphaFieldNames[13 + nAlphaOffset] + "=\"" + hpwhAlpha[13 + nAlphaOffset] + "\".");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                                ShowContinueError("required " + hpwhAlphaFieldNames[13 + nAlphaOffset] + " is blank.");
                                ErrorsFound = true;
                            }
                        }
                    }

                    // Read air inlet nodes after mixer/splitter nodes have been read in (cAlphaArgs 7-10),
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
                        HPWH.WHUseInletNode = GetOnlySingleNode(HPWHSaveNode.InletNodeName2,
                                                                ErrorsFound,
                                                                cCurrentModuleObject,
                                                                HPWH.Name,
                                                                NodeType_Water,
                                                                NodeConnectionType_Inlet,
                                                                1,
                                                                ObjectIsParent);
                        HPWH.WHUseOutletNode = GetOnlySingleNode(HPWHSaveNode.OutletNodeName2,
                                                                 ErrorsFound,
                                                                 cCurrentModuleObject,
                                                                 HPWH.Name,
                                                                 NodeType_Water,
                                                                 NodeConnectionType_Outlet,
                                                                 1,
                                                                 ObjectIsParent);
                    }

                    // DX Coil
                    // get Coil:DX:HeatPumpWaterHeater object
                    HPWH.DXCoilName = hpwhAlpha[19 + nAlphaOffset];
                    HPWH.DXCoilType = hpwhAlpha[18 + nAlphaOffset];

                    // check that the DX Coil exists
                    DXCoilErrFlag = false;

                    GetDXCoilIndex(HPWH.DXCoilName, HPWH.DXCoilNum, DXCoilErrFlag, cCurrentModuleObject, true);
                    if (DXCoilErrFlag) {
                        // This could be a variable speed heat pump water heater
                        bool bVSCoilErrFlag = false;

                        bool checkIHPFirst = IntegratedHeatPump::IHPInModel();
                        if (checkIHPFirst) {
                            HPWH.DXCoilNum = GetCoilIndexIHP("COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE", HPWH.DXCoilName, bVSCoilErrFlag);

                            if (!bVSCoilErrFlag) {
                                HPWH.bIsIHP = true;
                            }
                        }

                        if (bVSCoilErrFlag || !checkIHPFirst) {
                            bVSCoilErrFlag = false;
                            HPWH.DXCoilNum =
                                GetCoilIndexVariableSpeed("Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed", HPWH.DXCoilName, bVSCoilErrFlag);

                            if (bVSCoilErrFlag) {
                                ShowContinueError("...occurs in " + cCurrentModuleObject + " =" + HPWH.Name);
                                ShowContinueError("...could not find either DXCoil or Variable Speed Coil " + HPWH.DXCoilName);
                                ErrorsFound = true;
                            }
                        }

                        bIsVScoil = true;
                        HPWH.DXCoilTypeNum = 0;
                        if (HPWH.bIsIHP) {
                            HPWH.DXCoilType = "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE";
                        } else {
                            HPWH.DXCoilType = VarSpeedCoil(HPWH.DXCoilNum).VarSpeedCoilType;
                        }
                    } else {
                        // this is a single speed coil
                        DXCoils::DXCoilData &Coil = DXCoil(HPWH.DXCoilNum);
                        if (!UtilityRoutines::SameString(HPWH.DXCoilType, Coil.DXCoilType)) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                            ShowContinueError("specifies the coil " + HPWH.DXCoilType + "=\"" + HPWH.DXCoilName + "\".");
                            ShowContinueError("However, " + HPWH.DXCoilName + " is a coil of type " + Coil.DXCoilType + ".");
                            ErrorsFound = true;
                        }
                        HPWH.DXCoilTypeNum = Coil.DXCoilType_Num;
                    }

                    // Make sure that the coil and tank are compatible.
                    if (bIsVScoil) {
                        if (HPWH.TypeNum != TypeOf_HeatPumpWtrHeaterPumped) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed can only be used with a pumped condenser heat pump "
                                              "water heater.");
                            ErrorsFound = true;
                        }
                    } else {
                        if (!((HPWH.DXCoilTypeNum == DataHVACGlobals::CoilDX_HeatPumpWaterHeaterPumped &&
                               HPWH.TypeNum == TypeOf_HeatPumpWtrHeaterPumped) ||
                              (HPWH.DXCoilTypeNum == DataHVACGlobals::CoilDX_HeatPumpWaterHeaterWrapped &&
                               HPWH.TypeNum == TypeOf_HeatPumpWtrHeaterWrapped))) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            std::string ExpectedCoilType;
                            if (HPWH.TypeNum == TypeOf_HeatPumpWtrHeaterPumped) {
                                ExpectedCoilType = DataHVACGlobals::cAllCoilTypes(DataHVACGlobals::CoilDX_HeatPumpWaterHeaterPumped);
                            } else if (HPWH.TypeNum == TypeOf_HeatPumpWtrHeaterWrapped) {
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
                        DXCoils::DXCoilData &Coil = DXCoil(HPWH.DXCoilNum);

                        HPWHSaveNode.InletNodeName1 = "DUMMY CONDENSER INLET " + Coil.Name;
                        HPWH.CondWaterInletNode = GetOnlySingleNode(HPWHSaveNode.InletNodeName1,
                                                                    ErrorsFound,
                                                                    cCurrentModuleObject,
                                                                    HPWH.Name,
                                                                    NodeType_Water,
                                                                    NodeConnectionType_Inlet,
                                                                    2,
                                                                    ObjectIsParent);
                        HPWHSaveNode.OutletNodeName1 = "DUMMY CONDENSER OUTLET " + Coil.Name;
                        HPWH.CondWaterOutletNode = GetOnlySingleNode(HPWHSaveNode.OutletNodeName1,
                                                                     ErrorsFound,
                                                                     cCurrentModuleObject,
                                                                     HPWH.Name,
                                                                     NodeType_Water,
                                                                     NodeConnectionType_Outlet,
                                                                     2,
                                                                     ObjectIsParent);
                    }

                    // Minimum Inlet Air Temperature for Compressor Operation
                    HPWH.MinAirTempForHPOperation = hpwhNumeric[4 + nNumericOffset];
                    if (HPWH.MinAirTempForHPOperation < -5) {
                        ShowWarningError(
                            cCurrentModuleObject + "=\"" + HPWH.Name +
                            "\": minimum inlet air temperature for heat pump compressor operation must be greater than or equal to -5 C.");
                        ShowContinueError("...Minimum inlet air temperature = " + TrimSigDigits(hpwhNumeric[4 + nNumericOffset], 1));
                    }

                    // Maximum Inlet Air Temperature for Compressor Operation
                    HPWH.MaxAirTempForHPOperation = hpwhNumeric[5 + nNumericOffset];
                    if (HPWH.MaxAirTempForHPOperation <= HPWH.MinAirTempForHPOperation) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + HPWH.Name +
                                         "\": maximum inlet air temperature for heat pump compressor operation");
                        ShowContinueError("must be greater than the minimum inlet air temperature for heat pump compressor operation.");
                        ShowContinueError("...Minimum inlet air temperature = " + TrimSigDigits(HPWH.MinAirTempForHPOperation, 1));
                        ShowContinueError("...Maximum inlet air temperature = " + TrimSigDigits(HPWH.MaxAirTempForHPOperation, 1));
                    }

                    // Compressor Location
                    {
                        auto const SELECT_CASE_var(hpwhAlpha[20 + nAlphaOffset]);
                        if (SELECT_CASE_var == "SCHEDULE") {
                            HPWH.CrankcaseTempIndicator = CrankcaseTempSchedule;
                            if (!hpwhAlphaBlank[21 + nAlphaOffset]) {
                                // Compressor Ambient Temperature Schedule
                                HPWH.CrankcaseTempSchedule = GetScheduleIndex(hpwhAlpha[21 + nAlphaOffset]);
                                if (HPWH.CrankcaseTempSchedule == 0) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                    ShowContinueError(hpwhAlphaFieldNames[21 + nAlphaOffset] + "=\"" + hpwhAlpha[21 + nAlphaOffset] + "\".");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", ");
                                ShowContinueError("required " + hpwhAlphaFieldNames[21 + nAlphaOffset] + " is blank.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONE") {
                            HPWH.CrankcaseTempIndicator = CrankcaseTempZone;
                            if (HPWH.InletAirConfiguration == AmbientTempOutsideAir || HPWH.InletAirConfiguration == AmbientTempSchedule) {
                                ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name +
                                                "\":  Inlet Air Configuration must be Zone Air Only or Zone And");
                                ShowContinueError(" Outdoor Air when compressor location equals ZONE.");
                                ErrorsFound = true;
                            }

                            if (!hpwhAlphaBlank[21 + nAlphaOffset]) {
                                ShowWarningError(cCurrentModuleObject + "=\"" + HPWH.Name + "\"  " + hpwhAlphaFieldNames[21 + nAlphaOffset] +
                                                 " was provided but will not be used based on compressor location input=\"" +
                                                 hpwhAlpha[20 + nAlphaOffset] + "\".");
                            }
                        } else if (SELECT_CASE_var == "OUTDOORS") {
                            HPWH.CrankcaseTempIndicator = CrankcaseTempExterior;
                            if (!hpwhAlphaBlank[21 + nAlphaOffset]) {
                                ShowWarningError(cCurrentModuleObject + "=\"" + HPWH.Name + "\"  " + hpwhAlphaFieldNames[21 + nAlphaOffset] +
                                                 " was provided but will not be used based on " + hpwhAlphaFieldNames[21 + nAlphaOffset] + "=\"" +
                                                 hpwhAlpha[20 + nAlphaOffset] + "\".");
                            }
                        }
                    }

                    // Fan Name
                    HPWH.FanName = hpwhAlpha[23 + nAlphaOffset];
                    HPWH.FanType = hpwhAlpha[22 + nAlphaOffset];

                    // check that the fan exists
                    errFlag = false;
                    ValidateComponent(HPWH.FanType, HPWH.FanName, errFlag, cCurrentModuleObject);

                    if (errFlag) {
                        ShowContinueError("...occurs in " + cCurrentModuleObject + ", unit=\"" + HPWH.Name + "\".");
                        ErrorsFound = true;
                    } else {
                        if (UtilityRoutines::SameString(HPWH.FanType, "Fan:SystemModel")) {
                            HPWH.FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                            HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(HPWH.FanName)); // call constructor
                            HPWH.FanNum = HVACFan::getFanObjectVectorIndex(HPWH.FanName);
                            FanVolFlow = HVACFan::fanObjs[HPWH.FanNum]->designAirVolFlowRate;

                        } else {
                            GetFanType(HPWH.FanName, HPWH.FanType_Num, errFlag, cCurrentModuleObject, HPWH.Name);
                            GetFanIndex(HPWH.FanName, HPWH.FanNum, errFlag, cCurrentModuleObject);
                            GetFanVolFlow(HPWH.FanNum, FanVolFlow);
                        }
                    }
                    // issue #5630, set fan info in coils.
                    if (bIsVScoil == true) {
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
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\": illegal fan type specified.");
                        ShowContinueError(" The fan object (" + HPWH.FanName +
                                          ") type must be Fan:SystemModel or Fan:OnOff when used with a heat pump water heater.");
                        ErrorsFound = true;
                    } else if (!UtilityRoutines::SameString(HPWH.FanType, "Fan:OnOff") &&
                               !UtilityRoutines::SameString(HPWH.FanType, "Fan:SystemModel")) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\": illegal fan type specified.");
                        ShowContinueError(" The " + cCurrentModuleObject + " must specify that the fan object");
                        ShowContinueError(
                            " is of type FanSystemModel or Fan:OnOff in addition to the fan actually being of that type and defined elsewhere.");
                    }

                    if (FanVolFlow != AutoSize && !errFlag) {
                        if (FanVolFlow < HPWH.OperatingAirFlowRate) {
                            ShowSevereError(cCurrentModuleObject + " - air flow rate = " + TrimSigDigits(FanVolFlow, 7) + " in fan object " +
                                            HPWH.FanName + " is less than the  HPWHs evaporator air flow rate.");
                            ShowContinueError(" The fan flow rate must be >= to the HPWHs evaporator volumetric air flow rate.");
                            ShowContinueError(" Occurs in unit = " + HPWH.Name);
                            ErrorsFound = true;
                        }
                    }

                    // Fan Placement
                    if (UtilityRoutines::SameString(hpwhAlpha[24 + nAlphaOffset], "BlowThrough")) {
                        HPWH.FanPlacement = BlowThru;

                    } else if (UtilityRoutines::SameString(hpwhAlpha[24 + nAlphaOffset], "DrawThrough")) {
                        HPWH.FanPlacement = DrawThru;

                    } else {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", invalid ");
                        ShowContinueError(hpwhAlphaFieldNames[24 + nAlphaOffset] + "=\"" + hpwhAlpha[24 + nAlphaOffset] + "\".");
                        ErrorsFound = true;
                    }

                    if (HPWH.DXCoilNum > 0 && !bIsVScoil) {
                        // get HPWH capacity, air inlet node, and PLF curve info from DX coil object
                        HPWH.Capacity = DXCoil(HPWH.DXCoilNum).RatedTotCap2;
                        HPWH.DXCoilAirInletNode = DXCoil(HPWH.DXCoilNum).AirInNode;
                        HPWH.DXCoilPLFFPLR = DXCoil(HPWH.DXCoilNum).PLFFPLR(1);
                        // check the range of condenser pump power to be <= 5 gpm/ton
                        if (DXCoil(HPWH.DXCoilNum).HPWHCondPumpElecNomPower / DXCoil(HPWH.DXCoilNum).RatedTotCap2 > 0.1422) {
                            ShowWarningError(
                                DXCoil(HPWH.DXCoilNum).DXCoilType + "= " + DXCoil(HPWH.DXCoilNum).Name +
                                ": Rated condenser pump power per watt of rated heating capacity has exceeded the recommended maximum of 0.1422 W/W "
                                "(41.67 watt/MBH). Condenser pump power per watt = " +
                                TrimSigDigits((DXCoil(HPWH.DXCoilNum).HPWHCondPumpElecNomPower / DXCoil(HPWH.DXCoilNum).RatedTotCap2), 4));
                        }
                    } else if ((HPWH.DXCoilNum > 0) && (bIsVScoil)) {

                        if (HPWH.bIsIHP) {
                            HPWH.Capacity = GetDWHCoilCapacityIHP(HPWH.DXCoilType, HPWH.DXCoilName, IHPOperationMode::SCWHMatchWHMode, DXCoilErrFlag);
                            HPWH.DXCoilAirInletNode = GetCoilInletNodeIHP(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                            HPWH.DXCoilPLFFPLR =
                                GetIHPDWHCoilPLFFPLR(HPWH.DXCoilType, HPWH.DXCoilName, IHPOperationMode::SCWHMatchWHMode, DXCoilErrFlag);
                        } else {
                            HPWH.Capacity = GetCoilCapacityVariableSpeed(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                            HPWH.DXCoilAirInletNode = GetCoilInletNodeVariableSpeed(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                            HPWH.DXCoilPLFFPLR = GetVSCoilPLFFPLR(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                        }
                        //         check the range of condenser pump power to be <= 5 gpm/ton, will be checked in the coil object
                    }

                    if (HPWH.OperatingWaterFlowRate == AutoCalculate) {
                        HPWH.OperatingWaterFlowRate = 0.00000004487 * HPWH.Capacity;
                        HPWH.WaterFlowRateAutoSized = true;
                    }

                    if (HPWH.OperatingAirFlowRate == AutoCalculate) {
                        HPWH.OperatingAirFlowRate = 0.00005035 * HPWH.Capacity;
                        HPWH.AirFlowRateAutoSized = true;
                    }

                    // On Cycle Parasitic Electric Load
                    HPWH.OnCycParaLoad = hpwhNumeric[6 + nNumericOffset];
                    if (HPWH.OnCycParaLoad < 0.0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\",");
                        ShowContinueError(hpwhNumericFieldNames[6 + nNumericOffset] + " must be >= 0. " + hpwhNumericFieldNames[6 + nNumericOffset] +
                                          " = " + TrimSigDigits(hpwhNumeric[6 + nNumericOffset], 2));
                        ErrorsFound = true;
                    }

                    // Off Cycle Parasitic Electric Load
                    HPWH.OffCycParaLoad = hpwhNumeric[7 + nNumericOffset];
                    if (HPWH.OffCycParaLoad < 0.0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\",");
                        ShowContinueError(hpwhNumericFieldNames[7 + nNumericOffset] + " must be >= 0. " + hpwhNumericFieldNames[2 + nNumericOffset] +
                                          " = " + TrimSigDigits(hpwhNumeric[7 + nNumericOffset], 2));
                        ErrorsFound = true;
                    }

                    // Parasitic Heat Rejection Location
                    if (UtilityRoutines::SameString(hpwhAlpha[25 + nAlphaOffset], "Zone")) {
                        HPWH.ParasiticTempIndicator = AmbientTempZone;
                        if (HPWH.InletAirConfiguration == AmbientTempOutsideAir || HPWH.InletAirConfiguration == AmbientTempSchedule) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\",");
                            ShowContinueError(hpwhAlphaFieldNames[25 + nAlphaOffset] + " must be ZoneAirOnly or ZoneAndOutdoorAir");
                            ShowContinueError(" when parasitic heat rejection location equals Zone.");
                            ErrorsFound = true;
                        }
                    } else if (UtilityRoutines::SameString(hpwhAlpha[25 + nAlphaOffset], "Outdoors")) {
                        HPWH.ParasiticTempIndicator = AmbientTempOutsideAir;
                    } else {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                        ShowContinueError(" parasitic heat rejection location must be either Zone or Outdoors.");
                        ErrorsFound = true;
                    }

                    // Inlet Air Mixer Node
                    // get mixer/splitter nodes only when Inlet Air Configuration is ZoneAndOutdoorAir
                    if (!hpwhAlphaBlank[26 + nAlphaOffset]) {
                        // For the inlet air mixer node, NodeConnectionType is outlet from the HPWH inlet air node
                        if (HPWH.InletAirConfiguration == AmbientTempZoneAndOA) {
                            HPWH.InletAirMixerNode = GetOnlySingleNode(hpwhAlpha[26 + nAlphaOffset],
                                                                       ErrorsFound,
                                                                       cCurrentModuleObject + " inlet air mixer",
                                                                       HPWH.Name,
                                                                       NodeType_Air,
                                                                       NodeConnectionType_Outlet,
                                                                       1,
                                                                       ObjectIsNotParent);
                        } else {
                            ShowWarningError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Inlet air mixer node name specified but only required when Inlet Air Configuration is selected as "
                                              "Zone and OutdoorAir. Node name disregarded and simulation continues.");
                        }
                    } else if (hpwhAlphaBlank[26 + nAlphaOffset] && HPWH.InletAirConfiguration == AmbientTempZoneAndOA) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                        ShowContinueError("Inlet air mixer node name required when Inlet Air Configuration is selected as ZoneAndOutdoorAir.");
                        ErrorsFound = true;
                    }

                    // Outlet Air Splitter Node
                    if (!hpwhAlphaBlank[27 + nAlphaOffset]) {
                        //  For the outlet air splitter node, NodeConnectionType is inlet to the HPWH outlet air node
                        if (HPWH.InletAirConfiguration == AmbientTempZoneAndOA) {
                            HPWH.OutletAirSplitterNode = GetOnlySingleNode(hpwhAlpha[27 + nAlphaOffset],
                                                                           ErrorsFound,
                                                                           cCurrentModuleObject + "-OUTLET AIR SPLITTER",
                                                                           HPWH.Name,
                                                                           NodeType_Air,
                                                                           NodeConnectionType_Inlet,
                                                                           1,
                                                                           ObjectIsNotParent);
                        } else {
                            ShowWarningError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Outlet air splitter node name specified but only required when Inlet Air Configuration is selected as "
                                              "ZoneAndOutdoorAir. Node name disregarded and simulation continues.");
                        }
                    } else if (hpwhAlphaBlank[27 + nAlphaOffset] && HPWH.InletAirConfiguration == AmbientTempZoneAndOA) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                        ShowContinueError("Outlet air splitter node name required when Inlet Air Configuration is selected as ZoneAndOutdoorAir.");
                        ErrorsFound = true;
                    }

                    // get node data for HPWH
                    if (HPWH.InletAirMixerNode != 0) {
                        // when mixer/splitter nodes are used the HPWH's inlet/outlet node are set up as ObjectIsNotParent

                        HPWH.HeatPumpAirInletNode = GetOnlySingleNode(hpwhAlpha[7 + nAlphaOffset],
                                                                      ErrorsFound,
                                                                      cCurrentModuleObject + "-INLET AIR MIXER",
                                                                      HPWH.Name,
                                                                      NodeType_Air,
                                                                      NodeConnectionType_Inlet,
                                                                      1,
                                                                      ObjectIsNotParent);

                        HPWH.HeatPumpAirOutletNode = GetOnlySingleNode(hpwhAlpha[8 + nAlphaOffset],
                                                                       ErrorsFound,
                                                                       cCurrentModuleObject + "-OUTLET AIR SPLITTER",
                                                                       HPWH.Name,
                                                                       NodeType_Air,
                                                                       NodeConnectionType_Outlet,
                                                                       1,
                                                                       ObjectIsNotParent);

                        HPWH.OutsideAirNode = GetOnlySingleNode(hpwhAlpha[9 + nAlphaOffset],
                                                                ErrorsFound,
                                                                cCurrentModuleObject,
                                                                HPWH.Name,
                                                                NodeType_Air,
                                                                NodeConnectionType_OutsideAirReference,
                                                                1,
                                                                ObjectIsParent);
                        if (hpwhAlpha[9 + nAlphaOffset] != "") {
                            CheckAndAddAirNodeNumber(HPWH.OutsideAirNode, Okay);
                            if (!Okay) {
                                ShowWarningError(cCurrentModuleObject + "=\"" + HPWH.Name +
                                                 "\": Adding outdoor air node=" + hpwhAlpha[9 + nAlphaOffset]);
                            }
                        }

                        HPWH.ExhaustAirNode = GetOnlySingleNode(hpwhAlpha[10 + nAlphaOffset],
                                                                ErrorsFound,
                                                                cCurrentModuleObject,
                                                                HPWH.Name,
                                                                NodeType_Air,
                                                                NodeConnectionType_ReliefAir,
                                                                1,
                                                                ObjectIsParent);

                    } else {
                        // when mixer/splitter nodes are NOT used the HPWH's inlet/outlet nodes are set up as ObjectIsParent
                        if (HPWH.InletAirConfiguration == AmbientTempSchedule) {
                            // for scheduled HPWH's the inlet node is not on any branch or parent object, make it an outlet node
                            // to avoid node connection errors
                            HPWH.HeatPumpAirInletNode = GetOnlySingleNode(hpwhAlpha[7 + nAlphaOffset],
                                                                          ErrorsFound,
                                                                          cCurrentModuleObject,
                                                                          HPWH.Name,
                                                                          NodeType_Air,
                                                                          NodeConnectionType_Outlet,
                                                                          1,
                                                                          ObjectIsParent);

                            HPWH.HeatPumpAirOutletNode = GetOnlySingleNode(hpwhAlpha[8 + nAlphaOffset],
                                                                           ErrorsFound,
                                                                           cCurrentModuleObject,
                                                                           HPWH.Name,
                                                                           NodeType_Air,
                                                                           NodeConnectionType_Outlet,
                                                                           1,
                                                                           ObjectIsParent);

                        } else { // HPWH is connected to a zone with no mixer/splitter nodes
                            if (HPWH.InletAirConfiguration == AmbientTempZone) {
                                HPWH.HeatPumpAirInletNode = GetOnlySingleNode(hpwhAlpha[7 + nAlphaOffset],
                                                                              ErrorsFound,
                                                                              cCurrentModuleObject,
                                                                              HPWH.Name,
                                                                              NodeType_Air,
                                                                              NodeConnectionType_Inlet,
                                                                              1,
                                                                              ObjectIsParent);

                                HPWH.HeatPumpAirOutletNode = GetOnlySingleNode(hpwhAlpha[8 + nAlphaOffset],
                                                                               ErrorsFound,
                                                                               cCurrentModuleObject,
                                                                               HPWH.Name,
                                                                               NodeType_Air,
                                                                               NodeConnectionType_Outlet,
                                                                               1,
                                                                               ObjectIsParent);
                            } else { // HPWH is located outdoors
                                HPWH.OutsideAirNode = GetOnlySingleNode(hpwhAlpha[9 + nAlphaOffset],
                                                                        ErrorsFound,
                                                                        cCurrentModuleObject,
                                                                        HPWH.Name,
                                                                        NodeType_Air,
                                                                        NodeConnectionType_OutsideAirReference,
                                                                        1,
                                                                        ObjectIsParent);
                                if (!hpwhAlphaBlank[9 + nAlphaOffset]) {
                                    CheckAndAddAirNodeNumber(HPWH.OutsideAirNode, Okay);
                                    if (!Okay) {
                                        ShowWarningError(cCurrentModuleObject + "=\"" + HPWH.Name +
                                                         "\": Adding outdoor air node =" + hpwhAlpha[9 + nAlphaOffset]);
                                    }
                                }

                                HPWH.ExhaustAirNode = GetOnlySingleNode(hpwhAlpha[10 + nAlphaOffset],
                                                                        ErrorsFound,
                                                                        cCurrentModuleObject,
                                                                        HPWH.Name,
                                                                        NodeType_Air,
                                                                        NodeConnectionType_ReliefAir,
                                                                        1,
                                                                        ObjectIsParent);
                            }
                        }
                    }
                    // check that required node names are present
                    if (HPWH.InletAirConfiguration == AmbientTempSchedule || HPWH.InletAirConfiguration == AmbientTempZone) {
                        if (HPWH.HeatPumpAirInletNode == 0 || HPWH.HeatPumpAirOutletNode == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("When " + hpwhAlphaFieldNames[6 + nAlphaOffset] + "=\"" + hpwhAlpha[6 + nAlphaOffset] + "\".");
                            ShowContinueError(hpwhAlphaFieldNames[7 + nAlphaOffset] + " and " + hpwhAlphaFieldNames[8 + nAlphaOffset] +
                                              " must be specified.");
                            ErrorsFound = true;
                        }
                    } else if (HPWH.InletAirConfiguration == AmbientTempOutsideAir) {
                        if (HPWH.OutsideAirNode == 0 || HPWH.ExhaustAirNode == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("When " + hpwhAlphaFieldNames[6 + nAlphaOffset] + "=\"" + hpwhAlpha[6 + nAlphaOffset] + "\".");
                            ShowContinueError(hpwhAlphaFieldNames[9 + nAlphaOffset] + " and " + hpwhAlphaFieldNames[10 + nAlphaOffset] +
                                              " must be specified.");
                            ErrorsFound = true;
                        }
                    } else if (HPWH.InletAirMixerNode > 0 && HPWH.OutletAirSplitterNode > 0 && HPWH.InletAirConfiguration == AmbientTempZoneAndOA) {
                        if (HPWH.HeatPumpAirInletNode == 0 || HPWH.HeatPumpAirOutletNode == 0 || HPWH.OutsideAirNode == 0 ||
                            HPWH.ExhaustAirNode == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
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
                    if ((HPWH.InletAirConfiguration == AmbientTempZone || HPWH.InletAirConfiguration == AmbientTempZoneAndOA) &&
                        HPWH.AmbientTempZone > 0) {
                        if (!ZoneEquipInputsFilled) {
                            GetZoneEquipmentData();
                            ZoneEquipInputsFilled = true;
                        }
                        if (allocated(ZoneEquipConfig)) {
                            FoundInletNode = false;
                            FoundOutletNode = false;
                            for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                                if (HPWH.AmbientTempZone == ZoneEquipConfig(ZoneNum).ActualZoneNum) break;
                            }
                            if (ZoneNum <= NumOfZones) {
                                for (SupAirIn = 1; SupAirIn <= ZoneEquipConfig(ZoneNum).NumInletNodes; ++SupAirIn) {
                                    if (HPWH.HeatPumpAirOutletNode != ZoneEquipConfig(ZoneNum).InletNode(SupAirIn)) continue;
                                    FoundOutletNode = true;
                                }
                                for (ExhAirOut = 1; ExhAirOut <= ZoneEquipConfig(ZoneNum).NumExhaustNodes; ++ExhAirOut) {
                                    if (HPWH.HeatPumpAirInletNode != ZoneEquipConfig(ZoneNum).ExhaustNode(ExhAirOut)) continue;
                                    FoundInletNode = true;
                                }
                                if (!FoundInletNode) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                                    ShowContinueError("The HPWH's air inlet node name = " + hpwhAlpha[7 + nAlphaOffset] +
                                                      " was not properly specified ");
                                    ShowContinueError("as an exhaust air node for zone = " + hpwhAlpha[13 + nAlphaOffset] +
                                                      " in a ZoneHVAC:EquipmentConnections object.");
                                    ErrorsFound = true;
                                }
                                if (!FoundOutletNode) {
                                    ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                                    ShowContinueError("The HPWH's air outlet node name = " + hpwhAlpha[8 + nAlphaOffset] +
                                                      " was not properly specified ");
                                    ShowContinueError("as an inlet air node for zone = " + hpwhAlpha[13 + nAlphaOffset] +
                                                      " in a ZoneHVAC:EquipmentConnections object.");
                                    ErrorsFound = true;
                                }
                            }
                        } else {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Heat pump water heater air inlet node name and air outlet node name must be listed in a "
                                              "ZoneHVAC:EquipmentConnections object when Inlet Air Configuration is equal to ZoneAirOnly or "
                                              "ZoneAndOutdoorAir.");
                            ErrorsFound = true;
                        }
                    }

                    // only get the inlet air mixer schedule if the inlet air configuration is zone and outdoor air
                    if (!hpwhAlphaBlank[28 + nAlphaOffset] && HPWH.InletAirConfiguration == AmbientTempZoneAndOA) {
                        HPWH.InletAirMixerSchPtr = GetScheduleIndex(hpwhAlpha[28 + nAlphaOffset]);
                        if (HPWH.InletAirMixerSchPtr == 0) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                            ShowContinueError(hpwhAlphaFieldNames[28 + nAlphaOffset] + "=\"" + hpwhAlpha[28 + nAlphaOffset] + "\",");
                            ErrorsFound = true;
                        } else {
                            //           check schedule values to be between 0 and 1
                            ValidScheduleValue = CheckScheduleValueMinMax(HPWH.InletAirMixerSchPtr, ">=", 0.0, "<=", 1.0);
                            if (!ValidScheduleValue) {
                                ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\", not found");
                                ShowContinueError(hpwhAlphaFieldNames[28 + nAlphaOffset] + " values out of range of 0 to 1, Schedule=\"" +
                                                  hpwhAlpha[28 + nAlphaOffset] + "\".");
                                ErrorsFound = true;
                            }
                            //           set outlet air splitter schedule index equal to inlet air mixer schedule index
                            //           (place holder for when zone pressurization/depressurization is allowed and different schedules can be used)
                            HPWH.OutletAirSplitterSchPtr = GetScheduleIndex(hpwhAlpha[28 + nAlphaOffset]);
                        }
                    }

                    // set fan outlet node variable for use in setting Node(FanOutletNode)%MassFlowRateMax for fan object
                    if (HPWH.FanPlacement == DrawThru) {
                        if (HPWH.OutletAirSplitterNode != 0) {
                            HPWH.FanOutletNode = HPWH.OutletAirSplitterNode;
                        } else {
                            if (HPWH.InletAirConfiguration == AmbientTempOutsideAir) {
                                HPWH.FanOutletNode = HPWH.ExhaustAirNode;
                            } else {
                                HPWH.FanOutletNode = HPWH.HeatPumpAirOutletNode;
                            }
                        }
                    } else if (HPWH.FanPlacement == BlowThru) {
                        // set fan outlet node variable for use in setting Node(FanOutletNode)%MassFlowRateMax for fan object
                        if (bIsVScoil) {
                            if (HPWH.bIsIHP) {
                                HPWH.FanOutletNode = GetDWHCoilInletNodeIHP(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                            } else {
                                HPWH.FanOutletNode = GetCoilInletNodeVariableSpeed(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                            }
                        } else {
                            HPWH.FanOutletNode = DXCoil(HPWH.DXCoilNum).AirInNode;
                        }
                    }

                    // check that fan outlet node is indeed correct
                    int FanOutletNodeNum(0);
                    if (HPWH.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        FanOutletNodeNum = HVACFan::fanObjs[HPWH.FanNum]->outletNodeNum;
                    } else {
                        errFlag = false;
                        FanOutletNodeNum = GetFanOutletNode(HPWH.FanType, HPWH.FanName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in unit=\"" + HPWH.Name + "\".");
                            ErrorsFound = true;
                        }
                    }
                    if (FanOutletNodeNum != HPWH.FanOutletNode) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
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
                        FanInletNodeNum = GetFanInletNode(HPWH.FanType, HPWH.FanName, errFlag);
                        if (errFlag) {
                            ShowContinueError("...occurs in unit=\"" + HPWH.Name + "\".");
                            ErrorsFound = true;
                        }
                    }
                    int HPWHFanInletNodeNum(0);
                    if (HPWH.InletAirMixerNode != 0) {
                        HPWHFanInletNodeNum = HPWH.InletAirMixerNode;
                    } else {
                        if (HPWH.InletAirConfiguration == AmbientTempOutsideAir) {
                            HPWHFanInletNodeNum = HPWH.OutsideAirNode;
                        } else {
                            HPWHFanInletNodeNum = HPWH.HeatPumpAirInletNode;
                        }
                    }
                    if (HPWH.FanPlacement == BlowThru) {
                        if (FanInletNodeNum != HPWHFanInletNodeNum) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
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
                            DXCoilAirOutletNodeNum = GetDWHCoilOutletNodeIHP(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                        } else {
                            DXCoilAirOutletNodeNum = GetCoilOutletNodeVariableSpeed(HPWH.DXCoilType, HPWH.DXCoilName, DXCoilErrFlag);
                        };

                    } else if (HPWH.DXCoilNum > 0) {
                        DXCoilAirOutletNodeNum = DXCoil(HPWH.DXCoilNum).AirOutNode;
                    }
                    if (HPWH.FanPlacement == DrawThru) {
                        if (FanInletNodeNum != DXCoilAirOutletNodeNum) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
                            ShowContinueError("Heat pump water heater fan inlet node name does not match previous connected component.");
                            if (FanInletNodeNum != 0) {
                                ShowContinueError("Fan inlet node name = " + DataLoopNode::NodeID(FanInletNodeNum));
                            }
                            if (DXCoilAirOutletNodeNum != 0) {
                                ShowContinueError("Expected fan inlet node name = " + DataLoopNode::NodeID(DXCoilAirOutletNodeNum));
                            }
                            ErrorsFound = true;
                        }
                    } else if (HPWH.FanPlacement == BlowThru) {
                        int HPWHCoilOutletNodeNum(0);
                        if (HPWH.OutletAirSplitterNode != 0) {
                            HPWHCoilOutletNodeNum = HPWH.OutletAirSplitterNode;
                        } else {
                            if (HPWH.InletAirConfiguration == AmbientTempOutsideAir) {
                                HPWHCoilOutletNodeNum = HPWH.ExhaustAirNode;
                            } else {
                                HPWHCoilOutletNodeNum = HPWH.HeatPumpAirOutletNode;
                            }
                        }
                        if (DXCoilAirOutletNodeNum != HPWHCoilOutletNodeNum) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
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
                        Node(HPWH.FanOutletNode).MassFlowRateMax = HPWH.OperatingAirFlowRate * PsyRhoAirFnPbTdbW(OutBaroPress, 20.0, 0.0);

                    if (HPWH.FanPlacement == BlowThru) {
                        if (HPWH.InletAirMixerNode > 0) {
                            //           hpwhAlpha[ 26 + nAlphaOffset ] = Inlet Air Mixer Node
                            FanInletNode = hpwhAlpha[26 + nAlphaOffset];
                            FanOutletNode = "UNDEFINED";
                        } else {
                            if (HPWH.OutsideAirNode == 0) {
                                //             hpwhAlpha[ 7 + nAlphaOffset ] = Heat Pump Air Inlet Node
                                FanInletNode = hpwhAlpha[7 + nAlphaOffset];
                                FanOutletNode = "UNDEFINED";
                            } else {
                                //             hpwhAlpha[ 9 + nAlphaOffset ] = Outside Air Node
                                FanInletNode = hpwhAlpha[9 + nAlphaOffset];
                                FanOutletNode = "UNDEFINED";
                            }
                        }
                        if (HPWH.OutletAirSplitterNode > 0) {
                            //           hpwhAlpha[ 30 + nAlphaOffset ] = Outlet Air Splitter Node
                            CoilInletNode = "UNDEFINED";
                            CoilOutletNode = hpwhAlpha[27 + nAlphaOffset];
                        } else {
                            if (HPWH.OutsideAirNode == 0) {
                                //             hpwhAlpha[ 8 + nAlphaOffset ] = Heat Pump Air Outlet Node
                                CoilInletNode = "UNDEFINED";
                                CoilOutletNode = hpwhAlpha[8 + nAlphaOffset];
                            } else {
                                CoilInletNode = "UNDEFINED";
                                //             hpwhAlpha[ 10 + nAlphaOffset ] = Exhaust Air Node
                                CoilOutletNode = hpwhAlpha[10 + nAlphaOffset];
                            }
                        }
                    } else {
                        if (HPWH.InletAirMixerNode > 0) {
                            CoilInletNode = hpwhAlpha[26 + nAlphaOffset];
                            CoilOutletNode = "UNDEFINED";
                        } else {
                            if (HPWH.OutsideAirNode == 0) {
                                CoilInletNode = hpwhAlpha[7 + nAlphaOffset];
                                CoilOutletNode = "UNDEFINED";
                            } else {
                                CoilInletNode = hpwhAlpha[9 + nAlphaOffset];
                                CoilOutletNode = "UNDEFINED";
                            }
                        }
                        if (HPWH.OutletAirSplitterNode > 0) {
                            FanInletNode = "UNDEFINED";
                            FanOutletNode = hpwhAlpha[27 + nAlphaOffset];
                        } else {
                            if (HPWH.OutsideAirNode == 0) {
                                FanInletNode = "UNDEFINED";
                                FanOutletNode = hpwhAlpha[8 + nAlphaOffset];
                            } else {
                                FanInletNode = "UNDEFINED";
                                FanOutletNode = hpwhAlpha[10 + nAlphaOffset];
                            }
                        }
                    }

                    // set up comp set for air side nodes (can be blow thru or draw thru, may or may not have damper nodes)
                    if (HPWH.bIsIHP) {
                        SetUpCompSets(HPWH.Type, HPWH.Name, HPWH.DXCoilType, HPWH.DXCoilName + " Outdoor Coil", CoilInletNode, CoilOutletNode);
                    } else {
                        SetUpCompSets(HPWH.Type, HPWH.Name, HPWH.DXCoilType, HPWH.DXCoilName, CoilInletNode, CoilOutletNode);
                    }

                    SetUpCompSets(HPWH.Type, HPWH.Name, HPWH.FanType, HPWH.FanName, FanInletNode, FanOutletNode);

                    // Control Logic Flag
                    std::string CtrlLogicFlag = hpwhAlphaBlank[29 + nAlphaOffset] ? "SIMULTANEOUS" : hpwhAlpha[29 + nAlphaOffset];
                    if (UtilityRoutines::SameString(CtrlLogicFlag, "SIMULTANEOUS")) {
                        HPWH.AllowHeatingElementAndHeatPumpToRunAtSameTime = true;
                    } else if (UtilityRoutines::SameString(CtrlLogicFlag, "MUTUALLYEXCLUSIVE")) {
                        HPWH.AllowHeatingElementAndHeatPumpToRunAtSameTime = false;
                    } else {
                        ShowSevereError(cCurrentModuleObject + "=\"" + HPWH.Name + "\":");
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

            //!!=======   Get WATER HEATER:MIXED ===================================================================================
            if (NumWaterHeaterMixed > 0) {
                cCurrentModuleObject = cMixedWHModuleObj;
                for (WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterHeaterMixed; ++WaterThermalTankNum) {

                    inputProcessor->getObjectItem(cCurrentModuleObject,
                                                  WaterThermalTankNum,
                                                  cAlphaArgs,
                                                  NumAlphas,
                                                  rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  lNumericFieldBlanks,
                                                  lAlphaFieldBlanks,
                                                  cAlphaFieldNames,
                                                  cNumericFieldNames);
                    GlobalNames::VerifyUniqueInterObjectName(
                        UniqueWaterThermalTankNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

                    WaterThermalTank(WaterThermalTankNum).Name = cAlphaArgs(1);
                    WaterThermalTank(WaterThermalTankNum).Type = cCurrentModuleObject;
                    WaterThermalTank(WaterThermalTankNum).TypeNum = MixedWaterHeater;

                    // default to always on
                    WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = ScheduleAlwaysOn;
                    WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = ScheduleAlwaysOn;

                    // A user field will be added in a later release
                    WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName = "Water Heater";

                    WaterThermalTank(WaterThermalTankNum).Volume = rNumericArgs(1);
                    if (WaterThermalTank(WaterThermalTankNum).Volume == AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized = true;
                    }
                    if (rNumericArgs(1) == 0.0) {
                        // Set volume to a really small number to simulate a tankless/instantaneous water heater
                        WaterThermalTank(WaterThermalTankNum).Volume = 0.000001; // = 1 cm3
                    }

                    WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule = GetScheduleIndex(cAlphaArgs(2));
                    if (lAlphaFieldBlanks(2)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", missing data.");
                        ShowContinueError("blank field, missing " + cAlphaFieldNames(2) + " is required");
                        ErrorsFound = true;
                    } else if (WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  " + cAlphaFieldNames(2) + " not found = " + cAlphaArgs(2));
                        ErrorsFound = true;
                    }

                    if (rNumericArgs(2) > 0.0001) {
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = rNumericArgs(2);
                    } else {
                        // Default to very small number (however it can't be TINY or it will break the algorithm)
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = 0.5;
                    }

                    if (rNumericArgs(3) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = rNumericArgs(3);
                    } else {
                        // Default to very large number
                        // BG comment why a large number here why not boilng point of water?
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = 100.0; // 1.0E9
                    }

                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = rNumericArgs(4);
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacity == AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized = true;
                    }

                    if ((rNumericArgs(5) > WaterThermalTank(WaterThermalTankNum).MaxCapacity) &&
                        (!WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                        ":  Heater Minimum Capacity cannot be greater than Heater Maximum Capacity");
                        ErrorsFound = true;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).MinCapacity = rNumericArgs(5);
                    }

                    // Validate Heater Control Type
                    {
                        auto const SELECT_CASE_var(cAlphaArgs(3));
                        if (SELECT_CASE_var == "CYCLE") {
                            WaterThermalTank(WaterThermalTankNum).ControlType = ControlTypeCycle;
                            WaterThermalTank(WaterThermalTankNum).MinCapacity = WaterThermalTank(WaterThermalTankNum).MaxCapacity;

                        } else if (SELECT_CASE_var == "MODULATE") {
                            WaterThermalTank(WaterThermalTankNum).ControlType = ControlTypeModulate;

                            // CASE ('MODULATE WITH OVERHEAT')  ! Not yet implemented

                            // CASE ('MODULATE WITH UNDERHEAT')  ! Not yet implemented

                        } else {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Invalid Control Type entered=" + cAlphaArgs(3));
                            ErrorsFound = true;
                        }
                    }
                    WaterThermalTank(WaterThermalTankNum).VolFlowRateMin = rNumericArgs(6);
                    WaterThermalTank(WaterThermalTankNum).VolFlowRateMin = max(0.0, WaterThermalTank(WaterThermalTankNum).VolFlowRateMin);
                    //        rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'GetWaterThermalTankInput')
                    //        WaterThermalTank(WaterThermalTankNum)%MassFlowRateMin = rNumericArgs(6) * rho   ! Not yet implemented
                    WaterThermalTank(WaterThermalTankNum).IgnitionDelay = rNumericArgs(7); // Not yet implemented

                    // Validate Heater Fuel Type
                    {
                        auto const SELECT_CASE_var(cAlphaArgs(4));
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
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Invalid Heater Fuel Type entered=" + cAlphaArgs(4));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    if (rNumericArgs(8) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).Efficiency = rNumericArgs(8);
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Heater Thermal Efficiency must be greater than zero");
                        ErrorsFound = true;
                    }

                    if (!cAlphaArgs(5).empty()) {
                        WaterThermalTank(WaterThermalTankNum).PLFCurve = GetCurveIndex(cAlphaArgs(5));
                        if (WaterThermalTank(WaterThermalTankNum).PLFCurve == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Part Load Factor curve not found = " + cAlphaArgs(5));
                            ErrorsFound = true;
                        } else {
                            ValidatePLFCurve(WaterThermalTank(WaterThermalTankNum).PLFCurve, IsValid);

                            if (!IsValid) {
                                ShowSevereError(
                                    cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                    ":  Part Load Factor curve failed to evaluate to greater than zero for all numbers in the domain of 0 to 1");
                                ErrorsFound = true;
                            }
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycParaLoad = rNumericArgs(9);

                    // Validate Off-Cycle Parasitic Fuel Type
                    {
                        auto const SELECT_CASE_var(cAlphaArgs(6));
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
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            ":  Invalid Off-Cycle Parasitic Fuel Type entered=" + cAlphaArgs(6));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycParaFracToTank = rNumericArgs(10);

                    WaterThermalTank(WaterThermalTankNum).OnCycParaLoad = rNumericArgs(11);

                    // Validate On-Cycle Parasitic Fuel Type
                    {
                        auto const SELECT_CASE_var(cAlphaArgs(7));
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
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            ":  Invalid On-Cycle Parasitic Fuel Type entered=" + cAlphaArgs(7));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OnCycParaFracToTank = rNumericArgs(12);

                    {
                        auto const SELECT_CASE_var(cAlphaArgs(8));
                        if (SELECT_CASE_var == "SCHEDULE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempSchedule;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule = GetScheduleIndex(cAlphaArgs(9));
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                                ":  Ambient Temperature Schedule not found = " + cAlphaArgs(9));
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempZone;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempZone = UtilityRoutines::FindItemInList(cAlphaArgs(10), Zone);
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                                ":  Ambient Temperature Zone not found = " + cAlphaArgs(10));
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "OUTDOORS") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempOutsideAir;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode =
                                GetOnlySingleNode(cAlphaArgs(11),
                                                  ErrorsFound,
                                                  cCurrentModuleObject,
                                                  cAlphaArgs(1),
                                                  NodeType_Air,
                                                  NodeConnectionType_OutsideAirReference,
                                                  1,
                                                  ObjectIsNotParent);
                            if (cAlphaArgs(11) != "") {
                                if (!CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode)) {
                                    ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                                    ": Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                                    ShowContinueError("...Referenced Node Name=" + cAlphaArgs(11));
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError(
                                    "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            ":  Invalid Ambient Temperature Indicator entered=" + cAlphaArgs(8));
                            ShowContinueError(" Valid entries are SCHEDULE, ZONE, and OUTDOORS.");
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycLossCoeff = rNumericArgs(13);
                    WaterThermalTank(WaterThermalTankNum).OffCycLossFracToZone = rNumericArgs(14);

                    WaterThermalTank(WaterThermalTankNum).OnCycLossCoeff = rNumericArgs(15);
                    WaterThermalTank(WaterThermalTankNum).OnCycLossFracToZone = rNumericArgs(16);
                    rho = GetDensityGlycol(fluidNameWater, DataGlobals::InitConvTemp, DummyWaterIndex, RoutineNameNoColon);
                    WaterThermalTank(WaterThermalTankNum).MassFlowRateMax = rNumericArgs(17) * rho;

                    if ((cAlphaArgs(14).empty()) && (cAlphaArgs(15).empty())) {
                        if (!cAlphaArgs(12).empty()) {
                            WaterThermalTank(WaterThermalTankNum).FlowRateSchedule = GetScheduleIndex(cAlphaArgs(12));
                            if (WaterThermalTank(WaterThermalTankNum).FlowRateSchedule == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Flow Rate Schedule not found = " + cAlphaArgs(12));
                                ErrorsFound = true;
                            }
                        }
                    }

                    if (!cAlphaArgs(13).empty()) {
                        WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule = GetScheduleIndex(cAlphaArgs(13));
                        if (WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            ":  Cold Water Supply Temperature Schedule not found = " + cAlphaArgs(13));
                            ErrorsFound = true;
                        }
                    }

                    if (NumNums > 17) {
                        if ((rNumericArgs(18) > 1) || (rNumericArgs(18) < 0)) {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Use Side Effectiveness is out of bounds (0 to 1)");
                            ErrorsFound = true;
                        }
                        WaterThermalTank(WaterThermalTankNum).UseEffectiveness = rNumericArgs(18);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseEffectiveness = 1.0; // Default for stand-alone mode
                    }

                    if (NumNums > 18) {
                        if ((rNumericArgs(19) > 1) || (rNumericArgs(19) <= 0)) {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Source Side Effectiveness is out of bounds (>0 to 1)");
                            ErrorsFound = true;
                        }
                        WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = rNumericArgs(19);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = 1.0;
                    }

                    // If no plant nodes are connected, simulate in stand-alone mode.
                    if (cAlphaArgs(14).empty() && cAlphaArgs(15).empty() && cAlphaArgs(16).empty() && cAlphaArgs(17).empty()) {
                        WaterThermalTank(WaterThermalTankNum).StandAlone = true;
                    }

                    if (!lNumericFieldBlanks(20)) {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = rNumericArgs(20);
                        if (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate == AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized = true;
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = 0.0;
                    }
                    WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide = DemandSupply_No;

                    if (!lNumericFieldBlanks(21)) {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = rNumericArgs(21);
                        if (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate == AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized = true;
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = 0.0;
                    }
                    WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide = DemandSupply_No;

                    if (!lNumericFieldBlanks(22)) {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = rNumericArgs(22);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = 1.5;
                    }

                    if ((!cAlphaArgs(14).empty()) || (!cAlphaArgs(15).empty())) {
                        WaterThermalTank(WaterThermalTankNum).UseInletNode = GetOnlySingleNode(cAlphaArgs(14),
                                                                                               ErrorsFound,
                                                                                               cCurrentModuleObject,
                                                                                               cAlphaArgs(1),
                                                                                               NodeType_Water,
                                                                                               NodeConnectionType_Inlet,
                                                                                               1,
                                                                                               ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName1 = cAlphaArgs(14);
                        WaterThermalTank(WaterThermalTankNum).UseOutletNode = GetOnlySingleNode(cAlphaArgs(15),
                                                                                                ErrorsFound,
                                                                                                cCurrentModuleObject,
                                                                                                cAlphaArgs(1),
                                                                                                NodeType_Water,
                                                                                                NodeConnectionType_Outlet,
                                                                                                1,
                                                                                                ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName1 = cAlphaArgs(15);

                        if (rNumericArgs(17) > 0) {
                            ShowWarningError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Peak Volumetric Use Flow Rate will not be used");
                        }

                        if (WaterThermalTank(WaterThermalTankNum).FlowRateSchedule > 0) {
                            ShowWarningError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Use Flow Rate Fraction Schedule will not be used");
                        }

                        if (WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule > 0) {
                            ShowWarningError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Cold Water Supply Temperature Schedule will not be used");
                        }
                    }

                    if ((!cAlphaArgs(16).empty()) || (!cAlphaArgs(17).empty())) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode = GetOnlySingleNode(cAlphaArgs(16),
                                                                                                  ErrorsFound,
                                                                                                  cCurrentModuleObject,
                                                                                                  cAlphaArgs(1),
                                                                                                  NodeType_Water,
                                                                                                  NodeConnectionType_Inlet,
                                                                                                  2,
                                                                                                  ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName2 = cAlphaArgs(16);
                        WaterThermalTank(WaterThermalTankNum).SourceOutletNode = GetOnlySingleNode(cAlphaArgs(17),
                                                                                                   ErrorsFound,
                                                                                                   cCurrentModuleObject,
                                                                                                   cAlphaArgs(1),
                                                                                                   NodeType_Water,
                                                                                                   NodeConnectionType_Outlet,
                                                                                                   2,
                                                                                                   ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName2 = cAlphaArgs(17);
                    }

                    if (!lAlphaFieldBlanks(18)) {
                        {
                            auto const SELECT_CASE_var(cAlphaArgs(18));
                            if (SELECT_CASE_var == "STORAGETANK") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = SourceSideStorageTank;
                            } else if (SELECT_CASE_var == "INDIRECTHEATPRIMARYSETPOINT") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint;
                            } else if (SELECT_CASE_var == "INDIRECTHEATALTERNATESETPOINT") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = SourceSideIndirectHeatAltSetpoint;
                            } else {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Invalid Control Mode entered=" + cAlphaArgs(18));
                                ErrorsFound = true;
                            }
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint;
                    }

                    if (!lAlphaFieldBlanks(19)) {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAltSetpointSchedNum = GetScheduleIndex(cAlphaArgs(19));
                        if (WaterThermalTank(WaterThermalTankNum).SourceSideAltSetpointSchedNum == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  " + cAlphaFieldNames(19) +
                                            " not found = " + cAlphaArgs(19));
                            ErrorsFound = true;
                        }
                    }
                    if (NumAlphas > 19) {
                        WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName = cAlphaArgs(20);
                    }

                } // WaterThermalTankNum
            }

            //!!=======   Get WATER HEATER:STRATIFIED ==============================================================================
            if (NumWaterHeaterStratified > 0) {
                cCurrentModuleObject = cStratifiedWHModuleObj; //'WaterHeater:Stratified'

                for (WaterThermalTankNum = NumWaterHeaterMixed + 1; WaterThermalTankNum <= NumWaterHeaterMixed + NumWaterHeaterStratified;
                     ++WaterThermalTankNum) {

                    inputProcessor->getObjectItem(cCurrentModuleObject,
                                                  WaterThermalTankNum - NumWaterHeaterMixed,
                                                  cAlphaArgs,
                                                  NumAlphas,
                                                  rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  lNumericFieldBlanks,
                                                  lAlphaFieldBlanks,
                                                  cAlphaFieldNames,
                                                  cNumericFieldNames);
                    GlobalNames::VerifyUniqueInterObjectName(
                        UniqueWaterThermalTankNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

                    WaterThermalTank(WaterThermalTankNum).Name = cAlphaArgs(1);
                    WaterThermalTank(WaterThermalTankNum).Type = cCurrentModuleObject;
                    WaterThermalTank(WaterThermalTankNum).TypeNum = StratifiedWaterHeater;

                    // default to always on
                    WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = ScheduleAlwaysOn;
                    WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = ScheduleAlwaysOn;

                    WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName = cAlphaArgs(2);

                    WaterThermalTank(WaterThermalTankNum).Volume = rNumericArgs(1);
                    if (WaterThermalTank(WaterThermalTankNum).Volume == AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized = true;
                    }
                    rho = GetDensityGlycol(fluidNameWater, DataGlobals::InitConvTemp, DummyWaterIndex, RoutineNameNoColon);
                    WaterThermalTank(WaterThermalTankNum).Mass = WaterThermalTank(WaterThermalTankNum).Volume * rho;
                    WaterThermalTank(WaterThermalTankNum).Height = rNumericArgs(2);
                    if (WaterThermalTank(WaterThermalTankNum).Height == AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized = true;
                    }

                    {
                        auto const SELECT_CASE_var(cAlphaArgs(3));
                        if (SELECT_CASE_var == "VERTICALCYLINDER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = TankShapeVertCylinder;

                        } else if (SELECT_CASE_var == "HORIZONTALCYLINDER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = TankShapeHorizCylinder;

                        } else if (SELECT_CASE_var == "OTHER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = TankShapeOther;
                            if (rNumericArgs(3) > 0.0) {
                                WaterThermalTank(WaterThermalTankNum).Perimeter = rNumericArgs(3);
                            } else {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                                ":  Tank Perimeter must be greater than zero for Tank Shape=OTHER");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Invalid Tank Shape entered=" + cAlphaArgs(3));
                            WaterThermalTank(WaterThermalTankNum).Shape = TankShapeVertCylinder;
                            ErrorsFound = true;
                        }
                    }

                    if (rNumericArgs(4) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = rNumericArgs(4);
                    } else {
                        // Default to very large number
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = 1.0e9;
                    }

                    // Validate Heater Priority Control
                    {
                        auto const SELECT_CASE_var(cAlphaArgs(4));
                        if (SELECT_CASE_var == "MASTERSLAVE") {
                            WaterThermalTank(WaterThermalTankNum).ControlType = PriorityMasterSlave;

                        } else if (SELECT_CASE_var == "SIMULTANEOUS") {
                            WaterThermalTank(WaterThermalTankNum).ControlType = PrioritySimultaneous;

                        } else {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            ":  Invalid Heater Priority Control entered=" + cAlphaArgs(4));
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule = GetScheduleIndex(cAlphaArgs(5));
                    if (lAlphaFieldBlanks(5)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", missing data.");
                        ShowContinueError("blank field, missing " + cAlphaFieldNames(5) + " is required");
                        ErrorsFound = true;
                    } else if (WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ": " + cAlphaFieldNames(5) + " not found = " + cAlphaArgs(5));
                        ErrorsFound = true;
                    }

                    if (rNumericArgs(5) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = rNumericArgs(5);
                    } else {
                        // Default to very small number (however it can't be TINY or it will break the algorithm)
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = 0.0001;
                    }

                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = rNumericArgs(6);
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacity == AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).HeaterHeight1 = rNumericArgs(7);

                    // adjust tank height used in these calculations for testing heater height
                    Real64 tankHeightForTesting = 0.0;
                    if (WaterThermalTank(WaterThermalTankNum).Shape == TankShapeHorizCylinder) {
                        tankHeightForTesting =
                            2.0 *
                            sqrt((WaterThermalTank(WaterThermalTankNum).Volume / WaterThermalTank(WaterThermalTankNum).Height) / DataGlobals::Pi);
                    } else {
                        tankHeightForTesting = WaterThermalTank(WaterThermalTankNum).Height;
                    }

                    // Test if Heater height is within range
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).HeaterHeight1 > tankHeightForTesting)) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ": Heater 1 is located higher than overall tank height.");
                        ShowContinueError(cNumericFieldNames(2) + " = " + RoundSigDigits(rNumericArgs(2), 4));
                        ShowContinueError(cNumericFieldNames(7) + " = " + RoundSigDigits(rNumericArgs(7), 4));
                        ErrorsFound = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule2 = GetScheduleIndex(cAlphaArgs(6));
                    if (lAlphaFieldBlanks(6)) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", missing data.");
                        ShowContinueError("blank field, missing " + cAlphaFieldNames(6) + " is required");
                        ErrorsFound = true;
                    } else if (WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule2 == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  " + cAlphaFieldNames(6) + " not found = " + cAlphaArgs(6));
                        ErrorsFound = true;
                    }

                    if (rNumericArgs(5) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp2 = rNumericArgs(8);
                    } else {
                        // Default to very small number (however it can't be TINY or it will break the algorithm)
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp2 = 0.0001;
                    }

                    WaterThermalTank(WaterThermalTankNum).MaxCapacity2 = rNumericArgs(9);
                    WaterThermalTank(WaterThermalTankNum).HeaterHeight2 = rNumericArgs(10);

                    // Test if Heater height is within range
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).HeaterHeight2 > tankHeightForTesting)) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ": Heater 2 is located higher than overall tank height.");
                        ShowContinueError(cNumericFieldNames(2) + " = " + RoundSigDigits(rNumericArgs(2), 4));
                        ShowContinueError(cNumericFieldNames(10) + " = " + RoundSigDigits(rNumericArgs(10), 4));
                        ErrorsFound = true;
                    }

                    // Validate Heater Fuel Type
                    {
                        auto const SELECT_CASE_var(cAlphaArgs(7));
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
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Invalid Heater Fuel Type entered=" + cAlphaArgs(7));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).FuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    if (rNumericArgs(11) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).Efficiency = rNumericArgs(11);
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Heater Thermal Efficiency must be greater than zero");
                        ErrorsFound = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycParaLoad = rNumericArgs(12);

                    // Validate Off-Cycle Parasitic Fuel Type
                    {
                        auto const SELECT_CASE_var(cAlphaArgs(8));
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
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            ":  Invalid Off-Cycle Parasitic Fuel Type entered=" + cAlphaArgs(8));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).OffCycParaFuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycParaFracToTank = rNumericArgs(13);
                    WaterThermalTank(WaterThermalTankNum).OffCycParaHeight = rNumericArgs(14);

                    WaterThermalTank(WaterThermalTankNum).OnCycParaLoad = rNumericArgs(15);

                    // Validate On-Cycle Parasitic Fuel Type
                    {
                        auto const SELECT_CASE_var(cAlphaArgs(9));
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
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            ":  Invalid On-Cycle Parasitic Fuel Type entered=" + cAlphaArgs(9));
                            // Set to Electric to avoid errors when setting up output variables
                            WaterThermalTank(WaterThermalTankNum).OnCycParaFuelType = "Electric";
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OnCycParaFracToTank = rNumericArgs(16);
                    WaterThermalTank(WaterThermalTankNum).OnCycParaHeight = rNumericArgs(17);

                    {
                        auto const SELECT_CASE_var(cAlphaArgs(10));
                        if (SELECT_CASE_var == "SCHEDULE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempSchedule;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule = GetScheduleIndex(cAlphaArgs(11));
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                                ":  Ambient Temperature Schedule not found = " + cAlphaArgs(11));
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempZone;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempZone = UtilityRoutines::FindItemInList(cAlphaArgs(12), Zone);
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                                ":  Ambient Temperature Zone not found = " + cAlphaArgs(12));
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "OUTDOORS") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempOutsideAir;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode = GetOnlySingleNode(cAlphaArgs(13),
                                                                                                                ErrorsFound,
                                                                                                                cCurrentModuleObject,
                                                                                                                cAlphaArgs(1),
                                                                                                                NodeType_Air,
                                                                                                                NodeConnectionType_Inlet,
                                                                                                                1,
                                                                                                                ObjectIsNotParent);
                            if (cAlphaArgs(13) != "") {
                                if (!CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode)) {
                                    ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                                    ": Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                                    ShowContinueError("...Referenced Node Name=" + cAlphaArgs(13));
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError(
                                    "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            ":  Invalid Ambient Temperature Indicator entered=" + cAlphaArgs(10));
                            ShowContinueError(" Valid entries are Schedule, Zone, and Outdoors.");
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).SkinLossCoeff = rNumericArgs(18);
                    WaterThermalTank(WaterThermalTankNum).SkinLossFracToZone = rNumericArgs(19);
                    WaterThermalTank(WaterThermalTankNum).OffCycFlueLossCoeff = rNumericArgs(20);
                    WaterThermalTank(WaterThermalTankNum).OffCycFlueLossFracToZone = rNumericArgs(21);

                    // this is temporary until we know fluid type
                    rho = GetDensityGlycol(fluidNameWater, DataGlobals::InitConvTemp, DummyWaterIndex, RoutineNameNoColon);
                    WaterThermalTank(WaterThermalTankNum).MassFlowRateMax = rNumericArgs(22) * rho;

                    if ((cAlphaArgs(16).empty()) && (cAlphaArgs(17).empty())) {
                        if (!cAlphaArgs(14).empty()) {
                            WaterThermalTank(WaterThermalTankNum).FlowRateSchedule = GetScheduleIndex(cAlphaArgs(14));
                            if (WaterThermalTank(WaterThermalTankNum).FlowRateSchedule == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Flow Rate Schedule not found = " + cAlphaArgs(14));
                                ErrorsFound = true;
                            }
                        }
                    }

                    if (!cAlphaArgs(15).empty()) {
                        WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule = GetScheduleIndex(cAlphaArgs(15));
                        if (WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            ":  Cold Water Supply Temperature Schedule not found = " + cAlphaArgs(15));
                            ErrorsFound = true;
                        }
                    }

                    if (NumNums > 22) {
                        WaterThermalTank(WaterThermalTankNum).UseEffectiveness = rNumericArgs(23);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseEffectiveness = 1.0; // Default for stand-alone mode
                    }

                    if (NumNums > 23) {
                        WaterThermalTank(WaterThermalTankNum).UseInletHeight = rNumericArgs(24);
                    } else {
                        // Defaults to bottom of tank
                        WaterThermalTank(WaterThermalTankNum).UseInletHeight = 0.0;
                    }
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).UseInletHeight > WaterThermalTank(WaterThermalTankNum).Height)) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ": Use inlet is located higher than overall tank height.");
                        ShowContinueError(cNumericFieldNames(2) + " = " + RoundSigDigits(rNumericArgs(2), 4));
                        ShowContinueError(cNumericFieldNames(24) + " = " + RoundSigDigits(rNumericArgs(24), 4));
                        ErrorsFound = true;
                    }

                    if ((NumNums > 24) && (rNumericArgs(25) != AutoCalculate)) {
                        WaterThermalTank(WaterThermalTankNum).UseOutletHeight = rNumericArgs(25);
                    } else {
                        // Defaults to top of tank
                        WaterThermalTank(WaterThermalTankNum).UseOutletHeight = WaterThermalTank(WaterThermalTankNum).Height;
                    }
                    if (WaterThermalTank(WaterThermalTankNum).UseOutletHeight == AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).UseOutletHeightWasAutoSized = true;
                    }
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).UseOutletHeight > WaterThermalTank(WaterThermalTankNum).Height)) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ": Use outlet is located higher than overall tank height.");
                        ShowContinueError(cNumericFieldNames(2) + " = " + RoundSigDigits(rNumericArgs(2), 4));
                        ShowContinueError(cNumericFieldNames(25) + " = " + RoundSigDigits(rNumericArgs(25), 4));
                        ErrorsFound = true;
                    }

                    if (NumNums > 25) {
                        if ((rNumericArgs(26) > 1) || (rNumericArgs(26) <= 0)) {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Source Side Effectiveness is out of bounds (>0 to 1)");
                            ErrorsFound = true;
                        }
                        WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = rNumericArgs(26);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = 1.0;
                    }

                    if ((NumNums > 26) && (rNumericArgs(27) != AutoCalculate)) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletHeight = rNumericArgs(27);
                    } else {
                        // Defaults to top of tank
                        WaterThermalTank(WaterThermalTankNum).SourceInletHeight = WaterThermalTank(WaterThermalTankNum).Height;
                    }
                    if (WaterThermalTank(WaterThermalTankNum).SourceInletHeight == AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletHeightWasAutoSized = true;
                    }
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).SourceInletHeight > WaterThermalTank(WaterThermalTankNum).Height)) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ": Source inlet is located higher than overall tank height.");
                        ShowContinueError(cNumericFieldNames(2) + " = " + RoundSigDigits(rNumericArgs(2), 4));
                        ShowContinueError(cNumericFieldNames(27) + " = " + RoundSigDigits(rNumericArgs(27), 4));
                        ErrorsFound = true;
                    }

                    if ((NumNums > 27) && (rNumericArgs(28) != AutoCalculate)) {
                        WaterThermalTank(WaterThermalTankNum).SourceOutletHeight = rNumericArgs(28);
                    } else {
                        // Defaults to bottom of tank
                        WaterThermalTank(WaterThermalTankNum).SourceOutletHeight = 0.0;
                    }
                    if ((!WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).SourceOutletHeight > WaterThermalTank(WaterThermalTankNum).Height)) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ": Source outlet is located higher than overall tank height.");
                        ShowContinueError(cNumericFieldNames(2) + " = " + RoundSigDigits(rNumericArgs(2), 4));
                        ShowContinueError(cNumericFieldNames(28) + " = " + RoundSigDigits(rNumericArgs(28), 4));
                        ErrorsFound = true;
                    }

                    // If no plant nodes are connected, simulate in stand-alone mode.
                    if (cAlphaArgs(16).empty() && cAlphaArgs(17).empty() && cAlphaArgs(18).empty() && cAlphaArgs(19).empty())
                        WaterThermalTank(WaterThermalTankNum).StandAlone = true;

                    if (!lNumericFieldBlanks(29)) {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = rNumericArgs(29);
                        if (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate == AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized = true;
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = 0.0;
                    }

                    WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide = DemandSupply_No;

                    if (!lNumericFieldBlanks(30)) {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = rNumericArgs(30);
                        if (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate == AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized = true;
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = 0.0;
                    }

                    if (NumNums > 30) {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = rNumericArgs(31);
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = 1.5;
                    }

                    WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide = DemandSupply_No;

                    if ((!cAlphaArgs(16).empty()) || (!cAlphaArgs(17).empty())) {
                        WaterThermalTank(WaterThermalTankNum).UseInletNode = GetOnlySingleNode(cAlphaArgs(16),
                                                                                               ErrorsFound,
                                                                                               cCurrentModuleObject,
                                                                                               cAlphaArgs(1),
                                                                                               NodeType_Water,
                                                                                               NodeConnectionType_Inlet,
                                                                                               1,
                                                                                               ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName1 = cAlphaArgs(16);
                        WaterThermalTank(WaterThermalTankNum).UseOutletNode = GetOnlySingleNode(cAlphaArgs(17),
                                                                                                ErrorsFound,
                                                                                                cCurrentModuleObject,
                                                                                                cAlphaArgs(1),
                                                                                                NodeType_Water,
                                                                                                NodeConnectionType_Outlet,
                                                                                                1,
                                                                                                ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName1 = cAlphaArgs(17);

                        if (rNumericArgs(22) > 0) {
                            ShowWarningError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Peak Volumetric Use Flow Rate will not be used");
                        }

                        if (WaterThermalTank(WaterThermalTankNum).FlowRateSchedule > 0) {
                            ShowWarningError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Use Flow Rate Fraction Schedule will not be used");
                        }

                        if (WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule > 0) {
                            ShowWarningError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                             ":  Use side nodes are specified; Cold Water Supply Temperature Schedule will not be used");
                        }
                    }

                    if ((!cAlphaArgs(18).empty()) || (!cAlphaArgs(19).empty())) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode = GetOnlySingleNode(cAlphaArgs(18),
                                                                                                  ErrorsFound,
                                                                                                  cCurrentModuleObject,
                                                                                                  cAlphaArgs(1),
                                                                                                  NodeType_Water,
                                                                                                  NodeConnectionType_Inlet,
                                                                                                  2,
                                                                                                  ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName2 = cAlphaArgs(18);
                        WaterThermalTank(WaterThermalTankNum).SourceOutletNode = GetOnlySingleNode(cAlphaArgs(19),
                                                                                                   ErrorsFound,
                                                                                                   cCurrentModuleObject,
                                                                                                   cAlphaArgs(1),
                                                                                                   NodeType_Water,
                                                                                                   NodeConnectionType_Outlet,
                                                                                                   2,
                                                                                                   ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName2 = cAlphaArgs(19);
                    }

                    // Validate inlet mode
                    {
                        auto const SELECT_CASE_var(cAlphaArgs(20));
                        if (SELECT_CASE_var == "FIXED") {
                            WaterThermalTank(WaterThermalTankNum).InletMode = InletModeFixed;

                        } else if (SELECT_CASE_var == "SEEKING") {
                            WaterThermalTank(WaterThermalTankNum).InletMode = InletModeSeeking;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).Nodes = rNumericArgs(32);
                    WaterThermalTank(WaterThermalTankNum).AdditionalCond = rNumericArgs(33);

                    WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff.allocate(WaterThermalTank(WaterThermalTankNum).Nodes);
                    WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff = 0.0;
                    for (NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                        if (NumNums > 32 + NodeNum) {
                            WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff(NodeNum) = rNumericArgs(33 + NodeNum);
                        } else {
                            break;
                        }
                    }

                    if (NumNums > 33 + WaterThermalTank(WaterThermalTankNum).Nodes) {
                        ShowWarningError(
                            cCurrentModuleObject + " = " + cAlphaArgs(1) +
                            ":  More Additional Loss Coefficients were entered than the number of nodes; extra coefficients will not be used");
                    }

                    SetupStratifiedNodes(WaterThermalTankNum);

                    if (!lAlphaFieldBlanks(21)) {
                        {
                            auto const SELECT_CASE_var(cAlphaArgs(21));
                            if (SELECT_CASE_var == "STORAGETANK") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = SourceSideStorageTank;
                            } else if (SELECT_CASE_var == "INDIRECTHEATPRIMARYSETPOINT") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint;
                            } else if (SELECT_CASE_var == "INDIRECTHEATALTERNATESETPOINT") {
                                WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = SourceSideIndirectHeatAltSetpoint;
                            } else {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Invalid Control Mode entered=" + cAlphaArgs(21));
                                ErrorsFound = true;
                            }
                        }
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint;
                    }

                    if (!lAlphaFieldBlanks(22)) {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAltSetpointSchedNum = GetScheduleIndex(cAlphaArgs(22));
                        if (WaterThermalTank(WaterThermalTankNum).SourceSideAltSetpointSchedNum == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  " + cAlphaFieldNames(22) +
                                            " not found = " + cAlphaArgs(22));
                            ErrorsFound = true;
                        }
                    }

                } // WaterThermalTankNum
            }

            //!!=======   Get Chilled Water :MIXED ===================================================================================
            if (NumChilledWaterMixed > 0) {
                cCurrentModuleObject = cMixedCWTankModuleObj; // 'ThermalStorage:ChilledWater:Mixed'
                for (WaterThermalTankNum = NumWaterHeaterMixed + NumWaterHeaterStratified + 1;
                     WaterThermalTankNum <= NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed;
                     ++WaterThermalTankNum) {

                    inputProcessor->getObjectItem(cCurrentModuleObject,
                                                  WaterThermalTankNum - (NumWaterHeaterMixed + NumWaterHeaterStratified),
                                                  cAlphaArgs,
                                                  NumAlphas,
                                                  rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  lNumericFieldBlanks,
                                                  lAlphaFieldBlanks,
                                                  cAlphaFieldNames,
                                                  cNumericFieldNames);
                    GlobalNames::VerifyUniqueInterObjectName(
                        UniqueWaterThermalTankNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

                    WaterThermalTank(WaterThermalTankNum).Name = cAlphaArgs(1);
                    WaterThermalTank(WaterThermalTankNum).Type = cCurrentModuleObject;
                    WaterThermalTank(WaterThermalTankNum).TypeNum = MixedChilledWaterStorage;
                    WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank = true;
                    WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName = "Chilled Water Storage";

                    WaterThermalTank(WaterThermalTankNum).Volume = rNumericArgs(1);
                    if (WaterThermalTank(WaterThermalTankNum).Volume == AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized = true;
                    }
                    if (rNumericArgs(1) == 0.0) {
                        // Set volume to a really small number to continue simulation
                        WaterThermalTank(WaterThermalTankNum).Volume = 0.000001; // = 1 cm3
                    }

                    WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule = GetScheduleIndex(cAlphaArgs(2));
                    if (WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule == 0) {
                        ShowSevereError("Invalid, " + cAlphaFieldNames(2) + " = " + cAlphaArgs(2));
                        ShowContinueError("Entered in " + cCurrentModuleObject + '=' + cAlphaArgs(1));

                        ErrorsFound = true;
                    }

                    if (rNumericArgs(2) > 0.0001) {
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = rNumericArgs(2);
                    } else {
                        // Default to very small number (however it can't be TINY or it will break the algorithm)
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = 0.5;
                    }

                    if (rNumericArgs(3) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = rNumericArgs(3);
                    } else {
                        // default to just above freezing
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = 1.0;
                    }

                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = rNumericArgs(4);
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacity == AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).MinCapacity = 0.0;
                    WaterThermalTank(WaterThermalTankNum).ControlType = ControlTypeCycle;

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
                        auto const SELECT_CASE_var(cAlphaArgs(3));
                        if (SELECT_CASE_var == "SCHEDULE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempSchedule;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule = GetScheduleIndex(cAlphaArgs(4));
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule == 0) {
                                ShowSevereError("Invalid, " + cAlphaFieldNames(4) + " = " + cAlphaArgs(4));
                                ShowContinueError("Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError("Schedule was not found.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempZone;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempZone = UtilityRoutines::FindItemInList(cAlphaArgs(5), Zone);
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone == 0) {
                                ShowSevereError("Invalid, " + cAlphaFieldNames(5) + " = " + cAlphaArgs(5));
                                ShowContinueError("Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError("Zone was not found.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "OUTDOORS") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempOutsideAir;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode =
                                GetOnlySingleNode(cAlphaArgs(6),
                                                  ErrorsFound,
                                                  cCurrentModuleObject,
                                                  cAlphaArgs(1),
                                                  NodeType_Air,
                                                  NodeConnectionType_OutsideAirReference,
                                                  1,
                                                  ObjectIsNotParent);
                            if (!lAlphaFieldBlanks(6)) {
                                if (!CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode)) {
                                    ShowSevereError("Invalid, " + cAlphaFieldNames(6) + " = " + cAlphaArgs(6));
                                    ShowContinueError("Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                                    ShowContinueError("Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError(
                                    "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            ":  Invalid Ambient Temperature Indicator entered=" + cAlphaArgs(3));
                            ShowContinueError(" Valid entries are Schedule, Zone, and Outdoors.");
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).OffCycLossCoeff = rNumericArgs(5);
                    WaterThermalTank(WaterThermalTankNum).OffCycLossFracToZone = 1.0;

                    WaterThermalTank(WaterThermalTankNum).OnCycLossCoeff = rNumericArgs(5);
                    WaterThermalTank(WaterThermalTankNum).OnCycLossFracToZone = 1.0;

                    WaterThermalTank(WaterThermalTankNum).MassFlowRateMax = 0.0;
                    WaterThermalTank(WaterThermalTankNum).FlowRateSchedule = 0;
                    WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule = 0;

                    // default to always on
                    WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = ScheduleAlwaysOn;
                    WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = ScheduleAlwaysOn;

                    if ((rNumericArgs(6) > 1) || (rNumericArgs(6) < 0)) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Use Side Effectiveness is out of bounds (0 to 1)");
                        ErrorsFound = true;
                    }
                    WaterThermalTank(WaterThermalTankNum).UseEffectiveness = rNumericArgs(6);

                    if ((rNumericArgs(8) > 1) || (rNumericArgs(8) <= 0)) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Source Side Effectiveness is out of bounds (>0 to 1)");
                        ErrorsFound = true;
                    }
                    WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = rNumericArgs(8);

                    if (lNumericFieldBlanks(7)) {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = 0.0;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = rNumericArgs(7);
                        if (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate) {
                            WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide = DemandSupply_No;

                    if (lAlphaFieldBlanks(9)) {
                        WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = ScheduleAlwaysOn;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = GetScheduleIndex(cAlphaArgs(9));
                        if (WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum == 0) {
                            ShowSevereError("Invalid, " + cAlphaFieldNames(9) + " = " + cAlphaArgs(9));
                            ShowContinueError("Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                            ShowContinueError("Schedule was not found.");
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide = DemandSupply_No;

                    if (lNumericFieldBlanks(9)) {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = 0.0;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = rNumericArgs(9);
                        if (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate == AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized = true;
                        }
                    }

                    if (lAlphaFieldBlanks(12)) {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = ScheduleAlwaysOn;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = GetScheduleIndex(cAlphaArgs(12));
                        if (WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum == 0) {
                            ShowSevereError("Invalid, " + cAlphaFieldNames(12) + " = " + cAlphaArgs(12));
                            ShowContinueError("Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                            ShowContinueError("Schedule was not found.");
                            ErrorsFound = true;
                        }
                    }
                    if (lNumericFieldBlanks(10)) {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = 4.0;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = rNumericArgs(10);
                    }

                    if ((!lAlphaFieldBlanks(7)) || (!lAlphaFieldBlanks(8))) {
                        WaterThermalTank(WaterThermalTankNum).UseInletNode = GetOnlySingleNode(cAlphaArgs(7),
                                                                                               ErrorsFound,
                                                                                               cCurrentModuleObject,
                                                                                               cAlphaArgs(1),
                                                                                               NodeType_Water,
                                                                                               NodeConnectionType_Inlet,
                                                                                               1,
                                                                                               ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName1 = cAlphaArgs(7);
                        WaterThermalTank(WaterThermalTankNum).UseOutletNode = GetOnlySingleNode(cAlphaArgs(8),
                                                                                                ErrorsFound,
                                                                                                cCurrentModuleObject,
                                                                                                cAlphaArgs(1),
                                                                                                NodeType_Water,
                                                                                                NodeConnectionType_Outlet,
                                                                                                1,
                                                                                                ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName1 = cAlphaArgs(8);
                    }

                    if ((!lAlphaFieldBlanks(10)) || (!lAlphaFieldBlanks(11))) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode = GetOnlySingleNode(cAlphaArgs(10),
                                                                                                  ErrorsFound,
                                                                                                  cCurrentModuleObject,
                                                                                                  cAlphaArgs(1),
                                                                                                  NodeType_Water,
                                                                                                  NodeConnectionType_Inlet,
                                                                                                  2,
                                                                                                  ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName2 = cAlphaArgs(10);
                        WaterThermalTank(WaterThermalTankNum).SourceOutletNode = GetOnlySingleNode(cAlphaArgs(11),
                                                                                                   ErrorsFound,
                                                                                                   cCurrentModuleObject,
                                                                                                   cAlphaArgs(1),
                                                                                                   NodeType_Water,
                                                                                                   NodeConnectionType_Outlet,
                                                                                                   2,
                                                                                                   ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName2 = cAlphaArgs(11);
                    }

                    if (WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide == DemandSide &&
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode != 0) {
                        RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).SourceInletNode,
                                                    WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                    }

                } // WaterThermalTankNum
            }

            //! end chilled water mixed storage

            //!!=======   Get 'ThermalStorage:ChilledWater:Stratified' =======================================================
            if (NumChilledWaterStratified > 0) {
                cCurrentModuleObject = cStratifiedCWTankModuleObj; // 'ThermalStorage:ChilledWater:Stratified'

                for (WaterThermalTankNum = NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed + 1;
                     WaterThermalTankNum <= NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed + NumChilledWaterStratified;
                     ++WaterThermalTankNum) {

                    inputProcessor->getObjectItem(cCurrentModuleObject,
                                                  WaterThermalTankNum - (NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed),
                                                  cAlphaArgs,
                                                  NumAlphas,
                                                  rNumericArgs,
                                                  NumNums,
                                                  IOStat,
                                                  lNumericFieldBlanks,
                                                  lAlphaFieldBlanks,
                                                  cAlphaFieldNames,
                                                  cNumericFieldNames);
                    GlobalNames::VerifyUniqueInterObjectName(
                        UniqueWaterThermalTankNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound);

                    WaterThermalTank(WaterThermalTankNum).Name = cAlphaArgs(1);
                    WaterThermalTank(WaterThermalTankNum).Type = cCurrentModuleObject;
                    WaterThermalTank(WaterThermalTankNum).TypeNum = StratifiedChilledWaterStorage;
                    WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank = true;
                    WaterThermalTank(WaterThermalTankNum).EndUseSubcategoryName = "Chilled Water Storage";

                    WaterThermalTank(WaterThermalTankNum).Volume = rNumericArgs(1);
                    if (WaterThermalTank(WaterThermalTankNum).Volume == AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized = true;
                    }
                    rho = GetDensityGlycol(fluidNameWater, DataGlobals::InitConvTemp, DummyWaterIndex, RoutineNameNoColon);
                    WaterThermalTank(WaterThermalTankNum).Mass = WaterThermalTank(WaterThermalTankNum).Volume * rho;
                    WaterThermalTank(WaterThermalTankNum).Height = rNumericArgs(2);
                    if (WaterThermalTank(WaterThermalTankNum).Height == AutoSize) {
                        WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized = true;
                    }

                    {
                        auto const SELECT_CASE_var(cAlphaArgs(2));
                        if (SELECT_CASE_var == "VERTICALCYLINDER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = TankShapeVertCylinder;

                        } else if (SELECT_CASE_var == "HORIZONTALCYLINDER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = TankShapeHorizCylinder;

                        } else if (SELECT_CASE_var == "OTHER") {
                            WaterThermalTank(WaterThermalTankNum).Shape = TankShapeOther;
                            if (rNumericArgs(3) > 0.0) {
                                WaterThermalTank(WaterThermalTankNum).Perimeter = rNumericArgs(3);
                            } else {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                                ":  Tank Perimeter must be greater than zero for Tank Shape=OTHER");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Invalid Tank Shape entered=" + cAlphaArgs(2));
                            WaterThermalTank(WaterThermalTankNum).Shape = TankShapeVertCylinder;
                            ErrorsFound = true;
                        }
                    }

                    if (rNumericArgs(6) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = rNumericArgs(6);
                    } else {
                        // default to just above freezing
                        WaterThermalTank(WaterThermalTankNum).TankTempLimit = 1.0;
                    }

                    WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule = GetScheduleIndex(cAlphaArgs(3));
                    if (WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule == 0) {
                        ShowSevereError("Invalid, " + cAlphaFieldNames(3) + " = " + cAlphaArgs(3));
                        ShowContinueError("Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                        ShowContinueError("Schedule was not found.");
                        ErrorsFound = true;
                    }

                    if (rNumericArgs(4) > 0.0) {
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = rNumericArgs(4);
                    } else {
                        // Default to very small number (however it can't be TINY or it will break the algorithm)
                        WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp = 0.0001;
                    }

                    WaterThermalTank(WaterThermalTankNum).HeaterHeight1 = rNumericArgs(5);
                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = rNumericArgs(7);
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacity == AutoSize) {
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
                        auto const SELECT_CASE_var(cAlphaArgs(4));
                        if (SELECT_CASE_var == "SCHEDULE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempSchedule;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule = GetScheduleIndex(cAlphaArgs(5));
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule == 0) {
                                ShowSevereError("Invalid, " + cAlphaFieldNames(5) + " = " + cAlphaArgs(5));
                                ShowContinueError("Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError("Schedule was not found.");
                                ErrorsFound = true;
                            }

                        } else if (SELECT_CASE_var == "ZONE") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempZone;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempZone = UtilityRoutines::FindItemInList(cAlphaArgs(6), Zone);
                            if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone == 0) {
                                ShowSevereError("Invalid, " + cAlphaFieldNames(6) + " = " + cAlphaArgs(6));
                                ShowContinueError("Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError("Zone was not found.");
                                ErrorsFound = true;
                            }
                            WaterThermalTank(WaterThermalTankNum).OffCycLossFracToZone = 1.0;

                        } else if (SELECT_CASE_var == "OUTDOORS") {
                            WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator = AmbientTempOutsideAir;
                            WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode = GetOnlySingleNode(cAlphaArgs(7),
                                                                                                                ErrorsFound,
                                                                                                                cCurrentModuleObject,
                                                                                                                cAlphaArgs(1),
                                                                                                                NodeType_Air,
                                                                                                                NodeConnectionType_Inlet,
                                                                                                                1,
                                                                                                                ObjectIsNotParent);
                            if (!lAlphaFieldBlanks(7)) {
                                if (!CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode)) {
                                    ShowSevereError("Invalid, " + cAlphaFieldNames(7) + " = " + cAlphaArgs(7));
                                    ShowContinueError("Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                                    ShowContinueError("Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node");
                                    ErrorsFound = true;
                                }
                            } else {
                                ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1));
                                ShowContinueError(
                                    "An Ambient Outdoor Air Node name must be used when the Ambient Temperature Indicator is Outdoors.");
                                ErrorsFound = true;
                            }

                        } else {
                            ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) +
                                            ":  Invalid Ambient Temperature Indicator entered=" + cAlphaArgs(4));
                            ShowContinueError("  Valid entries are Schedule, Zone, and Outdoors.");
                            ErrorsFound = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).SkinLossCoeff = rNumericArgs(8);
                    WaterThermalTank(WaterThermalTankNum).SkinLossFracToZone = 1.0;
                    WaterThermalTank(WaterThermalTankNum).OffCycFlueLossCoeff = 0.0;
                    WaterThermalTank(WaterThermalTankNum).OffCycFlueLossFracToZone = 0.0;

                    WaterThermalTank(WaterThermalTankNum).MassFlowRateMax = 0.0;
                    WaterThermalTank(WaterThermalTankNum).FlowRateSchedule = 0;
                    WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule = 0;
                    WaterThermalTank(WaterThermalTankNum).UseEffectiveness = rNumericArgs(9);
                    WaterThermalTank(WaterThermalTankNum).UseInletHeight = rNumericArgs(10);

                    // default to always on
                    WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = ScheduleAlwaysOn;
                    WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = ScheduleAlwaysOn;

                    if (rNumericArgs(10) == AutoCalculate) {
                        WaterThermalTank(WaterThermalTankNum).UseInletHeight = WaterThermalTank(WaterThermalTankNum).Height; // top of tank
                    }
                    if (WaterThermalTank(WaterThermalTankNum).UseInletHeight > WaterThermalTank(WaterThermalTankNum).Height) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ": Use inlet is located higher than overall tank height.");
                        ShowContinueError(cNumericFieldNames(2) + " = " + RoundSigDigits(rNumericArgs(2), 4));
                        ShowContinueError(cNumericFieldNames(10) + " = " + RoundSigDigits(rNumericArgs(10), 4));
                        ErrorsFound = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).UseOutletHeight = rNumericArgs(11);
                    if (WaterThermalTank(WaterThermalTankNum).UseOutletHeight > WaterThermalTank(WaterThermalTankNum).Height) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ": Use outlet is located higher than overall tank height.");
                        ShowContinueError(cNumericFieldNames(2) + " = " + RoundSigDigits(rNumericArgs(2), 4));
                        ShowContinueError(cNumericFieldNames(11) + " = " + RoundSigDigits(rNumericArgs(11), 4));
                        ErrorsFound = true;
                    }

                    if ((rNumericArgs(13) > 1) || (rNumericArgs(13) <= 0)) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ":  Source Side Effectiveness is out of bounds (>0 to 1)");
                        ErrorsFound = true;
                    }
                    WaterThermalTank(WaterThermalTankNum).SourceEffectiveness = rNumericArgs(13);

                    WaterThermalTank(WaterThermalTankNum).SourceInletHeight = rNumericArgs(14);
                    if (WaterThermalTank(WaterThermalTankNum).SourceInletHeight > WaterThermalTank(WaterThermalTankNum).Height) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ": Source inlet is located higher than overall tank height.");
                        ShowContinueError(cNumericFieldNames(2) + " = " + RoundSigDigits(rNumericArgs(2), 4));
                        ShowContinueError(cNumericFieldNames(14) + " = " + RoundSigDigits(rNumericArgs(14), 4));
                        ErrorsFound = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).SourceOutletHeight = rNumericArgs(15);
                    if (rNumericArgs(15) == AutoCalculate) {
                        WaterThermalTank(WaterThermalTankNum).SourceOutletHeight = WaterThermalTank(WaterThermalTankNum).Height; // top of tank
                    }
                    if (WaterThermalTank(WaterThermalTankNum).SourceOutletHeight > WaterThermalTank(WaterThermalTankNum).Height) {
                        ShowSevereError(cCurrentModuleObject + " = " + cAlphaArgs(1) + ": Source outlet is located higher than overall tank height.");
                        ShowContinueError(cNumericFieldNames(2) + " = " + RoundSigDigits(rNumericArgs(2), 4));
                        ShowContinueError(cNumericFieldNames(15) + " = " + RoundSigDigits(rNumericArgs(15), 4));
                        ErrorsFound = true;
                    }

                    WaterThermalTank(WaterThermalTankNum).StandAlone = false;

                    if (lNumericFieldBlanks(12)) {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = 0.0;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = rNumericArgs(12);
                        if (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate == AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide = DemandSupply_No;

                    if (lNumericFieldBlanks(16)) {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = 0.0;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = rNumericArgs(16);
                        if (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate == AutoSize) {
                            WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized = true;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime = rNumericArgs(17);

                    WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide = DemandSupply_No;

                    if ((!lAlphaFieldBlanks(8)) || (!lAlphaFieldBlanks(9))) {
                        WaterThermalTank(WaterThermalTankNum).UseInletNode = GetOnlySingleNode(cAlphaArgs(8),
                                                                                               ErrorsFound,
                                                                                               cCurrentModuleObject,
                                                                                               cAlphaArgs(1),
                                                                                               NodeType_Water,
                                                                                               NodeConnectionType_Inlet,
                                                                                               1,
                                                                                               ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName1 = cAlphaArgs(8);
                        WaterThermalTank(WaterThermalTankNum).UseOutletNode = GetOnlySingleNode(cAlphaArgs(9),
                                                                                                ErrorsFound,
                                                                                                cCurrentModuleObject,
                                                                                                cAlphaArgs(1),
                                                                                                NodeType_Water,
                                                                                                NodeConnectionType_Outlet,
                                                                                                1,
                                                                                                ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName1 = cAlphaArgs(9);
                    }

                    if ((!lAlphaFieldBlanks(11)) || (!lAlphaFieldBlanks(12))) {
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode = GetOnlySingleNode(cAlphaArgs(11),
                                                                                                  ErrorsFound,
                                                                                                  cCurrentModuleObject,
                                                                                                  cAlphaArgs(1),
                                                                                                  NodeType_Water,
                                                                                                  NodeConnectionType_Inlet,
                                                                                                  2,
                                                                                                  ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName2 = cAlphaArgs(11);
                        WaterThermalTank(WaterThermalTankNum).SourceOutletNode = GetOnlySingleNode(cAlphaArgs(12),
                                                                                                   ErrorsFound,
                                                                                                   cCurrentModuleObject,
                                                                                                   cAlphaArgs(1),
                                                                                                   NodeType_Water,
                                                                                                   NodeConnectionType_Outlet,
                                                                                                   2,
                                                                                                   ObjectIsNotParent);
                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName2 = cAlphaArgs(12);
                    }

                    if (lAlphaFieldBlanks(10)) {
                        WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = ScheduleAlwaysOn;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum = GetScheduleIndex(cAlphaArgs(10));
                        if (WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum == 0) {
                            ShowSevereError("Invalid, " + cAlphaFieldNames(10) + " = " + cAlphaArgs(10));
                            ShowContinueError("Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                            ShowContinueError("Schedule was not found.");
                            ErrorsFound = true;
                        }
                    }

                    if (WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide == DemandSide &&
                        WaterThermalTank(WaterThermalTankNum).SourceInletNode != 0) {
                        RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).SourceInletNode,
                                                    WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                    }

                    if (lAlphaFieldBlanks(13)) {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = ScheduleAlwaysOn;
                    } else {
                        WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum = GetScheduleIndex(cAlphaArgs(13));
                        if (WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum == 0) {
                            ShowSevereError("Invalid, " + cAlphaFieldNames(13) + " = " + cAlphaArgs(13));
                            ShowContinueError("Entered in " + cCurrentModuleObject + " = " + cAlphaArgs(1));
                            ShowContinueError("Schedule was not found.");
                            ErrorsFound = true;
                        }
                    }

                    // Validate inlet mode
                    {
                        auto const SELECT_CASE_var(cAlphaArgs(14));
                        if (SELECT_CASE_var == "FIXED") {
                            WaterThermalTank(WaterThermalTankNum).InletMode = InletModeFixed;

                        } else if (SELECT_CASE_var == "SEEKING") {
                            WaterThermalTank(WaterThermalTankNum).InletMode = InletModeSeeking;
                        }
                    }

                    WaterThermalTank(WaterThermalTankNum).Nodes = rNumericArgs(18);
                    WaterThermalTank(WaterThermalTankNum).AdditionalCond = rNumericArgs(19);

                    WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff.allocate(WaterThermalTank(WaterThermalTankNum).Nodes);
                    WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff = 0.0;
                    for (NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                        if (NumNums > 19 + NodeNum) {
                            WaterThermalTank(WaterThermalTankNum).AdditionalLossCoeff(NodeNum) = rNumericArgs(19 + NodeNum);
                        } else {
                            break;
                        }
                    }

                    if (NumNums > 19 + WaterThermalTank(WaterThermalTankNum).Nodes) {
                        ShowWarningError(
                            cCurrentModuleObject + " = " + cAlphaArgs(1) +
                            ":  More Additional Loss Coefficients were entered than the number of nodes; extra coefficients will not be used");
                    }

                    SetupStratifiedNodes(WaterThermalTankNum);

                } // WaterThermalTankNum
            }
            //!  end stratified chilled water storage

            //!!=======   Check Water Heaters ======================================================================================

            //   Loop through all desuperheating coils and then search all water heaters for the tank connected to the desuperheating coil
            if (NumWaterHeaterDesuperheater > 0) {
                cCurrentModuleObject = "Coil:WaterHeating:Desuperheater";
                for (DesuperheaterNum = 1; DesuperheaterNum <= NumWaterHeaterDesuperheater; ++DesuperheaterNum) {

                    for (CheckWaterHeaterNum = 1; CheckWaterHeaterNum <= NumWaterThermalTank; ++CheckWaterHeaterNum) {
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
                            ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ':');
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
                            ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ':');
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
                        ShowSevereError(cCurrentModuleObject + " = " + WaterHeaterDesuperheater(DesuperheaterNum).Name + ':');
                        ShowContinueError(" Water heater tank = " + WaterHeaterDesuperheater(DesuperheaterNum).TankName + " not found.");
                        ErrorsFound = true;
                    }

                } // DO DesuperheaterNum = 1, NumWaterHeaterDesuperheater
            }

            // Loop through HPWH's and then search all water heaters for the tank connected to the HPWH
            if (NumHeatPumpWaterHeater > 0) {

                int const NumPumpedCondenser =
                    inputProcessor->getNumObjectsFound(cHPWHPumpedCondenser); // number of WaterHeater:HeatPump:PumpedCondenser objects
                for (HPWaterHeaterNum = 1; HPWaterHeaterNum <= NumHeatPumpWaterHeater; ++HPWaterHeaterNum) {

                    // Create reference to current HPWH object in array.
                    HeatPumpWaterHeaterData &HPWH = HPWaterHeater(HPWaterHeaterNum);
                    if (HPWaterHeaterNum <= NumPumpedCondenser) {
                        // Pumped Condenser
                        cCurrentModuleObject = cHPWHPumpedCondenser;
                    } else {
                        // Wrapped Condenser
                        cCurrentModuleObject = cHPWHWrappedCondenser;
                    }

                    // find the tank associated with the heat pump water heater and change its %TYPE to HEAT PUMP:WATER HEATER
                    for (CheckWaterHeaterNum = 1; CheckWaterHeaterNum <= NumWaterThermalTank; ++CheckWaterHeaterNum) {

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

                        if (((Tank.TypeNum == MixedWaterHeater) && (HPWH.TypeNum == TypeOf_HeatPumpWtrHeaterPumped)) ||
                            (Tank.TypeNum == StratifiedWaterHeater)) {
                            HPWH.TankType = Tank.Type;
                            HPWH.TankTypeNum = Tank.TypeNum;
                            // use typenum parameter to simulate heatpumpwaterheater in standard ratings procedure
                            // WaterThermalTank.TypeNum = HeatPumpWaterHeater for a HPWH
                            // Tank.TypeNum = HPWH.TypeNum
                        } else {
                            ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name + ':');
                            ShowContinueError("Invalid water heater tank type = " + Tank.Type);
                            ErrorsFound = true;
                        }

                        // Set up comp set for condenser water side nodes (reverse inlet/outlet for water heater)
                        WaterHeaterSaveNodes const &HPWHSaveNode = HPWHSaveNodeNames(HPWaterHeaterNum);
                        if (HPWH.bIsIHP) {
                            SetUpCompSets(HPWH.Type,
                                          HPWH.Name,
                                          HPWH.DXCoilType,
                                          HPWH.DXCoilName + " Water Coil",
                                          HPWHSaveNode.InletNodeName1,
                                          HPWHSaveNode.OutletNodeName1,
                                          "HPWH To Coil");
                        } else {
                            SetUpCompSets(HPWH.Type,
                                          HPWH.Name,
                                          HPWH.DXCoilType,
                                          HPWH.DXCoilName,
                                          HPWHSaveNode.InletNodeName1,
                                          HPWHSaveNode.OutletNodeName1,
                                          "HPWH To Coil");
                        }
                        SetUpCompSets(HPWH.Type,
                                      HPWH.Name,
                                      HPWH.TankType,
                                      HPWH.TankName,
                                      HPWHSaveNode.OutletNodeName1,
                                      HPWHSaveNode.InletNodeName1,
                                      "HPWH To Tank");

                        // do not allow modulating control for HPWH's (i.e. modulating control usually used for tankless WH's)
                        if (Tank.ControlType == ControlTypeModulate) {
                            ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name + ':');
                            ShowContinueError("Heater Control Type for " + Tank.Type + " = " + Tank.Name + " must be CYCLE.");
                            ErrorsFound = true;
                        }

                        Tank.HeatPumpNum = HPWaterHeaterNum;
                        HPWH.WaterHeaterTankNum = CheckWaterHeaterNum;
                        HPWH.FoundTank = true;

                        if (Tank.DesuperheaterNum > 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name +
                                            "and Coil:WaterHeating:Desuperheater = " + WaterHeaterDesuperheater(CheckWaterHeaterNum).Name +
                                            ":  cannot be connected to the same water heater tank = " + Tank.Name);
                        }

                        // check that water heater source side effectiveness is greater than 0
                        if (Tank.SourceEffectiveness <= 0.0) {
                            ShowSevereError(
                                cCurrentModuleObject + " = " + HPWH.Name +
                                ":  Invalid source side effectiveness for heat pump water heater = " + TrimSigDigits(Tank.SourceEffectiveness, 3));
                            ShowContinueError(" water heater source effectiveness will default to 1.0 and simulation continues.");
                            Tank.SourceEffectiveness = 1.0;
                        }

                        // Set up the source side nodes for wrapped condensers
                        if (HPWH.TypeNum == TypeOf_HeatPumpWtrHeaterWrapped) {
                            if (Tank.SourceInletNode > 0 || Tank.SourceOutletNode > 0) {
                                ShowSevereError(Tank.Type + " = " + Tank.Name + " has a source inlet or outlet node specified,");
                                ShowContinueError("but it is attached to " + HPWH.Type + " = " + HPWH.Name +
                                                  ", which doesn't permit source side connections.");
                                ShowContinueError("Please leave the source side inlet and outlet fields blank.");
                                ErrorsFound = true;
                            } else {
                                WaterHeaterSaveNodes &HPWHNodeNames = HPWHSaveNodeNames(HPWaterHeaterNum);
                                WaterHeaterSaveNodes &TankNodenames = WHSaveNodeNames(CheckWaterHeaterNum);
                                Tank.SourceInletNode = GetOnlySingleNode(HPWHNodeNames.OutletNodeName1,
                                                                         ErrorsFound,
                                                                         Tank.Type,
                                                                         Tank.Name,
                                                                         NodeType_Water,
                                                                         NodeConnectionType_Inlet,
                                                                         2,
                                                                         ObjectIsNotParent);
                                TankNodenames.InletNodeName2 = HPWHNodeNames.OutletNodeName1;
                                Tank.SourceOutletNode = GetOnlySingleNode(HPWHNodeNames.InletNodeName1,
                                                                          ErrorsFound,
                                                                          Tank.Type,
                                                                          Tank.Name,
                                                                          NodeType_Water,
                                                                          NodeConnectionType_Outlet,
                                                                          2,
                                                                          ObjectIsNotParent);
                                TankNodenames.OutletNodeName2 = HPWHNodeNames.InletNodeName1;
                            }

                            // Mark the tank as not stand alone because it is connected now.
                            Tank.StandAlone = false;
                        }

                        // Set HPWH structure variable StandAlone to TRUE if use nodes are not connected
                        if (Tank.UseInletNode == 0 && Tank.UseOutletNode == 0) HPWH.StandAlone = true;

                        if (HPWH.WHUseInletNode != Tank.UseInletNode || HPWH.WHUseOutletNode != Tank.UseOutletNode) {
                            ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name + ':');
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
                                //              CALL SetUpCompSets(HPWH%Type, HPWH%Name, &
                                //                     HPWH%TankType, &
                                //                     HPWH%TankName, &
                                //                     WHSaveNodeNames(CheckWaterHeaterNum)%InletNodeName1,WHSaveNodeNames(CheckWaterHeaterNum)%OutletNodeName1)
                                TestCompSet(HPWH.Type,
                                            HPWH.Name,
                                            WHSaveNodeNames(CheckWaterHeaterNum).InletNodeName1,
                                            WHSaveNodeNames(CheckWaterHeaterNum).OutletNodeName1,
                                            "Water Nodes");
                            }
                        }

                        // verify HP/tank source node connections
                        if (HPWH.CondWaterInletNode != Tank.SourceOutletNode) {
                            ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name + ':');
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
                            ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name + ':');
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
                        if (HPWH.TypeNum == TypeOf_HeatPumpWtrHeaterWrapped) {
                            // make sure the top of the condenser is not above the tank height.
                            if (HPWH.WrappedCondenserTopLocation > Tank.Height) {
                                ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name + ':');
                                ShowContinueError("The height of the top of the wrapped condenser is greater than the height of the tank.");
                                ErrorsFound = true;
                            }
                        }

                        // Verify tank name is in a zone equipment list if HPWH Inlet Air Configuration is Zone Air Only or Zone and Outdoor Air
                        if (HPWH.InletAirConfiguration == AmbientTempZone || HPWH.InletAirConfiguration == AmbientTempZoneAndOA) {
                            if (allocated(ZoneEquipConfig) && allocated(ZoneEquipList)) {
                                FoundTankInList = false;
                                TankNotLowestPriority = false;
                                for (ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= NumOfZones; ++ZoneEquipConfigNum) {
                                    if (ZoneEquipConfig(ZoneEquipConfigNum).ActualZoneNum != HPWH.AmbientTempZone) continue;
                                    if (ZoneEquipConfigNum <= NumOfZones) {
                                        for (ZoneEquipListNum = 1; ZoneEquipListNum <= NumOfZones; ++ZoneEquipListNum) {
                                            if (ZoneEquipConfig(ZoneEquipConfigNum).EquipListName != ZoneEquipList(ZoneEquipListNum).Name) continue;
                                            if (ZoneEquipConfigNum <= NumOfZones) {
                                                for (EquipmentTypeNum = 1; EquipmentTypeNum <= ZoneEquipList(ZoneEquipListNum).NumOfEquipTypes;
                                                     ++EquipmentTypeNum) {
                                                    if (ZoneEquipList(ZoneEquipListNum).EquipName(EquipmentTypeNum) != HPWH.Name) continue;
                                                    FoundTankInList = true;
                                                    TankCoolingPriority = ZoneEquipList(ZoneEquipListNum).CoolingPriority(EquipmentTypeNum);
                                                    TankHeatingPriority = ZoneEquipList(ZoneEquipListNum).HeatingPriority(EquipmentTypeNum);
                                                    break;
                                                } // EquipmentTypeNum
                                                if (!FoundTankInList) {
                                                    ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name + ':');
                                                    ShowContinueError("Heat pump water heater type and name must be listed in the correct "
                                                                      "ZoneHVAC:EquipmentList object when Inlet Air Configuration is equal to "
                                                                      "ZoneAirOnly or ZoneAndOutdoorAir.");
                                                    ErrorsFound = true;
                                                }
                                                //                     check that tank has lower priority than all other non-HPWH objects in Zone
                                                //                     Equipment List
                                                for (EquipmentTypeNum = 1; EquipmentTypeNum <= ZoneEquipList(ZoneEquipListNum).NumOfEquipTypes;
                                                     ++EquipmentTypeNum) {
                                                    if (UtilityRoutines::SameString(ZoneEquipList(ZoneEquipListNum).EquipType(EquipmentTypeNum),
                                                                                    cCurrentModuleObject))
                                                        continue;
                                                    if (TankCoolingPriority > ZoneEquipList(ZoneEquipListNum).CoolingPriority(EquipmentTypeNum) ||
                                                        TankHeatingPriority > ZoneEquipList(ZoneEquipListNum).HeatingPriority(EquipmentTypeNum)) {
                                                        TankNotLowestPriority = true;
                                                    }
                                                } // EquipmentTypeNum
                                                if (TankNotLowestPriority && FoundTankInList) {
                                                    ShowWarningError(cCurrentModuleObject + " = " + HPWH.Name + ':');
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
                                ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name + ':');
                                ShowContinueError("ZoneHVAC:EquipmentList and ZoneHVAC:EquipmentConnections objects are required when Inlet Air "
                                                  "Configuration is either ZoneAirOnly or ZoneAndOutdoorAir.");
                                ErrorsFound = true;
                            } // ALLOCATED
                        }     // InletAirConfiguration

                        if (Tank.TypeNum == StratifiedWaterHeater) {

                            // Nodal heat distribution fraction for stratified tank wrapped condensers
                            if (HPWH.TypeNum == TypeOf_HeatPumpWtrHeaterWrapped) {
                                if (Tank.Shape == TankShapeHorizCylinder) {
                                    ShowWarningError(cCurrentModuleObject + " = " + HPWH.Name + ":");
                                    ShowContinueError("A wrapped condenser HPWH model should not be used with a horizontal stratified tank.");
                                    ShowContinueError(
                                        "Ignoring condenser location and distributing heat evenly throughout the tank. Simulation continues.");
                                    Real64 const SameFrac = 1.0 / Tank.Nodes;
                                    for (NodeNum = 1; NodeNum <= Tank.Nodes; ++NodeNum) {
                                        Tank.Node(NodeNum).HPWHWrappedCondenserHeatingFrac = SameFrac;
                                    }
                                } else {
                                    Real64 H0 = Tank.Height; // height of top of node
                                    Real64 H;                // height of bottom of node
                                    Real64 SumFrac(0.0);
                                    // Get the fraction of each stratified node that is wrapped by the condenser
                                    for (NodeNum = 1; NodeNum <= Tank.Nodes; ++NodeNum) {
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
                                    for (NodeNum = 1; NodeNum <= Tank.Nodes; ++NodeNum) {
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
                            if (Tank.Shape == TankShapeVertCylinder || Tank.Shape == TankShapeOther) {
                                TankHeight = Tank.Height;
                            } else {
                                assert(Tank.Shape == TankShapeHorizCylinder);
                                // For horizontal cylinders, the tank "height" is actually the length.
                                // We need to calculate the height.
                                Real64 EndArea = Tank.Volume / Tank.Height;
                                Real64 Radius = std::sqrt(EndArea / DataGlobals::Pi);
                                TankHeight = 2.0 * Radius; // actual vertical height
                            }

                            // Make sure the control sensor locations are in the tank
                            if (HPWH.ControlSensor1Height < 0.0 || HPWH.ControlSensor1Height > TankHeight) {
                                ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name + ':');
                                ShowContinueError("Control Sensor 1 is located outside the tank.");
                                ErrorsFound = true;
                            }
                            if (HPWH.ControlSensor2Height < 0.0 || HPWH.ControlSensor2Height > TankHeight) {
                                ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name + ':');
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
                        ShowSevereError(cCurrentModuleObject + " = " + HPWH.Name + ':');
                        ShowContinueError("Water heater tank object not found = " + HPWH.TankType + ", " + HPWH.TankName);
                        ErrorsFound = true;
                    }

                } // DO HPWaterHeaterNum = 1, NumHeatPumpWaterHeater
            }

            // Get water heater sizing input.
            cCurrentModuleObject = "WaterHeater:Sizing";
            NumWaterHeaterSizing = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

            if (NumWaterHeaterSizing > 0) {

                for (WHsizingNum = 1; WHsizingNum <= NumWaterHeaterSizing; ++WHsizingNum) {
                    inputProcessor->getObjectItem(cCurrentModuleObject, WHsizingNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat);

                    // find which water heater this object is for
                    WaterThermalTankNum = UtilityRoutines::FindItemInList(cAlphaArgs(1), WaterThermalTank);
                    if (WaterThermalTankNum == 0) {
                        // did not match name throw warning.
                        ShowSevereError(cCurrentModuleObject + " object name: " + cAlphaArgs(1) +
                                        " does not match any of the water heaters defined in the file");
                        ErrorsFound = true;
                        continue;
                    } else { // we have a match
                        // store the sizing data in "sizing" nested derived type for the correct water heater

                        if (UtilityRoutines::SameString(cAlphaArgs(2), "PeakDraw")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizePeakDraw;
                        } else if (UtilityRoutines::SameString(cAlphaArgs(2), "ResidentialHUD-FHAMinimum")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizeResidentialMin;
                        } else if (UtilityRoutines::SameString(cAlphaArgs(2), "PerPerson")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizePerPerson;
                        } else if (UtilityRoutines::SameString(cAlphaArgs(2), "PerFloorArea")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizePerFloorArea;
                        } else if (UtilityRoutines::SameString(cAlphaArgs(2), "PerUnit")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizePerUnit;
                        } else if (UtilityRoutines::SameString(cAlphaArgs(2), "PerSolarCollectorArea")) {
                            WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode = SizePerSolarColArea;
                        } else {
                            // wrong design mode entered, throw error
                            ShowSevereError(cCurrentModuleObject + " object named: " + cAlphaArgs(1) +
                                            " contains an incorrect Design Mode of: " + cAlphaArgs(2));
                            ErrorsFound = true;
                        }

                        WaterThermalTank(WaterThermalTankNum).Sizing.TankDrawTime = rNumericArgs(1);
                        WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryTime = rNumericArgs(2);
                        WaterThermalTank(WaterThermalTankNum).Sizing.NominalVolForSizingDemandSideFlow = rNumericArgs(3);
                        WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms = int(rNumericArgs(4));
                        WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms = int(rNumericArgs(5));
                        WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerPerson = rNumericArgs(6);
                        WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerPerson = rNumericArgs(7);
                        WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerArea = rNumericArgs(8);
                        WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerArea = rNumericArgs(9);
                        WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits = rNumericArgs(10);
                        WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerUnit = rNumericArgs(11);
                        WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerUnit = rNumericArgs(12);
                        WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerCollectorArea = rNumericArgs(13);
                        WaterThermalTank(WaterThermalTankNum).Sizing.HeightAspectRatio = rNumericArgs(14);

                        {
                            auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode);

                            if (SELECT_CASE_var == SizeNotSet) {
                                // do nothing, error thrown if design mode not found
                            } else if (SELECT_CASE_var == SizePeakDraw) { // need to have entered a reasonable value for TankDrawTime
                                if (WaterThermalTank(WaterThermalTankNum).Sizing.TankDrawTime <= 0.0) {
                                    ShowSevereError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                    ", design mode set to Peak Draw but needs a positive value for tank draw time");
                                    ErrorsFound = true;
                                }
                                // constrain crazy sizes by limiting to 10 years or 8760*10
                                if (WaterThermalTank(WaterThermalTankNum).Sizing.TankDrawTime > 87600.0) {
                                    ShowWarningError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                     ",  has input with an unreasonably large Tank Draw Time, more than 10 years");
                                    ErrorsFound = true;
                                }
                                // if both volume and demand side flow connections are autosized, must be a good NominalVolForSizingDemandSideFlow
                                if ((WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide == DemandSide) &&
                                    (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized)) {
                                    if (WaterThermalTank(WaterThermalTankNum).Sizing.NominalVolForSizingDemandSideFlow <= 0.0) {
                                        ShowWarningError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                         " needs a value for Nominal Tank Volume for Autosizing Plant Connections");
                                        ErrorsFound = true;
                                    }
                                }
                                if ((WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide == DemandSide) &&
                                    (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized)) {
                                    if (WaterThermalTank(WaterThermalTankNum).Sizing.NominalVolForSizingDemandSideFlow <= 0.0) {
                                        ShowWarningError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                         " needs a value for Nominal Tank Volume for Autosizing Plant Connections");
                                        ErrorsFound = true;
                                    }
                                }

                            } else if (SELECT_CASE_var == SizeResidentialMin) {
                                // it would have to have at least on bedroom and any more than 10 is crazy for this mode
                                if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms < 1) {
                                    ShowSevereError(cCurrentModuleObject + ", named " + cAlphaArgs(1) + ", mode needs at least one bedroom");
                                    ErrorsFound = true;
                                }
                                if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms > 10) {
                                    ShowWarningError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                     ", probably has too many bedrooms for the selected design mode");
                                }

                            } else if (SELECT_CASE_var == SizePerPerson) {

                                if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerPerson <= 0.0)) {
                                    ShowSevereError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                    ", PerPerson mode needs positive value input for storage capacity per person");
                                    ErrorsFound = true;
                                }

                                if ((WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerPerson <= 0.0)) {
                                    ShowSevereError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                    ", PerPerson mode needs positive value input for recovery capacity per person");
                                    ErrorsFound = true;
                                }

                            } else if (SELECT_CASE_var == SizePerFloorArea) {
                                if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerArea <= 0.0)) {
                                    ShowSevereError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                    ", PerArea mode needs positive value input for storage capacity per floor area");
                                    ErrorsFound = true;
                                }
                                if ((WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerArea <= 0.0)) {
                                    ShowSevereError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                    ", PerArea mode needs positive value input for recovery capacity per floor area");
                                    ErrorsFound = true;
                                }

                            } else if (SELECT_CASE_var == SizePerUnit) {
                                if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerUnit <= 0.0)) {
                                    ShowSevereError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for storage capacity per unit");
                                    ErrorsFound = true;
                                }
                                if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits <= 0.0)) {
                                    ShowSevereError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for number of units");
                                    ErrorsFound = true;
                                }
                                if ((WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerUnit <= 0.0)) {
                                    ShowSevereError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for recovery capacity per unit");
                                    ErrorsFound = true;
                                }
                                if ((WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits <= 0.0)) {
                                    ShowSevereError(cCurrentModuleObject + ", named " + cAlphaArgs(1) +
                                                    ", PerUnit mode needs positive value input for number of units");
                                    ErrorsFound = true;
                                }
                            } else if (SELECT_CASE_var == SizePerSolarColArea) {
                                if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                                    (WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerCollectorArea <= 0.0)) {
                                    ShowSevereError(
                                        cCurrentModuleObject + ", named " + cAlphaArgs(1) +
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
                for (WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum) {

                    if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode == SizeNotSet)) {
                        ShowWarningError("Water heater named " + WaterThermalTank(WaterThermalTankNum).Name +
                                         "has tank volume set to AUTOSIZE but it is missing associated WaterHeater:Sizing object");
                        ErrorsFound = true;
                    }
                    if ((WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode == SizeNotSet)) {
                        ShowWarningError("Water heater named " + WaterThermalTank(WaterThermalTankNum).Name +
                                         "has heater capacity set to AUTOSIZE but it is missing associated WaterHeater:Sizing object");
                        ErrorsFound = true;
                    }
                    if ((WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) &&
                        (WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode == SizeNotSet)) {
                        ShowWarningError("Water heater named " + WaterThermalTank(WaterThermalTankNum).Name +
                                         "has tank height set to AUTOSIZE but it is missing associated WaterHeater:Sizing object");
                        ErrorsFound = true;
                    }
                }
            }

            //!   now do calls to TestCompSet for tanks, depending on nodes and heat pump water heater
            if (NumWaterThermalTank > 0) {
                for (WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum) {
                    if (WaterThermalTank(WaterThermalTankNum).UseInletNode > 0 && WaterThermalTank(WaterThermalTankNum).UseOutletNode > 0) {
                        if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                            // do nothing, Use nodes are tested for HeatPump:WaterHeater not tank
                        } else {
                            TestCompSet(WaterThermalTank(WaterThermalTankNum).Type,
                                        WaterThermalTank(WaterThermalTankNum).Name,
                                        WHSaveNodeNames(WaterThermalTankNum).InletNodeName1,
                                        WHSaveNodeNames(WaterThermalTankNum).OutletNodeName1,
                                        "Use Side Water Nodes");
                        }
                    }
                    if (WaterThermalTank(WaterThermalTankNum).SourceInletNode > 0 && WaterThermalTank(WaterThermalTankNum).SourceOutletNode > 0) {

                        TestCompSet(WaterThermalTank(WaterThermalTankNum).Type,
                                    WaterThermalTank(WaterThermalTankNum).Name,
                                    WHSaveNodeNames(WaterThermalTankNum).InletNodeName2,
                                    WHSaveNodeNames(WaterThermalTankNum).OutletNodeName2,
                                    "Source Side Water Nodes");
                    }
                }
            }

            if (NumWaterThermalTank > 0) {
                for (WaterThermalTankNum = 1; WaterThermalTankNum <= NumWaterThermalTank; ++WaterThermalTankNum) {
                    if ((WaterThermalTank(WaterThermalTankNum).TypeNum != MixedChilledWaterStorage) &&
                        (WaterThermalTank(WaterThermalTankNum).TypeNum != StratifiedChilledWaterStorage)) {
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
                        if (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedWaterHeater) {

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

                            for (NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                SetupOutputVariable("Water Heater Temperature Node " + TrimSigDigits(NodeNum),
                                                    OutputProcessor::Unit::C,
                                                    WaterThermalTank(WaterThermalTankNum).Node(NodeNum).TempAvg,
                                                    "System",
                                                    "Average",
                                                    WaterThermalTank(WaterThermalTankNum).Name);
                            }

                            for (NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                SetupOutputVariable("Water Heater Final Temperature Node " + TrimSigDigits(NodeNum),
                                                    OutputProcessor::Unit::C,
                                                    WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Temp,
                                                    "System",
                                                    "Average",
                                                    WaterThermalTank(WaterThermalTankNum).Name);
                            }
                        }

                        if (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedWaterHeater) {

                            for (NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                gio::write(OutputFileInits, Format_723)
                                    << TrimSigDigits(NodeNum) << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Height, 4)
                                    << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Volume, 4)
                                    << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).MaxCapacity, 3)
                                    << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).OffCycLossCoeff, 4)
                                    << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).OnCycLossCoeff, 4)
                                    << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Inlets)
                                    << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Outlets);
                            }
                        }

                    } else if ((WaterThermalTank(WaterThermalTankNum).TypeNum == MixedChilledWaterStorage) ||
                               (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedChilledWaterStorage)) {
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

                        if (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedChilledWaterStorage) {

                            for (NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                SetupOutputVariable("Chilled Water Thermal Storage Temperature Node " + TrimSigDigits(NodeNum) + "",
                                                    OutputProcessor::Unit::C,
                                                    WaterThermalTank(WaterThermalTankNum).Node(NodeNum).TempAvg,
                                                    "System",
                                                    "Average",
                                                    WaterThermalTank(WaterThermalTankNum).Name);
                            }

                            for (NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                SetupOutputVariable("Chilled Water Thermal Storage Final Temperature Node " + TrimSigDigits(NodeNum) + "",
                                                    OutputProcessor::Unit::C,
                                                    WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Temp,
                                                    "System",
                                                    "Average",
                                                    WaterThermalTank(WaterThermalTankNum).Name);
                            }
                        }

                        if (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedChilledWaterStorage) {

                            for (NodeNum = 1; NodeNum <= WaterThermalTank(WaterThermalTankNum).Nodes; ++NodeNum) {
                                gio::write(OutputFileInits, Format_724)
                                    << TrimSigDigits(NodeNum) << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Height, 4)
                                    << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Volume, 4)
                                    << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).OffCycLossCoeff, 4)
                                    << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Inlets)
                                    << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Node(NodeNum).Outlets);
                            }
                        }
                    }

                    // set up internal gains if tank is in a thermal zone
                    if (WaterThermalTank(WaterThermalTankNum).AmbientTempZone > 0) {
                        {
                            auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).TypeNum);

                            if (SELECT_CASE_var == MixedWaterHeater) {
                                SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum).AmbientTempZone,
                                                      "WaterHeater:Mixed",
                                                      WaterThermalTank(WaterThermalTankNum).Name,
                                                      IntGainTypeOf_WaterHeaterMixed,
                                                      WaterThermalTank(WaterThermalTankNum).AmbientZoneGain);
                            } else if (SELECT_CASE_var == StratifiedWaterHeater) {
                                SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum).AmbientTempZone,
                                                      "WaterHeater:Stratified",
                                                      WaterThermalTank(WaterThermalTankNum).Name,
                                                      IntGainTypeOf_WaterHeaterStratified,
                                                      WaterThermalTank(WaterThermalTankNum).AmbientZoneGain);
                            } else if (SELECT_CASE_var == MixedChilledWaterStorage) {
                                SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum).AmbientTempZone,
                                                      "ThermalStorage:ChilledWater:Mixed",
                                                      WaterThermalTank(WaterThermalTankNum).Name,
                                                      IntGainTypeOf_ThermalStorageChilledWaterMixed,
                                                      WaterThermalTank(WaterThermalTankNum).AmbientZoneGain);
                            } else if (SELECT_CASE_var == StratifiedChilledWaterStorage) {
                                SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum).AmbientTempZone,
                                                      "ThermalStorage:ChilledWater:Stratified",
                                                      WaterThermalTank(WaterThermalTankNum).Name,
                                                      IntGainTypeOf_ThermalStorageChilledWaterStratified,
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

        // Using/Aliasing
        using CurveManager::CurveValue;
        using CurveManager::GetCurveType;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // FLOW:
        IsValid = true;

        // Check 0 and 1
        if (CurveValue(CurveIndex, 0.0) <= 0) IsValid = false;
        if (CurveValue(CurveIndex, 1.0) <= 0) IsValid = false;

        if (IsValid) { // Check min/maxs

            {
                auto const SELECT_CASE_var(GetCurveType(CurveIndex));

                if (SELECT_CASE_var == "QUADRATIC") {
                    // Curve coeffs are not currently exposed so there's no good way to do this yet

                } else if (SELECT_CASE_var == "CUBIC") {
                }
            }
        }
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

        // Using/Aliasing
        using DataGlobals::Pi;
        using FluidProperties::GetDensityGlycol;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Real64 Tolerance(1.0e-8); // Tolerance for Newton-Raphson solution
        static Real64 FluidCond(0.6);    // Conductivity of water (W/m-K)
        static std::string const RoutineName("GetWaterThermalTankInput");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumNodes;         // Number of stratified nodes
        int NodeNum;          // Node number index
        Real64 NodeMass;      // Mass of one node (kg)
        Real64 EndArea;       // Circular area of one end of the cylinder (m2)
        Real64 CrossArea;     // Cross sectional area (for horizontal cylinders) (m2)
        Real64 NodeEndArea;   // Area of the node at the end of the horizontal cylinder (m2)
        Real64 NodeHeight;    // Height of one node (m)
        Real64 ApproxEndArea; // End area approximated by Newton-Raphson iteration (m2)
        Real64 CondCoeff;     // Coefficient for vertical conduction between nodes (W/K)
        Real64 Radius;        // Radius of the tank (m)
        Real64 Perimeter;     // Perimeter of the tank (m)
        Real64 SkinArea;      // Area of skin exposed to ambient environment (m2)
        Real64 ChordLength;   // Chord length for horizontal tanks (m)
        Real64 TankHeight;    // Dimension in the vertical direction; for horizontal tanks it is radius * 2 (m)
        Real64 TankLength;    // For horizontal tanks, it is the length in the axial direction (m)
        Real64 R;             // Radius (m)
        Real64 H0;            // Starting height (m)
        Real64 H;             // Ending height (m)
        Real64 a;             // Intermediate variables
        Real64 b;
        Real64 c;
        Real64 a0; // Intermediate variables
        Real64 b0;
        Real64 c0;
        Real64 G;   // Function that should converge to zero for the Newton-Raphson solution
        Real64 rho; // local fluid density (kg/m3)
        static int DummyWaterIndex(1);

        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum);

        // FLOW:
        NumNodes = Tank.Nodes;
        Tank.Node.allocate(NumNodes);

        if ((Tank.UseSidePlantLoopNum > 0) && allocated(PlantLoop)) {
            rho = GetDensityGlycol(PlantLoop(Tank.UseSidePlantLoopNum).FluidName,
                                   DataGlobals::InitConvTemp,
                                   PlantLoop(Tank.UseSidePlantLoopNum).FluidIndex,
                                   RoutineName);
        } else {
            rho = GetDensityGlycol(fluidNameWater, DataGlobals::InitConvTemp, DummyWaterIndex, RoutineName);
        }

        NodeMass = Tank.Volume * rho / NumNodes;

        // Mixing rate set to 50% of the max value for dt = 1.0
        Tank.InversionMixingRate = NodeMass * 0.5 * 1.0;

        if ((Tank.Shape == TankShapeVertCylinder) || (Tank.Shape == TankShapeOther)) {

            TankHeight = Tank.Height;
            EndArea = Tank.Volume / TankHeight;
            NodeHeight = TankHeight / NumNodes;
            CondCoeff = (FluidCond + Tank.AdditionalCond) * EndArea / NodeHeight;

            if (Tank.Shape == TankShapeVertCylinder) {
                Radius = std::sqrt(EndArea / Pi);
                Perimeter = 2.0 * Pi * Radius;
            } else { // TankShapeOther
                Perimeter = Tank.Perimeter;
            }

            for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                Tank.Node(NodeNum).Mass = NodeMass;
                Tank.Node(NodeNum).Volume = Tank.Volume / NumNodes;
                Tank.Node(NodeNum).Height = NodeHeight;
                Tank.Node(NodeNum).CondCoeffUp = CondCoeff;
                Tank.Node(NodeNum).CondCoeffDn = CondCoeff;

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
            TankLength = Tank.Height; // Height is the length in the axial direction
            EndArea = Tank.Volume / TankLength;
            Radius = std::sqrt(EndArea / Pi);
            TankHeight = 2.0 * Radius; // Actual vertical height
            NodeEndArea = EndArea / NumNodes;

            R = Radius;
            H0 = 0.0;
            ChordLength = 0.0;
            for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                Tank.Node(NodeNum).Mass = NodeMass;
                Tank.Node(NodeNum).Volume = Tank.Volume / NumNodes;

                if (NodeNum == NumNodes) {
                    H = TankHeight;

                } else {
                    // Use the Newton-Raphson method to solve the nonlinear algebraic equation for node height
                    H = H0 + TankHeight / NumNodes; // Initial guess

                    while (true) {
                        a = std::sqrt(H);
                        b = std::sqrt(2.0 * R - H);
                        c = 2.0 * R * R * std::atan(a / b) - (2.0 * R * R - 3.0 * H * R + H * H) * (a / b);

                        if (H0 > 0.0) {
                            a0 = std::sqrt(H0);
                            b0 = std::sqrt(2.0 * R - H0);
                            c0 = 2.0 * R * R * std::atan(a0 / b0) - (2.0 * R * R - 3.0 * H0 * R + H0 * H0) * (a0 / b0);
                        } else {
                            c0 = 0.0;
                        }

                        ApproxEndArea = c - c0;          // Area approximated by iteration
                        G = ApproxEndArea - NodeEndArea; // G is the function that should converge to zero

                        if (std::abs(G) < Tolerance) {
                            break; // Converged !!!
                        } else {
                            H -= G / (2.0 * a * b); // Calculate next guess:  H = Hprev - G/G'
                        }
                    } // Newton-Raphson
                }

                Tank.Node(NodeNum).Height = H - H0;

                if (NodeNum > 1) {
                    CrossArea = 2.0 * ChordLength * TankLength; // Use old ChordLength from previous node
                    CondCoeff = (FluidCond + Tank.AdditionalCond) * CrossArea / (0.5 * (H - H0) + 0.5 * Tank.Node(NodeNum - 1).Height);
                    Tank.Node(NodeNum - 1).CondCoeffUp = CondCoeff; // Set for previous node
                    Tank.Node(NodeNum).CondCoeffDn = CondCoeff;     // Set for this node
                }

                ChordLength = std::sqrt(2.0 * R * H - H * H); // Calc new ChordLength to be used with next node

                Perimeter = 2.0 * R * (std::acos((R - H) / R) - std::acos((R - H0) / R)); // Segments of circular perimeter
                SkinArea = Perimeter * TankLength + 2.0 * NodeEndArea;

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
        H0 = TankHeight;
        for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
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

                if ((NodeNum == Tank.HeaterNode1) && (Tank.ControlType == PrioritySimultaneous)) {
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

        // Using/Aliasing
        using DataEnvironment::OutBaroPress;
        using DataEnvironment::OutDryBulbTemp;
        using DataEnvironment::WaterMainsTemp;
        using DataGlobals::AnyPlantInModel;
        using DataGlobals::BeginEnvrnFlag;
        using DataGlobals::WarmupFlag;
        using DataHeatBalFanSys::MAT;
        using DataHVACGlobals::HPWHCrankcaseDBTemp;
        using DataHVACGlobals::HPWHInletDBTemp;
        using DataHVACGlobals::HPWHInletWBTemp;
        using DataLoopNode::Node;
        using DataSizing::AutoSize;
        using DataSizing::CurZoneEqNum;
        using DataSizing::DataNonZoneNonAirloopValue;
        using DataSizing::ZoneEqSizing;
        using DataZoneEquipment::CheckZoneEquipmentList;
        using DataZoneEquipment::ZoneEquipInputsFilled;
        using General::RoundSigDigits;
        using General::TrimSigDigits;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyTwbFnTdbWPb;
        using Psychrometrics::PsyWFnTdbRhPb;
        using Psychrometrics::PsyWFnTdbTwbPb;
        using Psychrometrics::RhoH2O;
        using ScheduleManager::GetCurrentScheduleValue;
        using namespace DataPlant;
        using Fans::GetFanVolFlow;
        using FluidProperties::GetDensityGlycol;
        using IntegratedHeatPump::IntegratedHeatPumps;
        using IntegratedHeatPump::SizeIHP;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::InterConnectTwoPlantLoopSides;
        using PlantUtilities::SetComponentFlowRate;
        using VariableSpeedCoils::SimVariableSpeedCoils;
        using VariableSpeedCoils::VarSpeedCoil;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int UseInletNode;                       // Water heater use inlet node number
        int UseOutletNode;                      // Water heater use outlet node number
        int SourceInletNode;                    // Water heater source inlet node number
        int SourceOutletNode;                   // Water heater source outlet node number
        int SchIndex;                           // Index to schedule
        int HPNum;                              // Index to heat pump
        int HPAirInletNode;                     // HP air inlet node number
        int HPAirOutletNode;                    // HP air outlet node number
        int OutdoorAirNode;                     // Outdoor air inlet node number
        int ExhaustAirNode;                     // Exhaust air outlet node number
        int HPWaterInletNode;                   // HP condenser water inlet node number
        int HPWaterOutletNode;                  // HP condenser water outlet node number
        int InletAirMixerNode;                  // HP inlet node number after inlet mixing damper
        int OutletAirSplitterNode;              // HP outlet node number before outlet mixing damper
        Real64 HPInletDryBulbTemp(0.0);         // HPWH's air inlet dry-bulb temperature, C
        Real64 HPInletHumRat(0.0);              // HPWH's air inlet humidity ratio, kg/kg
        Real64 HPInletRelHum;                   // HPWH's air inlet relative humidity
        Real64 DeadBandTemp;                    // Minimum tank temperature (SetPointTemp - DeadBandDeltaTemp) (C)
        Real64 MulSpeedFlowScale;               // scaling factor for adjusting flow rates of VS HPWH coil
        Real64 rhoAir;                          // air density
        int Iter;                               // iteration number
        Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); // place holder to calling function
        Real64 FanVolFlow(0.0);                 // Used for error checking fans used with HPWHs
        //  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .FALSE.  ! True after the Zone Equipment List has been checked for items
        //  Integer             :: Loop
        static Array1D_bool MyEnvrnFlag;      // flag for init once at start of environment
        static Array1D_bool MyWarmupFlag;     // flag for init after warmup complete
        static Array1D_bool SetLoopIndexFlag; // get loop number flag
                                              //		static Array1D_bool MySizingDoneFlag; // true if sizing is finished

        static std::string const RoutineName("InitWaterThermalTank");
        static std::string const GetWaterThermalTankInput("GetWaterThermalTankInput");
        static std::string const SizeTankForDemand("SizeTankForDemandSide");

        Real64 sensedTemp;
        int tmpNodeNum;
        Real64 mdotUse;    // local temporary for use side mass flow
        Real64 mdotSource; // local temporary for source side mass flow
        bool errFlag;
        Real64 rho; // local fluid density
        static int DummyWaterIndex(1);
        static Real64 TankChangeRateScale(0.0); // local temporary for nominal tank change rate
        static Real64 MaxSideVolFlow(0.0);      // local temporary for largest connection design flow
        int VSCoilID(0);                        // id of variable-speed HPWH coil

        // FLOW:

        if (InitWaterThermalTanksOnce) {
            MyEnvrnFlag.allocate(NumWaterThermalTank);
            MyWarmupFlag.allocate(NumWaterThermalTank);
            SetLoopIndexFlag.allocate(NumWaterThermalTank);
            //			MySizingDoneFlag.allocate( NumWaterThermalTank );
            AlreadyRated.dimension(NumWaterThermalTank, false);
            MyEnvrnFlag = true;
            MyWarmupFlag = false;
            InitWaterThermalTanksOnce = false;
            SetLoopIndexFlag = true;
        }

        UseInletNode = WaterThermalTank(WaterThermalTankNum).UseInletNode;
        UseOutletNode = WaterThermalTank(WaterThermalTankNum).UseOutletNode;
        SourceInletNode = WaterThermalTank(WaterThermalTankNum).SourceInletNode;
        SourceOutletNode = WaterThermalTank(WaterThermalTankNum).SourceOutletNode;

        if (SetLoopIndexFlag(WaterThermalTankNum) && allocated(PlantLoop)) {

            if ((UseInletNode > 0) && (WaterThermalTank(WaterThermalTankNum).HeatPumpNum == 0)) {
                errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(WaterThermalTank(WaterThermalTankNum).Name,
                                                        WaterThermalTank(WaterThermalTankNum).TypeNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantBranchNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        UseInletNode,
                                                        _,
                                                        errFlag);
                if (errFlag) {
                    ShowFatalError("InitWaterThermalTank: Program terminated due to previous condition(s).");
                }
                rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                       PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                       GetWaterThermalTankInput);
                WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax = WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate * rho;
                WaterThermalTank(WaterThermalTankNum).Mass = WaterThermalTank(WaterThermalTankNum).Volume * rho;
                WaterThermalTank(WaterThermalTankNum).UseSidePlantSizNum =
                    PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).PlantSizNum;
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
                errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).Name,
                                                        HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).TypeNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantBranchNum,
                                                        WaterThermalTank(WaterThermalTankNum).UseSidePlantCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        UseInletNode,
                                                        _,
                                                        errFlag);
                if (errFlag) {
                    ShowFatalError("InitWaterThermalTank: Program terminated due to previous condition(s).");
                }
                rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                       PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                       GetWaterThermalTankInput);
                WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax = WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate * rho;
                WaterThermalTank(WaterThermalTankNum).Mass = WaterThermalTank(WaterThermalTankNum).Volume * rho;
                WaterThermalTank(WaterThermalTankNum).UseSidePlantSizNum =
                    PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).PlantSizNum;
                if ((WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized) &&
                    (WaterThermalTank(WaterThermalTankNum).UseSidePlantSizNum == 0)) {
                    ShowSevereError("InitWaterThermalTank: Did not find Sizing:Plant object for use side of plant thermal tank = " +
                                    WaterThermalTank(WaterThermalTankNum).Name);
                    ShowFatalError("InitWaterThermalTank: Program terminated due to previous condition(s).");
                }
            }
            if ((SourceInletNode > 0) && (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum == 0) &&
                (WaterThermalTank(WaterThermalTankNum).HeatPumpNum == 0)) {
                errFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(WaterThermalTank(WaterThermalTankNum).Name,
                                                        WaterThermalTank(WaterThermalTankNum).TypeNum,
                                                        WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum,
                                                        WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide,
                                                        WaterThermalTank(WaterThermalTankNum).SourceSidePlantBranchNum,
                                                        WaterThermalTank(WaterThermalTankNum).SourceSidePlantCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        SourceInletNode,
                                                        _,
                                                        errFlag);
                if (UseInletNode > 0) {
                    InterConnectTwoPlantLoopSides(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum,
                                                  WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide,
                                                  WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum,
                                                  WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide,
                                                  WaterThermalTank(WaterThermalTankNum).TypeNum,
                                                  true);
                }

                if (errFlag) {
                    ShowFatalError("InitWaterThermalTank: Program terminated due to previous condition(s).");
                }
                rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                       PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidIndex,
                                       GetWaterThermalTankInput);
                WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax =
                    WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate * rho;
                WaterThermalTank(WaterThermalTankNum).SourceSidePlantSizNum =
                    PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).PlantSizNum;
                if ((WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized) &&
                    (WaterThermalTank(WaterThermalTankNum).SourceSidePlantSizNum == 0)) {
                    ShowSevereError("InitWaterThermalTank: Did not find Sizing:Plant object for source side of plant thermal tank = " +
                                    WaterThermalTank(WaterThermalTankNum).Name);
                    ShowFatalError("InitWaterThermalTank: Program terminated due to previous condition(s).");
                }
            }
            if (((SourceInletNode > 0) && (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0)) ||
                (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0)) {
                SetLoopIndexFlag(WaterThermalTankNum) = false;
            }

            if (PlantFirstSizesOkayToFinalize) SetLoopIndexFlag(WaterThermalTankNum) = false;
            if (WaterThermalTank(WaterThermalTankNum).StandAlone) {
                SizeStandAloneWaterHeater(WaterThermalTankNum);
                SetLoopIndexFlag(WaterThermalTankNum) = false;
            }

        } else if (SetLoopIndexFlag(WaterThermalTankNum) && !AnyPlantInModel) {
            if (WaterThermalTank(WaterThermalTankNum).StandAlone) {
                SizeStandAloneWaterHeater(WaterThermalTankNum);
            }
            SetLoopIndexFlag(WaterThermalTankNum) = false;
        }

        if (BeginEnvrnFlag && MyEnvrnFlag(WaterThermalTankNum) && !SetLoopIndexFlag(WaterThermalTankNum)) {

            if (PlantFirstSizesOkayToFinalize) {

                if (WaterThermalTank(WaterThermalTankNum).ControlType == ControlTypeCycle) {
                    WaterThermalTank(WaterThermalTankNum).MinCapacity = WaterThermalTank(WaterThermalTankNum).MaxCapacity;
                }

                // check for sizing issues that model can not suppport

                // if stratified tank model, ensure that nominal change over rate is greater than one minute, avoid numerical problems.

                if ((WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedWaterHeater) ||
                    (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedChilledWaterStorage)) {
                    MaxSideVolFlow = max(WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate,
                                         WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);

                    if (MaxSideVolFlow > 0.0) { // protect div by zero
                        TankChangeRateScale = WaterThermalTank(WaterThermalTankNum).Volume / MaxSideVolFlow;
                        if (TankChangeRateScale < 60.0) { // nominal change over in less than one minute
                            ShowSevereError("InitWaterThermalTank: Detected problem for stratified tank model.  Model cannot be applied.");
                            ShowContinueError("Occurs for stratified tank name = " + WaterThermalTank(WaterThermalTankNum).Name);
                            ShowContinueError("Tank volume = " + RoundSigDigits(WaterThermalTank(WaterThermalTankNum).Volume, 4) + " [m3]");
                            ShowContinueError("Tank use side volume flow rate = " +
                                              RoundSigDigits(WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate, 4) + " [m3/s]");
                            ShowContinueError("Tank source side volume flow rate = " +
                                              RoundSigDigits(WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate, 4) + " [m3/s]");
                            ShowContinueError("Nominal tank change over rate = " + RoundSigDigits(TankChangeRateScale, 2) + " [s]");
                            ShowContinueError(
                                "Change over rate is too fast, increase tank volume, decrease connection flow rates or use mixed tank model");

                            ShowFatalError("InitWaterThermalTank: Simulation halted because of sizing problem in stratified tank model.");
                        }
                    }
                }
            }

            // Clear node initial conditions
            if (UseInletNode > 0 && UseOutletNode > 0) {
                Node(UseInletNode).Temp = 0.0;
                rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                       PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                       GetWaterThermalTankInput);
                WaterThermalTank(WaterThermalTankNum).MassFlowRateMin = WaterThermalTank(WaterThermalTankNum).VolFlowRateMin * rho;
                WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax = WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate * rho;
                InitComponentNodes(WaterThermalTank(WaterThermalTankNum).MassFlowRateMin,
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
                WaterThermalTank(WaterThermalTankNum).UseBranchControlType = PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum)
                                                                                 .LoopSide(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide)
                                                                                 .Branch(WaterThermalTank(WaterThermalTankNum).UseSidePlantBranchNum)
                                                                                 .Comp(WaterThermalTank(WaterThermalTankNum).UseSidePlantCompNum)
                                                                                 .FlowCtrl;
            }

            if ((SourceInletNode > 0) && (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum == 0) &&
                (WaterThermalTank(WaterThermalTankNum).HeatPumpNum == 0)) {
                rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidName,
                                       DataGlobals::InitConvTemp,
                                       PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidIndex,
                                       GetWaterThermalTankInput);
                WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax =
                    WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate * rho;
                InitComponentNodes(0.0,
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
                    PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum)
                        .LoopSide(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide)
                        .Branch(WaterThermalTank(WaterThermalTankNum).SourceSidePlantBranchNum)
                        .Comp(WaterThermalTank(WaterThermalTankNum).SourceSidePlantCompNum)
                        .FlowCtrl;
            }

            if ((SourceInletNode > 0) &&
                ((WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0) || (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0))) {
                Node(SourceInletNode).Temp = 0.0;
                WaterThermalTank(WaterThermalTankNum).SourceOutletTemp = 0.0;
                WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = 0.0;
                WaterThermalTank(WaterThermalTankNum).SavedSourceOutletTemp = 0.0;
                rho = GetDensityGlycol(fluidNameWater, DataGlobals::InitConvTemp, DummyWaterIndex, SizeTankForDemand);
                WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax =
                    WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate * rho;
            }

            // Initialize tank temperature to setpoint of first hour of warm up period
            // (use HPWH or Desuperheater heating coil set point if applicable)
            if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).Mode = FloatMode;
                HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SaveMode = FloatMode;
                HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SaveWHMode = FloatMode;
                SchIndex = HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTempSchedule;
            } else if (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0) {
                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Mode = FloatMode;
                SchIndex = WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).SetPointTempSchedule;
            } else {
                SchIndex = WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule;
            }

            if (SchIndex > 0) {
                WaterThermalTank(WaterThermalTankNum).TankTemp = GetCurrentScheduleValue(SchIndex);
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
            WaterThermalTank(WaterThermalTankNum).Mode = 0;
            WaterThermalTank(WaterThermalTankNum).SavedMode = 0;
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

        if (!BeginEnvrnFlag) MyEnvrnFlag(WaterThermalTankNum) = true;

        if (MyWarmupFlag(WaterThermalTankNum) && (!WarmupFlag)) {
            // reInitialize tank temperature to setpoint of first hour (use HPWH or Desuperheater heating coil set point if applicable)
            // BG's interpetation here is that its better to reset initial condition to setpoint once warm up is over.
            // (otherwise with a dynamic storage model it is difficult for the user to see the initial performance if it isn't periodic.)
            if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).Mode = FloatMode;
                SchIndex = HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTempSchedule;
            } else if (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0) {
                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).Mode = FloatMode;
                SchIndex = WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).SetPointTempSchedule;
            } else {
                SchIndex = WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule;
            }

            if (SchIndex > 0) {
                WaterThermalTank(WaterThermalTankNum).TankTemp = GetCurrentScheduleValue(SchIndex);
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
            MyWarmupFlag(WaterThermalTankNum) = false;
        }
        if (WarmupFlag) MyWarmupFlag(WaterThermalTankNum) = true;

        if (FirstHVACIteration) {
            // Get all scheduled values
            SchIndex = WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule;
            WaterThermalTank(WaterThermalTankNum).SetPointTemp = GetCurrentScheduleValue(SchIndex);

            if (!WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                if (WaterThermalTank(WaterThermalTankNum).SetPointTemp > WaterThermalTank(WaterThermalTankNum).TankTempLimit) {
                    // Setpoint temperature scheduled higher than maximum tank temperature limit
                    WaterThermalTank(WaterThermalTankNum).SetPointTemp = WaterThermalTank(WaterThermalTankNum).TankTempLimit - 1.0;

                    if (WaterThermalTank(WaterThermalTankNum).ShowSetPointWarning) {
                        ShowSevereError("Water heater = " + WaterThermalTank(WaterThermalTankNum).Name +
                                        ":  Water heater tank set point temperature is greater than the maximum tank temperature limit.");
                        ShowContinueErrorTimeStamp("Water heater tank set point temperature is reset to Tank Temperature Limit minus 1 C (" +
                                                   TrimSigDigits(WaterThermalTank(WaterThermalTankNum).SetPointTemp, 2) +
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
                                                   TrimSigDigits(WaterThermalTank(WaterThermalTankNum).SetPointTemp, 2) +
                                                   ") and simulation continues.");
                        WaterThermalTank(WaterThermalTankNum).ShowSetPointWarning = false;
                    }
                }
            }

            SchIndex = WaterThermalTank(WaterThermalTankNum).SetPointTempSchedule2;
            if (SchIndex > 0) {
                WaterThermalTank(WaterThermalTankNum).SetPointTemp2 = GetCurrentScheduleValue(SchIndex);
            }

            {
                auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).AmbientTempIndicator);
                if (SELECT_CASE_var == AmbientTempSchedule) {
                    SchIndex = WaterThermalTank(WaterThermalTankNum).AmbientTempSchedule;
                    WaterThermalTank(WaterThermalTankNum).AmbientTemp = GetCurrentScheduleValue(SchIndex);

                } else if (SELECT_CASE_var == AmbientTempZone) {
                    WaterThermalTank(WaterThermalTankNum).AmbientTemp = MAT(WaterThermalTank(WaterThermalTankNum).AmbientTempZone);

                } else if (SELECT_CASE_var == AmbientTempOutsideAir) {
                    WaterThermalTank(WaterThermalTankNum).AmbientTemp = Node(WaterThermalTank(WaterThermalTankNum).AmbientTempOutsideAirNode).Temp;
                }
            }

            if (UseInletNode == 0) { // Stand-alone operation

                SchIndex = WaterThermalTank(WaterThermalTankNum).UseInletTempSchedule;
                if (SchIndex > 0) {
                    WaterThermalTank(WaterThermalTankNum).UseInletTemp = GetCurrentScheduleValue(SchIndex);
                } else {
                    WaterThermalTank(WaterThermalTankNum).UseInletTemp = WaterMainsTemp;
                }

                SchIndex = WaterThermalTank(WaterThermalTankNum).FlowRateSchedule;
                if (SchIndex > 0) {
                    WaterThermalTank(WaterThermalTankNum).UseMassFlowRate =
                        GetCurrentScheduleValue(SchIndex) * WaterThermalTank(WaterThermalTankNum).MassFlowRateMax;

                    WaterThermalTank(WaterThermalTankNum).VolFlowRate =
                        WaterThermalTank(WaterThermalTankNum).UseMassFlowRate / RhoH2O(DataGlobals::InitConvTemp);
                } else {
                    WaterThermalTank(WaterThermalTankNum).UseMassFlowRate = WaterThermalTank(WaterThermalTankNum).MassFlowRateMax;
                    WaterThermalTank(WaterThermalTankNum).VolFlowRate =
                        WaterThermalTank(WaterThermalTankNum).UseMassFlowRate / RhoH2O(DataGlobals::InitConvTemp);
                }
            }

            if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTemp =
                    GetCurrentScheduleValue(HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTempSchedule);
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
                            TrimSigDigits(HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).SetPointTemp, 2) +
                            ") and simulation continues.");
                        HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).ShowSetPointWarning = false;
                    }
                }
            }

            if (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum > 0) {
                WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).SetPointTemp =
                    GetCurrentScheduleValue(WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum).DesuperheaterNum).SetPointTempSchedule);
            }

        } // first HVAC Iteration

        if (UseInletNode > 0 && !SetLoopIndexFlag(WaterThermalTankNum)) { // setup mass flows for plant connections

            if (WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                DeadBandTemp = WaterThermalTank(WaterThermalTankNum).SetPointTemp + WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp;
            } else {
                DeadBandTemp = WaterThermalTank(WaterThermalTankNum).SetPointTemp - WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp;
            }

            mdotUse = PlantMassFlowRatesFunc(WaterThermalTankNum,
                                             UseInletNode,
                                             FirstHVACIteration,
                                             UseSide,
                                             WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide,
                                             WaterThermalTank(WaterThermalTankNum).UseSideSeries,
                                             WaterThermalTank(WaterThermalTankNum).UseBranchControlType,
                                             WaterThermalTank(WaterThermalTankNum).SavedUseOutletTemp,
                                             DeadBandTemp,
                                             WaterThermalTank(WaterThermalTankNum).SetPointTemp);
            SetComponentFlowRate(mdotUse,
                                 UseInletNode,
                                 UseOutletNode,
                                 WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum,
                                 WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide,
                                 WaterThermalTank(WaterThermalTankNum).UseSidePlantBranchNum,
                                 WaterThermalTank(WaterThermalTankNum).UseSidePlantCompNum);

            WaterThermalTank(WaterThermalTankNum).UseInletTemp = Node(UseInletNode).Temp;
            WaterThermalTank(WaterThermalTankNum).UseMassFlowRate = mdotUse;
        }

        if (SourceInletNode > 0 && !SetLoopIndexFlag(WaterThermalTankNum)) { // setup mass flows for plant connections

            if (WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                DeadBandTemp = WaterThermalTank(WaterThermalTankNum).SetPointTemp + WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp;
            } else {
                DeadBandTemp = WaterThermalTank(WaterThermalTankNum).SetPointTemp - WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp;
            }

            if (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedChilledWaterStorage) {
                tmpNodeNum = WaterThermalTank(WaterThermalTankNum).HeaterNode1;
                sensedTemp = WaterThermalTank(WaterThermalTankNum).Node(tmpNodeNum).SavedTemp;
            } else {
                sensedTemp = WaterThermalTank(WaterThermalTankNum).SavedSourceOutletTemp;
            }

            mdotSource = PlantMassFlowRatesFunc(WaterThermalTankNum,
                                                SourceInletNode,
                                                FirstHVACIteration,
                                                SourceSide,
                                                WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide,
                                                WaterThermalTank(WaterThermalTankNum).SourceSideSeries,
                                                WaterThermalTank(WaterThermalTankNum).SourceBranchControlType,
                                                sensedTemp,
                                                DeadBandTemp,
                                                WaterThermalTank(WaterThermalTankNum).SetPointTemp);
            if (WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum > 0) {
                SetComponentFlowRate(mdotSource,
                                     SourceInletNode,
                                     SourceOutletNode,
                                     WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum,
                                     WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide,
                                     WaterThermalTank(WaterThermalTankNum).SourceSidePlantBranchNum,
                                     WaterThermalTank(WaterThermalTankNum).SourceSidePlantCompNum);
            } else { // not really plant connected (desuperheater or heat pump)
                Node(SourceInletNode).MassFlowRate = mdotSource;
                Node(SourceOutletNode).MassFlowRate = mdotSource;
            }

            WaterThermalTank(WaterThermalTankNum).SourceInletTemp = Node(SourceInletNode).Temp;
            WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = mdotSource;
        }

        // initialize HPWHs each iteration
        if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {

            HPNum = WaterThermalTank(WaterThermalTankNum).HeatPumpNum;

            if (MyHPSizeFlag(HPNum)) {
                //     autosize info must be calculated in GetWaterThermalTankInputFlag for use in StandardRating procedure
                //       (called at end of GetWaterThermalTankInputFlag)
                //     report autosizing information here (must be done after GetWaterThermalTankInputFlag is complete)
                if (HPWaterHeater(HPNum).WaterFlowRateAutoSized &&
                    (PlantFirstSizesOkayToReport || !AnyPlantInModel || AlreadyRated(WaterThermalTankNum))) {
                    ReportSizingOutput(HPWaterHeater(HPNum).Type,
                                       HPWaterHeater(HPNum).Name,
                                       "Condenser water flow rate [m3/s]",
                                       HPWaterHeater(HPNum).OperatingWaterFlowRate);
                }
                if (HPWaterHeater(HPNum).AirFlowRateAutoSized &&
                    (PlantFirstSizesOkayToReport || !AnyPlantInModel || AlreadyRated(WaterThermalTankNum))) {
                    ReportSizingOutput(HPWaterHeater(HPNum).Type,
                                       HPWaterHeater(HPNum).Name,
                                       "Evaporator air flow rate [m3/s]",
                                       HPWaterHeater(HPNum).OperatingAirFlowRate);
                }
                DataNonZoneNonAirloopValue = HPWaterHeater(HPNum).OperatingAirFlowRate;
                HPWaterHeater(HPNum).OperatingAirMassFlowRate = HPWaterHeater(HPNum).OperatingAirFlowRate * DataEnvironment::StdRhoAir;
                if (CurZoneEqNum > 0) {
                    ZoneEqSizing(CurZoneEqNum).CoolingAirFlow = true;
                    ZoneEqSizing(CurZoneEqNum).CoolingAirVolFlow = DataNonZoneNonAirloopValue;
                }
                if (PlantFirstSizesOkayToReport || !AnyPlantInModel || AlreadyRated(WaterThermalTankNum)) MyHPSizeFlag(HPNum) = false;
            }

            HPAirInletNode = HPWaterHeater(HPNum).HeatPumpAirInletNode;
            HPAirOutletNode = HPWaterHeater(HPNum).HeatPumpAirOutletNode;
            OutdoorAirNode = HPWaterHeater(HPNum).OutsideAirNode;
            ExhaustAirNode = HPWaterHeater(HPNum).ExhaustAirNode;
            HPWaterInletNode = HPWaterHeater(HPNum).CondWaterInletNode;
            HPWaterOutletNode = HPWaterHeater(HPNum).CondWaterOutletNode;
            InletAirMixerNode = HPWaterHeater(HPNum).InletAirMixerNode;
            OutletAirSplitterNode = HPWaterHeater(HPNum).OutletAirSplitterNode;

            {
                auto const SELECT_CASE_var(HPWaterHeater(HPNum).CrankcaseTempIndicator);
                if (SELECT_CASE_var == CrankcaseTempZone) {
                    HPWHCrankcaseDBTemp = MAT(HPWaterHeater(HPNum).AmbientTempZone);
                } else if (SELECT_CASE_var == CrankcaseTempExterior) {
                    HPWHCrankcaseDBTemp = OutDryBulbTemp;
                } else if (SELECT_CASE_var == CrankcaseTempSchedule) {
                    HPWHCrankcaseDBTemp = GetCurrentScheduleValue(HPWaterHeater(HPNum).CrankcaseTempSchedule);
                }
            }

            //   initialize HPWH report variables to 0 and set tank inlet node equal to outlet node
            HPWaterHeater(HPNum).HPWaterHeaterSensibleCapacity = 0.0;
            HPWaterHeater(HPNum).HPWaterHeaterLatentCapacity = 0.0;
            WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = 0.0;
            HPWaterHeater(HPNum).HeatingPLR = 0.0;
            WaterThermalTank(WaterThermalTankNum).SourceInletTemp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;

            //   determine HPWH inlet air conditions based on inlet air configuration (Zone, ZoneAndOA, OutdoorAir, or Schedule)
            {
                auto const SELECT_CASE_var(HPWaterHeater(HPNum).InletAirConfiguration);
                if (SELECT_CASE_var == AmbientTempZone) {
                    MixerInletAirSchedule = 0.0;
                    HPInletDryBulbTemp = Node(HPAirInletNode).Temp;
                    HPInletHumRat = Node(HPAirInletNode).HumRat;
                } else if (SELECT_CASE_var == AmbientTempZoneAndOA) {
                    if (HPWaterHeater(HPNum).InletAirMixerSchPtr > 0) {
                        //         schedule values are checked for boundary of 0 and 1 in GetWaterThermalTankInputFlag
                        MixerInletAirSchedule = GetCurrentScheduleValue(HPWaterHeater(HPNum).InletAirMixerSchPtr);
                    } else {
                        MixerInletAirSchedule = 0.0;
                    }
                    HPInletDryBulbTemp =
                        MixerInletAirSchedule * Node(OutdoorAirNode).Temp + (1.0 - MixerInletAirSchedule) * Node(HPAirInletNode).Temp;
                    HPInletHumRat = MixerInletAirSchedule * Node(OutdoorAirNode).HumRat + (1.0 - MixerInletAirSchedule) * Node(HPAirInletNode).HumRat;
                } else if (SELECT_CASE_var == AmbientTempOutsideAir) {
                    MixerInletAirSchedule = 1.0;
                    HPInletDryBulbTemp = Node(OutdoorAirNode).Temp;
                    HPInletHumRat = Node(OutdoorAirNode).HumRat;

                } else if (SELECT_CASE_var == AmbientTempSchedule) {
                    HPInletDryBulbTemp = GetCurrentScheduleValue(HPWaterHeater(HPNum).AmbientTempSchedule);
                    HPInletRelHum = GetCurrentScheduleValue(HPWaterHeater(HPNum).AmbientRHSchedule);
                    HPInletHumRat = PsyWFnTdbRhPb(HPInletDryBulbTemp, HPInletRelHum, OutBaroPress, RoutineName);
                    Node(HPAirInletNode).Temp = HPInletDryBulbTemp;
                    Node(HPAirInletNode).HumRat = HPInletHumRat;
                    Node(HPAirInletNode).Enthalpy = PsyHFnTdbW(HPInletDryBulbTemp, HPInletHumRat);
                    Node(HPAirInletNode).Press = OutBaroPress;

                } else {
                    assert(false);
                }
            }

            MdotAir = HPWaterHeater(HPNum).OperatingAirMassFlowRate;

            //   set up initial conditions on nodes
            if (InletAirMixerNode > 0) {
                Node(InletAirMixerNode).MassFlowRate = 0.0;
                Node(InletAirMixerNode).MassFlowRateMax = MdotAir;
                Node(InletAirMixerNode).MassFlowRateMaxAvail = MdotAir;
                Node(InletAirMixerNode).Temp = HPInletDryBulbTemp;
                Node(InletAirMixerNode).HumRat = HPInletHumRat;
                Node(InletAirMixerNode).Enthalpy = PsyHFnTdbW(HPInletDryBulbTemp, HPInletHumRat);
                Node(HPAirInletNode).MassFlowRate = 0.0;
                Node(HPAirOutletNode).MassFlowRate = 0.0;
                Node(OutdoorAirNode).MassFlowRate = 0.0;
                Node(ExhaustAirNode).MassFlowRate = 0.0;
            } else {
                if (OutdoorAirNode == 0) {
                    Node(HPAirInletNode).MassFlowRate = 0.0;
                    Node(HPAirInletNode).MassFlowRateMax = MdotAir;
                    Node(HPAirInletNode).MassFlowRateMaxAvail = MdotAir;
                    Node(HPAirOutletNode).MassFlowRate = 0.0;
                } else {
                    Node(OutdoorAirNode).MassFlowRate = 0.0;
                    Node(OutdoorAirNode).MassFlowRateMax = MdotAir;
                    Node(OutdoorAirNode).MassFlowRateMaxAvail = MdotAir;
                    Node(ExhaustAirNode).MassFlowRate = 0.0;
                }
            }

            if (OutletAirSplitterNode > 0) Node(OutletAirSplitterNode).MassFlowRate = 0.0;
            // these are water nodes are not managed by plant. the HP connects
            // directly to the WH without using plant. will not change this code for DSU because of this
            if (HPWaterHeater(HPNum).TypeNum == TypeOf_HeatPumpWtrHeaterPumped) {
                Node(HPWaterInletNode).MassFlowRate = 0.0;
                Node(HPWaterOutletNode).MassFlowRate = 0.0;
            }

            //   set the max mass flow rate for outdoor fans
            Node(HPWaterHeater(HPNum).FanOutletNode).MassFlowRateMax = MdotAir;

            //   Curve objects in CalcHPWHDXCoil will use inlet conditions to HPWH not inlet air conditions to DX Coil
            //   HPWHInletDBTemp and HPWHInletWBTemp are DataHVACGlobals to pass info to HPWHDXCoil
            HPWHInletDBTemp = HPInletDryBulbTemp;
            HPWHInletWBTemp = PsyTwbFnTdbWPb(HPWHInletDBTemp, HPInletHumRat, OutBaroPress);

            // initialize flow rates at speed levels for variable-speed HPWH
            if ((HPWaterHeater(HPNum).bIsIHP) && (0 == HPWaterHeater(HPNum).NumofSpeed)) // use SCWH coil represents
            {
                SizeIHP(HPWaterHeater(HPNum).DXCoilNum); //
                // SimIHP(BlankString, HPWaterHeater(HPNum).DXCoilNum,
                //	0, EMP1, EMP2, EMP3, 0, 0.0, 1, 0.0, 0.0, 0.0, false, 0.0); //conduct the sizing operation in the IHP
                VSCoilID = IntegratedHeatPumps(HPWaterHeater(HPNum).DXCoilNum).SCWHCoilIndex;
                HPWaterHeater(HPNum).NumofSpeed = VarSpeedCoil(VSCoilID).NumOfSpeeds;

            } else if (UtilityRoutines::SameString(HPWaterHeater(HPNum).DXCoilType, "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed") &&
                       (HPWaterHeater(HPNum).NumofSpeed == 0)) {
                EMP1 = 4.0;
                EMP2 = 0.0;
                EMP3 = 0.0;
                SimVariableSpeedCoils(BlankString,
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
                VSCoilID = HPWaterHeater(HPNum).DXCoilNum;
                HPWaterHeater(HPNum).NumofSpeed = VarSpeedCoil(VSCoilID).NumOfSpeeds;
                // below pass the flow rates from the VS coil to the water heater object
            }

            if (HPWaterHeater(HPNum).NumofSpeed > 0) {

                if (HPWaterHeater(HPNum).bIsIHP)
                    VSCoilID = IntegratedHeatPumps(HPWaterHeater(HPNum).DXCoilNum).SCWHCoilIndex;
                else
                    VSCoilID = HPWaterHeater(HPNum).DXCoilNum;

                // scale air flow rates
                MulSpeedFlowScale =
                    VarSpeedCoil(VSCoilID).RatedAirVolFlowRate / VarSpeedCoil(VSCoilID).MSRatedAirVolFlowRate(VarSpeedCoil(VSCoilID).NormSpedLevel);
                for (Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                    HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) = VarSpeedCoil(VSCoilID).MSRatedAirVolFlowRate(Iter) * MulSpeedFlowScale;
                }

                // check fan flow rate, should be larger than the max flow rate of the VS coil
                if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    FanVolFlow = HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->designAirVolFlowRate;
                } else if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SimpleOnOff) {
                    GetFanVolFlow(HPWaterHeater(HPNum).FanNum, FanVolFlow);
                }

                if (FanVolFlow <
                    HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed)) { // but this is the not the scaled mas flow
                    // if ( FanVolFlow  < HPWaterHeater( HPNum ).HPWHAirVolFlowRate( HPWaterHeater( HPNum ).NumofSpeed ) ) {

                    ShowWarningError("InitWaterThermalTank: -air flow rate = " + TrimSigDigits(FanVolFlow, 7) +
                                     " in fan object "
                                     " is less than the MSHP system air flow rate"
                                     " when waterheating is required(" +
                                     TrimSigDigits(HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed), 7) + ").");
                    ShowContinueError(" The MSHP system flow rate when waterheating is required is reset to the"
                                      " fan flow rate and the simulation continues.");
                    ShowContinueError(" Occurs in " + HPWaterHeater(HPNum).Name);
                    HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed) = FanVolFlow;
                    // Check flow rates in other speeds and ensure flow rates are not above the max flow rate
                    for (Iter = HPWaterHeater(HPNum).NumofSpeed - 1; Iter >= 1; --Iter) {
                        if (HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) > HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter + 1)) {
                            ShowContinueError(" The MSHP system flow rate when waterheating is required is reset to the"
                                              " flow rate at higher speed and the simulation continues at Speed" +
                                              TrimSigDigits(Iter) + '.');
                            ShowContinueError(" Occurs in " + HPWaterHeater(HPNum).Name);
                            HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) = HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter + 1);
                        }
                    }
                }

                for (Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                    HPWaterHeater(HPNum).MSAirSpeedRatio(Iter) =
                        HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) / HPWaterHeater(HPNum).HPWHAirVolFlowRate(HPWaterHeater(HPNum).NumofSpeed);
                }

                // scale water flow rates
                MulSpeedFlowScale = VarSpeedCoil(VSCoilID).RatedWaterVolFlowRate /
                                    VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(VarSpeedCoil(VSCoilID).NormSpedLevel);
                for (Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                    HPWaterHeater(HPNum).HPWHWaterVolFlowRate(Iter) = VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(Iter) * MulSpeedFlowScale;
                    HPWaterHeater(HPNum).HPWHWaterMassFlowRate(Iter) = VarSpeedCoil(VSCoilID).MSRatedWaterMassFlowRate(Iter) * MulSpeedFlowScale;
                    HPWaterHeater(HPNum).MSWaterSpeedRatio(Iter) = VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(Iter) /
                                                                   VarSpeedCoil(VSCoilID).MSRatedWaterVolFlowRate(HPWaterHeater(HPNum).NumofSpeed);
                }

                rhoAir = PsyRhoAirFnPbTdbW(OutBaroPress, HPInletDryBulbTemp, HPInletHumRat);

                for (Iter = 1; Iter <= HPWaterHeater(HPNum).NumofSpeed; ++Iter) {
                    HPWaterHeater(HPNum).HPWHAirMassFlowRate(Iter) = HPWaterHeater(HPNum).HPWHAirVolFlowRate(Iter) * rhoAir;
                }

                //   set the max mass flow rate for outdoor fans
                Node(HPWaterHeater(HPNum).FanOutletNode).MassFlowRateMax = HPWaterHeater(HPNum).HPWHAirMassFlowRate(HPWaterHeater(HPNum).NumofSpeed);
            }

        } //  IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0)THEN

        // calling CalcStandardRatings early bypasses fan sizing since DataNonZoneNonAirloopValue has not been set yet
        if (!AlreadyRated(WaterThermalTankNum)) {
            if (WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                AlreadyRated(WaterThermalTankNum) = true;
            } else {
                if (!AnyPlantInModel || PlantFirstSizesOkayToReport || WaterThermalTank(WaterThermalTankNum).MaxCapacity > 0.0 ||
                    WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
                    CalcStandardRatings(WaterThermalTankNum);
                }
            }
        }
    }

    void CalcWaterThermalTankMixed(int const WaterThermalTankNum) // Water Heater being simulated
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

        // Using/Aliasing
        using DataGlobals::HourOfDay;
        using DataGlobals::TimeStep;
        using DataGlobals::TimeStepZone;
        using DataGlobals::WarmupFlag;
        using DataHVACGlobals::SysTimeElapsed;
        using DataHVACGlobals::TimeStepSys;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using General::RoundSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcWaterThermalTankMixed");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TimeElapsed;  // Fraction of the current hour that has elapsed (h)
        Real64 SetPointTemp; // Current setpoint temperature (C)
        Real64 DeadBandTemp; // Heating: Minimum tank temperature (SetPointTemp - DeadBandDeltaTemp) (C)
        // Cooling: Maximum Tank temperature (SetPointTemp + DeadBandDeltaTemp) (C)
        Real64 MaxTemp;            // Maximum tank temperature before venting (C)
        Real64 AmbientTemp;        // Current ambient air temperature around tank (C)
        Real64 TankMass;           // Mass of water in tank (kg)
        Real64 LossCoeff;          // Loss coefficient to ambient environment (W/K)
        Real64 LossFracToZone;     // Fraction of losses added to the zone as a gain
        Real64 TankTemp;           // Instantaneous tank temperature (C)
        Real64 NewTankTemp;        // Predicted new tank temperature (C)
        Real64 TankTempAvg;        // Average tank temperature over the timestep (C)
        Real64 Cp;                 // Specific heat of water (J/kg K)
        Real64 Quse;               // Heating rate due to use side mass flow (W)
        Real64 Qsource;            // Heating rate due to source side mass flow (W)
        Real64 Qloss;              // Heating rate due to ambient environment (W)
        Real64 Qlosszone;          // Heating rate of fraction of losses added to the zone as a gain (W)
        Real64 Qheat;              // Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
        Real64 Qheater;            // Heating rate of the burner or electric heating element (W)
        Real64 Qheatpump;          // Heating rate of the heat pump (W)
        Real64 Qmaxcap;            // Maximum capacity heating rate of the burner or electric heating element (W)
        Real64 Qmincap;            // Minimum capacity heating rate of the burner or electric heating element (W)
        Real64 Qoffcycfuel;        // Fuel consumption rate of off-cycle parasitics (W)
        Real64 Qoffcycheat;        // Heating rate of fraction of off-cycle parasitics added to the tank (W)
        Real64 Qoncycfuel;         // Fuel consumption rate on-cycle parasitics added to the tank (W)
        Real64 Qoncycheat;         // Heating rate of fraction of on-cycle parasitics added to the tank (W)
        Real64 Qneeded;            // Heating rate needed to recover or maintain the setpoint temperature (W)
        Real64 Qunmet;             // The difference between Qneeded and Qheater (W)
        Real64 Qvent;              // Heating rate due to venting because tank exceeded max temperature limit (W)
        Real64 Qnet;               // Net heat transfer rate including everything (W)
        Real64 Qfuel;              // Heating rate for fuel consumed (W)
        Real64 UseInletTemp;       // Use side inlet temperature (C)
        Real64 UseMassFlowRate;    // Use side flow rate, including effectiveness factor (kg/s)
        Real64 MinMassFlowRate;    // Minimum use side flow rate required before heater is enabled (kg/s)
        Real64 SourceInletTemp;    // Source side inlet temperature (C)
        Real64 SourceMassFlowRate; // Source side flow rate, including effectiveness factor (kg/s)
        int Mode;                  // Indicator for current operating mode (HeatMode=1 | FloatMode=0 | VentMode=-1)
        Real64 SecInTimeStep;      // Seconds in one timestep (s)
        Real64 TimeRemaining;      // Time remaining in the current timestep (s)
        Real64 TimeNeeded;         // Time needed to reach the next substep (s)
        int CycleOnCount;          // Number of times heater cycles on in the current time step
        int MaxCycles;             // Maximum number of cycles allowed before exiting loop
        Real64 Runtime;            // Time that heater is running (s)
        Real64 RTF;                // Runtime fraction, fraction of timestep that heater is running
        Real64 PLR;                // Part load ratio, fraction of maximum heater capacity
        Real64 PLRsum;             // Integrated part load ratio over the timestep (J)
        Real64 PLF;                // Part load factor, modifies thermal efficiency to get total energy efficiency
        Real64 Tsum;               // Integrated tank temp over the timestep, dividing by time gives the average (C s)
        Real64 deltaTsum;          // Change in integrated tank temperature, dividing by time gives the average (C s)
        Real64 Eloss;              // Energy change due to ambient losses over the timestep (J)
        Real64 Elosszone;          // Energy change to the zone due to ambient losses over the timestep (J)
        Real64 Euse;               // Energy change due to use side mass flow over the timestep (J)
        Real64 Esource;            // Energy change due to source side mass flow over the timestep (J)
        Real64 Eheater;            // Energy change due to the heater over the timestep (J)
        Real64 Eoncycfuel;         // Fuel energy consumed by on-cycle parasitics over the timestep (J)
        Real64 Eoffcycfuel;        // Fuel energy consumed by off-cycle parasitics over the timestep (J)
        Real64 Event;              // Energy change due to venting over the timestep (J)
        Real64 Eneeded;            // Energy change needed over the timestep (J)
        Real64 Eunmet;             // Energy change unmet over the timestep (J)
        Real64 Efuel;              // Energy change for fuel consumed over the timestep (J)
        bool SetPointRecovered;    // Flag to indicate when setpoint is recovered for the first time
        Real64 rho;
        Real64 HPWHCondenserDeltaT(0.0); // Temperature difference across the condenser for a heat pump water heater
        static int DummyWaterIndex(1);

        // Reference to objects
        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum); // Reference to the tank object to save typing

        // FLOW:
        TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;

        if (Tank.TimeElapsed != TimeElapsed) {
            // The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
            // timestep for use as the initial conditions of each iteration that does not advance the system timestep.
            Tank.SavedTankTemp = Tank.TankTemp;
            Tank.SavedMode = Tank.Mode;

            // Save outlet temperatures for demand-side flow control
            Tank.SavedUseOutletTemp = Tank.UseOutletTemp;
            Tank.SavedSourceOutletTemp = Tank.SourceOutletTemp;

            Tank.TimeElapsed = TimeElapsed;
        }

        TankTemp = Tank.SavedTankTemp;
        Mode = Tank.SavedMode;

        Qmaxcap = Tank.MaxCapacity;
        Qmincap = Tank.MinCapacity;
        Qoffcycfuel = Tank.OffCycParaLoad;
        Qoffcycheat = Qoffcycfuel * Tank.OffCycParaFracToTank;
        Qoncycfuel = Tank.OnCycParaLoad;
        Qoncycheat = Qoncycfuel * Tank.OnCycParaFracToTank;

        SetPointTemp = Tank.SetPointTemp;
        DeadBandTemp = Tank.getDeadBandTemp();
        MaxTemp = Tank.TankTempLimit;
        AmbientTemp = Tank.AmbientTemp;

        UseInletTemp = Tank.UseInletTemp;
        UseMassFlowRate = Tank.UseMassFlowRate * Tank.UseEffectiveness;
        MinMassFlowRate = Tank.MassFlowRateMin;
        SourceInletTemp = Tank.SourceInletTemp;
        SourceMassFlowRate = Tank.SourceMassFlowRate * Tank.SourceEffectiveness;

        if (Tank.UseSidePlantLoopNum > 0) {
            rho = GetDensityGlycol(
                PlantLoop(Tank.UseSidePlantLoopNum).FluidName, TankTemp, PlantLoop(Tank.UseSidePlantLoopNum).FluidIndex, RoutineName);
        } else {
            rho = GetDensityGlycol(fluidNameWater, TankTemp, DummyWaterIndex, RoutineName);
        }

        TankMass = rho * Tank.Volume;

        if (Tank.UseSidePlantLoopNum > 0) {
            Cp = GetSpecificHeatGlycol(
                PlantLoop(Tank.UseSidePlantLoopNum).FluidName, TankTemp, PlantLoop(Tank.UseSidePlantLoopNum).FluidIndex, RoutineName);
        } else {
            Cp = GetSpecificHeatGlycol(fluidNameWater, TankTemp, DummyWaterIndex, RoutineName);
        }

        SecInTimeStep = TimeStepSys * SecInHour;
        TimeRemaining = SecInTimeStep;
        TimeNeeded = 0.0;
        CycleOnCount = 0;
        MaxCycles = SecInTimeStep;
        Runtime = 0.0;
        SetPointRecovered = false;

        Tsum = 0.0;
        Eloss = 0.0;
        Elosszone = 0.0;
        Euse = 0.0;
        Esource = 0.0;
        Eheater = 0.0;
        Event = 0.0;
        Eneeded = 0.0;
        Eunmet = 0.0;
        Efuel = 0.0;
        Eoncycfuel = 0.0;
        Eoffcycfuel = 0.0;
        PLR = 0.0;
        PLRsum = 0.0;

        Qheat = 0.0;
        Qheater = 0.0;
        Qvent = 0.0;
        Qneeded = 0.0;
        Qunmet = 0.0;
        Qnet = 0.0;
        Qfuel = 0.0;

        // Calculate the heating rate from the heat pump.
        if (Tank.HeatPumpNum > 0) {
            HeatPumpWaterHeaterData const &HeatPump = HPWaterHeater(Tank.HeatPumpNum);
            DataLoopNode::NodeData const &HPWHCondWaterInletNode = DataLoopNode::Node(HeatPump.CondWaterInletNode);
            DataLoopNode::NodeData const &HPWHCondWaterOutletNode = DataLoopNode::Node(HeatPump.CondWaterOutletNode);
            HPWHCondenserDeltaT = HPWHCondWaterOutletNode.Temp - HPWHCondWaterInletNode.Temp;
        }
        assert(HPWHCondenserDeltaT >= 0);

        CalcMixedTankSourceSideHeatTransferRate(HPWHCondenserDeltaT, SourceInletTemp, Cp, SetPointTemp, SourceMassFlowRate, Qheatpump, Qsource);

        // Calculate steady-state use heat rate.
        Quse = UseMassFlowRate * Cp * (UseInletTemp - SetPointTemp);

        while (TimeRemaining > 0.0) {

            TimeNeeded = 0.0;

            NewTankTemp = TankTemp;

            {
                auto const SELECT_CASE_var(Mode);

                if (SELECT_CASE_var == HeatMode) { // Heater is on

                    // Calculate heat rate needed to maintain the setpoint at steady-state conditions
                    LossCoeff = Tank.OnCycLossCoeff;
                    LossFracToZone = Tank.OnCycLossFracToZone;
                    Qloss = LossCoeff * (AmbientTemp - SetPointTemp);
                    Qneeded = -Quse - Qsource - Qloss - Qoncycheat;

                    if (TankTemp > SetPointTemp) {
                        // Heater is not needed after all, possibly due to step change in scheduled SetPointTemp

                        Qheater = 0.0;
                        Qunmet = 0.0;
                        Mode = FloatMode;
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
                            Mode = FloatMode;
                            continue;

                        } else if (Qneeded < Qmincap) {
                            // Heater is required at less than the minimum capacity
                            // If cycling, Qmincap = Qmaxcap.  Once the setpoint is reached, heater will almost always be shut off here

                            {
                                auto const SELECT_CASE_var1(Tank.ControlType);

                                if (SELECT_CASE_var1 == ControlTypeCycle) {
                                    // Control will cycle on and off based on DeadBandTemp
                                    Qheater = 0.0;
                                    Qunmet = 0.0;
                                    Mode = FloatMode;
                                    continue;

                                } else if (SELECT_CASE_var1 == ControlTypeModulate) {
                                    // Control will cycle on and off based on DeadBandTemp until Qneeded > Qmincap again
                                    Qheater = 0.0;
                                    Qunmet = Qneeded;
                                    Mode = FloatMode;
                                    continue;

                                    // CASE (ControlTypeModulateWithOverheat)  ! Not yet implemented
                                    // Calculate time to reach steady-state temp; check for venting at MaxTemp limit
                                    // Qheater = Qmincap

                                    // CASE (ControlTypeModulateWithUnderheat)  ! Not yet implemented
                                    // Heater must not come back on until Qneeded >= Qmincap
                                    // Mode = FloatMode
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
                    PLF = PartLoadFactor(WaterThermalTankNum, PLR);
                    Efuel += Qheater * TimeNeeded / (PLF * Tank.Efficiency);

                    Runtime += TimeNeeded;
                    PLRsum += PLR * TimeNeeded;

                    if (!Tank.FirstRecoveryDone) {
                        Tank.FirstRecoveryFuel += Efuel + Eoffcycfuel + Eoncycfuel;
                        if (SetPointRecovered) Tank.FirstRecoveryDone = true;
                    }

                } else if ((SELECT_CASE_var == FloatMode) || (SELECT_CASE_var == CoolMode)) { // Heater is off

                    // Calculate heat rate needed to maintain the setpoint at steady-state conditions
                    LossCoeff = Tank.OffCycLossCoeff;
                    LossFracToZone = Tank.OffCycLossFracToZone;
                    Qloss = LossCoeff * (AmbientTemp - SetPointTemp);
                    Qneeded = -Quse - Qsource - Qloss - Qoffcycheat;

                    // This section really needs to work differently depending on ControlType
                    // CYCLE will look at TankTemp, MODULATE will look at Qneeded

                    if ((TankTemp < DeadBandTemp) && (!Tank.IsChilledWaterTank)) {
                        // Tank temperature is already below the minimum, possibly due to step change in scheduled SetPointTemp

                        Mode = HeatMode;
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
                            // Heating will be needed in this timestep

                            NewTankTemp = DeadBandTemp;
                            Mode = HeatMode;
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
                                // Venting will be needed in this timestep

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
                                Mode = VentMode;

                            } // NewTankTemp >= MaxTemp

                        } // TimeNeeded <= TimeRemaining

                    } else if ((TankTemp > DeadBandTemp) && (Tank.IsChilledWaterTank)) {
                        Mode = CoolMode;
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

                        if (TankTemp < SetPointTemp) Mode = FloatMode;

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

                } else if (SELECT_CASE_var == VentMode) { // Excess heat is vented

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

                        Mode = FloatMode;
                        continue;

                    } else { // NewTankTemp >= MaxTemp

                        TimeNeeded = TimeRemaining;

                        // Calculate the steady-state venting rate needed to maintain the tank at maximum temperature
                        Qloss = LossCoeff * (AmbientTemp - MaxTemp);
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

            deltaTsum = CalcTempIntegral(TankTemp,
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

                if (!WarmupFlag) {
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

        // Calculate average values over the timestep based on summed values, Q > 0 is a gain to the tank,  Q < 0 is a loss to the tank
        TankTempAvg = Tsum / SecInTimeStep;
        Qloss = Eloss / SecInTimeStep;
        Qlosszone = Elosszone / SecInTimeStep;
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
        RTF = Runtime / SecInTimeStep;
        PLR = PLRsum / SecInTimeStep;

        if (Tank.ControlType == ControlTypeCycle) {
            // Recalculate Part Load Factor and fuel energy based on Runtime Fraction, instead of Part Load Ratio
            PLF = PartLoadFactor(WaterThermalTankNum, RTF);
            Efuel = Eheater / (PLF * Tank.Efficiency);
        }

        Qfuel = Efuel / SecInTimeStep;

        Tank.Mode = Mode; // Operating mode for carry-over to next timestep

        Tank.TankTemp = TankTemp;            // Final tank temperature for carry-over to next timestep
        Tank.TankTempAvg = TankTempAvg;      // Average tank temperature over the timestep for reporting
        Tank.UseOutletTemp = TankTempAvg;    // Because entire tank is at same temperature
        Tank.SourceOutletTemp = TankTempAvg; // Because entire tank is at same temperature
        if (Tank.HeatPumpNum > 0) {
            Tank.SourceInletTemp = TankTempAvg + HPWHCondenserDeltaT; // Update the source inlet temperature to the average over the timestep
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

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const Infinity(99999999.9); // A time interval much larger than any single timestep (s)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 a;        // Intermediate variable
        Real64 b;        // Intermediate variable
        Real64 Tm;       // Mixed temperature after an infinite amount of time has passed (C)
        Real64 quotient; // Intermediate variable
        Real64 t;        // Time elapsed from Ti to Tf (s)

        // FLOW:
        if (Tf == Ti) {
            // Already at Tf; no time is needed
            t = 0.0;

        } else {

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
        // later divided by the elapsed time to yield the average tank temperature over the timestep.

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

        // METHODOLOGY EMPLOYED:
        // Uses CurveManager.  Part Load Factor is not allowed below 0.1.

        // Using/Aliasing
        using CurveManager::CurveValue;

        // Return value
        Real64 PartLoadFactor;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // FLOW:
        if (WaterThermalTank(WaterThermalTankNum).PLFCurve > 0) {
            PartLoadFactor = CurveValue(WaterThermalTank(WaterThermalTankNum).PLFCurve, PartLoadRatio);

            PartLoadFactor = max(PartLoadFactor, 0.1);
        } else {
            // No curve was defined
            PartLoadFactor = 1.0;
        }

        return PartLoadFactor;
    }

    void CalcWaterThermalTankStratified(int const WaterThermalTankNum) // Water Heater being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   January 2007
        //       MODIFIED       na
        //                      Nov 2011, BAN; modified the use and source outlet temperature calculation
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulates a stratified, multi-node water heater tank with up to two heating elements.

        // METHODOLOGY EMPLOYED:
        // This model uses a numerical calculation based on the forward Euler method.  A heat balance is calculated for each
        // node at a sub time step interval of one second.  Temperatures and energies change dynamically over the system
        // time step.  Final node temperatures are reported as final instantaneous values as well as averages over the
        // time step.  Heat transfer rates are averages over the time step.

        // Using/Aliasing
        using DataGlobals::HourOfDay;
        using DataGlobals::TimeStep;
        using DataGlobals::TimeStepZone;
        using DataHVACGlobals::SysTimeElapsed;
        using DataHVACGlobals::TimeStepSys;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const dt(1.0); // Sub time step interval (s)
        static std::string const RoutineName("CalcWaterThermalTankStratified");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TimeElapsed;         // Fraction of the current hour that has elapsed (h)
        Real64 SecInTimeStep;       // Seconds in one timestep (s)
        Real64 TimeRemaining;       // Time remaining in the current timestep (s)
        int NodeNum;                // Node number index
        Real64 NodeMass;            // Mass of water in a node (kg)
        Real64 NodeTemp;            // Instantaneous node temperature (C)
        Real64 TempUp;              // Temperature of the upper node (C)
        Real64 TempDn;              // Temperature of the lower node (C)
        Real64 InvMixUp;            // Inversion mixing rate with the upper node (kg/s)
        Real64 InvMixDn;            // Inversion mixing rate with the lower node (kg/s)
        Real64 Cp;                  // Specific heat of water (J/kg K)
        Real64 LossCoeff;           // Loss coefficient to ambient environment (W/K)
        Real64 AmbientTemp;         // Current ambient air temperature around tank (C)
        Real64 SetPointTemp1;       // Current set point temperature for heater 1 (C)
        Real64 SetPointTemp2;       // Current set point temperature for heater 2 (C)
        Real64 MinTemp1;            // Minimum tank temperature (SetPointTemp1 - DeadBandDeltaTemp1) (C)
        Real64 MinTemp2;            // Minimum tank temperature (SetPointTemp2 - DeadBandDeltaTemp2) (C)
        Real64 MaxTemp;             // Maximum tank temperature before venting (C)
        Real64 Quse;                // Heating rate due to use side mass flow (W)
        Real64 Qsource;             // Heating rate due to source side mass flow (W)
        Real64 Qcond;               // Heating rate due to vertical conduction between nodes
        Real64 Qflow;               // Heating rate due to fluid flow between inlet and outlet nodes
        Real64 Qmix;                // Heating rate due to temperature inversion mixing between nodes
        Real64 Qloss;               // Heating rate due to ambient environment (W)
        Real64 Qlosszone;           // Heating rate of fraction of losses added to the zone as a gain (W)
        Real64 Qheat;               // Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
        Real64 Qheater1;            // Heating rate of burner or electric heating element 1 (W)
        Real64 Qheater2;            // Heating rate of burner or electric heating element 2 (W)
        Real64 Qheater;             // Combined heating rate of heater 1 and 2 (W)
        Real64 Qheatpump(0.0);      // heat rate from the heat pump (W)
        Real64 Qoffcycfuel;         // Fuel consumption rate of off-cycle parasitics (W)
        Real64 Qoffcycheat;         // Heating rate of fraction of off-cycle parasitics added to the tank (W)
        Real64 Qoncycfuel;          // Fuel consumption rate on-cycle parasitics added to the tank (W)
        Real64 Qoncycheat;          // Heating rate of fraction of on-cycle parasitics added to the tank (W)
        Real64 Qneeded;             // Heating rate needed to recover or maintain the setpoint temperature (W)
        Real64 Qunmet;              // The difference between Qneeded and Qheater (W)
        Real64 Qvent;               // Heating rate due to venting because tank exceeded max temperature limit (W)
        Real64 Qfuel;               // Heating rate for fuel consumed (W)
        Real64 Qusl;                // Quse + Qsource + Qloss
        Real64 UseInletTemp;        // Use side inlet temperature (C)
        Real64 UseMassFlowRate;     // Use side flow rate, including effectiveness factor (kg/s)
        Real64 SourceInletTemp;     // Source side inlet temperature (C)
        Real64 SourceMassFlowRate;  // Source side flow rate, including effectiveness factor (kg/s)
        int CycleOnCount1;          // Number of times heater 1 cycles on in the current time step
        int CycleOnCount2;          // Number of times heater 2 cycles on in the current time step
        Real64 Runtime1;            // Time that heater 1 is running (s)
        Real64 Runtime2;            // Time that heater 2 is running (s)
        Real64 Runtime;             // Time that either heater is running (s)
        Real64 RTF1;                // Runtime fraction, fraction of timestep that heater 1 is running
        Real64 RTF2;                // Runtime fraction, fraction of timestep that heater 2 is running
        Real64 RTF;                 // Runtime fraction, fraction of timestep that either heater is running
        Real64 Eloss;               // Energy change due to ambient losses over the timestep (J)
        Real64 Elosszone;           // Energy change to the zone due to ambient losses over the timestep (J)
        Real64 Euse;                // Energy change due to use side mass flow over the timestep (J)
        Real64 Esource;             // Energy change due to source side mass flow over the timestep (J)
        Real64 Eheater1;            // Energy change due to heater 1 over the timestep (J)
        Real64 Eheater2;            // Energy change due to heater 2 over the timestep (J)
        Real64 Eoncycfuel;          // Fuel energy consumed by on-cycle parasitics over the timestep (J)
        Real64 Eoffcycfuel;         // Fuel energy consumed by off-cycle parasitics over the timestep (J)
        Real64 Event;               // Energy change due to venting over the timestep (J)
        Real64 Eneeded;             // Energy change needed over the timestep (J)
        Real64 Eunmet;              // Energy change unmet over the timestep (J)
        Real64 Efuel;               // Energy change for fuel consumed over the timestep (J)
        int HPWHCondenserConfig(0); // Condenser configuration of HPWH
        bool SetPointRecovered;     // Flag to indicate when set point is recovered for the first time
        static int DummyWaterIndex(1);

        // References
        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum); // Tank object

        // FLOW:
        TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed;

        if (Tank.TimeElapsed != TimeElapsed) {
            // The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
            // timestep for use as the initial conditions of each iteration that does not advance the system timestep.
            for (auto &e : Tank.Node)
                e.SavedTemp = e.Temp;

            Tank.SavedHeaterOn1 = Tank.HeaterOn1;
            Tank.SavedHeaterOn2 = Tank.HeaterOn2;

            // Save outlet temperatures for demand-side flow control
            Tank.SavedUseOutletTemp = Tank.UseOutletTemp;
            Tank.SavedSourceOutletTemp = Tank.SourceOutletTemp;

            Tank.TimeElapsed = TimeElapsed;
        }

        for (auto &e : Tank.Node)
            e.Temp = e.SavedTemp;

        Tank.HeaterOn1 = Tank.SavedHeaterOn1;
        Tank.HeaterOn2 = Tank.SavedHeaterOn2;

        SecInTimeStep = TimeStepSys * SecInHour;

        AmbientTemp = Tank.AmbientTemp;
        UseInletTemp = Tank.UseInletTemp;
        SourceInletTemp = Tank.SourceInletTemp;

        // Calculate the heating rate from the heat pump.
        if (Tank.HeatPumpNum > 0) {
            HeatPumpWaterHeaterData const &HPWH = HPWaterHeater(Tank.HeatPumpNum);
            if (HPWH.NumofSpeed > 0) {
                // VSHPWH
                VariableSpeedCoils::VariableSpeedCoilData const &Coil = VariableSpeedCoils::VarSpeedCoil(HPWH.DXCoilNum);
                Qheatpump = Coil.TotalHeatingEnergyRate * Tank.SourceEffectiveness;
            } else {
                // Single speed HPWH
                DXCoils::DXCoilData const &Coil = DXCoils::DXCoil(HPWH.DXCoilNum);
                Qheatpump = Coil.TotalHeatingEnergyRate * Tank.SourceEffectiveness;
            }
            HPWHCondenserConfig = HPWH.TypeNum;
        }

        SetPointTemp1 = Tank.SetPointTemp;
        MinTemp1 = SetPointTemp1 - Tank.DeadBandDeltaTemp;
        SetPointTemp2 = Tank.SetPointTemp2;
        MinTemp2 = SetPointTemp2 - Tank.DeadBandDeltaTemp2;
        MaxTemp = Tank.TankTempLimit;

        if (Tank.UseSidePlantLoopNum > 0) {
            Cp = GetSpecificHeatGlycol(
                PlantLoop(Tank.UseSidePlantLoopNum).FluidName, Tank.TankTemp, PlantLoop(Tank.UseSidePlantLoopNum).FluidIndex, RoutineName);
        } else {
            Cp = GetSpecificHeatGlycol(fluidNameWater, Tank.TankTemp, DummyWaterIndex, RoutineName);
        }
        Real64 const dt_Cp(dt / Cp);

        TempUp = 0.0;
        TempDn = 0.0;
        Eloss = 0.0;
        Elosszone = 0.0;
        Euse = 0.0;
        Esource = 0.0;
        Eheater1 = 0.0;
        Eheater2 = 0.0;
        Event = 0.0;
        Eneeded = 0.0;
        Eunmet = 0.0;
        Efuel = 0.0;
        Eoncycfuel = 0.0;
        Eoffcycfuel = 0.0;
        CycleOnCount1 = 0;
        CycleOnCount2 = 0;
        Runtime = 0.0;
        Runtime1 = 0.0;
        Runtime2 = 0.0;
        SetPointRecovered = false;

        if (Tank.InletMode == InletModeFixed) CalcNodeMassFlows(WaterThermalTankNum, InletModeFixed);

        TimeRemaining = SecInTimeStep;
        Real64 &tank_node1_newTemp(Tank.Node(1).NewTemp);
        Real64 const &tank_useOutletStratNode_temp(Tank.Node(Tank.UseOutletStratNode).Temp);
        while (TimeRemaining > 0.0) { //? Might be good to use a check that is tolerant to some epsilon * dt to avoid an extra pass due to roundoff
                                      // when subtracting dt (if dt is ever not a value with an exact floating point representation)

            if (Tank.InletMode == InletModeSeeking) CalcNodeMassFlows(WaterThermalTankNum, InletModeSeeking);

            if (!Tank.IsChilledWaterTank) {

                // Control the first heater element (master)
                if (Tank.MaxCapacity > 0.0) {
                    NodeTemp = Tank.Node(Tank.HeaterNode1).Temp;

                    if (Tank.HeaterOn1) {
                        if (NodeTemp >= SetPointTemp1) {
                            Tank.HeaterOn1 = false;
                            SetPointRecovered = true;
                        }
                    } else { // Heater is off
                        if (NodeTemp < MinTemp1) {
                            Tank.HeaterOn1 = true;
                            ++CycleOnCount1;
                        }
                    }
                }

                if (Tank.HeaterOn1) {
                    Qheater1 = Tank.MaxCapacity;
                    Runtime1 += dt;
                } else {
                    Qheater1 = 0.0;
                }

                // Control the second heater element (slave)
                if (Tank.MaxCapacity2 > 0.0) {
                    if ((Tank.ControlType == PriorityMasterSlave) && Tank.HeaterOn1) {
                        Tank.HeaterOn2 = false;

                    } else {
                        NodeTemp = Tank.Node(Tank.HeaterNode2).Temp;

                        if (Tank.HeaterOn2) {
                            if (NodeTemp >= SetPointTemp2) {
                                Tank.HeaterOn2 = false;
                                SetPointRecovered = true;
                            }
                        } else { // Heater is off
                            if (NodeTemp < MinTemp2) {
                                Tank.HeaterOn2 = true;
                                ++CycleOnCount2;
                            }
                        }
                    }
                }

                if (Tank.HeaterOn2) {
                    Qheater2 = Tank.MaxCapacity2;
                    Runtime2 += dt;
                } else {
                    Qheater2 = 0.0;
                }
            } else { // chilled water thank, no heating

                Qheater1 = 0.0;
                Qheater2 = 0.0;
            }
            Real64 const Qheaters(Qheater1 + Qheater2);

            if (Tank.HeaterOn1 || Tank.HeaterOn2) {
                Runtime += dt;

                Qfuel = Qheaters / Tank.Efficiency;
                Qoncycfuel = Tank.OnCycParaLoad;
                Qoffcycfuel = 0.0;
            } else {
                Qfuel = 0.0;
                Qoncycfuel = 0.0;
                Qoffcycfuel = Tank.OffCycParaLoad;
            }

            // Loop through all nodes and simulate heat balance
            for (int NodeNum = 1, nTankNodes = Tank.Nodes; NodeNum <= nTankNodes; ++NodeNum) {
                auto &tank_node(Tank.Node(NodeNum));
                NodeMass = tank_node.Mass;
                NodeTemp = tank_node.Temp;

                UseMassFlowRate = tank_node.UseMassFlowRate * Tank.UseEffectiveness;
                SourceMassFlowRate = tank_node.SourceMassFlowRate * Tank.SourceEffectiveness;

                // Heat transfer due to fluid flow entering an inlet node
                Quse = UseMassFlowRate * Cp * (UseInletTemp - NodeTemp);
                if (Tank.HeatPumpNum > 0) {
                    // Heat Pump Water Heater
                    if (HPWHCondenserConfig == TypeOf_HeatPumpWtrHeaterPumped) {
                        if (SourceMassFlowRate > 0.0) {
                            Qsource = Qheatpump;
                        } else {
                            Qsource = 0.0;
                        }
                    } else {
                        assert(HPWHCondenserConfig == TypeOf_HeatPumpWtrHeaterWrapped);
                        Qsource = Qheatpump * tank_node.HPWHWrappedCondenserHeatingFrac;
                    }
                } else {
                    // Constant temperature source side flow
                    Qsource = SourceMassFlowRate * Cp * (SourceInletTemp - NodeTemp);
                }

                InvMixUp = 0.0;
                if (NodeNum > 1) {
                    TempUp = Tank.Node(NodeNum - 1).Temp;
                    if (TempUp < NodeTemp) InvMixUp = Tank.InversionMixingRate;
                }
                // BUG? When NodeNum==1 TempUp is 0.0   Shouldn't it be NodeTemp to void the vertical conduction? !!!!!!!!!!!!!!!!!
                Real64 const delTempUp(TempUp - NodeTemp);

                InvMixDn = 0.0;
                if (NodeNum < nTankNodes) {
                    TempDn = Tank.Node(NodeNum + 1).Temp;
                    if (TempDn > NodeTemp) InvMixDn = Tank.InversionMixingRate;
                }
                Real64 const delTempDn(TempDn - NodeTemp);

                // Heat transfer due to vertical conduction between nodes
                Qcond = tank_node.CondCoeffUp * delTempUp + tank_node.CondCoeffDn * delTempDn;

                // Heat transfer due to fluid flow between inlet and outlet nodes
                Qflow = Cp * (tank_node.MassFlowFromUpper * delTempUp + tank_node.MassFlowFromLower * delTempDn);

                // Heat transfer due to temperature inversion mixing between nodes
                Qmix = Cp * (InvMixUp * delTempUp + InvMixDn * delTempDn);

                if (Tank.HeaterOn1 || Tank.HeaterOn2) {
                    LossCoeff = tank_node.OnCycLossCoeff;
                    Qloss = LossCoeff * (AmbientTemp - NodeTemp);
                    Qheat = Qoncycheat = tank_node.OnCycParaLoad * Tank.OnCycParaFracToTank;
                    Qusl = Quse + Qsource + Qloss;

                    Qneeded = max(-Qusl - Qoncycheat, 0.0);

                    if (NodeNum == Tank.HeaterNode1) {
                        Qheat += Qheater1;
                    } else if (NodeNum == Tank.HeaterNode2) {
                        Qheat += Qheater2;
                    }
                } else {
                    LossCoeff = tank_node.OffCycLossCoeff;
                    Qloss = LossCoeff * (AmbientTemp - NodeTemp);
                    Qheat = Qoffcycheat = tank_node.OffCycParaLoad * Tank.OffCycParaFracToTank;
                    Qusl = Quse + Qsource + Qloss;

                    Qneeded = max(-Qusl - Qoffcycheat, 0.0);
                }

                Qunmet = max(Qneeded - Qheaters, 0.0);

                // Calculate node heat balance
                tank_node.NewTemp = NodeTemp + (Qusl + Qcond + Qflow + Qmix + Qheat) * dt_Cp / NodeMass;

                if (!Tank.IsChilledWaterTank) {
                    if ((NodeNum == 1) && (tank_node1_newTemp > MaxTemp)) {
                        Event += NodeMass * (MaxTemp - tank_node1_newTemp);
                        tank_node1_newTemp = MaxTemp;
                    }
                }

                Esource += Qsource;
                Eloss += Qloss;
                Eneeded += Qneeded;
                Eunmet += Qunmet;

            } // NodeNum

            Euse += Tank.UseMassFlowRate * (UseInletTemp - tank_useOutletStratNode_temp);

            // Calculation for standard ratings
            if (!Tank.FirstRecoveryDone) {
                Tank.FirstRecoveryFuel += (Qfuel + Qoffcycfuel + Qoncycfuel) * dt;
                if (SetPointRecovered) Tank.FirstRecoveryDone = true;
            }

            // Update node temperatures
            for (auto &e : Tank.Node) {
                e.Temp = e.NewTemp;
                e.TempSum += e.Temp * dt;
            }

            TimeRemaining -= dt;

        } // TimeRemaining > 0.0

        // Finalize accumulated values
        Event *= Cp;
        Esource *= dt;
        Eloss *= dt;
        Elosszone = Eloss * Tank.SkinLossFracToZone;
        Eneeded *= dt;
        Eunmet *= dt;
        Euse *= Cp * dt;

        Eheater1 = Tank.MaxCapacity * Runtime1;
        Eheater2 = Tank.MaxCapacity2 * Runtime2;
        Efuel = (Eheater1 + Eheater2) / Tank.Efficiency;
        Eoffcycfuel = Tank.OffCycParaLoad * (SecInTimeStep - Runtime);
        Eoncycfuel = Tank.OnCycParaLoad * Runtime;

        // Calculate average values over the time step based on summed values, Q > 0 is a gain to the tank,  Q < 0 is a loss to the tank
        Qloss = Eloss / SecInTimeStep;
        Qlosszone = Elosszone / SecInTimeStep;
        Quse = Euse / SecInTimeStep;
        Qsource = Esource / SecInTimeStep;
        Qheater1 = Eheater1 / SecInTimeStep;
        Qheater2 = Eheater2 / SecInTimeStep;
        Qheater = Qheater1 + Qheater2;
        Qoffcycfuel = Eoffcycfuel / SecInTimeStep;
        Qoffcycheat = Qoffcycfuel * Tank.OffCycParaFracToTank;
        Qoncycfuel = Eoncycfuel / SecInTimeStep;
        Qoncycheat = Qoncycfuel * Tank.OnCycParaFracToTank;
        Qvent = Event / SecInTimeStep;
        Qneeded = Eneeded / SecInTimeStep;
        Qunmet = Eunmet / SecInTimeStep;
        RTF = Runtime / SecInTimeStep;
        RTF1 = Runtime1 / SecInTimeStep;
        RTF2 = Runtime2 / SecInTimeStep;
        Qfuel = Efuel / SecInTimeStep;

        // Calculate average node temperatures over the time step
        for (auto &e : Tank.Node) {
            e.TempAvg = e.TempSum / SecInTimeStep;
            e.TempSum = 0.0; // Reset for next time step
        }

        // Calculate instantaneous and average tank temperature (all nodes have equal mass)
        Tank.TankTemp = sum(Tank.Node, &StratifiedNodeData::Temp) / Tank.Nodes;
        Tank.TankTempAvg = sum(Tank.Node, &StratifiedNodeData::TempAvg) / Tank.Nodes;

        NodeNum = Tank.UseOutletStratNode;
        if (NodeNum > 0) Tank.UseOutletTemp = Tank.Node(NodeNum).TempAvg;
        // Revised use outlet temperature to ensure energy balance. Assumes a constant CP. CR8341/CR8570
        if (NodeNum > 0) {
            if (Tank.UseMassFlowRate > 0.0) {
                Tank.UseOutletTemp = Tank.UseInletTemp * (1.0 - Tank.UseEffectiveness) + Tank.UseOutletTemp * Tank.UseEffectiveness;
            }
        }
        NodeNum = Tank.SourceOutletStratNode;
        if (HPWHCondenserConfig == TypeOf_HeatPumpWtrHeaterWrapped) {
            Real64 WeightedAverageSourceOutletTemp(0.0);
            for (int i = 1; i <= Tank.Nodes; ++i) {
                WeightedAverageSourceOutletTemp += Tank.Node(i).TempAvg * Tank.Node(i).HPWHWrappedCondenserHeatingFrac;
            }
            Tank.SourceOutletTemp = WeightedAverageSourceOutletTemp;
        } else if (NodeNum > 0) {
            Tank.SourceOutletTemp = Tank.Node(NodeNum).TempAvg;
        }
        if (Tank.HeatPumpNum > 0 && HPWHCondenserConfig == TypeOf_HeatPumpWtrHeaterPumped) {
            HeatPumpWaterHeaterData const &HeatPump = HPWaterHeater(Tank.HeatPumpNum);
            DataLoopNode::NodeData const &HPWHCondWaterInletNode = DataLoopNode::Node(HeatPump.CondWaterInletNode);
            DataLoopNode::NodeData const &HPWHCondWaterOutletNode = DataLoopNode::Node(HeatPump.CondWaterOutletNode);
            Real64 const HPWHCondenserDeltaT = HPWHCondWaterOutletNode.Temp - HPWHCondWaterInletNode.Temp;
            Tank.SourceInletTemp = Tank.SourceOutletTemp + HPWHCondenserDeltaT;
        } else {
            Tank.SourceInletTemp = Tank.SourceOutletTemp;
        }
        // Revised use outlet temperature to ensure energy balance. Assumes a constant CP. CR8341/CR8570
        if (NodeNum > 0) {
            if (Tank.SourceMassFlowRate > 0.0) {
                Tank.SourceOutletTemp = Tank.SourceInletTemp * (1.0 - Tank.SourceEffectiveness) + Tank.SourceOutletTemp * Tank.SourceEffectiveness;
            }
        }

        Tank.LossRate = Qloss;
        Tank.UseRate = Quse;
        if (HPWHCondenserConfig == TypeOf_HeatPumpWtrHeaterWrapped) {
            Tank.SourceRate = 0.0;
        } else {
            Tank.SourceRate = Qsource;
        }
        Tank.OffCycParaRateToTank = Qoffcycheat;
        Tank.OnCycParaRateToTank = Qoncycheat;
        Tank.TotalDemandRate = -Quse - Qsource - Qloss - Qoffcycheat - Qoncycheat;
        Tank.HeaterRate = Qheater;
        Tank.HeaterRate1 = Qheater1;
        Tank.HeaterRate2 = Qheater2;

        Tank.UnmetRate = Qunmet;
        Tank.VentRate = Qvent;
        Tank.NetHeatTransferRate = Quse + Qsource + Qloss + Qoffcycheat + Qoncycheat + Qheater + Qvent;

        Tank.CycleOnCount = CycleOnCount1 + CycleOnCount2;
        Tank.CycleOnCount1 = CycleOnCount1;
        Tank.CycleOnCount2 = CycleOnCount2;

        Tank.RuntimeFraction = RTF;
        Tank.RuntimeFraction1 = RTF1;
        Tank.RuntimeFraction2 = RTF2;

        Tank.FuelRate = Qfuel;
        Tank.OffCycParaFuelRate = Qoffcycfuel;
        Tank.OnCycParaFuelRate = Qoncycfuel;

        // Add water heater skin losses and venting losses to ambient zone, if specified
        if (Tank.AmbientTempZone > 0) WaterThermalTank(WaterThermalTankNum).AmbientZoneGain = -Qlosszone - Qvent;
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

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumNodes;              // Number of stratified nodes
        int UseInletStratNode;     // Use-side inlet node number
        int UseOutletStratNode;    // Use-side outlet node number
        int SourceInletStratNode;  // Source-side inlet node number
        int SourceOutletStratNode; // Source-side outlet node number
        int NodeNum;               // Node number index
        Real64 UseMassFlowRate;    // Use side flow rate, including effectiveness factor (kg/s)
        Real64 SourceMassFlowRate; // Source side flow rate, including effectiveness factor (kg/s)
        int Step;                  // DO loop step direction, 1 or -1
        Real64 DeltaTemp;          // Temperature difference between node and inlet (delta C)
        Real64 MinDeltaTemp;       // Smallest temperature difference found so far (delta C)

        // References to objects
        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum);

        // FLOW:
        NumNodes = Tank.Nodes;

        UseInletStratNode = Tank.UseInletStratNode;
        UseOutletStratNode = Tank.UseOutletStratNode;
        SourceInletStratNode = Tank.SourceInletStratNode;
        SourceOutletStratNode = Tank.SourceOutletStratNode;

        UseMassFlowRate = Tank.UseMassFlowRate * Tank.UseEffectiveness;
        SourceMassFlowRate = Tank.SourceMassFlowRate * Tank.SourceEffectiveness;

        for (auto &e : Tank.Node) {
            e.UseMassFlowRate = 0.0;
            e.SourceMassFlowRate = 0.0;
            e.MassFlowFromUpper = 0.0;
            e.MassFlowFromLower = 0.0;
            e.MassFlowToUpper = 0.0;
            e.MassFlowToLower = 0.0;
        }

        if (InletMode == InletModeSeeking) {
            // 'Seek' the node with the temperature closest to the inlet temperature
            // Start at the user-specified inlet node and search to the user-specified outlet node

            if (UseMassFlowRate > 0.0) {
                if (UseInletStratNode > UseOutletStratNode) {
                    Step = -1;
                } else {
                    Step = 1;
                }
                MinDeltaTemp = 1.0e6; // Some big number
                int const NodeNum_stop(floop_end(UseInletStratNode, UseOutletStratNode, Step));
                for (NodeNum = UseInletStratNode; NodeNum != NodeNum_stop; NodeNum += Step) {
                    DeltaTemp = std::abs(Tank.Node(NodeNum).Temp - Tank.UseInletTemp);
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
                MinDeltaTemp = 1.0e6; // Some big number
                int const NodeNum_stop(floop_end(SourceInletStratNode, SourceOutletStratNode, Step));
                for (NodeNum = SourceInletStratNode; NodeNum != NodeNum_stop; NodeNum += Step) {
                    DeltaTemp = std::abs(Tank.Node(NodeNum).Temp - Tank.SourceInletTemp);
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
                for (NodeNum = UseInletStratNode; NodeNum <= UseOutletStratNode - 1; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowToLower += UseMassFlowRate;
                }
                for (NodeNum = UseInletStratNode + 1; NodeNum <= UseOutletStratNode; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowFromUpper += UseMassFlowRate;
                }

            } else if (UseOutletStratNode < UseInletStratNode) {
                // Use-side flow is up
                for (NodeNum = UseOutletStratNode; NodeNum <= UseInletStratNode - 1; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowFromLower += UseMassFlowRate;
                }
                for (NodeNum = UseOutletStratNode + 1; NodeNum <= UseInletStratNode; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowToUpper += UseMassFlowRate;
                }

            } else {
                // Use-side flow is across the node; no flow to other nodes
            }
        }

        if (SourceMassFlowRate > 0.0) {
            if (SourceOutletStratNode > SourceInletStratNode) {
                // Source-side flow is down
                for (NodeNum = SourceInletStratNode; NodeNum <= SourceOutletStratNode - 1; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowToLower += SourceMassFlowRate;
                }
                for (NodeNum = SourceInletStratNode + 1; NodeNum <= SourceOutletStratNode; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowFromUpper += SourceMassFlowRate;
                }

            } else if (SourceOutletStratNode < SourceInletStratNode) {
                // Source-side flow is up
                for (NodeNum = SourceOutletStratNode; NodeNum <= SourceInletStratNode - 1; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowFromLower += SourceMassFlowRate;
                }
                for (NodeNum = SourceOutletStratNode + 1; NodeNum <= SourceInletStratNode; ++NodeNum) {
                    Tank.Node(NodeNum).MassFlowToUpper += SourceMassFlowRate;
                }

            } else {
                // Source-side flow is across the node; no flow to other nodes
            }
        }

        // Cancel out any up and down flows
        for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
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

        // Using/Aliasing
        using CurveManager::CurveValue;
        using DataEnvironment::OutDryBulbTemp;
        using DataGlobals::DoingSizing;
        using DataGlobals::KickOffSimulation;
        using DataGlobals::SecInHour;
        using DataGlobals::WarmupFlag;
        using DataHeatBalance::HeatReclaimDXCoil;
        using DataHVACGlobals::ShortenTimeStepSys;
        using DataHVACGlobals::TimeStepSys;
        using DataLoopNode::Node;
        using DXCoils::DXCoil;
        using General::RoundSigDigits;
        using General::SolveRoot;
        using Psychrometrics::CPHW;
        using Psychrometrics::RhoH2O;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);     // Maximum number of iterations for RegulaFalsi
        Real64 const Acc(0.00001); // Accuracy of result from RegulaFalsi

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AvailSchedule;       // desuperheater availability schedule
        Real64 SetPointTemp;        // desuperheater set point temperature (cut-out temperature, C)
        Real64 DeadBandTempDiff;    // desuperheater dead band temperature difference (C)
        Real64 CutInTemp;           // desuperheater cut-in temperature (SetPointTemp - DeadBandTempDiff, C)
        Real64 TankTemp;            // tank temperature before simulation, C
        Real64 NewTankTemp;         // tank temperature after simulation, C
        Real64 MdotWater;           // mass flow rate through desuperheater, kg/s
        Real64 PartLoadRatio;       // desuperheater part load ratio
        Real64 QHeatRate;           // desuperheater heating rate (W)
        Real64 AverageWasteHeat;    // average hating rate from DX system condenser (W)
        Real64 HEffFTemp;           // output of heating efficiency as a function of temperature curve
        Real64 Effic;               // efficiency of desuperheater heating coil
        Real64 CpWater;             // specific heat of water (J/Kg/k)
        int WaterInletNode;         // desuperheater water inlet node number
        int WaterOutletNode;        // desuperheater water outlet node number
        int DesuperheaterNum;       // Index to desuperheater
        int SolFla;                 // Flag of RegulaFalsi solver
        int SourceID;               // Waste Heat Source ID number
        Array1D<Real64> Par(5);     // Parameters passed to RegulaFalsi
        static Real64 MinTemp(0.0); // used for error messages, C
        std::string IterNum;        // Max number of iterations for warning message

        // FLOW:
        DesuperheaterNum = WaterThermalTank(WaterThermalTankNum).DesuperheaterNum;
        AvailSchedule = GetCurrentScheduleValue(WaterHeaterDesuperheater(DesuperheaterNum).AvailSchedPtr);
        WaterInletNode = WaterHeaterDesuperheater(DesuperheaterNum).WaterInletNode;
        WaterOutletNode = WaterHeaterDesuperheater(DesuperheaterNum).WaterOutletNode;

        // initialize variables before invoking any RETURN statement
        PartLoadRatio = 0.0;
        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = 0.0;
        // reset tank inlet temp from previous time step
        WaterThermalTank(WaterThermalTankNum).SourceInletTemp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;
        WaterHeaterDesuperheater(DesuperheaterNum).DesuperheaterPLR = 0.0;

        Node(WaterInletNode).MassFlowRate = 0.0;
        Node(WaterOutletNode).MassFlowRate = 0.0;
        Node(WaterOutletNode).Temp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;

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

        // set up initial conditions
        QHeatRate = 0.0;
        SetPointTemp = WaterHeaterDesuperheater(DesuperheaterNum).SetPointTemp;
        DeadBandTempDiff = WaterHeaterDesuperheater(DesuperheaterNum).DeadBandTempDiff;

        // simulate only the water heater tank if the desuperheater coil is scheduled off
        if (AvailSchedule == 0.0) {
            WaterHeaterDesuperheater(DesuperheaterNum).Mode = FloatMode;
            CalcWaterThermalTankMixed(WaterThermalTankNum);
            return;
        }

        // simulate only the water heater tank if the lowest temperature available from the desuperheater coil
        // is less than water inlet temperature if the reclaim source is a refrigeration condenser
        if (ValidSourceType(DesuperheaterNum)) {
            SourceID = WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum;
            if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == CONDENSER_REFRIGERATION) {
                if (HeatReclaimRefrigCondenser(SourceID).AvailTemperature <= WaterThermalTank(WaterThermalTankNum).SourceInletTemp) {
                    WaterHeaterDesuperheater(DesuperheaterNum).Mode = FloatMode;
                    CalcWaterThermalTankMixed(WaterThermalTankNum);
                    ShowRecurringWarningErrorAtEnd("WaterHeating:Desuperheater " + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                                       " - Waste heat source temperature was too low to be useful.",
                                                   WaterHeaterDesuperheater(DesuperheaterNum).InsuffTemperatureWarn);
                    return;
                } // Temp too low
            }     // desuperheater source is condenser_refrigeration
        }         // validsourcetype

        WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelRate =
            WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaLoad * (1.0 - PartLoadRatio);
        WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelEnergy =
            WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelRate * TimeStepSys * SecInHour;

        // check that water heater tank cut-in temp is greater than desuperheater cut-in temp
        if ((SetPointTemp - DeadBandTempDiff) <= WaterThermalTank(WaterThermalTankNum).SetPointTemp) {
            if (!WarmupFlag && !DoingSizing && !KickOffSimulation) {
                MinTemp = SetPointTemp - DeadBandTempDiff;
                ++WaterHeaterDesuperheater(DesuperheaterNum).SetPointError;
                if (WaterHeaterDesuperheater(DesuperheaterNum).SetPointError < 5) {
                    ShowWarningError(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" + WaterHeaterDesuperheater(DesuperheaterNum).Name +
                                     "\":  Water heater tank set point temperature is greater than or equal to the cut-in temperature of the "
                                     "desuperheater. Desuperheater will be disabled.");
                    ShowContinueErrorTimeStamp(" ...Desuperheater cut-in temperature = " + RoundSigDigits(MinTemp, 2));
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
            CalcWaterThermalTankMixed(WaterThermalTankNum);
            return;
        }

        Effic = WaterHeaterDesuperheater(DesuperheaterNum).HeatReclaimRecoveryEff;

        // store first iteration tank temperature and desuperheater mode of operation
        if (FirstHVACIteration && !ShortenTimeStepSys) {
            // Save conditions from end of previous system timestep
            // Every iteration that does not advance time should reset to these values
            WaterThermalTank(WaterThermalTankNum).SavedTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
            WaterHeaterDesuperheater(DesuperheaterNum).SaveMode = WaterHeaterDesuperheater(DesuperheaterNum).Mode;
        }

        TankTemp = WaterThermalTank(WaterThermalTankNum).SavedTankTemp;
        WaterHeaterDesuperheater(DesuperheaterNum).Mode = WaterHeaterDesuperheater(DesuperheaterNum).SaveMode;

        if (WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp > 0) {
            HEffFTemp = max(0.0, CurveValue(WaterHeaterDesuperheater(DesuperheaterNum).HEffFTemp, TankTemp, OutDryBulbTemp));
        } else {
            HEffFTemp = 1.0;
        }

        // set limits on heat recovery efficiency
        if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == CONDENSER_REFRIGERATION) {
            if ((HEffFTemp * Effic) > 0.9) HEffFTemp = 0.9 / Effic;
        } else { // max is 0.3 for all other sources
            if ((HEffFTemp * Effic) > 0.3) HEffFTemp = 0.3 / Effic;
        } // setting limits on heat recovery efficiency

        // Access the appropriate structure to find the average heating capacity of the desuperheater heating coil
        if (ValidSourceType(DesuperheaterNum)) {
            SourceID = WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum;
            if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == COMPRESSORRACK_REFRIGERATEDCASE) {
                // Refrigeration systems are solved outside the time step iteration, so the
                //  appropriate decrement for other waste heat applications is handled differently
                AverageWasteHeat = HeatReclaimRefrigeratedRack(SourceID).AvailCapacity - HeatReclaimRefrigeratedRack(SourceID).UsedHVACCoil;
                WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR = 1.0;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == CONDENSER_REFRIGERATION) {
                AverageWasteHeat = HeatReclaimRefrigCondenser(SourceID).AvailCapacity - HeatReclaimRefrigCondenser(SourceID).UsedHVACCoil;
                WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR = 1.0;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == COIL_DX_COOLING ||
                       WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == COIL_DX_MULTISPEED ||
                       WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == COIL_DX_MULTIMODE) {
                AverageWasteHeat = HeatReclaimDXCoil(SourceID).AvailCapacity;
                WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR = DXCoil(SourceID).PartLoadRatio;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == COIL_DX_VARIABLE_COOLING) {
                AverageWasteHeat = DataHeatBalance::HeatReclaimVS_DXCoil(SourceID).AvailCapacity;
                WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR = VariableSpeedCoils::VarSpeedCoil(SourceID).PartLoadRatio;
            }
        } else {
            AverageWasteHeat = 0.0;
        }

        // simulate only water heater tank if reclaim heating source is off
        if (WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR == 0.0) {
            CalcWaterThermalTankMixed(WaterThermalTankNum);
            return;
        }

        // If the set point is higher than the maximum water temp, reset both the set point and the dead band temperature difference
        if (SetPointTemp > WaterHeaterDesuperheater(DesuperheaterNum).MaxInletWaterTemp) {
            CutInTemp = SetPointTemp - DeadBandTempDiff;
            SetPointTemp = WaterHeaterDesuperheater(DesuperheaterNum).MaxInletWaterTemp;
            DeadBandTempDiff = max(0.0, (SetPointTemp - CutInTemp));
        }

        // set the water-side mass flow rate
        CpWater = CPHW(Node(WaterInletNode).Temp);
        MdotWater = WaterHeaterDesuperheater(DesuperheaterNum).OperatingWaterFlowRate * RhoH2O(Node(WaterInletNode).Temp);
        if (Node(WaterInletNode).Temp <= WaterHeaterDesuperheater(DesuperheaterNum).MaxInletWaterTemp + Acc) {
            QHeatRate = ((AverageWasteHeat * Effic * HEffFTemp) / WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR) +
                        (WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower * WaterHeaterDesuperheater(DesuperheaterNum).PumpFracToWater);
        }

        if (MdotWater > 0.0) {
            Node(WaterOutletNode).Temp = Node(WaterInletNode).Temp + QHeatRate / (MdotWater * CpWater);
        } else {
            Node(WaterOutletNode).Temp = Node(WaterInletNode).Temp;
        }

        // change to tanktypenum using parameters?
        {
            auto const SELECT_CASE_var(WaterHeaterDesuperheater(DesuperheaterNum).TankTypeNum);

            if (SELECT_CASE_var == MixedWaterHeater) {

                WaterHeaterDesuperheater(DesuperheaterNum).SaveWHMode = WaterThermalTank(WaterThermalTankNum).Mode;

                {
                    auto const SELECT_CASE_var1(WaterHeaterDesuperheater(DesuperheaterNum).Mode);
                    if (SELECT_CASE_var1 == HeatMode) {

                        PartLoadRatio = WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR;

                        //         set the full load outlet temperature on the water heater source inlet node (init has already been called)
                        WaterThermalTank(WaterThermalTankNum).SourceInletTemp = Node(WaterOutletNode).Temp;

                        //         set the source mass flow rate for the tank
                        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = MdotWater * PartLoadRatio;

                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = WaterHeaterDesuperheater(DesuperheaterNum).BackupElementCapacity;
                        WaterThermalTank(WaterThermalTankNum).MinCapacity = WaterHeaterDesuperheater(DesuperheaterNum).BackupElementCapacity;

                        CalcWaterThermalTankMixed(WaterThermalTankNum);
                        NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;

                        if (NewTankTemp > SetPointTemp) {
                            //           Only revert to floating mode if the tank temperature is higher than the cut out temperature
                            if (NewTankTemp > WaterHeaterDesuperheater(DesuperheaterNum).SetPointTemp) {
                                WaterHeaterDesuperheater(DesuperheaterNum).Mode = FloatMode;
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
                            SolveRoot(Acc,
                                      MaxIte,
                                      SolFla,
                                      PartLoadRatio,
                                      PLRResidualMixedTank,
                                      0.0,
                                      WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR,
                                      Par);
                            if (SolFla == -1) {
                                gio::write(IterNum, fmtLD) << MaxIte;
                                strip(IterNum);
                                if (!WarmupFlag) {
                                    ++WaterHeaterDesuperheater(DesuperheaterNum).IterLimitExceededNum1;
                                    if (WaterHeaterDesuperheater(DesuperheaterNum).IterLimitExceededNum1 == 1) {
                                        ShowWarningError(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                         WaterHeaterDesuperheater(DesuperheaterNum).Name + "\"");
                                        ShowContinueError(
                                            "Iteration limit exceeded calculating desuperheater unit part-load ratio, maximum iterations = " +
                                            IterNum + ". Part-load ratio returned = " + RoundSigDigits(PartLoadRatio, 3));
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
                                if (!WarmupFlag) {
                                    ++WaterHeaterDesuperheater(DesuperheaterNum).RegulaFalsiFailedNum1;
                                    if (WaterHeaterDesuperheater(DesuperheaterNum).RegulaFalsiFailedNum1 == 1) {
                                        ShowWarningError(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                         WaterHeaterDesuperheater(DesuperheaterNum).Name + "\"");
                                        ShowContinueError("Desuperheater unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded. "
                                                          "Part-load ratio used = " +
                                                          RoundSigDigits(PartLoadRatio, 3));
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

                    } else if (SELECT_CASE_var1 == FloatMode) {

                        //         check tank temperature by setting source inlet mass flow rate to zero
                        PartLoadRatio = 0.0;

                        //         set the full load outlet temperature on the water heater source inlet node (init has already been called)
                        WaterThermalTank(WaterThermalTankNum).SourceInletTemp = Node(WaterOutletNode).Temp;

                        //         check tank temperature by setting source inlet mass flow rate to zero
                        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = 0.0;

                        //         disable the tank heater to find PLR of the HPWH
                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = 0.0;
                        WaterThermalTank(WaterThermalTankNum).MinCapacity = 0.0;

                        CalcWaterThermalTankMixed(WaterThermalTankNum);
                        NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;

                        if (NewTankTemp <= (SetPointTemp - DeadBandTempDiff)) {
                            WaterHeaterDesuperheater(DesuperheaterNum).Mode = HeatMode;
                            WaterThermalTank(WaterThermalTankNum).Mode = WaterHeaterDesuperheater(DesuperheaterNum).SaveWHMode;
                            if ((TankTemp - NewTankTemp) != 0.0) {
                                PartLoadRatio = min(WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR,
                                                    max(0.0, ((SetPointTemp - DeadBandTempDiff) - NewTankTemp) / (TankTemp - NewTankTemp)));
                            } else {
                                PartLoadRatio = WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR;
                            }

                            //           set the full load outlet temperature on the water heater source inlet node
                            WaterThermalTank(WaterThermalTankNum).SourceInletTemp = Node(WaterOutletNode).Temp;

                            //           set the source mass flow rate for the tank and enable backup heating element
                            WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = MdotWater * PartLoadRatio;
                            WaterThermalTank(WaterThermalTankNum).MaxCapacity = WaterHeaterDesuperheater(DesuperheaterNum).BackupElementCapacity;
                            WaterThermalTank(WaterThermalTankNum).MinCapacity = WaterHeaterDesuperheater(DesuperheaterNum).BackupElementCapacity;

                            CalcWaterThermalTankMixed(WaterThermalTankNum);
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
                                SolveRoot(Acc,
                                          MaxIte,
                                          SolFla,
                                          PartLoadRatio,
                                          PLRResidualMixedTank,
                                          0.0,
                                          WaterHeaterDesuperheater(DesuperheaterNum).DXSysPLR,
                                          Par);
                                if (SolFla == -1) {
                                    gio::write(IterNum, fmtLD) << MaxIte;
                                    strip(IterNum);
                                    if (!WarmupFlag) {
                                        ++WaterHeaterDesuperheater(DesuperheaterNum).IterLimitExceededNum2;
                                        if (WaterHeaterDesuperheater(DesuperheaterNum).IterLimitExceededNum2 == 1) {
                                            ShowWarningError(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                             WaterHeaterDesuperheater(DesuperheaterNum).Name + "\"");
                                            ShowContinueError(
                                                "Iteration limit exceeded calculating desuperheater unit part-load ratio, maximum iterations = " +
                                                IterNum + ". Part-load ratio returned = " + RoundSigDigits(PartLoadRatio, 3));
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
                                    if (!WarmupFlag) {
                                        ++WaterHeaterDesuperheater(DesuperheaterNum).RegulaFalsiFailedNum2;
                                        if (WaterHeaterDesuperheater(DesuperheaterNum).RegulaFalsiFailedNum2 == 1) {
                                            ShowWarningError(WaterHeaterDesuperheater(DesuperheaterNum).Type + " \"" +
                                                             WaterHeaterDesuperheater(DesuperheaterNum).Name + "\"");
                                            ShowContinueError("Desuperheater unit part-load ratio calculation failed: PLR limits of 0 to 1 exceeded. "
                                                              "Part-load ratio used = " +
                                                              RoundSigDigits(PartLoadRatio, 3));
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

        Node(WaterOutletNode).MassFlowRate = MdotWater * PartLoadRatio;
        WaterHeaterDesuperheater(DesuperheaterNum).HEffFTempOutput = HEffFTemp;
        WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate = QHeatRate * PartLoadRatio;
        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = MdotWater * PartLoadRatio;

        if (PartLoadRatio == 0) {
            WaterThermalTank(WaterThermalTankNum).SourceInletTemp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;
            Node(WaterOutletNode).Temp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;
            WaterHeaterDesuperheater(DesuperheaterNum).HEffFTempOutput = 0.0;
            WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate = 0.0;
        }

        WaterHeaterDesuperheater(DesuperheaterNum).HeaterEnergy = WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate * TimeStepSys * SecInHour;
        WaterHeaterDesuperheater(DesuperheaterNum).DesuperheaterPLR = PartLoadRatio;
        WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaFuelRate = WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaLoad * PartLoadRatio;
        WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaFuelEnergy =
            WaterHeaterDesuperheater(DesuperheaterNum).OnCycParaFuelRate * TimeStepSys * SecInHour;
        WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelRate =
            WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaLoad * (1 - PartLoadRatio);
        WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelEnergy =
            WaterHeaterDesuperheater(DesuperheaterNum).OffCycParaFuelRate * TimeStepSys * SecInHour;
        WaterHeaterDesuperheater(DesuperheaterNum).PumpPower = WaterHeaterDesuperheater(DesuperheaterNum).PumpElecPower * (PartLoadRatio);
        WaterHeaterDesuperheater(DesuperheaterNum).PumpEnergy = WaterHeaterDesuperheater(DesuperheaterNum).PumpPower * TimeStepSys * SecInHour;

        // Update remaining waste heat (just in case multiple users of waste heat use same source)
        if (ValidSourceType(DesuperheaterNum)) {
            SourceID = WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSourceIndexNum;
            if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == COMPRESSORRACK_REFRIGERATEDCASE) {
                //    Refrigeration systems are simulated at the zone time step, do not decrement available capacity
                HeatReclaimRefrigeratedRack(SourceID).UsedWaterHeater = WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == CONDENSER_REFRIGERATION) {
                HeatReclaimRefrigCondenser(SourceID).UsedWaterHeater = WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == COIL_DX_COOLING ||
                       WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == COIL_DX_MULTISPEED ||
                       WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == COIL_DX_MULTIMODE) {
                HeatReclaimDXCoil(SourceID).AvailCapacity -= WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate;
            } else if (WaterHeaterDesuperheater(DesuperheaterNum).ReclaimHeatingSource == COIL_DX_VARIABLE_COOLING) {
                DataHeatBalance::HeatReclaimVS_DXCoil(SourceID).AvailCapacity -= WaterHeaterDesuperheater(DesuperheaterNum).HeaterRate;
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

        // Using/Aliasing
        using DataGlobals::DoingSizing;
        using DataGlobals::KickOffSimulation;
        using DataGlobals::WarmupFlag;
        using DataHVACGlobals::BlowThru;
        using DataHVACGlobals::CycFanCycCoil;
        using DataHVACGlobals::HPWHCrankcaseDBTemp;
        using DataHVACGlobals::HPWHInletDBTemp;
        using DataHVACGlobals::HPWHInletWBTemp;
        using DataHVACGlobals::ShortenTimeStepSys;
        using DataHVACGlobals::SmallTempDiff;
        using DataHVACGlobals::TimeStepSys;
        using DataLoopNode::Node;
        using DXCoils::CalcHPWHDXCoil;
        using DXCoils::SimDXCoil;
        using General::RoundSigDigits;
        using General::SolveRoot;
        using IntegratedHeatPump::ClearCoils;
        using IntegratedHeatPump::GetCurWorkMode;
        using IntegratedHeatPump::GetLowSpeedNumIHP;
        using IntegratedHeatPump::GetMaxSpeedNumIHP;
        using IntegratedHeatPump::IHPOperationMode;
        using IntegratedHeatPump::IntegratedHeatPumps;
        using IntegratedHeatPump::SimIHP;
        using Psychrometrics::CPHW; // , PsyWFnTdbTwbPb
        using Psychrometrics::PsyCpAirFnWTdb;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::RhoH2O;
        using ScheduleManager::GetCurrentScheduleValue;
        using VariableSpeedCoils::SimVariableSpeedCoils;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);   // maximum number of iterations
        Real64 const Acc(0.001); // Accuracy of result from RegulaFalsi

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AvailSchedule;        // HP compressor availability schedule
        Real64 SetPointTemp;         // HP set point temperature (cut-out temperature, C)
        Real64 DeadBandTempDiff;     // HP dead band temperature difference (C)
        Real64 TankTemp(0.0);        // tank temperature before simulation, C
        Real64 NewTankTemp(0.0);     // tank temperature after simulation, C
        Real64 CpAir;                // specific heat of air, kJ/kg/K
        Real64 MdotWater;            // mass flow rate of condenser water, kg/s
        Real64 OutletAirSplitterSch; // output of outlet air splitter schedule
        int HPAirInletNode;          // HP air inlet node number
        int HPAirOutletNode;         // HP air outlet node number
        int OutdoorAirNode;          // Outdoor air inlet node number
        int ExhaustAirNode;          // Exhaust air outlet node number
        int HPWaterInletNode;        // HP condenser water inlet node number
        int HPWaterOutletNode;       // HP condenser water outlet node number
        int InletAirMixerNode;       // HP inlet air mixer node number
        int OutletAirSplitterNode;   // HP outlet air splitter node number
        int DXCoilAirInletNode;      // Inlet air node number of DX coil
        int SolFla(0);               // Flag of RegulaFalsi solver
        Array1D<Real64> Par(5);      // Parameters passed to RegulaFalsi
        Array1D<Real64> ParVS(10);   // Parameters passed to RegulaFalsi, for variable-speed HPWH
        Real64 HPMinTemp;            // used for error messages, C
        std::string HPMinTempChar;   // used for error messages
        std::string IterNum;         // Max number of iterations for warning message
        int CompOp;                  // DX compressor operation; 1=on, 0=off
        Real64 CondenserDeltaT;      // HPWH condenser water temperature difference
        // new variables for variable-speed HPWH
        int MaxSpeedNum(0);                     // speed number of variable speed HPWH coil
        int SpeedNum(1);                        // selected speed number
        Real64 RhoWater;                        // water density
        Real64 SpeedRatio(0.0);                 // speed ratio for interpolating between two speed levels
        bool bIterSpeed(false);                 // interpolation between speed level or not
        Real64 zeroResidual(1.0);               // residual when running VSHPWH at 0.0 part-load ratio, 1.0 needed setting >0
        int i;                                  // index for iteration
        Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); // place holder to calling vs HPWH function
        Real64 LowSpeedTankTemp(0.0);           // tank temperature resulted by a lower compressor speed
        Real64 HPWHCondInletNodeLast;           // Water temp sent from WH on last iteration
        // Real64 HPWaterInletNodeTempSaved; // Water temp saved from previous timestep
        int loopIter; // iteration loop counter
        int VSCoilNum(0);
        IHPOperationMode IHPMode(IHPOperationMode::IdleMode); // IHP working mode
        int LowSpeedNum(2);                                   // low speed number for iteration
        Real64 SourceEffectivenessBackup(1.0);

        // References to objects used in this function
        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum);
        HeatPumpWaterHeaterData &HeatPump = HPWaterHeater(Tank.HeatPumpNum);

        // FLOW:
        // initialize local variables
        AvailSchedule = GetCurrentScheduleValue(HeatPump.AvailSchedPtr);
        HPAirInletNode = HeatPump.HeatPumpAirInletNode;
        HPAirOutletNode = HeatPump.HeatPumpAirOutletNode;
        OutdoorAirNode = HeatPump.OutsideAirNode;
        ExhaustAirNode = HeatPump.ExhaustAirNode;
        HPWaterInletNode = HeatPump.CondWaterInletNode;
        HPWaterOutletNode = HeatPump.CondWaterOutletNode;
        InletAirMixerNode = HeatPump.InletAirMixerNode;
        OutletAirSplitterNode = HeatPump.OutletAirSplitterNode;
        DXCoilAirInletNode = HeatPump.DXCoilAirInletNode;
        HPPartLoadRatio = 0.0;
        CompOp = 0;
        HeatPump.OnCycParaFuelRate = 0.0;
        HeatPump.OnCycParaFuelEnergy = 0.0;
        HeatPump.OffCycParaFuelRate = 0.0;
        HeatPump.OffCycParaFuelEnergy = 0.0;
        Node(HPWaterOutletNode) = Node(HPWaterInletNode);
        MaxSpeedNum = HeatPump.NumofSpeed;

        // assign set point temperature (cut-out) and dead band temp diff (cut-in = cut-out minus dead band temp diff)
        SetPointTemp = HeatPump.SetPointTemp;
        DeadBandTempDiff = HeatPump.DeadBandTempDiff;
        RhoWater = RhoH2O(SetPointTemp); // initialize

        // store first iteration tank temperature and HP mode of operation
        // this code can be called more than once with FirstHVACIteration = .TRUE., use FirstTimeThroughFlag to control save
        if (FirstHVACIteration && !ShortenTimeStepSys && HeatPump.FirstTimeThroughFlag) {
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
        if (AvailSchedule == 0.0 || (SetPointTemp - DeadBandTempDiff) <= Tank.SetPointTemp || HPWHInletDBTemp < HeatPump.MinAirTempForHPOperation ||
            HPWHInletDBTemp > HeatPump.MaxAirTempForHPOperation || SetPointTemp >= Tank.TankTempLimit ||
            (!HeatPump.AllowHeatingElementAndHeatPumpToRunAtSameTime && Tank.TypeNum == MixedWaterHeater && Tank.SavedMode == HeatMode) ||
            (!HeatPump.AllowHeatingElementAndHeatPumpToRunAtSameTime && Tank.TypeNum == StratifiedWaterHeater &&
             (Tank.SavedHeaterOn1 || Tank.SavedHeaterOn2))) {
            //   revert to float mode any time HPWH compressor is OFF
            HeatPump.Mode = FloatMode;
            if (InletAirMixerNode > 0) {
                Node(InletAirMixerNode) = Node(HPAirInletNode);
            }
            //   pass node info and simulate crankcase heater
            if (MaxSpeedNum > 0) {
                VSCoilNum = HeatPump.DXCoilNum;

                if (HeatPump.bIsIHP) {
                    VSCoilNum = IntegratedHeatPumps(VSCoilNum).SCWHCoilIndex;
                }
                // set the SCWH mode
                SpeedRatio = 1.0;
                SpeedNum = 1;
                if (HeatPump.FanPlacement == BlowThru) {
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                    if (HeatPump.bIsIHP)
                        SimVariableSpeedCoils(
                            "", VSCoilNum, CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                    else
                        SimVariableSpeedCoils(
                            HeatPump.DXCoilName, VSCoilNum, CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                } else {
                    SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                    if (HeatPump.bIsIHP)
                        SimVariableSpeedCoils(
                            "", VSCoilNum, CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                    else
                        SimVariableSpeedCoils(
                            HeatPump.DXCoilName, VSCoilNum, CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                }

                // set the DWH mode
                if (HeatPump.bIsIHP) {
                    VSCoilNum = IntegratedHeatPumps(HeatPump.DXCoilNum).DWHCoilIndex;

                    if (VSCoilNum > 0) // if DWH coil exists
                    {
                        if (HeatPump.FanPlacement == BlowThru) {
                            if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                            }
                            SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                            SimVariableSpeedCoils(
                                "", VSCoilNum, CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                        } else {
                            SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                            SimVariableSpeedCoils(
                                "", VSCoilNum, CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);
                            if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                            }
                        }
                    }
                }

            } else {
                if (HeatPump.FanPlacement == BlowThru) {
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
                } else {
                    SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                }
            }

            if (OutletAirSplitterNode > 0) {
                Node(HPAirOutletNode) = Node(OutletAirSplitterNode);
            }

            //   Simulate tank if HP compressor unavailable for water heating
            CalcWaterThermalTank(WaterThermalTankNum);

            //   If HPWH compressor is available and unit is off for another reason, off-cycle parasitics are calculated
            if (AvailSchedule != 0) {
                HeatPump.OffCycParaFuelRate = HeatPump.OffCycParaLoad * (1.0 - HPPartLoadRatio);
                HeatPump.OffCycParaFuelEnergy = HeatPump.OffCycParaFuelRate * TimeStepSys * SecInHour;
            }

            //   Warn if HPWH compressor cut-in temperature is less than the water heater tank's set point temp
            if (!WarmupFlag && !DoingSizing && !KickOffSimulation) {
                if ((SetPointTemp - DeadBandTempDiff) <= Tank.SetPointTemp) {
                    HPMinTemp = SetPointTemp - DeadBandTempDiff;
                    gio::write(HPMinTempChar, fmtLD) << HPMinTemp;
                    ++HeatPump.HPSetPointError;
                    //! add logic for warmup, kickoffsimulation and doing sizing here
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
        TankTemp = Tank.SavedTankTemp;
        HeatPump.Mode = HeatPump.SaveMode;

        RhoWater = RhoH2O(TankTemp); // udpate water density using tank temp

        // set the heat pump air- and water-side mass flow rate
        MdotWater = HeatPump.OperatingWaterFlowRate * RhoH2O(TankTemp);

        // Select mode of operation (float mode or heat mode) from last iteration.
        // Determine if heating will occur this iteration and get an estimate of the PLR
        if (HeatPump.Mode == HeatMode) {
            // HPWH was heating last iteration and will continue to heat until the set point is reached
            HPPartLoadRatio = 1.0;
            if (TankTemp > SetPointTemp) { // tank set point temp may have been reduced since last iteration and float mode may be needed
                HeatPump.Mode = FloatMode;
                HPPartLoadRatio = 0.0;
                // check to see if HP needs to operate
                // set the condenser inlet node temperature and full mass flow rate prior to calling the HPWH DX coil
                {
                    auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                    if (SELECT_CASE_var1 == MixedWaterHeater) {
                        Node(HPWaterInletNode).Temp = TankTemp;
                        Node(HPWaterOutletNode).Temp = TankTemp;
                    } else if (SELECT_CASE_var1 == StratifiedWaterHeater) {
                        Node(HPWaterInletNode).Temp = Tank.SourceOutletTemp;
                        Node(HPWaterOutletNode).Temp = Tank.SourceInletTemp;
                    }
                }
                Node(HPWaterInletNode).MassFlowRate = 0.0;
                Node(HPWaterOutletNode).MassFlowRate = 0.0;

                // Check tank temperature by setting source inlet mass flow rate to zero.
                HPPartLoadRatio = 0.0;

                // Set the full load outlet temperature on the water heater source inlet node (init has already been called).
                Tank.SourceInletTemp = Node(HPWaterOutletNode).Temp;

                // Disable the tank's internal heating element to find PLR of the HPWH using floating temperatures.
                Tank.MaxCapacity = 0.0;
                Tank.MinCapacity = 0.0;
                Tank.SourceMassFlowRate = 0.0; // disables heat pump for mixed tanks
                SourceEffectivenessBackup = Tank.SourceEffectiveness;
                Tank.SourceEffectiveness = 0.0; // disables heat pump for stratified tanks
                CalcWaterThermalTank(WaterThermalTankNum);
                Tank.SourceEffectiveness = SourceEffectivenessBackup;
                NewTankTemp = GetHPWHSensedTankTemp(Tank);

                // Reset the tank's internal heating element capacity.
                Tank.MaxCapacity = HeatPump.BackupElementCapacity;
                Tank.MinCapacity = HeatPump.BackupElementCapacity;

                // Check to see if the tank drifts below set point if no heating happens.
                if (NewTankTemp <= (SetPointTemp - DeadBandTempDiff)) {

                    // HPWH is now in heating mode
                    HeatPump.Mode = HeatMode;

                    // Reset the water heater's mode (call above may have changed modes)
                    Tank.Mode = HeatPump.SaveWHMode;

                    HPPartLoadRatio = 1.0;
                }
            } else { // or use side nodes may meet set point without need for heat pump compressor operation
                // check to see if HP needs to operate
                {
                    auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                    if (SELECT_CASE_var1 == MixedWaterHeater) {
                        Node(HPWaterInletNode).Temp = TankTemp;
                        Node(HPWaterOutletNode).Temp = TankTemp;
                    } else if (SELECT_CASE_var1 == StratifiedWaterHeater) {
                        Node(HPWaterInletNode).Temp = Tank.SourceOutletTemp;
                        Node(HPWaterOutletNode).Temp = Tank.SourceInletTemp;
                    }
                }
                // Check tank temperature by setting source inlet mass flow rate to zero.
                Node(HPWaterInletNode).MassFlowRate = 0.0;
                Node(HPWaterOutletNode).MassFlowRate = 0.0;

                HPPartLoadRatio = 0.0;

                // Set the full load outlet temperature on the water heater source inlet node (init has already been called).
                Tank.SourceInletTemp = Node(HPWaterOutletNode).Temp;

                // Disable the tank's internal heating element to find PLR of the HPWH using floating temperatures.
                Tank.MaxCapacity = 0.0;
                Tank.MinCapacity = 0.0;
                Tank.SourceMassFlowRate = 0.0; // disables heat pump for mixed tanks
                SourceEffectivenessBackup = Tank.SourceEffectiveness;
                Tank.SourceEffectiveness = 0.0; // disables heat pump for stratified tanks
                CalcWaterThermalTank(WaterThermalTankNum);
                Tank.SourceEffectiveness = SourceEffectivenessBackup;
                NewTankTemp = GetHPWHSensedTankTemp(Tank);

                // Reset the tank's internal heating element capacity.
                Tank.MaxCapacity = HeatPump.BackupElementCapacity;
                Tank.MinCapacity = HeatPump.BackupElementCapacity;

                // Check to see if the tank meets set point if no heating happens.
                if (NewTankTemp > SetPointTemp) {

                    // HPWH is now in floating mode
                    HeatPump.Mode = FloatMode;

                } else {

                    // HPWH remains in heating mode
                    HPPartLoadRatio = 1.0;
                }

                // Reset the water heater's mode (call above may have changed modes)
                Tank.Mode = HeatPump.SaveWHMode;
            }
        } else {
            assert(HeatPump.Mode == FloatMode);
            // HPWH was floating last iteration and will continue to float until the cut-in temperature is reached

            // set the condenser inlet node temperature and full mass flow rate prior to calling the HPWH DX coil
            {
                auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                if (SELECT_CASE_var1 == MixedWaterHeater) {
                    Node(HPWaterInletNode).Temp = TankTemp;
                    Node(HPWaterOutletNode).Temp = TankTemp;
                } else if (SELECT_CASE_var1 == StratifiedWaterHeater) {
                    Node(HPWaterInletNode).Temp = Tank.SourceOutletTemp;
                    Node(HPWaterOutletNode).Temp = Tank.SourceInletTemp;
                }
            }
            Node(HPWaterInletNode).MassFlowRate = 0.0;
            Node(HPWaterOutletNode).MassFlowRate = 0.0;

            // Check tank temperature by setting source inlet mass flow rate to zero.
            HPPartLoadRatio = 0.0;

            // Set the full load outlet temperature on the water heater source inlet node (init has already been called).
            Tank.SourceInletTemp = Node(HPWaterOutletNode).Temp;

            // Disable the tank's internal heating element to find PLR of the HPWH using floating temperatures.
            Tank.MaxCapacity = 0.0;
            Tank.MinCapacity = 0.0;
            Tank.SourceMassFlowRate = 0.0; // disables heat pump for mixed tanks
            SourceEffectivenessBackup = Tank.SourceEffectiveness;
            Tank.SourceEffectiveness = 0.0; // disables heat pump for stratified tanks
            CalcWaterThermalTank(WaterThermalTankNum);
            Tank.SourceEffectiveness = SourceEffectivenessBackup;
            NewTankTemp = GetHPWHSensedTankTemp(Tank);

            // Reset the tank's internal heating element capacity.
            Tank.MaxCapacity = HeatPump.BackupElementCapacity;
            Tank.MinCapacity = HeatPump.BackupElementCapacity;

            // Check to see if the tank drifts below set point if no heating happens.
            if (NewTankTemp <= (SetPointTemp - DeadBandTempDiff)) {

                // HPWH is now in heating mode
                HeatPump.Mode = HeatMode;

                // Reset the water heater's mode (call above may have changed modes)
                Tank.Mode = HeatPump.SaveWHMode;

                HPPartLoadRatio = 1.0;
            }
        }

        if (HeatPump.bIsIHP) // mark the water heating call, if existing
        {
            if (IntegratedHeatPumps(HeatPump.DXCoilNum).CheckWHCall) {
                if (1 == HeatPump.Mode)
                    IntegratedHeatPumps(HeatPump.DXCoilNum).IsWHCallAvail = true;
                else
                    IntegratedHeatPumps(HeatPump.DXCoilNum).IsWHCallAvail = false;
            }
        }

        // If the HPWH was in heating mode during the last timestep or if it was determined that
        // heating would be needed during this timestep to maintain setpoint, do the heating calculation.
        if (HeatPump.Mode == HeatMode) {

            // set up air flow on DX coil inlet node
            Node(DXCoilAirInletNode).MassFlowRate = MdotAir * HPPartLoadRatio;

            // set the condenser inlet node mass flow rate prior to calling the CalcHPWHDXCoil
            Node(HPWaterInletNode).MassFlowRate = MdotWater * HPPartLoadRatio;
            Tank.SourceMassFlowRate = MdotWater * HPPartLoadRatio;

            // Do the coil and tank calculations at full PLR to see if it overshoots setpoint.
            if (MaxSpeedNum > 0) { // lowest speed of VS HPWH coil
                SpeedRatio = 1.0;
                HPPartLoadRatio = 1.0;
                bIterSpeed = true; // prepare for iterating between speed levels
                SpeedNum = 1;
                SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

                if (HeatPump.bIsIHP) {
                    bIterSpeed = false; // don't iterate speed unless match conditions below
                    IHPMode = GetCurWorkMode(HeatPump.DXCoilNum);

                    if (IntegratedHeatPumps(HeatPump.DXCoilNum).CheckWHCall) {
                        if (IHPOperationMode::DWHMode == IHPMode) {
                            VSCoilNum = IntegratedHeatPumps(HeatPump.DXCoilNum).DWHCoilIndex;
                            IntegratedHeatPumps(HeatPump.DXCoilNum).CurMode = IHPOperationMode::DWHMode;
                        } else {
                            VSCoilNum = IntegratedHeatPumps(HeatPump.DXCoilNum).SCWHCoilIndex;
                            IntegratedHeatPumps(HeatPump.DXCoilNum).CurMode = IHPOperationMode::SCWHMatchWHMode;
                        }

                        SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

                        SimVariableSpeedCoils(
                            "", VSCoilNum, CycFanCycCoil, EMP1, EMP2, EMP3, 1, HPPartLoadRatio, SpeedNum, SpeedRatio, 0.0, 0.0, 1.0);

                        IntegratedHeatPumps(HeatPump.DXCoilNum).CurMode = IHPMode;
                        SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                    } else {
                        SpeedNum = GetLowSpeedNumIHP(HeatPump.DXCoilNum);

                        SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                        SimIHP(HeatPump.DXCoilName,
                               HeatPump.DXCoilNum,
                               CycFanCycCoil,
                               EMP1,
                               EMP2,
                               EMP3,
                               1,
                               HPPartLoadRatio,
                               SpeedNum,
                               SpeedRatio,
                               0.0,
                               0.0,
                               true,
                               false,
                               1.0);

                        if ((IHPOperationMode::SCWHMatchWHMode == IHPMode) || (IHPOperationMode::DWHMode == IHPMode)) {
                            bIterSpeed = true;
                        } else {
                            Tank.SourceMassFlowRate = IntegratedHeatPumps(HeatPump.DXCoilNum).TankSourceWaterMassFlowRate;
                            MdotWater = Tank.SourceMassFlowRate;
                        }

                        if (IHPOperationMode::SHDWHElecHeatOffMode == IHPMode) // turn off heater element
                        {
                            Tank.MaxCapacity = 0.0;
                            Tank.MinCapacity = 0.0;
                        };
                    }
                } else {
                    SimVariableSpeedCoils(HeatPump.DXCoilName,
                                          HeatPump.DXCoilNum,
                                          CycFanCycCoil,
                                          EMP1,
                                          EMP2,
                                          EMP3,
                                          1,
                                          HPPartLoadRatio,
                                          SpeedNum,
                                          SpeedRatio,
                                          0.0,
                                          0.0,
                                          1.0);
                }

                CalcWaterThermalTank(WaterThermalTankNum);
            } else {
                ConvergeSingleSpeedHPWHCoilAndTank(WaterThermalTankNum, HPPartLoadRatio);
            }

            NewTankTemp = GetHPWHSensedTankTemp(Tank);
            LowSpeedTankTemp = NewTankTemp;
            HPWHCondInletNodeLast = Node(HPWaterInletNode).Temp;

            if (NewTankTemp > SetPointTemp) {
                HeatPump.Mode = FloatMode;
                Par(1) = SetPointTemp;
                Par(2) = HeatPump.SaveWHMode;
                Par(3) = WaterThermalTankNum;
                if (FirstHVACIteration) {
                    Par(4) = 1.0;
                } else {
                    Par(4) = 0.0;
                }
                Par(5) = MdotWater;

                if (MaxSpeedNum > 0) {
                    // square the solving, and avoid warning
                    // due to very small capacity at lowest speed of VSHPWH coil
                    if (bIterSpeed)
                        zeroResidual = PLRResidualHPWH(0.0, Par);
                    else
                        zeroResidual = -1.0;
                }

                if (zeroResidual > 0.0) { // then iteration
                    SolveRoot(Acc, MaxIte, SolFla, HPPartLoadRatio, PLRResidualHPWH, 0.0, 1.0, Par);
                    if (SolFla == -1) {
                        gio::write(IterNum, fmtLD) << MaxIte;
                        strip(IterNum);
                        if (!WarmupFlag) {
                            ++HeatPump.IterLimitExceededNum2;
                            if (HeatPump.IterLimitExceededNum2 == 1) {
                                ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
                                ShowContinueError(
                                    "Iteration limit exceeded calculating heat pump water heater compressor part-load ratio, maximum iterations = " +
                                    IterNum + ". Part-load ratio returned = " + RoundSigDigits(HPPartLoadRatio, 3));
                                ShowContinueErrorTimeStamp("This error occurred in float mode.");
                            } else {
                                ShowRecurringWarningErrorAtEnd(
                                    HeatPump.Type + " \"" + HeatPump.Name +
                                        "\":  Iteration limit exceeded in float mode warning continues. Part-load ratio statistics follow.",
                                    HeatPump.IterLimitErrIndex2,
                                    HPPartLoadRatio,
                                    HPPartLoadRatio);
                            }
                        }
                    } else if (SolFla == -2) {
                        HPPartLoadRatio = max(0.0, min(1.0, (SetPointTemp - TankTemp) / (NewTankTemp - TankTemp)));
                        if (!WarmupFlag) {
                            ++HeatPump.RegulaFalsiFailedNum2;
                            if (HeatPump.RegulaFalsiFailedNum2 == 1) {
                                ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
                                ShowContinueError("Heat pump water heater compressor part-load ratio calculation failed: PLR limits of 0 to 1 "
                                                  "exceeded. Part-load ratio used = " +
                                                  RoundSigDigits(HPPartLoadRatio, 3));
                                ShowContinueError("Please send this information to the EnergyPlus support group.");
                                ShowContinueErrorTimeStamp("This error occurred in float mode.");
                            } else {
                                ShowRecurringWarningErrorAtEnd(
                                    HeatPump.Type + " \"" + HeatPump.Name +
                                        "\": Part-load ratio calculation failed in float mode warning continues. Part-load ratio statistics follow.",
                                    HeatPump.RegulaFalsiFailedIndex2,
                                    HPPartLoadRatio,
                                    HPPartLoadRatio);
                            }
                        }
                    }
                } else {
                    HPPartLoadRatio = 0.0;
                };

                // Re-calculate the HPWH Coil to get the correct heat transfer rate.
                Node(HPWaterInletNode).Temp = Tank.SourceOutletTemp;
                if (MaxSpeedNum > 0) {
                    SpeedRatio = 1.0;
                    SpeedNum = 1;

                    SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

                    if (HeatPump.bIsIHP) {
                        if (bIterSpeed) {
                            SimIHP(HeatPump.DXCoilName,
                                   HeatPump.DXCoilNum,
                                   CycFanCycCoil,
                                   EMP1,
                                   EMP2,
                                   EMP3,
                                   1,
                                   HPPartLoadRatio,
                                   SpeedNum,
                                   SpeedRatio,
                                   0.0,
                                   0.0,
                                   true,
                                   false,
                                   1.0);
                        }
                    } else {
                        SimVariableSpeedCoils(HeatPump.DXCoilName,
                                              HeatPump.DXCoilNum,
                                              CycFanCycCoil,
                                              EMP1,
                                              EMP2,
                                              EMP3,
                                              1,
                                              HPPartLoadRatio,
                                              SpeedNum,
                                              SpeedRatio,
                                              0.0,
                                              0.0,
                                              1.0);
                    }

                    bIterSpeed = false;

                } else {
                    CalcHPWHDXCoil(HeatPump.DXCoilNum, HPPartLoadRatio);
                }
            } else if (bIterSpeed) {
                for (loopIter = 1; loopIter <= 4; ++loopIter) {
                    HeatPump.Mode = HeatMode; // HeatMode is important for system convergence
                    HPPartLoadRatio = 1.0;
                    SpeedRatio = 1.0;
                    LowSpeedNum = 2;

                    if (HeatPump.bIsIHP) {
                        LowSpeedNum = GetLowSpeedNumIHP(HeatPump.DXCoilNum);
                        MaxSpeedNum = GetMaxSpeedNumIHP(HeatPump.DXCoilNum);
                    }

                    for (i = LowSpeedNum; i <= MaxSpeedNum; ++i) {
                        SpeedNum = i;
                        SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);
                        if (HeatPump.bIsIHP) {
                            SimIHP(HeatPump.DXCoilName,
                                   HeatPump.DXCoilNum,
                                   CycFanCycCoil,
                                   EMP1,
                                   EMP2,
                                   EMP3,
                                   1,
                                   HPPartLoadRatio,
                                   SpeedNum,
                                   SpeedRatio,
                                   0.0,
                                   0.0,
                                   true,
                                   false,
                                   1.0);
                        } else {
                            SimVariableSpeedCoils(HeatPump.DXCoilName,
                                                  HeatPump.DXCoilNum,
                                                  CycFanCycCoil,
                                                  EMP1,
                                                  EMP2,
                                                  EMP3,
                                                  1,
                                                  HPPartLoadRatio,
                                                  SpeedNum,
                                                  SpeedRatio,
                                                  0.0,
                                                  0.0,
                                                  1.0);
                        }

                        CondenserDeltaT = Node(HPWaterOutletNode).Temp - Node(HPWaterInletNode).Temp;

                        //           move the full load outlet temperature rate to the water heater structure variables
                        //           (water heaters source inlet node temperature/mdot are set in Init, set it here after CalcHPWHDXCoil has been
                        //           called)
                        WaterThermalTank(WaterThermalTankNum).SourceInletTemp = Node(HPWaterInletNode).Temp + CondenserDeltaT;
                        //				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = MdotWater;

                        //           this CALL does not update node temps, must use WaterThermalTank variables
                        // select tank type
                        {
                            auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                            if (SELECT_CASE_var1 == MixedWaterHeater) {
                                CalcWaterThermalTankMixed(WaterThermalTankNum);
                                NewTankTemp = Tank.TankTemp;
                            } else if (SELECT_CASE_var1 == StratifiedWaterHeater) {
                                CalcWaterThermalTankStratified(WaterThermalTankNum);
                                NewTankTemp = FindStratifiedTankSensedTemp(Tank);
                            }
                        }

                        if (NewTankTemp > SetPointTemp) {
                            SpeedNum = i;
                            break;
                        } else {
                            LowSpeedTankTemp = NewTankTemp;
                        }
                    }

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

                        SolveRoot(Acc, MaxIte, SolFla, SpeedRatio, PLRResidualIterSpeed, 1.0e-10, 1.0, ParVS);

                        if (SolFla == -1) {
                            gio::write(IterNum, fmtLD) << MaxIte;
                            strip(IterNum);
                            if (!WarmupFlag) {
                                ++HeatPump.IterLimitExceededNum1;
                                if (HeatPump.IterLimitExceededNum1 == 1) {
                                    ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
                                    ShowContinueError("Iteration limit exceeded calculating heat pump water heater speed"
                                                      " speed ratio ratio, maximum iterations = " +
                                                      IterNum + ". speed ratio returned = " + RoundSigDigits(SpeedRatio, 3));
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
                            if (!WarmupFlag) {
                                ++HeatPump.RegulaFalsiFailedNum1;
                                if (HeatPump.RegulaFalsiFailedNum1 == 1) {
                                    ShowWarningError(HeatPump.Type + " \"" + HeatPump.Name + "\"");
                                    ShowContinueError("Heat pump water heater speed ratio calculation failed: speed ratio limits "
                                                      "of 0 to 1 exceeded. speed ratio used = " +
                                                      RoundSigDigits(SpeedRatio, 3));
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

                    HPPartLoadRatio = 1.0;
                    SetVSHPWHFlowRates(WaterThermalTankNum, Tank.HeatPumpNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

                    if (HeatPump.bIsIHP) {
                        SimIHP(HeatPump.DXCoilName,
                               HeatPump.DXCoilNum,
                               CycFanCycCoil,
                               EMP1,
                               EMP2,
                               EMP3,
                               1,
                               HPPartLoadRatio,
                               SpeedNum,
                               SpeedRatio,
                               0.0,
                               0.0,
                               true,
                               false,
                               1.0);
                    } else {
                        SimVariableSpeedCoils(HeatPump.DXCoilName,
                                              HeatPump.DXCoilNum,
                                              CycFanCycCoil,
                                              EMP1,
                                              EMP2,
                                              EMP3,
                                              1,
                                              HPPartLoadRatio,
                                              SpeedNum,
                                              SpeedRatio,
                                              0.0,
                                              0.0,
                                              1.0);
                    }

                    CondenserDeltaT = Node(HPWaterOutletNode).Temp - Node(HPWaterInletNode).Temp;

                    //           move the full load outlet temperature rate to the water heater structure variables
                    //           (water heaters source inlet node temperature/mdot are set in Init, set it here after CalcHPWHDXCoil has been called)
                    WaterThermalTank(WaterThermalTankNum).SourceInletTemp = Node(HPWaterInletNode).Temp + CondenserDeltaT;
                    //				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = MdotWater;

                    //           this CALL does not update node temps, must use WaterThermalTank variables
                    // select tank type
                    {
                        auto const SELECT_CASE_var1(HeatPump.TankTypeNum);
                        if (SELECT_CASE_var1 == MixedWaterHeater) {
                            CalcWaterThermalTankMixed(WaterThermalTankNum);
                            NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                        } else if (SELECT_CASE_var1 == StratifiedWaterHeater) {
                            CalcWaterThermalTankStratified(WaterThermalTankNum);
                            NewTankTemp = FindStratifiedTankSensedTemp(WaterThermalTank(WaterThermalTankNum));
                        }
                    }
                    // update inlet temp
                    Node(HPWaterInletNode).Temp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;
                    if (std::abs(Node(HPWaterInletNode).Temp - HPWHCondInletNodeLast) < SmallTempDiff) break;
                    HPWHCondInletNodeLast = Node(HPWaterInletNode).Temp;
                }

            } else {
                // Set the PLR to 1 if we're not going to reach setpoint during this timestep.
                HPPartLoadRatio = 1.0;
            }
        }

        if (HeatPump.bIsIHP) {
            if (IntegratedHeatPumps(HeatPump.DXCoilNum).CheckWHCall) {
                ClearCoils(HeatPump.DXCoilNum); // clear node info when checking the heating load
            }
        }

        // set air-side mass flow rate for final calculation
        if (InletAirMixerNode > 0) {
            Node(InletAirMixerNode).MassFlowRate = MdotAir * HPPartLoadRatio;
            Node(HPAirInletNode).MassFlowRate = MdotAir * HPPartLoadRatio * (1.0 - MixerInletAirSchedule);
            Node(OutdoorAirNode).MassFlowRate = MdotAir * HPPartLoadRatio * MixerInletAirSchedule;
            //   IF HPWH is off, pass zone node conditions through HPWH air-side
            if (HPPartLoadRatio == 0) Node(InletAirMixerNode) = Node(HPAirInletNode);
        } else {
            if (OutdoorAirNode == 0) {
                Node(HPAirInletNode).MassFlowRate = MdotAir * HPPartLoadRatio;
            } else {
                Node(OutdoorAirNode).MassFlowRate = MdotAir * HPPartLoadRatio;
            }
        }
        if (HPPartLoadRatio == 0) Tank.SourceInletTemp = Tank.SourceOutletTemp;

        // set water-side mass flow rate for final calculation
        Node(HPWaterInletNode).MassFlowRate = MdotWater * HPPartLoadRatio;

        if (MaxSpeedNum > 0) {

            // it is important to use MdotAir to reset the notes, otherwise, could fail to converge
            if (InletAirMixerNode > 0) {
                Node(InletAirMixerNode).MassFlowRateMax = MdotAir;
                Node(InletAirMixerNode).MassFlowRateMaxAvail = MdotAir;
            } else {
                if (OutdoorAirNode == 0) {
                    Node(HPAirInletNode).MassFlowRateMax = MdotAir;
                    Node(HPAirInletNode).MassFlowRateMaxAvail = MdotAir;
                } else {
                    Node(OutdoorAirNode).MassFlowRateMax = MdotAir;
                    Node(OutdoorAirNode).MassFlowRateMaxAvail = MdotAir;
                }
            }

            //   set the max mass flow rate for outdoor fans
            Node(HeatPump.FanOutletNode).MassFlowRateMax = MdotAir;

            if (HeatPump.bIsIHP) {
                // pass node information using resulting PLR
                if (HeatPump.FanPlacement == BlowThru) {
                    //   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    SimIHP(HeatPump.DXCoilName,
                           HeatPump.DXCoilNum,
                           CycFanCycCoil,
                           EMP1,
                           EMP2,
                           EMP3,
                           CompOp,
                           HPPartLoadRatio,
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
                    SimIHP(HeatPump.DXCoilName,
                           HeatPump.DXCoilNum,
                           CycFanCycCoil,
                           EMP1,
                           EMP2,
                           EMP3,
                           CompOp,
                           HPPartLoadRatio,
                           SpeedNum,
                           SpeedRatio,
                           0.0,
                           0.0,
                           true,
                           false,
                           1.0);
                } else {
                    //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
                    SimIHP(HeatPump.DXCoilName,
                           HeatPump.DXCoilNum,
                           CycFanCycCoil,
                           EMP1,
                           EMP2,
                           EMP3,
                           CompOp,
                           HPPartLoadRatio,
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
                    SimIHP(HeatPump.DXCoilName,
                           HeatPump.DXCoilNum,
                           CycFanCycCoil,
                           EMP1,
                           EMP2,
                           EMP3,
                           CompOp,
                           HPPartLoadRatio,
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
                if (HeatPump.FanPlacement == BlowThru) {
                    //   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
                    if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                    } else {
                        Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                    }
                    SimVariableSpeedCoils(HeatPump.DXCoilName,
                                          HeatPump.DXCoilNum,
                                          CycFanCycCoil,
                                          EMP1,
                                          EMP2,
                                          EMP3,
                                          CompOp,
                                          HPPartLoadRatio,
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
                    SimVariableSpeedCoils(HeatPump.DXCoilName,
                                          HeatPump.DXCoilNum,
                                          CycFanCycCoil,
                                          EMP1,
                                          EMP2,
                                          EMP3,
                                          CompOp,
                                          HPPartLoadRatio,
                                          SpeedNum,
                                          SpeedRatio,
                                          0.0,
                                          0.0,
                                          1.0);
                } else {
                    //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
                    SimVariableSpeedCoils(HeatPump.DXCoilName,
                                          HeatPump.DXCoilNum,
                                          CycFanCycCoil,
                                          EMP1,
                                          EMP2,
                                          EMP3,
                                          CompOp,
                                          HPPartLoadRatio,
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
                    SimVariableSpeedCoils(HeatPump.DXCoilName,
                                          HeatPump.DXCoilNum,
                                          CycFanCycCoil,
                                          EMP1,
                                          EMP2,
                                          EMP3,
                                          CompOp,
                                          HPPartLoadRatio,
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
            if (HeatPump.FanPlacement == BlowThru) {
                //   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                } else {
                    Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                } else {
                    Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
            } else {
                //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
                SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                } else {
                    Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
                SimDXCoil(HeatPump.DXCoilName, CompOp, FirstHVACIteration, HeatPump.DXCoilNum, CycFanCycCoil, HPPartLoadRatio);
                if (HeatPump.FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[HeatPump.FanNum]->simulate(_, _, _, _);
                } else {
                    Fans::SimulateFanComponents(HeatPump.FanName, FirstHVACIteration, HeatPump.FanNum);
                }
            }
        }

        // Call the tank one more time with the final PLR
        if (HeatPump.TankTypeNum == MixedWaterHeater) {
            CalcWaterThermalTankMixed(WaterThermalTankNum);
        } else if (HeatPump.TankTypeNum == StratifiedWaterHeater) {
            CalcWaterThermalTankStratified(WaterThermalTankNum);
        } else {
            assert(0);
        }

        // set HPWH outlet node equal to the outlet air splitter node conditions if outlet air splitter node exists
        if (OutletAirSplitterNode > 0) {
            Node(HPAirOutletNode) = Node(OutletAirSplitterNode);
            Node(ExhaustAirNode) = Node(OutletAirSplitterNode);
        }

        // Check schedule to divert air-side cooling to outdoors.
        if (HeatPump.OutletAirSplitterSchPtr > 0) {
            OutletAirSplitterSch = GetCurrentScheduleValue(HeatPump.OutletAirSplitterSchPtr);
            Node(HPAirOutletNode).MassFlowRate = MdotAir * HPPartLoadRatio * (1.0 - OutletAirSplitterSch);
            Node(ExhaustAirNode).MassFlowRate = MdotAir * HPPartLoadRatio * OutletAirSplitterSch;
        }

        HeatPump.HeatingPLR = HPPartLoadRatio;
        HeatPump.OnCycParaFuelRate = HeatPump.OnCycParaLoad * HPPartLoadRatio;
        HeatPump.OnCycParaFuelEnergy = HeatPump.OnCycParaFuelRate * TimeStepSys * SecInHour;
        HeatPump.OffCycParaFuelRate = HeatPump.OffCycParaLoad * (1.0 - HPPartLoadRatio);
        HeatPump.OffCycParaFuelEnergy = HeatPump.OffCycParaFuelRate * TimeStepSys * SecInHour;
        if (HeatPump.TankTypeNum == MixedWaterHeater) {
            HeatPump.ControlTempAvg = Tank.TankTempAvg;
            HeatPump.ControlTempFinal = Tank.TankTemp;
        } else if (HeatPump.TankTypeNum == StratifiedWaterHeater) {
            HeatPump.ControlTempAvg = FindStratifiedTankSensedTemp(Tank, true);
            HeatPump.ControlTempFinal = FindStratifiedTankSensedTemp(Tank);
        } else {
            assert(0);
        }

        {
            auto const SELECT_CASE_var(HeatPump.InletAirConfiguration);

            //   no sensible capacity to zone for outdoor and scheduled HPWH
            if (SELECT_CASE_var == AmbientTempOutsideAir) {
                HeatPump.HPWaterHeaterSensibleCapacity = 0.0;
                HeatPump.HPWaterHeaterLatentCapacity = 0.0;

            } else if (SELECT_CASE_var == AmbientTempSchedule) {
                HeatPump.HPWaterHeaterSensibleCapacity = 0.0;
                HeatPump.HPWaterHeaterLatentCapacity = 0.0;

                //   calculate sensible capacity to zone for inlet air configuration equals Zone Only or Zone And Outdoor Air configurations
            } else {
                CpAir = PsyCpAirFnWTdb(Node(HPAirInletNode).HumRat, Node(HPAirInletNode).Temp);

                //     add parasitics to zone heat balance if parasitic heat load is to zone otherwise neglect parasitics
                if (HeatPump.ParasiticTempIndicator == AmbientTempZone) {
                    HeatPump.HPWaterHeaterSensibleCapacity =
                        (Node(HPAirOutletNode).MassFlowRate * CpAir * (Node(HPAirOutletNode).Temp - Node(HPAirInletNode).Temp)) +
                        HeatPump.OnCycParaFuelRate + HeatPump.OffCycParaFuelRate;
                } else {
                    HeatPump.HPWaterHeaterSensibleCapacity =
                        Node(HPAirOutletNode).MassFlowRate * CpAir * (Node(HPAirOutletNode).Temp - Node(HPAirInletNode).Temp);
                }

                HeatPump.HPWaterHeaterLatentCapacity =
                    Node(HPAirOutletNode).MassFlowRate * (Node(HPAirOutletNode).HumRat - Node(HPAirInletNode).HumRat);
            }
        }
    }

    void CalcWaterThermalTank(int const WaterThermalTankNum)
    {
        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum);
        if (Tank.TypeNum == MixedWaterHeater) {
            CalcWaterThermalTankMixed(WaterThermalTankNum);
        } else if (Tank.TypeNum == StratifiedWaterHeater) {
            CalcWaterThermalTankStratified(WaterThermalTankNum);
        } else {
            assert(false);
        }
    }

    Real64 GetHPWHSensedTankTemp(WaterThermalTankData const &Tank)
    {
        Real64 TankTemperature;
        if (Tank.TypeNum == MixedWaterHeater) {
            TankTemperature = Tank.TankTemp;
        } else {
            assert(Tank.TypeNum == StratifiedWaterHeater);
            TankTemperature = FindStratifiedTankSensedTemp(Tank);
        }
        return TankTemperature;
    }

    void ConvergeSingleSpeedHPWHCoilAndTank(int const WaterThermalTankNum, // Index of WaterThermalTank
                                            Real64 const PartLoadRatio     // Part Load Ratio of the Coil
    )
    {
        using DataHVACGlobals::SmallTempDiff;
        WaterThermalTankData &Tank = WaterThermalTank(WaterThermalTankNum);
        HeatPumpWaterHeaterData &HPWH = HPWaterHeater(Tank.HeatPumpNum);
        DXCoils::DXCoilData &Coil = DXCoils::DXCoil(HPWH.DXCoilNum);

        Real64 PrevTankTemp = Tank.SourceOutletTemp;
        for (int i = 1; i <= 10; ++i) {

            DXCoils::CalcHPWHDXCoil(HPWH.DXCoilNum, PartLoadRatio);
            Tank.SourceInletTemp = DataLoopNode::Node(HPWH.CondWaterOutletNode).Temp;

            CalcWaterThermalTank(WaterThermalTankNum);
            DataLoopNode::Node(Coil.WaterInNode).Temp = Tank.SourceOutletTemp;

            if (std::abs(Tank.SourceOutletTemp - PrevTankTemp) < SmallTempDiff) {
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

        // REFERENCES:

        // USE STATEMENTS:

        // Using/Aliasing
        using DataLoopNode::Node;
        using IntegratedHeatPump::GetAirMassFlowRateIHP;
        using IntegratedHeatPump::GetAirVolFlowRateIHP;
        using IntegratedHeatPump::GetWaterVolFlowRateIHP;
        using IntegratedHeatPump::IntegratedHeatPumps;

        int DXCoilAirInletNode; // Inlet air node number of DX coil
        int HPWaterInletNode;   // HP condenser water inlet node number
        int SpeedLow;           // lower speed level
        int FanInNode(0);
        double MdotAirSav(0);

        SpeedLow = SpeedNum - 1;
        if (SpeedLow < 1) SpeedLow = 1;

        HPWaterInletNode = HPWaterHeater(HPNum).CondWaterInletNode;
        DXCoilAirInletNode = HPWaterHeater(HPNum).DXCoilAirInletNode;
        if (HPWaterHeater(HPNum).bIsIHP) {
            HPWaterHeater(HPNum).OperatingWaterFlowRate = GetWaterVolFlowRateIHP(HPWaterHeater(HPNum).DXCoilNum, SpeedNum, SpeedRatio, true);
            MdotAir = GetAirMassFlowRateIHP(HPWaterHeater(HPNum).DXCoilNum, SpeedNum, SpeedRatio, true);
            HPWaterHeater(HPNum).OperatingAirFlowRate = GetAirVolFlowRateIHP(HPWaterHeater(HPNum).DXCoilNum, SpeedNum, SpeedRatio, true);
            Node(DXCoilAirInletNode).MassFlowRate = MdotAir;
            Node(DXCoilAirInletNode).MassFlowRateMaxAvail = MdotAir;
            Node(DXCoilAirInletNode).MassFlowRateMax = MdotAir;
        } else {
            HPWaterHeater(HPNum).OperatingWaterFlowRate = HPWaterHeater(HPNum).HPWHWaterVolFlowRate(SpeedNum) * SpeedRatio +
                                                          HPWaterHeater(HPNum).HPWHWaterVolFlowRate(SpeedLow) * (1.0 - SpeedRatio);
            HPWaterHeater(HPNum).OperatingAirFlowRate = HPWaterHeater(HPNum).HPWHAirVolFlowRate(SpeedNum) * SpeedRatio +
                                                        HPWaterHeater(HPNum).HPWHAirVolFlowRate(SpeedLow) * (1.0 - SpeedRatio);
            MdotAir = HPWaterHeater(HPNum).HPWHAirMassFlowRate(SpeedNum) * SpeedRatio +
                      HPWaterHeater(HPNum).HPWHAirMassFlowRate(SpeedLow) * (1.0 - SpeedRatio);
        }

        MdotWater = HPWaterHeater(HPNum).OperatingWaterFlowRate * WaterDens;
        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = MdotWater;

        Node(DXCoilAirInletNode).MassFlowRate = MdotAir;
        Node(HPWaterInletNode).MassFlowRate = MdotWater;
        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = MdotWater;

        if (HPWaterHeater(HPNum).InletAirMixerNode > 0) {
            Node(HPWaterHeater(HPNum).InletAirMixerNode).MassFlowRate = MdotAir;
            Node(HPWaterHeater(HPNum).InletAirMixerNode).MassFlowRateMaxAvail = MdotAir;
        } else {
            if (HPWaterHeater(HPNum).OutsideAirNode == 0) {
                Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).MassFlowRate = MdotAir;
                Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).MassFlowRateMaxAvail = MdotAir;
            } else {
                Node(HPWaterHeater(HPNum).OutsideAirNode).MassFlowRate = MdotAir;
                Node(HPWaterHeater(HPNum).OutsideAirNode).MassFlowRateMaxAvail = MdotAir;
            }
        }

        // put fan component first, regardless placement, to calculate fan power
        if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            FanInNode = HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->inletNodeNum;
        } else {
            FanInNode = Fans::Fan(HPWaterHeater(HPNum).FanNum).InletNodeNum;
        }

        Node(FanInNode).MassFlowRate = MdotAir;
        Node(FanInNode).MassFlowRateMaxAvail = MdotAir;
        Node(FanInNode).MassFlowRateMax = MdotAir;
        if (!(HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject)) {
            Fans::Fan(HPWaterHeater(HPNum).FanNum).MassFlowRateMaxAvail = MdotAir;
        } // system fan will use the inlet node max avail.

        MdotAirSav = MdotAir;
        if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
        } else {
            Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, FirstHVACIteration, HPWaterHeater(HPNum).FanNum);
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
        //  and calculates the residual as defined respectively for MixedWaterHeater or StratifiedWaterHeater

        // REFERENCES:

        // USE STATEMENTS:

        // Using/Aliasing
        using DataHVACGlobals::CycFanCycCoil;
        using DataLoopNode::Node;
        using IntegratedHeatPump::SimIHP;
        using VariableSpeedCoils::SimVariableSpeedCoils;

        int WaterThermalTankNum; // index of water heater
        Real64 NewTankTemp(0);   // resulting tank temperature [C]
        int SpeedNum;
        int HPNum;
        Real64 MdotWater(0);
        Real64 RhoWater;
        int HPWaterInletNode;
        int HPWaterOutletNode;
        Real64 CondenserDeltaT;
        Real64 PLRResidualIterSpeed;
        bool FirstHVACIteration;                // FirstHVACIteration flag
        Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); // place holder to calling variable-speed coil function

        WaterThermalTankNum = int(Par(1));
        HPNum = int(Par(2));
        SpeedNum = int(Par(3));
        HPWaterInletNode = int(Par(4));
        HPWaterOutletNode = int(Par(5));
        RhoWater = Par(6);
        WaterThermalTank(WaterThermalTankNum).Mode = int(Par(8));
        // FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
        FirstHVACIteration = (Par(9) == 1.0);

        HPPartLoadRatio = 1.0;
        SetVSHPWHFlowRates(WaterThermalTankNum, HPNum, SpeedNum, SpeedRatio, RhoWater, MdotWater, FirstHVACIteration);

        if (HPWaterHeater(HPNum).bIsIHP) {
            SimIHP(HPWaterHeater(HPNum).DXCoilName,
                   HPWaterHeater(HPNum).DXCoilNum,
                   CycFanCycCoil,
                   EMP1,
                   EMP2,
                   EMP3,
                   1,
                   HPPartLoadRatio,
                   SpeedNum,
                   SpeedRatio,
                   0.0,
                   0.0,
                   true,
                   false,
                   1.0);
        } else {
            SimVariableSpeedCoils(HPWaterHeater(HPNum).DXCoilName,
                                  HPWaterHeater(HPNum).DXCoilNum,
                                  CycFanCycCoil,
                                  EMP1,
                                  EMP2,
                                  EMP3,
                                  1,
                                  HPPartLoadRatio,
                                  SpeedNum,
                                  SpeedRatio,
                                  0.0,
                                  0.0,
                                  1.0);
        }

        CondenserDeltaT = Node(HPWaterOutletNode).Temp - Node(HPWaterInletNode).Temp;

        //           move the full load outlet temperature rate to the water heater structure variables
        //           (water heaters source inlet node temperature/mdot are set in Init, set it here after CalcHPWHDXCoil has been called)
        WaterThermalTank(WaterThermalTankNum).SourceInletTemp = Node(HPWaterInletNode).Temp + CondenserDeltaT;
        //				WaterThermalTank( WaterThermalTankNum ).SourceMassFlowRate = MdotWater;

        //           this CALL does not update node temps, must use WaterThermalTank variables
        // select tank type
        {
            auto const SELECT_CASE_var1(HPWaterHeater(HPNum).TankTypeNum);
            if (SELECT_CASE_var1 == MixedWaterHeater) {
                CalcWaterThermalTankMixed(WaterThermalTankNum);
                NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
            } else if (SELECT_CASE_var1 == StratifiedWaterHeater) {
                CalcWaterThermalTankStratified(WaterThermalTankNum);
                NewTankTemp = FindStratifiedTankSensedTemp(WaterThermalTank(WaterThermalTankNum));
            }
        }

        PLRResidualIterSpeed = Par(7) - NewTankTemp;
        return PLRResidualIterSpeed;
    }

    Real64 PLRResidualMixedTank(Real64 const HPPartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                Array1<Real64> const &Par     // par(1) = HP set point temperature [C]
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   May 2005
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function (desired tank temp - actual tank temp)
        //  HP water heater output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        //  Calls CalcWaterThermalTankMixed to get tank temperature at the given part load ratio (source water mass flow rate)
        //  and calculates the residual as defined above

        // Return value
        Real64 PLRResidualMixedTank;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // par(2) = tank mode
        // par(3) = water heater num
        // par(4) = FirstHVACIteration
        // par(5) = MdotWater

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WaterThermalTankNum; // index of water heater
        Real64 NewTankTemp;      // resulting tank temperature [C]
        bool FirstHVACIteration; // FirstHVACIteration flag

        WaterThermalTankNum = int(Par(3));
        WaterThermalTank(WaterThermalTankNum).Mode = int(Par(2));
        WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = Par(5) * HPPartLoadRatio;
        // FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
        FirstHVACIteration = (Par(4) == 1.0);
        CalcWaterThermalTankMixed(WaterThermalTankNum);
        NewTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
        PLRResidualMixedTank = Par(1) - NewTankTemp;
        return PLRResidualMixedTank;
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
        HeatPumpWaterHeaterData &HPWH = HPWaterHeater(Tank.HeatPumpNum);
        bool const isVariableSpeed = (HPWH.NumofSpeed > 0);
        Tank.Mode = int(Par(2));
        // Apply the PLR
        if (Tank.TypeNum == MixedWaterHeater) {
            // For a mixed tank, the PLR is applied to the source mass flow rate.
            Tank.SourceMassFlowRate = Par(5) * HPPartLoadRatio;
            CalcWaterThermalTankMixed(WaterThermalTankNum);
        } else {
            assert(Tank.TypeNum == StratifiedWaterHeater);
            // For a stratified tank, the PLR is applied to the Coil.TotalHeatingEnergyRate
            // whether that's a VarSpeedCoil or DXCoil.
            // Here we create a pointer to the TotalHeatingEnergyRate for the appropriate coil type.
            Real64 *CoilTotalHeatingEnergyRatePtr;
            if (isVariableSpeed) {
                if (HPWH.bIsIHP)
                    CoilTotalHeatingEnergyRatePtr = &IntegratedHeatPump::IntegratedHeatPumps(HPWH.DXCoilNum).TotalWaterHeatingRate;
                else
                    CoilTotalHeatingEnergyRatePtr = &VariableSpeedCoils::VarSpeedCoil(HPWH.DXCoilNum).TotalHeatingEnergyRate;
            } else {
                CoilTotalHeatingEnergyRatePtr = &DXCoils::DXCoil(HPWH.DXCoilNum).TotalHeatingEnergyRate;
            }
            // Copy the value of the total heating energy rate
            Real64 const CoilTotalHeatingEnergyRateBackup = *CoilTotalHeatingEnergyRatePtr;
            // Apply the PLR
            *CoilTotalHeatingEnergyRatePtr *= HPPartLoadRatio;
            // Tank Calculation
            CalcWaterThermalTankStratified(WaterThermalTankNum);
            // Restore the original value
            *CoilTotalHeatingEnergyRatePtr = CoilTotalHeatingEnergyRateBackup;
        }
        Real64 const NewTankTemp = GetHPWHSensedTankTemp(Tank);
        Real64 const PLRResidualHPWH = Par(1) - NewTankTemp;
        return PLRResidualHPWH;
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

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataLoopNode::Node;
        using namespace DataBranchAirLoopPlant;
        using ScheduleManager::GetCurrentScheduleValue;

        // Return value
        Real64 PlantMassFlowRatesFunc;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        int const PassingFlowThru(1);
        int const MaybeRequestingFlow(2);
        int const ThrottlingFlow(3);

        // FUNCTION BLOCK SPECIFICATIONS:
        // na

        // FUNCTION TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CurrentMode;
        Real64 MassFlowRequest(0.0);
        bool NeedsHeat;
        bool NeedsCool;
        Real64 FlowResult(0.0);
        bool ScheduledAvail;
        Real64 AltSetpointTemp;
        Real64 AltDeadBandTemp;

        NeedsHeat = false; // init
        NeedsCool = false; // init

        // determine current mode.  there are three possible
        //  1.  passing thru what was given to inlet node
        //  2.  potentially making a flow request
        //  3.  throttling flow in response to Plant's restrictions (MassFlowRateMaxAvail)
        // init default mode to passing thru
        CurrentMode = PassingFlowThru; // default

        if (PlantLoopSide == DemandSupply_No) {
            CurrentMode = PassingFlowThru;
        } else if (PlantLoopSide == SupplySide) {
            // If FlowLock is False (0), the tank sets the plant loop mdot
            // If FlowLock is True (1),  the new resolved plant loop mdot is used
            if (WaterThermalTank(WaterThermalTankNum).UseCurrentFlowLock == 0) {
                CurrentMode = PassingFlowThru;
                if ((WaterThermalTank(WaterThermalTankNum).UseSideLoadRequested > 0.0) && (WaterThermalTankSide == UseSide)) {
                    CurrentMode = MaybeRequestingFlow;
                }
            } else {
                CurrentMode = PassingFlowThru;
            }
            if (WaterThermalTankSide == SourceSide) {
                CurrentMode = MaybeRequestingFlow;
            }
        } else if (PlantLoopSide == DemandSide) {
            //  1.  pass thru is default
            CurrentMode = PassingFlowThru;

            //  2.  Might be Requesting Flow.
            if (FirstHVACIteration) {
                if (BranchControlType == ControlType_Bypass) {
                    CurrentMode = PassingFlowThru;
                } else {
                    // IF (.not. PlumbedInSeries) THEN
                    CurrentMode = MaybeRequestingFlow;
                    // ELSE
                    //   CurrentMode = PassingFlowThru
                    // ENDIF
                }
                // ENDIF
            } else { //(.not. FirstHVACIteration)
                if (BranchControlType == ControlType_Bypass) {
                    CurrentMode = PassingFlowThru;
                } else {
                    // 3.  ThrottlingFlow
                    //  IF (.not. PlumbedInSeries) THEN
                    CurrentMode = ThrottlingFlow;
                    //  ELSE
                    //    CurrentMode = PassingFlowThru
                    //  ENDIF
                }
            }
        }

        // evaluate Availability schedule,
        ScheduledAvail = true;
        if (WaterThermalTankSide == UseSide) {
            //    IF (WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum > 0) Then
            if (GetCurrentScheduleValue(WaterThermalTank(WaterThermalTankNum).UseSideAvailSchedNum) == 0.0) {
                ScheduledAvail = false;
            }
            //    ENDIF
        } else if (WaterThermalTankSide == SourceSide) {
            //    IF (WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum > 0) Then
            if (GetCurrentScheduleValue(WaterThermalTank(WaterThermalTankNum).SourceSideAvailSchedNum) == 0.0) {
                ScheduledAvail = false;
            }
            //    ENDIF
        }

        // now act based on current mode
        {
            auto const SELECT_CASE_var(CurrentMode);

            if (SELECT_CASE_var == PassingFlowThru) {
                if (!ScheduledAvail) {
                    FlowResult = 0.0;
                } else {
                    FlowResult = Node(InNodeNum).MassFlowRate;
                }

            } else if (SELECT_CASE_var == ThrottlingFlow) {
                // first determine what mass flow would be if it is to requested
                if (!ScheduledAvail) {
                    MassFlowRequest = 0.0;
                } else {
                    if (WaterThermalTankSide == UseSide) {
                        MassFlowRequest = WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax;
                    } else if (WaterThermalTankSide == SourceSide) {
                        MassFlowRequest = WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax;
                    } else {
                        assert(false);
                    }
                }

                // next determine if tank temperature is such that source side flow might be requested
                if (!WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                    if (OutletTemp < DeadBandTemp) {
                        NeedsHeat = true;
                    } else if ((OutletTemp >= DeadBandTemp) && (OutletTemp < SetPointTemp)) {
                        // inside the deadband, use saved mode from water heater calcs
                        if (WaterThermalTank(WaterThermalTankNum).SavedMode == HeatMode) {
                            NeedsHeat = true;
                        } else if (WaterThermalTank(WaterThermalTankNum).SavedMode == FloatMode) {
                            NeedsHeat = false;
                        }
                    } else if (OutletTemp >= SetPointTemp) {
                        NeedsHeat = false;
                    }
                } else { // is a chilled water tank so flip logic
                    if (OutletTemp > DeadBandTemp) {
                        NeedsCool = true;
                    } else if ((OutletTemp <= DeadBandTemp) && (OutletTemp > SetPointTemp)) {
                        // inside the deadband, use saved mode from water thermal tank calcs (modes only for mixed)
                        if (WaterThermalTank(WaterThermalTankNum).TypeNum == MixedChilledWaterStorage) {
                            if (WaterThermalTank(WaterThermalTankNum).SavedMode == CoolMode) {
                                NeedsCool = true;
                            } else if (WaterThermalTank(WaterThermalTankNum).SavedMode == FloatMode) {
                                NeedsCool = false;
                            }
                        } else if (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedChilledWaterStorage) {
                            NeedsCool = true;
                        }

                    } else if (OutletTemp <= SetPointTemp) {
                        NeedsCool = false;
                    }
                }

                if (MassFlowRequest > 0.0) {
                    if (WaterThermalTankSide == UseSide) {
                        FlowResult = MassFlowRequest;
                    } else if (WaterThermalTankSide == SourceSide) {
                        if (NeedsHeat || NeedsCool) {
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
                FlowResult = max(Node(InNodeNum).MassFlowRateMinAvail, FlowResult); // okay for compliance (reverse dd)
                FlowResult = max(Node(InNodeNum).MassFlowRateMin, FlowResult);      // okay for compliance (reverse dd)
                FlowResult = min(Node(InNodeNum).MassFlowRateMaxAvail, FlowResult);
                //=> following might take out of reverse dd compliance
                FlowResult = min(Node(InNodeNum).MassFlowRateMax, FlowResult);

                // DSU> use SetComponentFlowRate for above?

            } else if (SELECT_CASE_var == MaybeRequestingFlow) {

                // first determine what mass flow would be if it is to requested
                if (!ScheduledAvail) {
                    MassFlowRequest = 0.0;
                } else {
                    if (WaterThermalTankSide == UseSide) {
                        if ((WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) &&
                            (WaterThermalTank(WaterThermalTankNum).UseSideLoadRequested > 0.0)) {
                            MassFlowRequest = WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax;
                        } else if ((WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) &&
                                   (WaterThermalTank(WaterThermalTankNum).UseSideLoadRequested == 0.0)) {
                            MassFlowRequest = 0.0;
                        } else {
                            MassFlowRequest = WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax;
                        }

                    } else if (WaterThermalTankSide == SourceSide) {
                        MassFlowRequest = WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax;
                    }
                }

                if (WaterThermalTankSide == SourceSide) { // temperature dependent controls for indirect heating/cooling
                    if (!WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                        // next determine if tank temperature is such that flow is requested depending on mode
                        if (WaterThermalTank(WaterThermalTankNum).SourceSideControlMode == SourceSideIndirectHeatPrimarySetpoint) {
                            if (OutletTemp < DeadBandTemp) {
                                NeedsHeat = true;
                            } else if ((OutletTemp >= DeadBandTemp) && (OutletTemp < SetPointTemp)) {
                                // inside the deadband, use saved mode from water heater calcs
                                if (WaterThermalTank(WaterThermalTankNum).SavedMode == HeatMode) {
                                    NeedsHeat = true;
                                } else if (WaterThermalTank(WaterThermalTankNum).SavedMode == FloatMode) {
                                    NeedsHeat = false;
                                }

                            } else if (OutletTemp >= SetPointTemp) {
                                NeedsHeat = false;
                            }
                        } else if (WaterThermalTank(WaterThermalTankNum).SourceSideControlMode == SourceSideIndirectHeatAltSetpoint) {
                            // get alternate setpoint
                            AltSetpointTemp = GetCurrentScheduleValue(WaterThermalTank(WaterThermalTankNum).SourceSideAltSetpointSchedNum);
                            AltDeadBandTemp = AltSetpointTemp - WaterThermalTank(WaterThermalTankNum).DeadBandDeltaTemp;
                            if (OutletTemp < AltDeadBandTemp) {
                                NeedsHeat = true;
                            } else if ((OutletTemp >= AltDeadBandTemp) && (OutletTemp < AltSetpointTemp)) {
                                // inside the deadband, use saved mode from water heater calcs
                                if (WaterThermalTank(WaterThermalTankNum).SavedMode == HeatMode) {
                                    NeedsHeat = true;
                                } else if (WaterThermalTank(WaterThermalTankNum).SavedMode == FloatMode) {
                                    NeedsHeat = false;
                                }

                            } else if (OutletTemp >= AltSetpointTemp) {
                                NeedsHeat = false;
                            }
                        } else if (WaterThermalTank(WaterThermalTankNum).SourceSideControlMode == SourceSideStorageTank) {
                            if (OutletTemp < WaterThermalTank(WaterThermalTankNum).TankTempLimit) {
                                NeedsHeat = true;
                            } else {
                                NeedsHeat = false;
                            }
                        }
                    } else { // is a chilled water tank so flip logic
                        if (OutletTemp > DeadBandTemp) {
                            NeedsCool = true;
                        } else if ((OutletTemp <= DeadBandTemp) && (OutletTemp > SetPointTemp)) {
                            // inside the deadband, use saved mode from water thermal tank calcs (modes only for mixed)
                            if (WaterThermalTank(WaterThermalTankNum).TypeNum == MixedChilledWaterStorage) {
                                if (WaterThermalTank(WaterThermalTankNum).SavedMode == CoolMode) {
                                    NeedsCool = true;
                                } else if (WaterThermalTank(WaterThermalTankNum).SavedMode == FloatMode) {
                                    NeedsCool = false;
                                }
                            } else if (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedChilledWaterStorage) {
                                NeedsCool = true;
                            }
                        } else if (OutletTemp >= SetPointTemp) {
                            NeedsCool = false;
                        }

                    } // chilled water

                    if (MassFlowRequest > 0.0) {
                        if (NeedsHeat || NeedsCool) {
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
                //    IF (FirstHVACIteration) Then
                //      Node(InNodeNum)%MassFlowRateMaxAvail = FlowResult
                //      Node(InNodeNum)%MassFlowRateMinAvail = 0.0D0
                //    ENDIF
            }
        }

        PlantMassFlowRatesFunc = FlowResult;

        return PlantMassFlowRatesFunc;
    }

    void MinePlantStructForInfo(int const WaterThermalTankNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   October 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // get information from plant loop data structure
        // check what we can learn from plant structure against user inputs

        // METHODOLOGY EMPLOYED:
        // looping

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataSizing::AutoSize;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PlantLoopNum; // Used for looking up plant info
        int LoopSideNum;  // Used for looking up plant info
        // unused  INTEGER             :: BranchNum               ! Used for looking up plant info
        //  INTEGER             :: CompNum                 ! Used for looking up plant info
        int SplitNum;        // used for checking series parallel in plant
        int UseInletNode;    // Water heater use inlet node number
        int SourceInletNode; // Water heater source inlet node number
        bool ErrorsFound;

        ErrorsFound = false;

        // IF (WaterThermalTank(WaterThermalTankNum)%PlantStructureCheck .AND. ALLOCATED(PlantLoop)) THEN
        if (allocated(PlantLoop) && WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum > 0) {

            // check plant structure for useful data.

            UseInletNode = WaterThermalTank(WaterThermalTankNum).UseInletNode;
            PlantLoopNum = WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum;
            LoopSideNum = WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide;

            if ((WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized) &&
                (WaterThermalTank(WaterThermalTankNum).UseSidePlantSizNum == 0)) {
                ShowSevereError("Water heater = " + WaterThermalTank(WaterThermalTankNum).Name +
                                " for autosizing Use side flow rate, did not find Sizing:Plant object " + PlantLoop(PlantLoopNum).Name);
                ErrorsFound = true;
            }
            // Is this wh Use side plumbed in series (default) or are there other branches in parallel?
            if (allocated(PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter)) {
                for (SplitNum = 1; SplitNum <= PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).NumSplitters; ++SplitNum) {
                    if (any_eq(PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter(SplitNum).NodeNumOut,
                               UseInletNode)) { // this wh is on the splitter
                        if (PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter(SplitNum).TotalOutletNodes > 1) {
                            WaterThermalTank(WaterThermalTankNum).UseSideSeries = false;
                        }
                    }
                }
            }
        }

        if (allocated(PlantLoop) && WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum > 0) {
            SourceInletNode = WaterThermalTank(WaterThermalTankNum).SourceInletNode;
            PlantLoopNum = WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum;
            LoopSideNum = WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide;
            // was user's input correct for plant loop name?
            if ((WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized) &&
                (WaterThermalTank(WaterThermalTankNum).SourceSidePlantSizNum == 0) && (WaterThermalTank(WaterThermalTankNum).DesuperheaterNum == 0) &&
                (WaterThermalTank(WaterThermalTankNum).HeatPumpNum == 0)) {
                ShowSevereError("Water heater = " + WaterThermalTank(WaterThermalTankNum).Name +
                                "for autosizing Source side flow rate, did not find Sizing:Plant object " + PlantLoop(PlantLoopNum).Name);
                ErrorsFound = true;
            }
            // Is this wh Source side plumbed in series (default) or are there other branches in parallel?
            if (allocated(PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter)) {
                for (SplitNum = 1; SplitNum <= PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).NumSplitters; ++SplitNum) {
                    if (any_eq(PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter(SplitNum).NodeNumOut,
                               SourceInletNode)) { // this wh is on the splitter
                        if (PlantLoop(PlantLoopNum).LoopSide(LoopSideNum).Splitter(SplitNum).TotalOutletNodes > 1) {
                            WaterThermalTank(WaterThermalTankNum).SourceSideSeries = false;
                        }
                    }
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding water heater input errors cause program termination");
        }
    }

    void SizeSupplySidePlantConnections(int const WaterThermalTankNum, Optional_int_const LoopNum, Optional_int_const LoopSideNum)
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataSizing;
        using DataHVACGlobals::SmallWaterVolFlow;
        using DataPlant::PlantLoop;
        using FluidProperties::GetDensityGlycol;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using ReportSizingManager::ReportSizingOutput;
        using namespace OutputReportPredefined;
        using DataPlant::PlantFinalSizesOkayToReport;
        using DataPlant::PlantFirstSizesOkayToFinalize;
        using DataPlant::PlantFirstSizesOkayToReport;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeSupplySidePlantConnections");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizNum;    // Plant Sizing index corresponding to CurLoopNum
        bool ErrorsFound; // If errors detected in input
        static int DummyWaterIndex(1);
        Real64 rho; // temporary fluid density
        int tmpLoopNum;
        int tmpLoopSideNum;
        Real64 tmpUseDesignVolFlowRate;    // local use side design flow rate
        Real64 tmpSourceDesignVolFlowRate; // local source side design flow rate

        PltSizNum = 0;
        ErrorsFound = false;
        tmpUseDesignVolFlowRate = WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate;
        tmpSourceDesignVolFlowRate = WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate;

        if (!present(LoopSideNum)) {
            tmpLoopSideNum = SupplySide;
        } else {
            tmpLoopSideNum = LoopSideNum;
        }
        if (!present(LoopNum)) {
            tmpLoopNum = WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum;
        } else {
            tmpLoopNum = LoopNum;
        }

        if ((WaterThermalTank(WaterThermalTankNum).UseInletNode > 0) && (tmpLoopNum == WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum)) {
            if (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized) {
                PltSizNum = WaterThermalTank(WaterThermalTankNum).UseSidePlantSizNum;
                if (PltSizNum > 0) { // we have a Plant Sizing Object
                    if (WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide == SupplySide) {
                        if (PlantSizData(PltSizNum).DesVolFlowRate >= SmallWaterVolFlow) {
                            if (PlantFirstSizesOkayToFinalize) {
                                WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = PlantSizData(PltSizNum).DesVolFlowRate;
                            } else {
                                tmpUseDesignVolFlowRate = PlantSizData(PltSizNum).DesVolFlowRate;
                            }
                        } else {
                            if (PlantFirstSizesOkayToFinalize) {
                                WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate = 0.0;
                            } else {
                                tmpUseDesignVolFlowRate = 0.0;
                            }
                        }
                        if (PlantFinalSizesOkayToReport) {
                            ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                               WaterThermalTank(WaterThermalTankNum).Name,
                                               "Use Side Design Flow Rate [m3/s]",
                                               WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate);
                        }
                        if (PlantFirstSizesOkayToReport) {
                            ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                               WaterThermalTank(WaterThermalTankNum).Name,
                                               "Initial Use Side Design Flow Rate [m3/s]",
                                               WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate);
                        }
                        if (PlantFirstSizesOkayToFinalize) {
                            RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).UseInletNode,
                                                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate);
                        } else {
                            RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).UseInletNode, tmpUseDesignVolFlowRate);
                        }

                        rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                               PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                        if (PlantFirstSizesOkayToFinalize) {
                            WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax =
                                WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate * rho;
                        } else {
                            WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax = tmpUseDesignVolFlowRate * rho;
                        }
                    }
                } else {
                    // do nothing
                } // plant sizing object
            } else {
                RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).UseInletNode,
                                            WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate);
                if (WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum > 0) {
                    rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                           DataGlobals::InitConvTemp,
                                           PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                           RoutineName);
                } else {
                    rho = GetDensityGlycol(fluidNameWater, DataGlobals::InitConvTemp, DummyWaterIndex, RoutineName);
                }

                WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax = WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate * rho;

            } // autosizing needed.
        }     // connected to plant

        if ((WaterThermalTank(WaterThermalTankNum).SourceInletNode > 0) &&
            (tmpLoopNum == WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum)) {
            if (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized) {
                PltSizNum = WaterThermalTank(WaterThermalTankNum).SourceSidePlantSizNum;
                if (PltSizNum > 0) {
                    if (WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide == SupplySide) {
                        if (PlantSizData(PltSizNum).DesVolFlowRate >= SmallWaterVolFlow) {
                            if (PlantFirstSizesOkayToFinalize) {
                                WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = PlantSizData(PltSizNum).DesVolFlowRate;
                            } else {
                                tmpSourceDesignVolFlowRate = PlantSizData(PltSizNum).DesVolFlowRate;
                            }
                        } else {
                            if (PlantFirstSizesOkayToFinalize) {
                                WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate = 0.0;
                            } else {
                                tmpSourceDesignVolFlowRate = 0.0;
                            }
                        }
                        if (PlantFinalSizesOkayToReport) {
                            ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                               WaterThermalTank(WaterThermalTankNum).Name,
                                               "Source Side Design Flow Rate [m3/s]",
                                               WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                        }
                        if (PlantFirstSizesOkayToReport) {
                            ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                               WaterThermalTank(WaterThermalTankNum).Name,
                                               "Initial Source Side Design Flow Rate [m3/s]",
                                               WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                        }
                        if (PlantFirstSizesOkayToFinalize) {
                            RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).SourceInletNode,
                                                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                        } else {
                            RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).SourceInletNode, tmpSourceDesignVolFlowRate);
                        }
                        rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                               PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                        if (PlantFirstSizesOkayToFinalize) {
                            WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax =
                                WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate * rho;
                        } else {
                            WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax = tmpSourceDesignVolFlowRate * rho;
                        }
                    } // plant loop allocation
                } else {
                    // do nothing
                } // plant sizing object
            } else {
                if (WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide == SupplySide) {
                    RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).SourceInletNode,
                                                WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                    if (WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum > 0) {
                        rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                               PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                    } else {
                        rho = GetDensityGlycol(fluidNameWater, DataGlobals::InitConvTemp, DummyWaterIndex, RoutineName);
                    }
                    WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax =
                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate * rho;
                }
            } // autosizing needed.
        }     // connected to plant

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void SizeTankForDemandSide(int const WaterThermalTankNum)
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

        // Using/Aliasing
        using DataHeatBalance::Zone;
        using namespace DataSizing;
        using DataGlobals::Pi;
        using DataHVACGlobals::SmallWaterVolFlow;
        using DataPlant::PlantLoop;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using namespace OutputReportPredefined;
        using DataPlant::PlantFinalSizesOkayToReport;
        using DataPlant::PlantFirstSizesOkayToFinalize;
        using DataPlant::PlantFirstSizesOkayToReport;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeTankForDemandSide");
        Real64 const GalTocubicMeters(0.0037854);
        Real64 const kBtuPerHrToWatts(293.1);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 Tstart;  // initial tank temp for sizing.
        Real64 Tfinish; // final target temp for sizing
        Real64 SumPeopleAllZones;
        Real64 SumFloorAreaAllZones;
        // unused  INTEGER   :: CollectorNum  ! do loop index
        //		static bool SizeVolume( false );
        //		static bool SizeMaxCapacity( false );
        Real64 rho;
        Real64 Cp;
        static int DummyWaterIndex(1);
        Real64 tmpTankVolume;  // local temporary for tank volume m3
        Real64 tmpMaxCapacity; // local temporary for heating capacity W
        static bool FuelTypeIsLikeGas(false);

        // local inits
        Tstart = 14.44;
        Tfinish = 57.22;

        tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Volume;
        tmpMaxCapacity = WaterThermalTank(WaterThermalTankNum).MaxCapacity;

        {
            auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode);

            if (SELECT_CASE_var == SizeNotSet) {

            } else if (SELECT_CASE_var == SizePeakDraw) {

            } else if (SELECT_CASE_var == SizeResidentialMin) {

                // assume can propagate rules for gas to other fuels.
                FuelTypeIsLikeGas = false;
                if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Gas")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Diesel")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Gasoline")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Coal")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "FuelOil#1")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "FuelOil#2")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Propane")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Steam")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "OtherFuel1")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "OtherFuel2")) {
                    FuelTypeIsLikeGas = true;
                } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "DistrictHeating")) {
                    FuelTypeIsLikeGas = true;
                }

                if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms == 1) {
                    if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                        if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                        if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 2.5 * 1000.0; // 2.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                        if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 27.0 * kBtuPerHrToWatts; // 27kBtu/hr
                    }

                } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms == 2) {
                    if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms <= 1.5) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 3.5 * 1000.0; // 3.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if ((WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms > 1.5) &&
                               (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms < 3.0)) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms >= 3.0) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    }
                } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms == 3) {
                    if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms <= 1.5) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if ((WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms > 1.5) &&
                               (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms < 3.0)) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms >= 3.0) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                        }
                    }
                } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms == 4) {
                    if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms <= 1.5) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                        }
                    } else if ((WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms > 1.5) &&
                               (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms < 3.0)) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                        }
                    } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms >= 3.0) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                        }
                    }
                } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms == 5) {
                    if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                        if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                        if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 47.0 * kBtuPerHrToWatts; // 47 kBtu/hr
                    }
                } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms >= 6) {
                    if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                        if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                        if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                    } else if (FuelTypeIsLikeGas) {
                        if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                        if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 50.0 * kBtuPerHrToWatts; // 50 kBtu/hr
                    }
                }

                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                }
                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                }
            } else if (SELECT_CASE_var == SizePerPerson) {
                // how to get number of people?

                SumPeopleAllZones = sum(Zone, &DataHeatBalance::ZoneData::TotOccupants);
                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized)
                    tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerPerson * SumPeopleAllZones;
                if (WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum > 0) {
                    rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                           ((Tfinish + Tstart) / 2.0),
                                           PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                           RoutineName);
                    Cp = GetSpecificHeatGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                               ((Tfinish + Tstart) / 2.0),
                                               PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                } else {
                    rho = GetDensityGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                    Cp = GetSpecificHeatGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                }

                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                    tmpMaxCapacity = SumPeopleAllZones * WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerPerson * (Tfinish - Tstart) *
                                     (1.0 / SecInHour) * rho * Cp; // m3/hr/person | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                }
                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                }
            } else if (SELECT_CASE_var == SizePerFloorArea) {

                SumFloorAreaAllZones = sum(Zone, &DataHeatBalance::ZoneData::FloorArea);
                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized)
                    tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerArea * SumFloorAreaAllZones;
                if (WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum > 0) {
                    rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                           ((Tfinish + Tstart) / 2.0),
                                           PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                           RoutineName);
                    Cp = GetSpecificHeatGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                               ((Tfinish + Tstart) / 2.0),
                                               PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                } else {
                    rho = GetDensityGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                    Cp = GetSpecificHeatGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                }

                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                    tmpMaxCapacity = SumFloorAreaAllZones * WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerArea *
                                     (Tfinish - Tstart) * (1.0 / SecInHour) * rho *
                                     Cp; // m2 | m3/hr/m2 | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                }
                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                }
            } else if (SELECT_CASE_var == SizePerUnit) {

                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized)
                    tmpTankVolume =
                        WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerUnit * WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits;
                if (WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum > 0) {
                    rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                           ((Tfinish + Tstart) / 2.0),
                                           PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                           RoutineName);
                    Cp = GetSpecificHeatGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                               ((Tfinish + Tstart) / 2.0),
                                               PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                } else {
                    rho = GetDensityGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                    Cp = GetSpecificHeatGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                }
                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                    tmpMaxCapacity = WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits *
                                     WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerUnit * (Tfinish - Tstart) * (1.0 / SecInHour) *
                                     rho * Cp; // m3/hr/ea | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                }
                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                }
            } else if (SELECT_CASE_var == SizePerSolarColArea) {
            }
        }

        // if stratified, might set height.
        if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) && (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedWaterHeater) &&
            PlantFirstSizesOkayToFinalize) { // might set height
            if ((WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) && (!WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized)) {
                WaterThermalTank(WaterThermalTankNum).Height = std::pow(
                    (4.0 * WaterThermalTank(WaterThermalTankNum).Volume * pow_2(WaterThermalTank(WaterThermalTankNum).Sizing.HeightAspectRatio)) / Pi,
                    0.3333333333333333);
                if (PlantFinalSizesOkayToReport) {
                    ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                       WaterThermalTank(WaterThermalTankNum).Name,
                                       "Tank Height [m]",
                                       WaterThermalTank(WaterThermalTankNum).Height);
                }
                if (PlantFirstSizesOkayToReport) {
                    ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                       WaterThermalTank(WaterThermalTankNum).Name,
                                       "Initial Tank Height [m]",
                                       WaterThermalTank(WaterThermalTankNum).Height);
                }
                // check if autocalculate Use outlet and source inlet are still set to autosize by earlier
                if (WaterThermalTank(WaterThermalTankNum).UseOutletHeightWasAutoSized) {
                    WaterThermalTank(WaterThermalTankNum).UseOutletHeight = WaterThermalTank(WaterThermalTankNum).Height;
                }
                if (WaterThermalTank(WaterThermalTankNum).SourceInletHeightWasAutoSized) {
                    WaterThermalTank(WaterThermalTankNum).SourceInletHeight = WaterThermalTank(WaterThermalTankNum).Height;
                }
            }
        }
    }

    void SizeTankForSupplySide(int const WaterThermalTankNum)
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

        // Using/Aliasing
        using DataSizing::AutoSize;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using namespace OutputReportPredefined;
        using DataGlobals::Pi;
        using DataPlant::PlantFinalSizesOkayToReport;
        using DataPlant::PlantFirstSizesOkayToFinalize;
        using DataPlant::PlantFirstSizesOkayToReport;
        using DataSurfaces::Surface;
        using SolarCollectors::Collector;
        using SolarCollectors::NumOfCollectors;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeTankForSupplySide");
        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 Tstart;  // initial tank temp for sizing.
        Real64 Tfinish; // final target temp for sizing
        int CollectorNum;
        Real64 rho;
        Real64 Cp;
        static int DummyWaterIndex(1);
        Real64 tmpTankVolume;  // local temporary for tank volume m3
        Real64 tmpMaxCapacity; // local temporary for heating capacity W

        // local inits
        Tstart = 14.44;
        Tfinish = 57.22;

        tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Volume;
        tmpMaxCapacity = WaterThermalTank(WaterThermalTankNum).MaxCapacity;

        {
            auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode);

            if (SELECT_CASE_var == SizePeakDraw) {
                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized)
                    tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Sizing.TankDrawTime *
                                    WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate * SecInHour; // hours | m3/s | (3600 s/1 hour)
                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                }
                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) {
                    if (WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryTime > 0.0) {
                        if (WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum > 0) {
                            rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidName,
                                                   ((Tfinish + Tstart) / 2.0),
                                                   PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidIndex,
                                                   RoutineName);
                            Cp = GetSpecificHeatGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidName,
                                                       ((Tfinish + Tstart) / 2.0),
                                                       PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidIndex,
                                                       RoutineName);
                        } else {
                            rho = GetDensityGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                            Cp = GetSpecificHeatGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                        }
                        tmpMaxCapacity = (WaterThermalTank(WaterThermalTankNum).Volume * rho * Cp * (Tfinish - Tstart)) /
                                         (WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryTime * SecInHour); // m3 | kg/m3 | J/Kg/K | K | seconds
                    } else {
                        ShowFatalError("SizeTankForSupplySide: Tank=\"" + WaterThermalTank(WaterThermalTankNum).Name +
                                       "\", requested sizing for max capacity but entered Recovery Time is zero.");
                    }
                }

                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                }
            } else if (SELECT_CASE_var == SizePerSolarColArea) {

                WaterThermalTank(WaterThermalTankNum).Sizing.TotalSolarCollectorArea = 0.0;
                for (CollectorNum = 1; CollectorNum <= NumOfCollectors; ++CollectorNum) {
                    WaterThermalTank(WaterThermalTankNum).Sizing.TotalSolarCollectorArea += Surface(Collector(CollectorNum).Surface).Area;
                }

                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized)
                    tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Sizing.TotalSolarCollectorArea *
                                    WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerCollectorArea;
                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 0.0;
                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                }
                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized && PlantFirstSizesOkayToFinalize) {
                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                    if (PlantFinalSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                    if (PlantFirstSizesOkayToReport) {
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Initial Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                }
            }
        }

        if ((WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) && (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedWaterHeater) &&
            PlantFirstSizesOkayToFinalize) { // might set height
            if ((WaterThermalTank(WaterThermalTankNum).HeightWasAutoSized) && (!WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized)) {
                WaterThermalTank(WaterThermalTankNum).Height = std::pow(
                    (4.0 * WaterThermalTank(WaterThermalTankNum).Volume * pow_2(WaterThermalTank(WaterThermalTankNum).Sizing.HeightAspectRatio)) / Pi,
                    0.3333333333333333);
                if (PlantFinalSizesOkayToReport) {
                    ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                       WaterThermalTank(WaterThermalTankNum).Name,
                                       "Tank Height [m]",
                                       WaterThermalTank(WaterThermalTankNum).Height);
                }
                if (PlantFirstSizesOkayToReport) {
                    ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                       WaterThermalTank(WaterThermalTankNum).Name,
                                       "Initial Tank Height [m]",
                                       WaterThermalTank(WaterThermalTankNum).Height);
                }
            }
        }
    }

    void SizeDemandSidePlantConnections(int const WaterThermalTankNum)
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataSizing;
        using DataHVACGlobals::SmallWaterVolFlow;
        using DataPlant::PlantLoop;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        using ReportSizingManager::ReportSizingOutput;
        using namespace OutputReportPredefined;
        using DataPlant::PlantFinalSizesOkayToReport;
        using DataPlant::PlantFirstSizesOkayToFinalize;
        using DataPlant::PlantFirstSizesOkayToReport;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeDemandSidePlantConnections");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TankVolume;       // local variable for tank volume.
        Real64 Tpdesign;         // plant sizing exit temperature
        Real64 Tstart;           // initial tank temp for sizing.
        Real64 Tfinish;          // final target temp for sizing
        Real64 tankRecoverhours; // parameter in sizing, hours to recover
        int PltSizNum;           // Plant Sizing index corresponding to CurLoopNum
        bool ErrorsFound;        // If errors detected in input

        Real64 eff; // temporary effectiveness value for heat exchanger inside tank

        static int DummyWaterIndex(1);
        Real64 rho;
        Real64 tmpUseDesignVolFlowRate;    // local use side design flow rate
        Real64 tmpSourceDesignVolFlowRate; // local use side design flow rate

        tankRecoverhours = WaterThermalTank(WaterThermalTankNum).SizingRecoveryTime;
        PltSizNum = 0;
        ErrorsFound = false;
        tmpUseDesignVolFlowRate = WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate;
        tmpSourceDesignVolFlowRate = WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate;

        if (!WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
            Tstart = 14.44;
            Tfinish = 57.22;
        } else {
            Tstart = 14.44;
            Tfinish = 9.0;
        }

        // determine tank volume to use for sizing.
        TankVolume = WaterThermalTank(WaterThermalTankNum).Volume;
        if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) {
            TankVolume = WaterThermalTank(WaterThermalTankNum).Sizing.NominalVolForSizingDemandSideFlow;
        }

        if (WaterThermalTank(WaterThermalTankNum).UseInletNode > 0) {
            if (WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRateWasAutoSized) {
                PltSizNum = WaterThermalTank(WaterThermalTankNum).UseSidePlantSizNum;
                if (PltSizNum > 0) { // we have a Plant Sizing Object
                    if (WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopSide == DemandSide) {
                        // probably shouldn't come here as Use side is unlikley to be on demand side (?)
                        // but going to treat component with symetry so if connections are reversed it'll still work
                        // choose a flow rate that will allow the entire volume of the tank to go from 14.44 to 57.22 C
                        // in user specified hours.
                        //  using the plant inlet design temp for sizing.
                        Tpdesign = PlantSizData(PltSizNum).ExitTemp;
                        eff = WaterThermalTank(WaterThermalTankNum).UseEffectiveness;
                        if ((Tpdesign >= 58.0) && (!WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank)) {
                            if (PlantFirstSizesOkayToFinalize) {
                                WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            } else {
                                tmpUseDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            }
                        } else if ((Tpdesign <= 8.0) && (WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank)) {
                            if (PlantFirstSizesOkayToFinalize) {
                                WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            } else {
                                tmpUseDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            }
                        } else {
                            if (!WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                                // plant sizing object design temperature is set too low throw warning.
                                ShowSevereError("Autosizing of Use side water heater design flow rate requires Sizing:Plant object to have an exit "
                                                "temperature >= 58C");
                                ShowContinueError("Occurs for water heater object=" + WaterThermalTank(WaterThermalTankNum).Name);
                            } else {
                                // plant sizing object design temperature is set too hi throw warning.
                                ShowSevereError("Autosizing of Use side chilled water tank design flow rate requires Sizing:Plant object to have an "
                                                "exit temperature <= 8C");
                                ShowContinueError("Occurs for chilled water storage tank object=" + WaterThermalTank(WaterThermalTankNum).Name);
                            }
                            ErrorsFound = true;
                        }
                        if (PlantFinalSizesOkayToReport) {
                            ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                               WaterThermalTank(WaterThermalTankNum).Name,
                                               "Use Side Design Flow Rate [m3/s]",
                                               WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate);
                        }
                        if (PlantFirstSizesOkayToReport) {
                            ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                               WaterThermalTank(WaterThermalTankNum).Name,
                                               "Initial Use Side Design Flow Rate [m3/s]",
                                               WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate);
                        }
                        if (PlantFirstSizesOkayToFinalize) {
                            RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).UseInletNode,
                                                        WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate);
                        } else {
                            RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).UseInletNode, tmpUseDesignVolFlowRate);
                        }
                        rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                               PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                        if (PlantFirstSizesOkayToFinalize) {
                            WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax =
                                WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate * rho;
                        } else {
                            WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax = tmpUseDesignVolFlowRate * rho;
                        }
                    } // Demand side
                } else {
                    // do nothing
                } // plant sizing object

            } else {
                // not autosized - report flow to RegisterPlantCompDesignFlow for supply side component sizing
                RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).UseInletNode,
                                            WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate);
                if (WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum > 0) {
                    rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidName,
                                           DataGlobals::InitConvTemp,
                                           PlantLoop(WaterThermalTank(WaterThermalTankNum).UseSidePlantLoopNum).FluidIndex,
                                           RoutineName);
                } else {
                    rho = GetDensityGlycol(fluidNameWater, DataGlobals::InitConvTemp, DummyWaterIndex, RoutineName);
                }
                WaterThermalTank(WaterThermalTankNum).PlantUseMassFlowRateMax = WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate * rho;
            } // autosizing needed.
        }     // connected to plant

        if (WaterThermalTank(WaterThermalTankNum).SourceInletNode > 0) {
            if (WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRateWasAutoSized) {
                PltSizNum = WaterThermalTank(WaterThermalTankNum).SourceSidePlantSizNum;
                if (PltSizNum > 0) {
                    if (WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopSide == DemandSide) {
                        //  choose a flow rate that will allow the entire volume of the tank to go from 14.44 to 57.22 C
                        // in user specified hours.
                        //  using the plant inlet design temp for sizing.
                        Tpdesign = PlantSizData(PltSizNum).ExitTemp;
                        eff = WaterThermalTank(WaterThermalTankNum).SourceEffectiveness;
                        if ((Tpdesign >= 58.0) && (!WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank)) {

                            if (PlantFirstSizesOkayToFinalize) {
                                WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            } else {
                                tmpSourceDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            }
                        } else if ((Tpdesign <= 8.0) && (WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank)) {
                            if (PlantFirstSizesOkayToFinalize) {
                                WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            } else {
                                tmpSourceDesignVolFlowRate =
                                    -1.0 * (TankVolume / (tankRecoverhours * SecInHour * eff)) * std::log((Tpdesign - Tfinish) / (Tpdesign - Tstart));
                            }
                        } else {
                            if (!WaterThermalTank(WaterThermalTankNum).IsChilledWaterTank) {
                                // plant sizing object design temperature is set too low throw warning.
                                ShowSevereError("Autosizing of Source side water heater design flow rate requires Sizing:Plant object to have an "
                                                "exit temperature >= 58C");
                                ShowContinueError("Occurs for WaterHeater:Mixed object=" + WaterThermalTank(WaterThermalTankNum).Name);
                            } else {
                                // plant sizing object design temperature is set too hi throw warning.
                                ShowSevereError("Autosizing of Source side chilled water tank design flow rate requires Sizing:Plant object to have "
                                                "an exit temperature <= 8C");
                                ShowContinueError("Occurs for chilled water storage tank object=" + WaterThermalTank(WaterThermalTankNum).Name);
                            }
                            ErrorsFound = true;
                        }
                        if (PlantFinalSizesOkayToReport) {
                            ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                               WaterThermalTank(WaterThermalTankNum).Name,
                                               "Source Side Design Flow Rate [m3/s]",
                                               WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                        }
                        if (PlantFirstSizesOkayToReport) {
                            ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                               WaterThermalTank(WaterThermalTankNum).Name,
                                               "Initial Source Side Design Flow Rate [m3/s]",
                                               WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                        }
                        if (PlantFirstSizesOkayToFinalize) {
                            RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).SourceInletNode,
                                                        WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                        } else {
                            RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).SourceInletNode, tmpSourceDesignVolFlowRate);
                        }
                        rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidName,
                                               DataGlobals::InitConvTemp,
                                               PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidIndex,
                                               RoutineName);
                        if (PlantFirstSizesOkayToFinalize) {
                            WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax =
                                WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate * rho;
                        } else {
                            WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax = tmpSourceDesignVolFlowRate * rho;
                        }
                    } // demand side
                } else {
                    // do nothing
                } // plant sizing object

            } else {
                // not autosized - report flow to RegisterPlantCompDesignFlow for supply side component sizing
                RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum).SourceInletNode,
                                            WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate);
                if (WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum > 0) {
                    rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidName,
                                           DataGlobals::InitConvTemp,
                                           PlantLoop(WaterThermalTank(WaterThermalTankNum).SourceSidePlantLoopNum).FluidIndex,
                                           RoutineName);
                } else {
                    rho = GetDensityGlycol(fluidNameWater, DataGlobals::InitConvTemp, DummyWaterIndex, RoutineName);
                }
                WaterThermalTank(WaterThermalTankNum).PlantSourceMassFlowRateMax =
                    WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate * rho;
            } // autosizing needed.
        }     // connected to plant

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void SizeStandAloneWaterHeater(int const WaterThermalTankNum)
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

        // Using/Aliasing
        using DataHeatBalance::Zone;
        using DataSizing::AutoSize;
        using DataSurfaces::Surface;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using ScheduleManager::GetScheduleMaxValue;
        using SolarCollectors::Collector;
        using SolarCollectors::NumOfCollectors;

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const GalTocubicMeters(0.0037854);
        Real64 const kBtuPerHrToWatts(293.1);
        static std::string const RoutineName("SizeStandAloneWaterHeater");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 tmpTankVolume;  // local temporary for tank volume m3
        Real64 tmpMaxCapacity; // local temporary for heating capacity W
        Real64 Tstart;         // initial tank temp for sizing.
        Real64 Tfinish;        // final target temp for sizing
        static bool FuelTypeIsLikeGas(false);
        static int DummyWaterIndex(1);
        Real64 rho;
        Real64 Cp;
        Real64 DrawDesignVolFlowRate;
        Real64 SumPeopleAllZones;
        Real64 SumFloorAreaAllZones;
        int CollectorNum;

        // local inits
        Tstart = 14.44;
        Tfinish = 57.22;

        tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Volume;
        tmpMaxCapacity = WaterThermalTank(WaterThermalTankNum).MaxCapacity;

        if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized || WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) {

            {
                auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).Sizing.DesignMode);

                if (SELECT_CASE_var == SizePeakDraw) {
                    // get draw rate from maximum in schedule
                    rho = GetDensityGlycol(fluidNameWater, DataGlobals::InitConvTemp, DummyWaterIndex, RoutineName);
                    DrawDesignVolFlowRate = GetScheduleMaxValue(WaterThermalTank(WaterThermalTankNum).FlowRateSchedule) *
                                            WaterThermalTank(WaterThermalTankNum).MassFlowRateMax / rho;

                    if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) {
                        tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Sizing.TankDrawTime * DrawDesignVolFlowRate *
                                        SecInHour; // hours | m3/s | (3600 s/1 hour)
                        WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) {
                        if (WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryTime > 0.0) {
                            rho = GetDensityGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                            Cp = GetSpecificHeatGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);

                            tmpMaxCapacity =
                                (WaterThermalTank(WaterThermalTankNum).Volume * rho * Cp * (Tfinish - Tstart)) /
                                (WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryTime * SecInHour); // m3 | kg/m3 | J/Kg/K | K | seconds
                        } else {
                            ShowFatalError("SizeStandAloneWaterHeater: Tank=\"" + WaterThermalTank(WaterThermalTankNum).Name +
                                           "\", requested sizing for max capacity but entered Recovery Time is zero.");
                        }
                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }

                } else if (SELECT_CASE_var == SizeResidentialMin) {
                    // assume can propagate rules for gas to other fuels.
                    FuelTypeIsLikeGas = false;
                    if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Gas")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Diesel")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Gasoline")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Coal")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "FuelOil#1")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "FuelOil#2")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Propane")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Steam")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "OtherFuel1")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "OtherFuel2")) {
                        FuelTypeIsLikeGas = true;
                    } else if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "DistrictHeating")) {
                        FuelTypeIsLikeGas = true;
                    }

                    if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms == 1) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 2.5 * 1000.0; // 2.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 20.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 27.0 * kBtuPerHrToWatts; // 27kBtu/hr
                        }

                    } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms == 2) {
                        if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms <= 1.5) {
                            if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 3.5 * 1000.0; // 3.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        } else if ((WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms > 1.5) &&
                                   (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms < 3.0)) {
                            if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms >= 3.0) {
                            if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        }
                    } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms == 3) {
                        if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms <= 1.5) {
                            if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 4.5 * 1000.0; // 4.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 30.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        } else if ((WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms > 1.5) &&
                                   (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms < 3.0)) {
                            if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms >= 3.0) {
                            if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                            }
                        }
                    } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms == 4) {
                        if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms <= 1.5) {
                            if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 36.0 * kBtuPerHrToWatts; // 36 kBtu/hr
                            }
                        } else if ((WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms > 1.5) &&
                                   (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms < 3.0)) {
                            if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 40.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                            }
                        } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBathrooms >= 3.0) {
                            if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                            } else if (FuelTypeIsLikeGas) {
                                if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                                if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized)
                                    tmpMaxCapacity = 38.0 * kBtuPerHrToWatts; // 38 kBtu/hr
                            }
                        }
                    } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms == 5) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 47.0 * kBtuPerHrToWatts; // 47 kBtu/hr
                        }
                    } else if (WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfBedrooms >= 6) {
                        if (UtilityRoutines::SameString(WaterThermalTank(WaterThermalTankNum).FuelType, "Electric")) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 66.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 5.5 * 1000.0; // 5.5 kW
                        } else if (FuelTypeIsLikeGas) {
                            if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) tmpTankVolume = 50.0 * GalTocubicMeters;
                            if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 50.0 * kBtuPerHrToWatts; // 50 kBtu/hr
                        }
                    }
                    if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) {
                        WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }

                } else if (SELECT_CASE_var == SizePerPerson) {
                    // how to get number of people?

                    SumPeopleAllZones = sum(Zone, &DataHeatBalance::ZoneData::TotOccupants);
                    if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) {
                        tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerPerson * SumPeopleAllZones;
                    }
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) {
                        rho = GetDensityGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                        Cp = GetSpecificHeatGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                        tmpMaxCapacity = SumPeopleAllZones * WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerPerson *
                                         (Tfinish - Tstart) * (1.0 / SecInHour) * rho *
                                         Cp; // m3/hr/person | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                    }

                    if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) {
                        WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }

                } else if (SELECT_CASE_var == SizePerFloorArea) {

                    SumFloorAreaAllZones = sum(Zone, &DataHeatBalance::ZoneData::FloorArea);
                    if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) {
                        tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerArea * SumFloorAreaAllZones;
                    }

                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) {
                        rho = GetDensityGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                        Cp = GetSpecificHeatGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                        tmpMaxCapacity = SumFloorAreaAllZones * WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerArea *
                                         (Tfinish - Tstart) * (1.0 / SecInHour) * rho *
                                         Cp; // m2 | m3/hr/m2 | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                    }
                    if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) {
                        WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                } else if (SELECT_CASE_var == SizePerUnit) {

                    if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized)
                        tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerUnit *
                                        WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits;

                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) {
                        rho = GetDensityGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                        Cp = GetSpecificHeatGlycol(fluidNameWater, ((Tfinish + Tstart) / 2.0), DummyWaterIndex, RoutineName);
                        tmpMaxCapacity = WaterThermalTank(WaterThermalTankNum).Sizing.NumberOfUnits *
                                         WaterThermalTank(WaterThermalTankNum).Sizing.RecoveryCapacityPerUnit * (Tfinish - Tstart) *
                                         (1.0 / SecInHour) * rho * Cp; // m3/hr/ea | delta T  in K | 1 hr/ 3600 s | kg/m3 | J/Kg/k
                    }

                    if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) {
                        WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                } else if (SELECT_CASE_var == SizePerSolarColArea) {
                    WaterThermalTank(WaterThermalTankNum).Sizing.TotalSolarCollectorArea = 0.0;
                    for (CollectorNum = 1; CollectorNum <= NumOfCollectors; ++CollectorNum) {
                        WaterThermalTank(WaterThermalTankNum).Sizing.TotalSolarCollectorArea += Surface(Collector(CollectorNum).Surface).Area;
                    }

                    if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized)
                        tmpTankVolume = WaterThermalTank(WaterThermalTankNum).Sizing.TotalSolarCollectorArea *
                                        WaterThermalTank(WaterThermalTankNum).Sizing.TankCapacityPerCollectorArea;
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) tmpMaxCapacity = 0.0;
                    if (WaterThermalTank(WaterThermalTankNum).VolumeWasAutoSized) {
                        WaterThermalTank(WaterThermalTankNum).Volume = tmpTankVolume;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Tank Volume [m3]",
                                           WaterThermalTank(WaterThermalTankNum).Volume);
                    }
                    if (WaterThermalTank(WaterThermalTankNum).MaxCapacityWasAutoSized) {
                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = tmpMaxCapacity;
                        ReportSizingOutput(WaterThermalTank(WaterThermalTankNum).Type,
                                           WaterThermalTank(WaterThermalTankNum).Name,
                                           "Maximum Heater Capacity [W]",
                                           WaterThermalTank(WaterThermalTankNum).MaxCapacity);
                    }
                }
            }
        }
    }

    void UpdateWaterThermalTank(int const WaterThermalTankNum)
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

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        using DataLoopNode::Node;
        using Psychrometrics::CPHW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int UseInletNode;
        int UseOutletNode;
        int SourceInletNode;
        int SourceOutletNode;

        // FLOW:
        UseInletNode = WaterThermalTank(WaterThermalTankNum).UseInletNode;
        UseOutletNode = WaterThermalTank(WaterThermalTankNum).UseOutletNode;
        SourceInletNode = WaterThermalTank(WaterThermalTankNum).SourceInletNode;
        SourceOutletNode = WaterThermalTank(WaterThermalTankNum).SourceOutletNode;

        if (UseInletNode > 0 && UseOutletNode > 0) {
            Node(UseOutletNode) = Node(UseInletNode); // this could wipe out setpoints on outlet node

            Node(UseOutletNode).Temp = WaterThermalTank(WaterThermalTankNum).UseOutletTemp;
        }

        if (SourceInletNode > 0 && SourceOutletNode > 0) {
            Node(SourceOutletNode) = Node(SourceInletNode);

            Node(SourceOutletNode).Temp = WaterThermalTank(WaterThermalTankNum).SourceOutletTemp;
        }
    }

    void ReportWaterThermalTank(int const WaterThermalTankNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brandon Anderson
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  Feb 2004, PGE

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates report variables.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        using DataGlobals::SecInHour;
        using DataHVACGlobals::TimeStepSys;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 SecInTimeStep;

        // FLOW:
        SecInTimeStep = TimeStepSys * SecInHour;

        WaterThermalTank(WaterThermalTankNum).UnmetEnergy = WaterThermalTank(WaterThermalTankNum).UnmetRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).LossEnergy = WaterThermalTank(WaterThermalTankNum).LossRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).FlueLossEnergy = WaterThermalTank(WaterThermalTankNum).FlueLossRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).UseEnergy = WaterThermalTank(WaterThermalTankNum).UseRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).TotalDemandEnergy = WaterThermalTank(WaterThermalTankNum).TotalDemandRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).SourceEnergy = WaterThermalTank(WaterThermalTankNum).SourceRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).HeaterEnergy = WaterThermalTank(WaterThermalTankNum).HeaterRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).HeaterEnergy1 = WaterThermalTank(WaterThermalTankNum).HeaterRate1 * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).HeaterEnergy2 = WaterThermalTank(WaterThermalTankNum).HeaterRate2 * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).FuelEnergy = WaterThermalTank(WaterThermalTankNum).FuelRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).VentEnergy = WaterThermalTank(WaterThermalTankNum).VentRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).OffCycParaFuelEnergy = WaterThermalTank(WaterThermalTankNum).OffCycParaFuelRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).OffCycParaEnergyToTank = WaterThermalTank(WaterThermalTankNum).OffCycParaRateToTank * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).OnCycParaFuelEnergy = WaterThermalTank(WaterThermalTankNum).OnCycParaFuelRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).OnCycParaEnergyToTank = WaterThermalTank(WaterThermalTankNum).OnCycParaRateToTank * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).NetHeatTransferEnergy = WaterThermalTank(WaterThermalTankNum).NetHeatTransferRate * SecInTimeStep;
        WaterThermalTank(WaterThermalTankNum).VolumeConsumed = WaterThermalTank(WaterThermalTankNum).VolFlowRate * SecInTimeStep;
    }

    void CalcStandardRatings(int const WaterThermalTankNum)
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

        // Using/Aliasing
        using CurveManager::CurveValue;
        using DataEnvironment::OutBaroPress;
        using DataHVACGlobals::BlowThru;
        using DataHVACGlobals::CycFanCycCoil;
        using DataHVACGlobals::DXCoilTotalCapacity;
        using DataHVACGlobals::HPWHCrankcaseDBTemp;
        using DataHVACGlobals::HPWHInletDBTemp;
        using DataHVACGlobals::HPWHInletWBTemp;
        using DataHVACGlobals::TimeStepSys;
        using DataLoopNode::Node;
        using DXCoils::HPWHHeatingCapacity;
        using DXCoils::HPWHHeatingCOP;
        using DXCoils::SimDXCoil;
        using Psychrometrics::CPHW;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhoAirFnPbTdbW;
        using Psychrometrics::PsyTwbFnTdbWPb;
        using Psychrometrics::RhoH2O;
        using namespace OutputReportPredefined;
        using General::TrimSigDigits;
        using IntegratedHeatPump::IntegratedHeatPumps;
        using VariableSpeedCoils::SimVariableSpeedCoils;
        using VariableSpeedCoils::VarSpeedCoil;
        using VariableSpeedCoils::VSHPWHHeatingCapacity;
        using VariableSpeedCoils::VSHPWHHeatingCOP;

        // Locals
        Real64 MdotAir; // air mass flow rate through HP water heater evaporator (kg/s)

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 TotalDrawMass;            // Total mass of hot water drawn during the test (kg), equivalent to 64.3 gallons
        Real64 DrawMass;                 // Mass of a single draw of hot water (kg)
        Real64 SecInTimeStep;            // Seconds per timestep, depends on user-specified system timestep (s)
        Real64 DrawMassFlowRate;         // Mass flow rate of all test draw (m3/s)
        int TimeStepPerHour;             // Number of timesteps per hour
        int Step;                        // Current timestep in the self-contained water heater simulation
        Real64 FuelEnergy;               // Cumulative fuel energy used to heat the tank (J)
        Real64 MaxCapacity;              // Maximum heating capacity (W)
        Real64 RecoveryEfficiency;       // Standard water heater rating
        Real64 EnergyFactor;             // Standard water heater rating
        int HPNum(0);                    // index to heat pump water heater
        Real64 MdotWater;                // water mass flow rate through HP water heater condenser (kg/s)
        Real64 AmbientHumRat;            // used during HPWH rating procedure
        Real64 RatedDXCoilTotalCapacity; // used during HPWH rating procedure
        bool FirstTimeFlag;              // used during HPWH rating procedure
        std::string equipName;
        Real64 EMP1(0.0), EMP2(0.0), EMP3(0.0); // place holder to calling vs HPWH function
        bool bIsVSCoil(false);                  // variable-speed HPWH identifier
        Real64 RhoWater;                        // water density
        int VSCoilNum(0);
        std::string VSCoilName = "";

        // Formats
        static gio::Fmt Format_720("('Water Heater Information',6(',',A))");
        static gio::Fmt Format_721("('Heat Pump Water Heater Information',7(',',A))");

        if (AlreadyRated(WaterThermalTankNum)) { // bail we already did this one
            return;
        }

        // FLOW:
        if (WaterThermalTank(WaterThermalTankNum).MaxCapacity > 0.0 || WaterThermalTank(WaterThermalTankNum).HeatPumpNum > 0) {
            // Set test conditions
            WaterThermalTank(WaterThermalTankNum).AmbientTemp = 19.7222;   // 67.5 F
            WaterThermalTank(WaterThermalTankNum).UseInletTemp = 14.4444;  // 58 F
            WaterThermalTank(WaterThermalTankNum).SetPointTemp = 57.2222;  // 135 F
            WaterThermalTank(WaterThermalTankNum).SetPointTemp2 = 57.2222; // 135 F
            WaterThermalTank(WaterThermalTankNum).TankTemp = 57.2222;      // Initialize tank temperature
            if (WaterThermalTank(WaterThermalTankNum).Nodes > 0)
                for (auto &e : WaterThermalTank(WaterThermalTankNum).Node)
                    e.Temp = 57.2222;

            TotalDrawMass = 0.243402 * RhoH2O(DataGlobals::InitConvTemp); // 64.3 gal * rho
            DrawMass = TotalDrawMass / 6.0;                               // 6 equal draws
            SecInTimeStep = TimeStepSys * SecInHour;
            DrawMassFlowRate = DrawMass / SecInTimeStep;
            FuelEnergy = 0.0;
            FirstTimeFlag = true;

            TimeStepPerHour = int(1.0 / TimeStepSys);

            // Simulate 24 hour test
            for (Step = 1; Step <= TimeStepPerHour * 24; ++Step) {

                if (Step == 1 || Step == (1 + TimeStepPerHour) || Step == (1 + TimeStepPerHour * 2) || Step == (1 + TimeStepPerHour * 3) ||
                    Step == (1 + TimeStepPerHour * 4) || Step == (1 + TimeStepPerHour * 5)) { // Hour 1 | Hour 2 | Hour 3 | Hour 4 | Hour 5 | Hour 6

                    WaterThermalTank(WaterThermalTankNum).UseMassFlowRate = DrawMassFlowRate;
                } else {
                    WaterThermalTank(WaterThermalTankNum).UseMassFlowRate = 0.0;
                }

                WaterThermalTank(WaterThermalTankNum).SavedTankTemp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                WaterThermalTank(WaterThermalTankNum).SavedMode = WaterThermalTank(WaterThermalTankNum).Mode;
                if (WaterThermalTank(WaterThermalTankNum).Nodes > 0) {
                    for (auto &e : WaterThermalTank(WaterThermalTankNum).Node)
                        e.SavedTemp = e.Temp;
                    WaterThermalTank(WaterThermalTankNum).SavedHeaterOn1 = WaterThermalTank(WaterThermalTankNum).HeaterOn1;
                    WaterThermalTank(WaterThermalTankNum).SavedHeaterOn2 = WaterThermalTank(WaterThermalTankNum).HeaterOn2;
                }

                if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum == 0) {

                    {
                        auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).TypeNum);

                        if (SELECT_CASE_var == MixedWaterHeater) {
                            CalcWaterThermalTankMixed(WaterThermalTankNum);

                        } else if (SELECT_CASE_var == StratifiedWaterHeater) {
                            CalcWaterThermalTankStratified(WaterThermalTankNum);

                        } else {
                            //         Unhandled water heater type
                        }
                    }

                } else {

                    HPNum = WaterThermalTank(WaterThermalTankNum).HeatPumpNum;
                    AmbientHumRat = 0.00717; // Humidity ratio at 67.5 F / 50% RH

                    //       set the heat pump air- and water-side mass flow rate
                    MdotWater = HPWaterHeater(HPNum).OperatingWaterFlowRate * RhoH2O(WaterThermalTank(WaterThermalTankNum).TankTemp);
                    MdotAir = HPWaterHeater(HPNum).OperatingAirMassFlowRate;

                    // ?? why is HPWH condenser inlet node temp reset inside the for loop? shouldn't it chnage with the tank temp throughout these
                    // iterations?
                    if (HPWaterHeater(HPNum).TypeNum == TypeOf_HeatPumpWtrHeaterPumped) {
                        // set the condenser inlet node mass flow rate and temperature
                        Node(HPWaterHeater(HPNum).CondWaterInletNode).MassFlowRate = MdotWater;
                        Node(HPWaterHeater(HPNum).CondWaterInletNode).Temp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                    }

                    //       initialize temperatures for HPWH DX Coil heating capacity and COP curves
                    HPWHInletDBTemp = WaterThermalTank(WaterThermalTankNum).AmbientTemp;
                    HPWHInletWBTemp = PsyTwbFnTdbWPb(HPWHInletDBTemp, AmbientHumRat, OutBaroPress);

                    //       set up full air flow on DX coil inlet node
                    if (HPWaterHeater(HPNum).InletAirMixerNode > 0) {
                        Node(HPWaterHeater(HPNum).InletAirMixerNode).MassFlowRate = MdotAir;
                        Node(HPWaterHeater(HPNum).InletAirMixerNode).MassFlowRateMaxAvail = MdotAir;
                        Node(HPWaterHeater(HPNum).InletAirMixerNode).Temp = WaterThermalTank(WaterThermalTankNum).AmbientTemp;
                        Node(HPWaterHeater(HPNum).InletAirMixerNode).HumRat = AmbientHumRat;
                        Node(HPWaterHeater(HPNum).InletAirMixerNode).Enthalpy =
                            PsyHFnTdbW(WaterThermalTank(WaterThermalTankNum).AmbientTemp, AmbientHumRat);
                    } else {
                        if (HPWaterHeater(HPNum).OutsideAirNode == 0) {
                            Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).MassFlowRate = MdotAir;
                            Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).MassFlowRateMaxAvail = MdotAir;
                            Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).Temp = WaterThermalTank(WaterThermalTankNum).AmbientTemp;
                            Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).HumRat = AmbientHumRat;
                            Node(HPWaterHeater(HPNum).HeatPumpAirInletNode).Enthalpy =
                                PsyHFnTdbW(WaterThermalTank(WaterThermalTankNum).AmbientTemp, AmbientHumRat);
                        } else {
                            Node(HPWaterHeater(HPNum).OutsideAirNode).MassFlowRate = MdotAir;
                            Node(HPWaterHeater(HPNum).OutsideAirNode).MassFlowRateMaxAvail = MdotAir;
                            Node(HPWaterHeater(HPNum).OutsideAirNode).Temp = WaterThermalTank(WaterThermalTankNum).AmbientTemp;
                            Node(HPWaterHeater(HPNum).OutsideAirNode).HumRat = AmbientHumRat;
                            Node(HPWaterHeater(HPNum).OutsideAirNode).Enthalpy =
                                PsyHFnTdbW(WaterThermalTank(WaterThermalTankNum).AmbientTemp, AmbientHumRat);
                        }
                    }

                    HPWHCrankcaseDBTemp = WaterThermalTank(WaterThermalTankNum).AmbientTemp;

                    if (UtilityRoutines::SameString(HPWaterHeater(HPNum).DXCoilType, "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed") ||
                        (HPWaterHeater(HPNum).bIsIHP)) {
                        bIsVSCoil = true;
                        VSCoilName = HPWaterHeater(HPNum).DXCoilName;
                        VSCoilNum = HPWaterHeater(HPNum).DXCoilNum;
                        if (HPWaterHeater(HPNum).bIsIHP) {
                            VSCoilNum = IntegratedHeatPumps(HPWaterHeater(HPNum).DXCoilNum).SCWHCoilIndex;
                            VSCoilName = IntegratedHeatPumps(HPWaterHeater(HPNum).DXCoilNum).SCWHCoilName;
                        }

                        RhoWater = RhoH2O(WaterThermalTank(WaterThermalTankNum).TankTemp);
                        SetVSHPWHFlowRates(
                            WaterThermalTankNum, HPNum, VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel, 1.0, RhoWater, MdotWater, true);
                        //       simulate the HPWH coil/fan to find heating capacity
                        if (HPWaterHeater(HPNum).FanPlacement == BlowThru) {
                            //   simulate fan and DX coil twice
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            SimVariableSpeedCoils(VSCoilName,
                                                  VSCoilNum,
                                                  CycFanCycCoil,
                                                  EMP1,
                                                  EMP2,
                                                  EMP3,
                                                  1,
                                                  1.0,
                                                  VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                                                  1.0,
                                                  0.0,
                                                  0.0,
                                                  1.0);
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            SimVariableSpeedCoils(VSCoilName,
                                                  VSCoilNum,
                                                  CycFanCycCoil,
                                                  EMP1,
                                                  EMP2,
                                                  EMP3,
                                                  1,
                                                  1.0,
                                                  VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                                                  1.0,
                                                  0.0,
                                                  0.0,
                                                  1.0);
                        } else {
                            //   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
                            SimVariableSpeedCoils(VSCoilName,
                                                  VSCoilNum,
                                                  CycFanCycCoil,
                                                  EMP1,
                                                  EMP2,
                                                  EMP3,
                                                  1,
                                                  1.0,
                                                  VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
                                                  1.0,
                                                  0.0,
                                                  0.0,
                                                  1.0);
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            SimVariableSpeedCoils(VSCoilName,
                                                  VSCoilNum,
                                                  CycFanCycCoil,
                                                  EMP1,
                                                  EMP2,
                                                  EMP3,
                                                  1,
                                                  1.0,
                                                  VarSpeedCoil(HPWaterHeater(HPNum).DXCoilNum).NormSpedLevel,
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

                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = VSHPWHHeatingCapacity;
                        WaterThermalTank(WaterThermalTankNum).MinCapacity = VSHPWHHeatingCapacity;
                        WaterThermalTank(WaterThermalTankNum).Efficiency = VSHPWHHeatingCOP;
                    } else {
                        bIsVSCoil = false;
                        //       simulate the HPWH coil/fan to find heating capacity
                        if (HPWaterHeater(HPNum).FanPlacement == BlowThru) {
                            if (FirstTimeFlag) { // first time DXCoil is called, it's sized at the RatedCondenserWaterInlet temp, size and reset water
                                                 // inlet temp. If already sized, no harm.
                                if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                    HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                                } else {
                                    Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                                }
                                SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, CycFanCycCoil, 1.0);
                                Node(HPWaterHeater(HPNum).CondWaterInletNode).Temp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                            }
                            // ?? should only need to call twice if PLR<1 since this might affect OnOffFanPartLoadFraction which impacts fan energy.
                            // PLR=1 here.
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, CycFanCycCoil, 1.0);
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, CycFanCycCoil, 1.0);
                        } else {
                            if (FirstTimeFlag) { // first time DXCoil is called, it's sized at the RatedCondenserWaterInlet temp, size and reset water
                                                 // inlet temp. If already sized, no harm.
                                SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, CycFanCycCoil, 1.0);
                                Node(HPWaterHeater(HPNum).CondWaterInletNode).Temp = WaterThermalTank(WaterThermalTankNum).TankTemp;
                            }
                            // ?? should only need to call twice if PLR<1 since this might affect OnOffFanPartLoadFraction which impacts fan energy.
                            // PLR=1 here.
                            SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, CycFanCycCoil, 1.0);
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                            SimDXCoil(HPWaterHeater(HPNum).DXCoilName, 1, true, HPWaterHeater(HPNum).DXCoilNum, CycFanCycCoil, 1.0);
                            if (HPWaterHeater(HPNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                HVACFan::fanObjs[HPWaterHeater(HPNum).FanNum]->simulate(_, _, _, _);
                            } else {
                                Fans::SimulateFanComponents(HPWaterHeater(HPNum).FanName, true, HPWaterHeater(HPNum).FanNum);
                            }
                        }

                        WaterThermalTank(WaterThermalTankNum).MaxCapacity = HPWHHeatingCapacity;
                        WaterThermalTank(WaterThermalTankNum).MinCapacity = HPWHHeatingCapacity;
                        WaterThermalTank(WaterThermalTankNum).Efficiency = HPWHHeatingCOP;
                    }

                    if (FirstTimeFlag) {
                        RatedDXCoilTotalCapacity = DXCoilTotalCapacity;
                        FirstTimeFlag = false;
                    }

                    //       Switch the HPWH info with the tank info and call CalcWaterThermalTankMixed to get Standard Rating
                    //       (backup element is assumed to be disabled during the rating procedure)
                    WaterThermalTank(WaterThermalTankNum).SourceMassFlowRate = 0.0;
                    // WaterThermalTank( WaterThermalTankNum ).MaxCapacity = HPWHHeatingCapacity;
                    // WaterThermalTank( WaterThermalTankNum ).MinCapacity = HPWHHeatingCapacity;
                    // WaterThermalTank( WaterThermalTankNum ).Efficiency = HPWHHeatingCOP; //* WaterHeater(WaterHeaterNum)%Efficiency
                    WaterThermalTank(WaterThermalTankNum).OnCycParaLoad = HPWaterHeater(HPNum).OnCycParaLoad;
                    WaterThermalTank(WaterThermalTankNum).OffCycParaLoad = HPWaterHeater(HPNum).OffCycParaLoad;
                    WaterThermalTank(WaterThermalTankNum).OffCycParaFracToTank = 0.0;
                    WaterThermalTank(WaterThermalTankNum).OnCycParaFracToTank = 0.0;
                    WaterThermalTank(WaterThermalTankNum).PLFCurve = HPWaterHeater(HPNum).DXCoilPLFFPLR;

                    {
                        auto const SELECT_CASE_var(WaterThermalTank(WaterThermalTankNum).TypeNum);

                        if (SELECT_CASE_var == MixedWaterHeater) {
                            if (WaterThermalTank(WaterThermalTankNum).Efficiency > 0.0) CalcWaterThermalTankMixed(WaterThermalTankNum);

                        } else if (SELECT_CASE_var == StratifiedWaterHeater) {
                            if (WaterThermalTank(WaterThermalTankNum).Efficiency > 0.0) CalcWaterThermalTankStratified(WaterThermalTankNum);

                        } else {
                            //         Unhandled water heater type
                        }
                    }

                    //       reset the water heater data to original values
                    WaterThermalTank(WaterThermalTankNum).MaxCapacity = HPWaterHeater(HPNum).BackupElementCapacity;
                    WaterThermalTank(WaterThermalTankNum).MinCapacity = HPWaterHeater(HPNum).BackupElementCapacity;
                    WaterThermalTank(WaterThermalTankNum).Efficiency = HPWaterHeater(HPNum).BackupElementEfficiency;
                    WaterThermalTank(WaterThermalTankNum).OnCycParaLoad = HPWaterHeater(HPNum).WHOnCycParaLoad;
                    WaterThermalTank(WaterThermalTankNum).OffCycParaLoad = HPWaterHeater(HPNum).WHOffCycParaLoad;
                    WaterThermalTank(WaterThermalTankNum).OnCycParaFracToTank = HPWaterHeater(HPNum).WHOnCycParaFracToTank;
                    WaterThermalTank(WaterThermalTankNum).OffCycParaFracToTank = HPWaterHeater(HPNum).WHOffCycParaFracToTank;
                    WaterThermalTank(WaterThermalTankNum).PLFCurve = HPWaterHeater(HPNum).WHPLFCurve;
                }

                FuelEnergy += (WaterThermalTank(WaterThermalTankNum).FuelRate + WaterThermalTank(WaterThermalTankNum).OffCycParaFuelRate +
                               WaterThermalTank(WaterThermalTankNum).OnCycParaFuelRate) *
                              SecInTimeStep;

            } // Step

            if (WaterThermalTank(WaterThermalTankNum).FirstRecoveryDone && WaterThermalTank(WaterThermalTankNum).FirstRecoveryFuel > 0.0) {
                // Calculate Recovery Efficiency based on energy used to recover from the first draw
                // FirstRecoveryFuel is recorded inside the CalcWaterThermalTank subroutine
                RecoveryEfficiency = DrawMass * CPHW(57.2222) * (57.2222 - 14.4444) / WaterThermalTank(WaterThermalTankNum).FirstRecoveryFuel;

                // Calculate Energy Factor based on total energy (including parasitics) used over entire test
                EnergyFactor = TotalDrawMass * CPHW(57.2222) * (57.2222 - 14.4444) / FuelEnergy;

            } else {
                RecoveryEfficiency = 0.0;
                EnergyFactor = 0.0;
                if (HPWaterHeater.empty() || !HPWaterHeater(HPNum).bIsIHP) {
                    ShowWarningError("Water heater = " + WaterThermalTank(WaterThermalTankNum).Name +
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
        if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum == 0) {
            equipName = WaterThermalTank(WaterThermalTankNum).Name;
            PreDefTableEntry(pdchSWHType, equipName, WaterThermalTank(WaterThermalTankNum).Type);
            PreDefTableEntry(pdchSWHVol, equipName, WaterThermalTank(WaterThermalTankNum).Volume);
            PreDefTableEntry(pdchSWHHeatIn, equipName, WaterThermalTank(WaterThermalTankNum).MaxCapacity);
            PreDefTableEntry(pdchSWHThEff, equipName, WaterThermalTank(WaterThermalTankNum).Efficiency);
            PreDefTableEntry(pdchSWHRecEff, equipName, RecoveryEfficiency);
            PreDefTableEntry(pdchSWHEnFac, equipName, EnergyFactor);
        } else {
            equipName = HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).Name;
            PreDefTableEntry(pdchSWHType, equipName, HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).Type);
            PreDefTableEntry(pdchSWHVol, equipName, WaterThermalTank(WaterThermalTankNum).Volume);
            if (bIsVSCoil) {
                PreDefTableEntry(pdchSWHHeatIn, equipName, VSHPWHHeatingCapacity);
            } else {
                PreDefTableEntry(pdchSWHHeatIn, equipName, HPWHHeatingCapacity);
            }
            PreDefTableEntry(pdchSWHThEff, equipName, WaterThermalTank(WaterThermalTankNum).Efficiency);
            PreDefTableEntry(pdchSWHRecEff, equipName, RecoveryEfficiency);
            PreDefTableEntry(pdchSWHEnFac, equipName, EnergyFactor);
        }

        // Write test results
        if (WaterThermalTank(WaterThermalTankNum).HeatPumpNum == 0) {

            if (WaterThermalTank(WaterThermalTankNum).TypeNum == StratifiedWaterHeater) {
                if (WaterThermalTank(WaterThermalTankNum).ControlType == PriorityMasterSlave) {
                    MaxCapacity = max(WaterThermalTank(WaterThermalTankNum).MaxCapacity, WaterThermalTank(WaterThermalTankNum).MaxCapacity2);
                } else { // PrioritySimultaneous
                    MaxCapacity = WaterThermalTank(WaterThermalTankNum).MaxCapacity + WaterThermalTank(WaterThermalTankNum).MaxCapacity2;
                }
            } else { // WaterHeaterMixed
                MaxCapacity = WaterThermalTank(WaterThermalTankNum).MaxCapacity;
            }

            gio::write(OutputFileInits, Format_720) << WaterThermalTank(WaterThermalTankNum).Type << WaterThermalTank(WaterThermalTankNum).Name
                                                    << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Volume, 4) << TrimSigDigits(MaxCapacity, 1)
                                                    << TrimSigDigits(RecoveryEfficiency, 3) << TrimSigDigits(EnergyFactor, 4);
        } else {
            gio::write(OutputFileInits, Format_721)
                << HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).Type
                << HPWaterHeater(WaterThermalTank(WaterThermalTankNum).HeatPumpNum).Name
                << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Volume, 4) << TrimSigDigits(HPWHHeatingCapacity, 1)
                << TrimSigDigits(RecoveryEfficiency, 3) << TrimSigDigits(EnergyFactor, 4) << TrimSigDigits(RatedDXCoilTotalCapacity, 0);
        }

        AlreadyRated(WaterThermalTankNum) = true;
    }

    void ReportCWTankInits(int const WaterThermalTankNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   March 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // send chilled water tank info to EIO

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // Using/Aliasing
        using General::TrimSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool MyOneTimeSetupFlag(true); // one time setup flag
        static Array1D_bool AlreadyReported;  // control so we don't repeat again

        // Formats
        static gio::Fmt Format_728("('Chilled Water Tank Information',5(',',A))");

        if (MyOneTimeSetupFlag) {
            AlreadyReported.dimension(NumWaterThermalTank, false);
            MyOneTimeSetupFlag = false;
        }

        if (AlreadyReported(WaterThermalTankNum)) { // bail we already did this one
            return;
        }

        gio::write(OutputFileInits, Format_728) << WaterThermalTank(WaterThermalTankNum).Type << WaterThermalTank(WaterThermalTankNum).Name
                                                << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).Volume, 4)
                                                << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).UseDesignVolFlowRate, 4)
                                                << TrimSigDigits(WaterThermalTank(WaterThermalTankNum).SourceDesignVolFlowRate, 4);

        AlreadyReported(WaterThermalTankNum) = true;
    }

    Real64 FindStratifiedTankSensedTemp(WaterThermalTankData const &Tank, bool UseAverage)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  Noel Merket, April 2015

        // PURPOSE OF THIS FUNCTION:
        // find tank temperature depending on how sensed

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        Real64 SensedTemp;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        HeatPumpWaterHeaterData const &HPWH = HPWaterHeater(Tank.HeatPumpNum);
        Real64 ControlSensor1Temp;
        Real64 ControlSensor2Temp;

        if (UseAverage) {
            ControlSensor1Temp = Tank.Node(HPWH.ControlSensor1Node).TempAvg;
            ControlSensor2Temp = Tank.Node(HPWH.ControlSensor2Node).TempAvg;
        } else {
            ControlSensor1Temp = Tank.Node(HPWH.ControlSensor1Node).Temp;
            ControlSensor2Temp = Tank.Node(HPWH.ControlSensor2Node).Temp;
        }

        SensedTemp = ControlSensor1Temp * HPWH.ControlSensor1Weight + ControlSensor2Temp * HPWH.ControlSensor2Weight;

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

    void clear_state()
    {
        ValidSourceType.deallocate();
        MyHPSizeFlag.deallocate();
        CheckWTTEquipName.deallocate();
        CheckHPWHEquipName.deallocate();

        NumChilledWaterMixed = 0;
        NumChilledWaterStratified = 0;
        NumWaterHeaterMixed = 0;
        NumWaterHeaterStratified = 0;
        NumWaterThermalTank = 0;
        NumWaterHeaterDesuperheater = 0;
        NumHeatPumpWaterHeater = 0;

        HPPartLoadRatio = 0.0;
        GetWaterThermalTankInputFlag = true;
        MixerInletAirSchedule = 0.0;
        MdotAir = 0.0;
        NumWaterHeaterSizing = 0;
        AlreadyRated.deallocate();

        SimWaterThermalTank_OneTimeSetupFlag = true;
        InitWaterThermalTanksOnce = true;
        CalcWaterThermalTankZoneGains_MyEnvrnFlag = true;
        WaterThermalTank.deallocate();
        UniqueWaterThermalTankNames.clear();
        HPWaterHeater.deallocate();
        WaterHeaterDesuperheater.deallocate();
    }

} // namespace WaterThermalTanks

} // namespace EnergyPlus
