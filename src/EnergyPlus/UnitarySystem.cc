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

#include <BranchInputManager.hh>
#include <BranchNodeConnections.hh>
#include <CurveManager.hh>
#include <DXCoils.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataAirflowNetwork.hh>
#include <DataGlobals.hh>
#include <DataHVACControllers.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalance.hh>
#include <DataPlant.hh>
#include <DataSizing.hh>
#include <DataZoneControls.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <Fans.hh>
#include <FaultsManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HVACFan.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <HeatingCoils.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutdoorAirUnit.hh>
#include <PackagedThermalStorageCoil.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportCoilSelection.hh>
#include <ReportSizingManager.hh>
#include <SZVAVModel.hh>
#include <ScheduleManager.hh>
#include <SetPointManager.hh>
#include <SimAirServingZones.hh>
#include <SingleDuct.hh>
#include <SteamCoils.hh>
#include <UnitarySystem.hh>
#include <UserDefinedComponents.hh>
#include <UtilityRoutines.hh>
#include <VariableSpeedCoils.hh>
#include <WaterCoils.hh>
#include <WaterToAirHeatPump.hh>
#include <WaterToAirHeatPumpSimple.hh>
#include <string> // std::string, std::to_string

namespace EnergyPlus {
namespace UnitarySystems {

    namespace {
        bool initUnitarySystemsErrFlag(false);
        bool initUnitarySystemsErrorsFound(false);
        bool initLoadBasedControlFlowFracFlagReady(true);
        Real64 initLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax(0.0);
        Real64 initUnitarySystemsQActual(0.0);
    } // namespace

    // MODULE PARAMETER DEFINITIONS
    bool economizerFlag(false);      // holds air loop economizer status
    bool SuppHeatingCoilFlag(false); // set to TRUE when simulating supplemental heating coil
    Real64 const MinAirMassFlow(0.001);
    int numUnitarySystems(0);
    bool myOneTimeFlag(true);
    bool getInputFlag(true);
    bool getInputOnceFlag(true);
    bool getMSHPInputOnceFlag(true);
    std::vector<UnitarySys> unitarySys;
    std::vector<DesignSpecMSHP> designSpecMSHP;
    static std::string const fluidNameSteam("STEAM");
    static std::string const blankString("");
    Real64 m_massFlow1(0.0);           // Mass flow rate in operating mode 1 (CompOn) [kg/s]
    Real64 m_massFlow2(0.0);           // Mass flow rate in operating mode 2 (CompOff) [kg/s]
    Real64 m_runTimeFraction1(0.0);    // Fan runtime fraction in operating mode 1 (CompOn)
    Real64 m_runTimeFraction2(0.0);    // Fan runtime fraction in operating mode 2 (CompOff)
    Real64 CompOnMassFlow(0.0);        // Supply air mass flow rate w/ compressor ON [kg/s]
    Real64 CompOffMassFlow(0.0);       // Supply air mass flow rate w/ compressor OFF [kg/s]
    Real64 CompOnFlowRatio(0.0);       // fan flow ratio when coil on
    Real64 CompOffFlowRatio(0.0);      // fan flow ratio when coil off
    Real64 FanSpeedRatio(0.0);         // ratio of air flow ratio passed to fan object
    Real64 CoolHeatPLRRat(1.0);        // ratio of cooling to heating PLR, used for cycling fan RH control
    Real64 OnOffAirFlowRatioSave(0.0); // Saves the OnOffAirFlowRatio calculated in RegulaFalsi calls.
    Real64 QToCoolSetPt(0.0);          // load to cooling set point {W}
    Real64 QToHeatSetPt(0.0);          // load to heating set point {W}
    bool HeatingLoad(false);           // True when zone needs heating
    bool CoolingLoad(false);           // True when zone needs cooling
    Real64 MoistureLoad(0.0);          // Dehumidification Load (W)

    // Supply Air Sizing Option
    int const None(1);
    int const SupplyAirFlowRate(2);
    int const FlowPerFloorArea(3);
    int const FractionOfAutoSizedCoolingValue(4);
    int const FractionOfAutoSizedHeatingValue(5);
    int const FlowPerCoolingCapacity(6);
    int const FlowPerHeatingCapacity(7);

    // Coil type for SimWater and SimSteamCoil
    int const CoolingCoil(0);
    int const HeatingCoil(1);
    int const SuppHeatCoil(2);

    // Last mode of operation
    int const CoolingMode(1); // last compressor operating mode was in cooling
    int const HeatingMode(2); // last compressor operating mode was in heating
    int const NoCoolHeat(3);  // last operating mode was no cooling or heating

    // Compressor operation
    int const On(1);  // normal compressor operation
    int const Off(0); // signal DXCoil that compressor shouldn't run

    DesignSpecMSHP::DesignSpecMSHP()
        : numOfSpeedHeating(0), numOfSpeedCooling(0), noLoadAirFlowRateRatio(1.0), m_DesignSpecMSHPType_Num(0), m_SingleModeFlag(false)
    {
    }

    UnitarySys::UnitarySys() // constructor
        : m_UnitarySysNum(-1), m_unitarySystemType_Num(0), m_ThisSysInputShouldBeGotten(true), m_SysAvailSchedPtr(0),
          m_ControlType(ControlType::None), m_DehumidControlType_Num(DehumCtrlType::None), m_Humidistat(false), m_ValidASHRAECoolCoil(false),
          m_ValidASHRAEHeatCoil(false), m_SimASHRAEModel(false), m_setFaultModelInput(true), m_FanIndex(0), m_FanPlace(FanPlace::NotYetSet),
          m_FanOpModeSchedPtr(0), m_FanExists(false), m_FanType_Num(0), m_RequestAutoSize(false), m_ActualFanVolFlowRate(0.0),
          m_DesignFanVolFlowRate(0.0), m_DesignMassFlowRate(0.0), m_FanAvailSchedPtr(0), m_FanOpMode(0), m_ATMixerIndex(0), m_ATMixerPriNode(0),
          m_ATMixerSecNode(0), m_AirLoopEquipment(true), m_ZoneInletNode(0), m_ZoneSequenceCoolingNum(0), m_ZoneSequenceHeatingNum(0),
          m_HeatCoilExists(false), m_HeatingSizingRatio(1.0), m_HeatingCoilType_Num(0), m_DXHeatingCoil(false), m_HeatingCoilIndex(0),
          m_HeatingCoilAvailSchPtr(0), m_DesignHeatingCapacity(0.0), m_MaxHeatAirVolFlow(0.0), m_NumOfSpeedHeating(0), m_MultiSpeedHeatingCoil(false),
          m_VarSpeedHeatingCoil(false), m_SystemHeatControlNodeNum(0), m_CoolCoilExists(false), m_CoolingCoilType_Num(0), m_NumOfSpeedCooling(0),
          m_CoolingCoilAvailSchPtr(0), m_DesignCoolingCapacity(0.0), m_MaxCoolAirVolFlow(0.0), m_CondenserNodeNum(0), m_CondenserType(0),
          m_CoolingCoilIndex(0), m_HeatPump(false), m_ActualDXCoilIndexForHXAssisted(0), m_MultiSpeedCoolingCoil(false), m_VarSpeedCoolingCoil(false),
          m_SystemCoolControlNodeNum(0), m_WaterCyclingMode(0), m_ISHundredPercentDOASDXCoil(false), m_RunOnSensibleLoad(false),
          m_RunOnLatentLoad(false), m_RunOnLatentOnlyWithSensible(false), m_DehumidificationMode(0), m_SuppHeatCoilType_Num(0),
          m_SuppCoilExists(false), m_DesignSuppHeatingCapacity(0.0), m_SuppCoilAirInletNode(0), m_SuppCoilAirOutletNode(0),
          m_SuppCoilFluidInletNode(0), m_MaxSuppCoilFluidFlow(0.0), m_SuppHeatCoilIndex(0), m_SuppHeatControlNodeNum(0), m_SupHeaterLoad(0.0),
          m_CoolingSAFMethod(0), m_HeatingSAFMethod(0), m_NoCoolHeatSAFMethod(0), m_MaxNoCoolHeatAirVolFlow(0.0),
          m_AirFlowControl(UseCompFlow::FlowNotYetSet), m_CoolingCoilUpstream(true), m_MaxOATSuppHeat(0.0), m_MinOATCompressorCooling(0.0),
          m_MinOATCompressorHeating(0.0), m_MaxONOFFCyclesperHour(0.0), m_HPTimeConstant(0.0), m_OnCyclePowerFraction(0.0), m_FanDelayTime(0.0),
          m_AncillaryOnPower(0.0), m_AncillaryOffPower(0.0), m_DesignHRWaterVolumeFlow(0.0), m_MaxHROutletWaterTemp(0.0), m_HeatRecActive(false),
          m_HeatRecoveryInletNodeNum(0), m_HeatRecoveryOutletNodeNum(0), m_DesignSpecMSHPIndex(-1), m_NoLoadAirFlowRateRatio(1.0),
          m_IdleMassFlowRate(0.0), m_IdleVolumeAirRate(0.0), m_IdleSpeedRatio(0.0), m_SingleMode(0), m_MultiOrVarSpeedHeatCoil(false),
          m_MultiOrVarSpeedCoolCoil(false), m_PartLoadFrac(0.0), m_CoolingPartLoadFrac(0.0), m_HeatingPartLoadFrac(0.0), m_SuppHeatPartLoadFrac(0.0),
          m_HeatCompPartLoadRatio(0.0), m_CoolCompPartLoadRatio(0.0), m_SpeedRatio(0.0), m_CycRatio(0.0), m_MyEnvrnFlag(true), m_MyEnvrnFlag2(true),
          m_MyPlantScanFlag(true), m_MySuppCoilPlantScanFlag(true), m_MySetPointCheckFlag(true), m_MySizingCheckFlag(true), m_InitHeatPump(false),
          m_HRLoopNum(0), m_HRLoopSideNum(0), m_HRBranchNum(0), m_HRCompNum(0), m_SuppCoilLoopNum(0), m_SuppCoilLoopSide(0), m_SuppCoilBranchNum(0),
          m_SuppCoilCompNum(0), m_SuppCoilFluidOutletNodeNum(0), m_WSHPRuntimeFrac(0.0), m_CompPartLoadRatio(0.0), m_CoolingCoilSensDemand(0.0),
          m_CoolingCoilLatentDemand(0.0), m_HeatingCoilSensDemand(0.0), m_SenLoadLoss(0.0), m_LatLoadLoss(0.0), m_DesignHeatRecMassFlowRate(0.0),
          m_HeatRecoveryMassFlowRate(0.0), m_HeatRecoveryRate(0.0), m_HeatRecoveryEnergy(0.0), m_HeatRecoveryInletTemp(0.0),
          m_HeatRecoveryOutletTemp(0.0), m_IterationCounter(0), m_DesiredOutletTemp(0.0), m_DesiredOutletHumRat(0.0), m_FrostControlStatus(0),
          m_CoolingCycRatio(0.0), m_CoolingSpeedRatio(0.0), m_CoolingSpeedNum(0), m_HeatingCycRatio(0.0), m_HeatingSpeedRatio(0.0),
          m_HeatingSpeedNum(0), m_SpeedNum(0), m_DehumidInducedHeatingDemandRate(0.0), m_TotalAuxElecPower(0.0), m_HeatingAuxElecConsumption(0.0),
          m_CoolingAuxElecConsumption(0.0), m_ElecPower(0.0), m_ElecPowerConsumption(0.0), m_LastMode(0), m_FirstPass(true), m_TotCoolEnergyRate(0.0),
          m_SensCoolEnergyRate(0.0), m_LatCoolEnergyRate(0.0), m_TotHeatEnergyRate(0.0), m_SensHeatEnergyRate(0.0), m_LatHeatEnergyRate(0.0),
          m_DesignFanVolFlowRateEMSOverrideOn(false), m_MaxHeatAirVolFlowEMSOverrideOn(false), m_MaxCoolAirVolFlowEMSOverrideOn(false),
          m_MaxNoCoolHeatAirVolFlowEMSOverrideOn(false), m_DesignFanVolFlowRateEMSOverrideValue(0.0), m_MaxHeatAirVolFlowEMSOverrideValue(0.0),
          m_MaxCoolAirVolFlowEMSOverrideValue(0.0), m_MaxNoCoolHeatAirVolFlowEMSOverrideValue(0.0), m_EMSOverrideSensZoneLoadRequest(false),
          m_EMSOverrideMoistZoneLoadRequest(false), m_EMSSensibleZoneLoadValue(0.0), m_EMSMoistureZoneLoadValue(0.0), m_StageNum(0), m_Staged(false),
          m_HeatingFanSpeedRatio(0.0), m_CoolingFanSpeedRatio(0.0), m_NoHeatCoolSpeedRatio(0.0), m_MyFanFlag(true), m_MyCheckFlag(true),
          m_SensibleLoadMet(0.0), m_LatentLoadMet(0.0), m_MyStagedFlag(false), m_SensibleLoadPredicted(0.0), m_MoistureLoadPredicted(0.0),
          m_FaultyCoilSATFlag(false), m_FaultyCoilSATIndex(0), m_FaultyCoilSATOffset(0.0), m_TESOpMode(0), m_initLoadBasedControlAirLoopPass(false),
          m_airLoopPassCounter(0), m_airLoopReturnCounter(0), m_FanCompNotSetYet(true), m_CoolCompNotSetYet(true), m_HeatCompNotSetYet(true),
          m_SuppCompNotSetYet(true), m_OKToPrintSizing(false), UnitarySystemType_Num(0), MaxIterIndex(0), RegulaFalsiFailedIndex(0),
          NodeNumOfControlledZone(0), FanPartLoadRatio(0.0), CoolCoilWaterFlowRatio(0.0), HeatCoilWaterFlowRatio(0.0), ControlZoneNum(0),
          AirInNode(0), AirOutNode(0), MaxCoolAirMassFlow(0.0), MaxHeatAirMassFlow(0.0), MaxNoCoolHeatAirMassFlow(0.0), DesignMinOutletTemp(0.0),
          DesignMaxOutletTemp(0.0), LowSpeedCoolFanRatio(0.0), LowSpeedHeatFanRatio(0.0), MaxCoolCoilFluidFlow(0.0), MaxHeatCoilFluidFlow(0.0),
          CoolCoilInletNodeNum(0), CoolCoilOutletNodeNum(0), CoolCoilFluidOutletNodeNum(0), CoolCoilLoopNum(0), CoolCoilLoopSide(0),
          CoolCoilBranchNum(0), CoolCoilCompNum(0), CoolCoilFluidInletNode(0), HeatCoilLoopNum(0), HeatCoilLoopSide(0), HeatCoilBranchNum(0),
          HeatCoilCompNum(0), HeatCoilFluidInletNode(0), HeatCoilFluidOutletNodeNum(0), HeatCoilInletNodeNum(0), HeatCoilOutletNodeNum(0),
          ATMixerExists(false), ATMixerType(0), ATMixerOutNode(0), ControlZoneMassFlowFrac(0.0), m_CompPointerMSHP(nullptr)
    {
    }

    // Clears the global data in UnitarySystem.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        numUnitarySystems = 0;
        HeatingLoad = false;
        CoolingLoad = false;
        MoistureLoad = 0.0;
        SuppHeatingCoilFlag = false;
        CompOnMassFlow = 0.0;
        CompOffMassFlow = 0.0;
        CompOnFlowRatio = 0.0;
        CompOffFlowRatio = 0.0;
        FanSpeedRatio = 0.0;
        CoolHeatPLRRat = 1.0;
        OnOffAirFlowRatioSave = 0.0;
        QToCoolSetPt = 0.0;
        QToHeatSetPt = 0.0;
        m_massFlow1 = 0.0;
        m_massFlow2 = 0.0;
        m_runTimeFraction1 = 0.0;
        m_runTimeFraction2 = 0.0;

        initUnitarySystemsErrFlag = false;
        initUnitarySystemsErrorsFound = false;
        initLoadBasedControlFlowFracFlagReady = true;
        initLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax = 0.0;
        initUnitarySystemsQActual = 0.0;
        getMSHPInputOnceFlag = true;
        getInputOnceFlag = true;
        unitarySys.clear();
        if (designSpecMSHP.size() > 0) designSpecMSHP.clear();
    }

    void UnitarySys::simulate(std::string const &Name,
                              bool const FirstHVACIteration,
                              int const &AirLoopNum,
                              int &CompIndex,
                              bool &HeatActive,
                              bool &CoolActive,
                              int const ZoneOAUnitNum,
                              Real64 const OAUCoilOutTemp,
                              bool const ZoneEquipment)
    {
        int CompOn = 0;

        // Obtains and Allocates unitary system related parameters from input file
        if (this->m_ThisSysInputShouldBeGotten) {
            // Get the unitary system input
            getUnitarySystemInput(Name, ZoneEquipment, ZoneOAUnitNum);
        }
        CompIndex = this->m_UnitarySysNum;

        FanSpeedRatio = 1.0;
        if (ZoneEquipment) {
            this->initUnitarySystems(0, FirstHVACIteration, ZoneOAUnitNum, OAUCoilOutTemp);
        } else {
            this->initUnitarySystems(AirLoopNum, FirstHVACIteration, ZoneOAUnitNum, OAUCoilOutTemp);
        }

        // MassFlowRateMaxAvail issues are impeding non-VAV air loop equipment by limiting air flow
        // temporarily open up flow limits while simulating, and then set this same value at the INLET after this parent has simulated
        Real64 tempMassFlowRateMaxAvail = DataLoopNode::Node(this->AirInNode).MassFlowRateMaxAvail;
        DataLoopNode::Node(this->AirInNode).MassFlowRateMaxAvail = this->m_DesignMassFlowRate;

        if (this->m_OKToPrintSizing) {
            bool HXUnitOn = false;
            {
                auto const SELECT_CASE_var(this->m_ControlType);
                if (SELECT_CASE_var == ControlType::Setpoint) {
                    if (ZoneEquipment) {
                        this->controlUnitarySystemtoSP(0, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                    } else {
                        this->controlUnitarySystemtoSP(AirLoopNum, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                    }
                } else if (SELECT_CASE_var == ControlType::Load || SELECT_CASE_var == ControlType::CCMASHRAE) {
                    if (ZoneEquipment) {
                        this->controlUnitarySystemtoLoad(0, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                    } else {
                        this->controlUnitarySystemtoLoad(AirLoopNum, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                    }
                }
            }
        }

        // Report the current output
        if (ZoneEquipment) {
            this->reportUnitarySystem(0);
        } else {
            this->reportUnitarySystem(AirLoopNum);
        }

        CoolActive = false;
        if (this->m_CoolingPartLoadFrac * double(CompOn) > 0.0) CoolActive = true;
        HeatActive = false;
        if (this->m_HeatingPartLoadFrac * double(CompOn) > 0.0 || this->m_SuppHeatPartLoadFrac * double(CompOn) > 0.0) HeatActive = true;

        // set econo lockout flag
        // If the sysem is not an equipment of Outdoor air unit
        //  IF (AirLoopNum /=-1 .AND. ALLOCATED(AirLoopControlInfo) .AND. UnitarySystem(UnitarySysNum)%AirLoopEquipment) THEN
        if (AirLoopNum > 0 && allocated(DataAirLoop::AirLoopControlInfo) && this->m_AirLoopEquipment) {

            if ((this->m_HeatCompPartLoadRatio > 0.0 || this->m_SpeedRatio > 0.0 || this->m_CycRatio > 0.0) &&
                DataAirLoop::AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor) {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithCompressor = true;
            } else {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithCompressor = false;
            }

            if ((HeatActive) && (DataAirLoop::AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor ||
                                 DataAirLoop::AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithHeating)) {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithHeating = true;
            } else {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithHeating = false;
            }
        }

        // Calculate heat recovery
        if (this->m_HeatRecActive) {
            this->unitarySystemHeatRecovery();
        }

        // Coils should have been sized by now. Set this flag to false in case other equipment is downstream of Unitary System.
        // No, can't do this since there are other checks that need this flag (e.g., HVACManager, SetHeatToReturnAirFlag())
        //  AirLoopControlInfo(AirLoopNum)%UnitarySys = .FALSE.

        DataLoopNode::Node(this->AirInNode).MassFlowRateMaxAvail = tempMassFlowRateMaxAvail;
    }

    DesignSpecMSHP *DesignSpecMSHP::factory(int object_type_of_num, std::string const objectName)
    {

        if (getMSHPInputOnceFlag) {
            DesignSpecMSHP::getDesignSpecMSHP();
            getMSHPInputOnceFlag = false;
        }
        for (auto &dSpec : designSpecMSHP) {
            if (UtilityRoutines::SameString(dSpec.name, objectName) && dSpec.m_DesignSpecMSHPType_Num == object_type_of_num) {
                return &dSpec;
            }
        }
        ShowSevereError("Design Specification MultiSpeed Heat Pump factory: Error getting inputs for system named: " + objectName);
        return nullptr;
    }

    void DesignSpecMSHP::getDesignSpecMSHP()
    {
        bool errorsFound(false);

        DesignSpecMSHP::getDesignSpecMSHPdata(errorsFound);

        if (errorsFound) {
            ShowFatalError("Design Specification MultiSpeed Heat Pump: Previous errors cause termination.");
        }
    }

    void DesignSpecMSHP::getDesignSpecMSHPdata(bool errorsFound)
    {
        std::string cCurrentModuleObject = "UnitarySystemPerformance:Multispeed";

        auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == inputProcessor->epJSON.end()) {
            errorsFound = true;
        } else {
            int designSpecNum = 0;
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                // *************** used only to eliminate unused object warning when using only Json type getInput **********
                int TotalArgs = 0;
                int NumAlphas = 0;
                int NumNumbers = 0;
                inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);
                int IOStatus = 0;
                Array1D_string Alphas(NumAlphas);
                Array1D<Real64> Numbers(NumNumbers, 0.0);
                Array1D_bool lNumericBlanks(NumNumbers, true);
                Array1D_bool lAlphaBlanks(NumAlphas, true);
                Array1D_string cAlphaFields(NumAlphas);
                Array1D_string cNumericFields(NumNumbers);
                inputProcessor->getObjectItem(cCurrentModuleObject,
                                              ++designSpecNum,
                                              Alphas,
                                              NumAlphas,
                                              Numbers,
                                              NumNumbers,
                                              IOStatus,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);
                // **********************************************************************************************************

                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                DesignSpecMSHP thisDesignSpec;

                thisDesignSpec.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisDesignSpec.numOfSpeedHeating = fields.at("number_of_speeds_for_heating"); // required field
                thisDesignSpec.numOfSpeedCooling = fields.at("number_of_speeds_for_cooling"); // required field
                int maxSpeeds = max(thisDesignSpec.numOfSpeedHeating, thisDesignSpec.numOfSpeedCooling);
                thisDesignSpec.m_DesignSpecMSHPType_Num = 1; // add global int value for factory

                std::string loc_m_SingleModeOp("No");
                if (fields.find("single_mode_operation") != fields.end()) { // not required field
                    loc_m_SingleModeOp = UtilityRoutines::MakeUPPERCase(fields.at("single_mode_operation"));
                }
                // set single mode flag
                if (UtilityRoutines::SameString(loc_m_SingleModeOp, "Yes")) {
                    thisDesignSpec.m_SingleModeFlag = true;
                } else if (UtilityRoutines::SameString(loc_m_SingleModeOp, "No")) {
                    thisDesignSpec.m_SingleModeFlag = false;
                } else {
                }

                Real64 loc_m_NoLoadAirFlowRateRatio(1.0);
                if (fields.find("no_load_supply_air_flow_rate_ratio") != fields.end()) { // not required field
                    loc_m_NoLoadAirFlowRateRatio = fields.at("no_load_supply_air_flow_rate_ratio");
                }
                thisDesignSpec.noLoadAirFlowRateRatio = loc_m_NoLoadAirFlowRateRatio;

                thisDesignSpec.heatingVolFlowRatio.resize(maxSpeeds + 1);
                thisDesignSpec.coolingVolFlowRatio.resize(maxSpeeds + 1);

                auto speedFlowRatios = fields.find("flow_ratios");
                if (speedFlowRatios != fields.end()) {
                    auto flowRatioArray = speedFlowRatios.value();
                    int numSpeedInputs = flowRatioArray.size();
                    if (numSpeedInputs >= maxSpeeds) {
                        int speedNum = -1;
                        for (auto flowRatio : flowRatioArray) {
                            speedNum += 1;
                            auto m_CoolingSpeedRatioObject = flowRatio.at("cooling_speed_supply_air_flow_ratio");
                            if (m_CoolingSpeedRatioObject == "Autosize") {
                                if (speedNum < (maxSpeeds + 1)) thisDesignSpec.coolingVolFlowRatio[speedNum] = -99999;
                            } else {
                                if (speedNum < (maxSpeeds + 1)) thisDesignSpec.coolingVolFlowRatio[speedNum] = m_CoolingSpeedRatioObject;
                            }
                            auto m_HeatingSpeedRatioObject = flowRatio.at("heating_speed_supply_air_flow_ratio");
                            if (m_HeatingSpeedRatioObject == "Autosize") {
                                if (speedNum < (maxSpeeds + 1)) thisDesignSpec.heatingVolFlowRatio[speedNum] = -99999;
                            } else {
                                if (speedNum < (maxSpeeds + 1)) thisDesignSpec.heatingVolFlowRatio[speedNum] = m_HeatingSpeedRatioObject;
                            }
                        }
                    } else if (numSpeedInputs < maxSpeeds) {
                        ShowSevereError(cCurrentModuleObject + ": Error getting inputs for system named: " + thisObjectName);
                        ShowContinueError("Number of speed inputs (" + General::TrimSigDigits(Real64(numSpeedInputs), 0) +
                                          " is less than number of speeds (" + General::TrimSigDigits(Real64(maxSpeeds), 0) + ").");
                        errorsFound = true;
                    }
                }
                designSpecMSHP.push_back(thisDesignSpec);
            }
        }
    } // namespace UnitarySystems

    UnitarySys *UnitarySys::factory(int const object_type_of_num, std::string const objectName, bool const ZoneEquipment, int const ZoneOAUnitNum)
    {
        if (UnitarySystems::getInputOnceFlag) {
            UnitarySys::getUnitarySystemInput(objectName, ZoneEquipment, ZoneOAUnitNum);
            UnitarySystems::getInputOnceFlag = false;
        }
        int sysNum = -1;
        for (auto &sys : unitarySys) {
            ++sysNum;
            if (UtilityRoutines::SameString(sys.Name, objectName) && object_type_of_num == DataHVACGlobals::UnitarySys_AnyCoilType) {
                unitarySys[sysNum].m_UnitarySysNum = sysNum;
                return &sys;
            }
        }
        ShowFatalError("UnitarySystem factory: Error getting inputs for " + DataHVACGlobals::cFurnaceTypes(DataHVACGlobals::UnitarySys_AnyCoilType) +
                       " named: " + objectName);
        return nullptr;
    }

    int getDesignSpecMSHPIndex(       // lookup vector index for design spec object name in object array EnergyPlus::UnitarySystems::designSpecMSHP
        std::string const &objectName // IDF name in input
    )
    {
        int index = -1;
        for (std::size_t loop = 0; loop < designSpecMSHP.size(); ++loop) {
            DesignSpecMSHP *thisDesignSpecMSHPObjec = &designSpecMSHP[loop];
            if (UtilityRoutines::SameString(objectName, thisDesignSpecMSHPObjec->name)) {
                index = loop;
                return index;
            }
        }
        ShowSevereError("getDesignSpecMSHPIndex: did not find UnitarySystemPerformance:Multispeed name =" + objectName + ". Check inputs");
        return index;
    }

    int getUnitarySystemIndex(        // lookup vector index for UnitarySystem object name in object array EnergyPlus::UnitarySystems::unitarySys
        std::string const &objectName // IDF name in input
    )
    {
        int index = -1;
        bool found = false;
        for (std::size_t loop = 0; loop < unitarySys.size(); ++loop) {
            UnitarySys *thisUnitarySysObjec = &unitarySys[loop];
            if (UtilityRoutines::SameString(objectName, thisUnitarySysObjec->Name)) {
                index = loop;
                found = true;
                break;
            }
        }
        return index;
    }

    void UnitarySys::initUnitarySystems(int const &AirLoopNum, bool const &FirstHVACIteration, int const ZoneOAUnitNum, Real64 const OAUCoilOutTemp)
    {
        static std::string const routineName("InitUnitarySystems");
        bool errorsFound = false; // error flag for mining functions

        if (myOneTimeFlag) {
            // initialize or allocate something once
            myOneTimeFlag = false;
        }

        if (!DataGlobals::SysSizingCalc && this->m_MySizingCheckFlag && !this->m_ThisSysInputShouldBeGotten) {
            if (AirLoopNum > 0) {
                if (this->m_FanExists && (this->m_CoolCoilExists && (this->m_HeatCoilExists || this->m_SuppCoilExists)))
                    DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySys = true;
                DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating = true;
            }
            this->sizeUnitarySystem(FirstHVACIteration, AirLoopNum);
            this->m_MySizingCheckFlag = false;
            if (AirLoopNum > 0) {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).FanOpMode = this->m_FanOpMode;
                DataAirLoop::AirLoopControlInfo(AirLoopNum).CycFanSchedPtr = this->m_FanOpModeSchedPtr;
            } else if (AirLoopNum < 0) {
                if (this->m_ControlType == ControlType::CCMASHRAE) {
                    ShowSevereError(this->UnitType + ": " + this->Name);
                    ShowContinueError("  Invalid application of Control Type = SingleZoneVAV in outdoor air system.");
                    ShowFatalError("InitUnitarySystems: Program terminated for previous conditions.");
                }
            }
        }

        if (this->m_MyFanFlag) {
            std::string FanType = ""; // used in warning messages
            std::string FanName = ""; // used in warning messages
            if (this->m_ActualFanVolFlowRate != DataSizing::AutoSize) {
                if (this->m_ActualFanVolFlowRate > 0.0) {
                    this->m_HeatingFanSpeedRatio = this->m_MaxHeatAirVolFlow / this->m_ActualFanVolFlowRate;
                    this->m_CoolingFanSpeedRatio = this->m_MaxCoolAirVolFlow / this->m_ActualFanVolFlowRate;
                    this->m_NoHeatCoolSpeedRatio = this->m_MaxNoCoolHeatAirVolFlow / this->m_ActualFanVolFlowRate;
                    if (this->m_FanExists && !this->m_MultiOrVarSpeedHeatCoil && !this->m_MultiOrVarSpeedCoolCoil) {
                        bool fanHasPowerSpeedRatioCurve = false;
                        if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            if (HVACFan::fanObjs[this->m_FanIndex]->powerModFuncFlowFractionCurveIndex > 0) fanHasPowerSpeedRatioCurve = true;
                            FanType = "Fan:SystemModel";
                            FanName = this->m_FanName;
                        } else {
                            if (Fans::GetFanSpeedRatioCurveIndex(FanType, FanName, this->m_FanIndex) > 0) fanHasPowerSpeedRatioCurve = true;
                        }
                        if (fanHasPowerSpeedRatioCurve) {

                            if (this->m_ActualFanVolFlowRate == this->m_MaxHeatAirVolFlow &&
                                this->m_ActualFanVolFlowRate == this->m_MaxCoolAirVolFlow &&
                                this->m_ActualFanVolFlowRate == this->m_MaxNoCoolHeatAirVolFlow) {
                                ShowWarningError(this->UnitType + " \"" + this->Name + "\"");
                                ShowContinueError("...For fan type and name = " + FanType + " \"" + FanName + "\"");
                                ShowContinueError("...Fan power ratio function of speed ratio curve has no impact if fan volumetric flow rate is the "
                                                  "same as the unitary system volumetric flow rate.");
                                ShowContinueError("...Fan volumetric flow rate            = " +
                                                  General::RoundSigDigits(this->m_ActualFanVolFlowRate, 5) + " m3/s.");
                                ShowContinueError(
                                    "...Unitary system volumetric flow rate = " + General::RoundSigDigits(this->m_MaxHeatAirVolFlow, 5) + " m3/s.");
                            }
                        }
                    }
                    if (this->m_MultiOrVarSpeedHeatCoil || this->m_MultiOrVarSpeedCoolCoil) {
                        if (this->m_MultiOrVarSpeedCoolCoil) {
                            int NumSpeeds = this->m_NumOfSpeedCooling;
                            if (this->m_MSCoolingSpeedRatio.size() == 0) this->m_MSCoolingSpeedRatio.resize(NumSpeeds);
                            for (int Iter = 1; Iter <= NumSpeeds; ++Iter) {
                                this->m_MSCoolingSpeedRatio[Iter] = this->m_CoolVolumeFlowRate[Iter] / this->m_ActualFanVolFlowRate;
                            }
                        }
                        if (this->m_MultiOrVarSpeedHeatCoil) {
                            int NumSpeeds = this->m_NumOfSpeedHeating;
                            if (this->m_MSHeatingSpeedRatio.size() == 0) this->m_MSHeatingSpeedRatio.resize(NumSpeeds);
                            for (int Iter = 1; Iter <= NumSpeeds; ++Iter) {
                                this->m_MSHeatingSpeedRatio[Iter] = this->m_HeatVolumeFlowRate[Iter] / this->m_ActualFanVolFlowRate;
                            }
                        }
                        this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_ActualFanVolFlowRate;
                    }
                }
                this->m_MyFanFlag = false;
            } else {
                if (this->m_FanExists) {
                    if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        this->m_ActualFanVolFlowRate = HVACFan::fanObjs[this->m_FanIndex]->designAirVolFlowRate;
                    } else {
                        this->m_ActualFanVolFlowRate = Fans::GetFanDesignVolumeFlowRate(blankString, blankString, errorsFound, this->m_FanIndex);
                    }
                }
            }
        }

        // why is this here, it doesn't do anything
        int OutdoorAirUnitNum = 0;
        int OAUCoilOutletTemp = 0.0;
        if (AirLoopNum == -1) { // This DX system is component of ZoneHVAC:OutdoorAirUnit
            OutdoorAirUnitNum = ZoneOAUnitNum;
            OAUCoilOutletTemp = OAUCoilOutTemp;
        }

        // Scan hot water and steam heating coil plant components for one time initializations
        if (this->m_MyPlantScanFlag && allocated(DataPlant::PlantLoop)) {
            if (this->m_HeatRecActive) {
                initUnitarySystemsErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(this->Name,
                                                        DataPlant::TypeOf_UnitarySysRecovery,
                                                        this->m_HRLoopNum,
                                                        this->m_HRLoopSideNum,
                                                        this->m_HRBranchNum,
                                                        this->m_HRCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        initUnitarySystemsErrFlag);
                if (initUnitarySystemsErrFlag) {
                    ShowFatalError("InitUnitarySystems: Program terminated for previous conditions.");
                }
            }
            int TypeOfCoilWaterCooling = 0;
            std::string CoolingCoilType = "";
            std::string CoolingCoilName = "";
            if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed ||
                this->m_CoolingCoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted) {
                if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater) {
                    TypeOfCoilWaterCooling = DataPlant::TypeOf_CoilWaterCooling;
                    CoolingCoilType = "Coil:Cooling:Water";
                    CoolingCoilName = this->m_CoolingCoilName;
                } else if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {
                    TypeOfCoilWaterCooling = DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
                    CoolingCoilType = "Coil:Cooling:Water:DetailedGeometry";
                    CoolingCoilName = this->m_CoolingCoilName;
                } else {
                    TypeOfCoilWaterCooling = HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(
                        DataHVACGlobals::cAllCoilTypes(this->m_CoolingCoilType_Num), this->m_CoolingCoilName, initUnitarySystemsErrFlag, true);
                    if (TypeOfCoilWaterCooling == DataHVACGlobals::Coil_CoolingWater) {
                        TypeOfCoilWaterCooling = DataPlant::TypeOf_CoilWaterCooling;
                        CoolingCoilType = "Coil:Cooling:Water";
                    } else if (TypeOfCoilWaterCooling == DataHVACGlobals::Coil_CoolingWaterDetailed) {
                        TypeOfCoilWaterCooling = DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
                        CoolingCoilType = "Coil:Cooling:Water:DetailedGeometry";
                    }
                    CoolingCoilName = HVACHXAssistedCoolingCoil::GetHXDXCoilName(
                        DataHVACGlobals::cAllCoilTypes(this->m_CoolingCoilType_Num), this->m_CoolingCoilName, initUnitarySystemsErrFlag);
                }
                initUnitarySystemsErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(CoolingCoilName,
                                                        TypeOfCoilWaterCooling,
                                                        this->CoolCoilLoopNum,
                                                        this->CoolCoilLoopSide,
                                                        this->CoolCoilBranchNum,
                                                        this->CoolCoilCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        initUnitarySystemsErrFlag);
                if (initUnitarySystemsErrFlag) {
                    ShowFatalError("InitUnitarySystem: Program terminated for previous conditions.");
                }
                this->MaxCoolCoilFluidFlow = WaterCoils::GetCoilMaxWaterFlowRate(CoolingCoilType, CoolingCoilName, initUnitarySystemsErrorsFound);

                if (this->MaxCoolCoilFluidFlow > 0.0) {
                    Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CoolCoilLoopNum).FluidName,
                                                                   DataGlobals::CWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->CoolCoilLoopNum).FluidIndex,
                                                                   routineName);
                    this->MaxCoolCoilFluidFlow *= rho;
                }
                // fill outlet node for coil
                this->CoolCoilFluidOutletNodeNum = DataPlant::PlantLoop(this->CoolCoilLoopNum)
                                                       .LoopSide(this->CoolCoilLoopSide)
                                                       .Branch(this->CoolCoilBranchNum)
                                                       .Comp(this->CoolCoilCompNum)
                                                       .NodeNumOut;
            }
            int TypeOfCoilWaterHeating = 0;
            std::string HeatingCoilType = "";
            if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater ||
                this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                    TypeOfCoilWaterHeating = DataPlant::TypeOf_CoilWaterSimpleHeating;
                    HeatingCoilType = "Coil:Heating:Water";
                    WaterCoils::SetCoilDesFlow(DataHVACGlobals::cAllCoilTypes(this->m_HeatingCoilType_Num),
                                               this->m_HeatingCoilName,
                                               this->m_MaxHeatAirVolFlow,
                                               initUnitarySystemsErrorsFound);
                } else {
                    TypeOfCoilWaterHeating = DataPlant::TypeOf_CoilSteamAirHeating;
                    HeatingCoilType = "Coil:Heating:Steam";
                }
                initUnitarySystemsErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(this->m_HeatingCoilName,
                                                        TypeOfCoilWaterHeating,
                                                        this->HeatCoilLoopNum,
                                                        this->HeatCoilLoopSide,
                                                        this->HeatCoilBranchNum,
                                                        this->HeatCoilCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        initUnitarySystemsErrFlag);
                if (initUnitarySystemsErrFlag) {
                    ShowFatalError("InitUnitarySystem: Program terminated for previous conditions.");
                }
                if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                    this->MaxHeatCoilFluidFlow =
                        WaterCoils::GetCoilMaxWaterFlowRate(HeatingCoilType, this->m_HeatingCoilName, initUnitarySystemsErrorsFound);

                    if (this->MaxHeatCoilFluidFlow > 0.0) {
                        Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HeatCoilLoopNum).FluidName,
                                                                       DataGlobals::HWInitConvTemp,
                                                                       DataPlant::PlantLoop(this->HeatCoilLoopNum).FluidIndex,
                                                                       routineName);
                        this->MaxHeatCoilFluidFlow =
                            WaterCoils::GetCoilMaxWaterFlowRate(HeatingCoilType, this->m_HeatingCoilName, initUnitarySystemsErrorsFound) * rho;
                    }
                } else {
                    this->MaxHeatCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(this->m_HeatingCoilIndex, initUnitarySystemsErrorsFound);
                    if (this->MaxHeatCoilFluidFlow > 0.0) {
                        int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        Real64 TempSteamIn = 100.0;
                        Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                        this->MaxHeatCoilFluidFlow *= SteamDensity;
                    }
                }
                // fill outlet node for coil
                this->HeatCoilFluidOutletNodeNum = DataPlant::PlantLoop(this->HeatCoilLoopNum)
                                                       .LoopSide(this->HeatCoilLoopSide)
                                                       .Branch(this->HeatCoilBranchNum)
                                                       .Comp(this->HeatCoilCompNum)
                                                       .NodeNumOut;
            }

            this->m_MyPlantScanFlag = false;

        } else if (this->m_MyPlantScanFlag && !DataGlobals::AnyPlantInModel) {
            this->m_MyPlantScanFlag = false;
        }

        // Scan Supplemental hot water and steam heating coil plant components for one time initializations
        if (this->m_MySuppCoilPlantScanFlag && allocated(DataPlant::PlantLoop)) {
            if (this->m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                initUnitarySystemsErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(this->m_SuppHeatCoilName,
                                                        DataPlant::TypeOf_CoilWaterSimpleHeating,
                                                        this->m_SuppCoilLoopNum,
                                                        this->m_SuppCoilLoopSide,
                                                        this->m_SuppCoilBranchNum,
                                                        this->m_SuppCoilCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        initUnitarySystemsErrFlag);
                WaterCoils::SetCoilDesFlow(DataHVACGlobals::cAllCoilTypes(this->m_SuppHeatCoilType_Num),
                                           this->m_SuppHeatCoilName,
                                           this->m_MaxHeatAirVolFlow,
                                           initUnitarySystemsErrorsFound);

                if (initUnitarySystemsErrFlag) {
                    ShowFatalError("InitUnitarySystems: Program terminated for previous conditions.");
                }
                this->m_MaxSuppCoilFluidFlow =
                    WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->m_SuppHeatCoilName, initUnitarySystemsErrorsFound);

                if (this->m_MaxSuppCoilFluidFlow > 0.0) {
                    Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->m_SuppCoilLoopNum).FluidName,
                                                                   DataGlobals::CWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->m_SuppCoilLoopNum).FluidIndex,
                                                                   routineName);
                    this->m_MaxSuppCoilFluidFlow =
                        WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->m_SuppHeatCoilName, initUnitarySystemsErrorsFound) * rho;
                }
                // fill outlet node for coil
                this->m_SuppCoilFluidOutletNodeNum = DataPlant::PlantLoop(this->m_SuppCoilLoopNum)
                                                         .LoopSide(this->m_SuppCoilLoopSide)
                                                         .Branch(this->m_SuppCoilBranchNum)
                                                         .Comp(this->m_SuppCoilCompNum)
                                                         .NodeNumOut;

            } else if (this->m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                initUnitarySystemsErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(this->m_SuppHeatCoilName,
                                                        DataPlant::TypeOf_CoilSteamAirHeating,
                                                        this->m_SuppCoilLoopNum,
                                                        this->m_SuppCoilLoopSide,
                                                        this->m_SuppCoilBranchNum,
                                                        this->m_SuppCoilCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        initUnitarySystemsErrFlag);
                if (initUnitarySystemsErrFlag) {
                    ShowFatalError("InitUnitarySystems: Program terminated for previous conditions.");
                }
                this->m_MaxSuppCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(this->m_SuppHeatCoilIndex, initUnitarySystemsErrorsFound);
                if (this->m_MaxSuppCoilFluidFlow > 0.0) {
                    int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                    Real64 TempSteamIn = 100.0;
                    Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                    this->m_MaxSuppCoilFluidFlow *= SteamDensity;
                }

                // fill outlet node for coil
                this->m_SuppCoilFluidOutletNodeNum = DataPlant::PlantLoop(this->m_SuppCoilLoopNum)
                                                         .LoopSide(this->m_SuppCoilLoopSide)
                                                         .Branch(this->m_SuppCoilBranchNum)
                                                         .Comp(this->m_SuppCoilCompNum)
                                                         .NodeNumOut;
            }

            this->m_MySuppCoilPlantScanFlag = false;

        } else if (this->m_MySuppCoilPlantScanFlag && !DataGlobals::AnyPlantInModel) {
            this->m_MySuppCoilPlantScanFlag = false;
        }

        // do the Begin Environment initializations
        if (DataGlobals::BeginEnvrnFlag && this->m_MyEnvrnFlag) {
            this->m_DesignMassFlowRate = this->m_DesignFanVolFlowRate * DataEnvironment::StdRhoAir;
            this->MaxCoolAirMassFlow = this->m_MaxCoolAirVolFlow * DataEnvironment::StdRhoAir;
            this->MaxHeatAirMassFlow = this->m_MaxHeatAirVolFlow * DataEnvironment::StdRhoAir;
            this->MaxNoCoolHeatAirMassFlow = this->m_MaxNoCoolHeatAirVolFlow * DataEnvironment::StdRhoAir;
            this->m_WSHPRuntimeFrac = 0.0;
            this->m_CompPartLoadRatio = 0.0;
            this->m_CoolingCoilSensDemand = 0.0;
            this->m_CoolingCoilLatentDemand = 0.0;
            this->m_HeatingCoilSensDemand = 0.0;
            this->m_SenLoadLoss = 0.0;
            if (this->m_Humidistat) {
                this->m_LatLoadLoss = 0.0;
            }

            if ((this->m_HeatRecActive) && (!this->m_MyPlantScanFlag)) {

                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->m_HRLoopNum).FluidName,
                                                               DataGlobals::HWInitConvTemp,
                                                               DataPlant::PlantLoop(this->m_HRLoopNum).FluidIndex,
                                                               routineName);

                this->m_DesignHeatRecMassFlowRate = this->m_DesignHRWaterVolumeFlow * rho;

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->m_DesignHeatRecMassFlowRate,
                                                   this->m_HeatRecoveryInletNodeNum,
                                                   this->m_HeatRecoveryOutletNodeNum,
                                                   this->m_HRLoopNum,
                                                   this->m_HRLoopSideNum,
                                                   this->m_HRBranchNum,
                                                   this->m_HRCompNum);
            }
            //   set fluid-side hardware limits
            if (this->CoolCoilFluidInletNode > 0) {

                if (this->MaxCoolCoilFluidFlow == DataSizing::AutoSize) {
                    // If water coil max water flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                    std::string CoolingCoilType = "";
                    if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater) {
                        CoolingCoilType = "Coil:Cooling:Water";
                    } else {
                        CoolingCoilType = "Coil:Cooling:Water:DetailedGeometry";
                    }
                    WaterCoils::SimulateWaterCoilComponents(this->m_CoolingCoilName, FirstHVACIteration, this->m_CoolingCoilIndex);
                    Real64 CoilMaxVolFlowRate =
                        WaterCoils::GetCoilMaxWaterFlowRate(CoolingCoilType, this->m_CoolingCoilName, initUnitarySystemsErrorsFound);
                    if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                        Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CoolCoilLoopNum).FluidName,
                                                                       DataGlobals::CWInitConvTemp,
                                                                       DataPlant::PlantLoop(this->CoolCoilLoopNum).FluidIndex,
                                                                       routineName);
                        this->MaxCoolCoilFluidFlow = CoilMaxVolFlowRate * rho;
                    }
                }

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->MaxCoolCoilFluidFlow,
                                                   this->CoolCoilFluidInletNode,
                                                   this->CoolCoilFluidOutletNodeNum,
                                                   this->CoolCoilLoopNum,
                                                   this->CoolCoilLoopSide,
                                                   this->CoolCoilBranchNum,
                                                   this->CoolCoilCompNum);
            }
            if (this->HeatCoilFluidInletNode > 0) {

                if (this->MaxHeatCoilFluidFlow == DataSizing::AutoSize) {
                    // IF water coil max water flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                    if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                        WaterCoils::SimulateWaterCoilComponents(this->m_HeatingCoilName, FirstHVACIteration, this->m_HeatingCoilIndex);
                        Real64 CoilMaxVolFlowRate =
                            WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->m_HeatingCoilName, initUnitarySystemsErrorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HeatCoilLoopNum).FluidName,
                                                                           DataGlobals::CWInitConvTemp,
                                                                           DataPlant::PlantLoop(this->HeatCoilLoopNum).FluidIndex,
                                                                           routineName);
                            this->MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                    }
                    // If steam coil max steam flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                    if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                        SteamCoils::SimulateSteamCoilComponents(this->m_HeatingCoilName,
                                                                FirstHVACIteration,
                                                                this->m_HeatingCoilIndex,
                                                                1.0,
                                                                initUnitarySystemsQActual); // QCoilReq, simulate any load > 0 to get max capacity
                        Real64 CoilMaxVolFlowRate = SteamCoils::GetCoilMaxSteamFlowRate(this->m_HeatingCoilIndex, initUnitarySystemsErrorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            Real64 TempSteamIn = 100.0;
                            Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                            this->MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                    }
                }

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->MaxHeatCoilFluidFlow,
                                                   this->HeatCoilFluidInletNode,
                                                   this->HeatCoilFluidOutletNodeNum,
                                                   this->HeatCoilLoopNum,
                                                   this->HeatCoilLoopSide,
                                                   this->HeatCoilBranchNum,
                                                   this->HeatCoilCompNum);
            }
            if (this->m_SuppCoilFluidInletNode > 0) {
                if (this->m_MaxSuppCoilFluidFlow == DataSizing::AutoSize) {
                    if (this->m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                        // If water coil max water flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                        WaterCoils::SimulateWaterCoilComponents(this->m_SuppHeatCoilName, FirstHVACIteration, this->m_SuppHeatCoilIndex);
                        Real64 CoilMaxVolFlowRate =
                            WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->m_SuppHeatCoilName, initUnitarySystemsErrorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->m_SuppCoilLoopNum).FluidName,
                                                                           DataGlobals::CWInitConvTemp,
                                                                           DataPlant::PlantLoop(this->m_SuppCoilLoopNum).FluidIndex,
                                                                           routineName);
                            this->m_MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                    }
                    if (this->m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                        SteamCoils::SimulateSteamCoilComponents(this->m_SuppHeatCoilName,
                                                                FirstHVACIteration,
                                                                this->m_SuppHeatCoilIndex,
                                                                1.0,
                                                                initUnitarySystemsQActual); // QCoilReq, simulate any load > 0 to get max capacity
                        Real64 CoilMaxVolFlowRate = SteamCoils::GetCoilMaxSteamFlowRate(this->m_SuppHeatCoilIndex, initUnitarySystemsErrorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            Real64 TempSteamIn = 100.0;
                            Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                            this->m_MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                    }
                    PlantUtilities::InitComponentNodes(0.0,
                                                       this->m_MaxSuppCoilFluidFlow,
                                                       this->m_SuppCoilFluidInletNode,
                                                       this->m_SuppCoilFluidOutletNodeNum,
                                                       this->m_SuppCoilLoopNum,
                                                       this->m_SuppCoilLoopSide,
                                                       this->m_SuppCoilBranchNum,
                                                       this->m_SuppCoilCompNum);
                }
            }
            this->m_MyEnvrnFlag = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            this->m_MyEnvrnFlag = true;
        }

        // Init maximum available Heat Recovery flow rate
        if ((this->m_HeatRecActive) && (!this->m_MyPlantScanFlag)) {
            Real64 mdotHR = 0.0;
            if (ScheduleManager::GetCurrentScheduleValue(this->m_SysAvailSchedPtr) > 0.0) {
                if (FirstHVACIteration) {
                    mdotHR = this->m_DesignHeatRecMassFlowRate;
                } else {
                    if (this->m_HeatRecoveryMassFlowRate > 0.0) {
                        mdotHR = this->m_HeatRecoveryMassFlowRate;
                    } else {
                        mdotHR = this->m_DesignHeatRecMassFlowRate;
                    }
                }
            } else {
                mdotHR = 0.0;
            }

            mdotHR = min(DataLoopNode::Node(this->m_HeatRecoveryOutletNodeNum).MassFlowRateMaxAvail, mdotHR);
            DataLoopNode::Node(this->m_HeatRecoveryInletNodeNum).MassFlowRate = mdotHR;
        }

        // get operating capacity of water and steam coil
        if (FirstHVACIteration || this->m_DehumidControlType_Num == DehumCtrlType::CoolReheat) {
            if (FirstHVACIteration) {
                this->m_IterationCounter = 0;
                this->m_IterationMode.clear();
                this->m_IterationMode.resize(21);

                if (this->m_ControlType == ControlType::Setpoint) {
                    if (ScheduleManager::GetCurrentScheduleValue(this->m_SysAvailSchedPtr) > 0.0) {
                        if (this->m_LastMode == CoolingMode) {
                            if (this->m_MultiOrVarSpeedCoolCoil) {
                                DataLoopNode::Node(this->AirInNode).MassFlowRate = this->m_CoolMassFlowRate[this->m_NumOfSpeedCooling];
                            } else {
                                DataLoopNode::Node(this->AirInNode).MassFlowRate = this->MaxCoolAirMassFlow;
                            }
                        } else if (this->m_LastMode == HeatingMode) {
                            if (this->m_MultiOrVarSpeedHeatCoil) {
                                DataLoopNode::Node(this->AirInNode).MassFlowRate = this->m_HeatMassFlowRate[this->m_NumOfSpeedHeating];
                            } else {
                                DataLoopNode::Node(this->AirInNode).MassFlowRate = this->MaxHeatAirMassFlow;
                            }
                        } else {
                            if (this->m_MultiOrVarSpeedCoolCoil) {
                                DataLoopNode::Node(this->AirInNode).MassFlowRate = this->m_IdleMassFlowRate;
                            } else {
                                DataLoopNode::Node(this->AirInNode).MassFlowRate = this->MaxNoCoolHeatAirMassFlow;
                            }
                        }
                    } else {
                        DataLoopNode::Node(this->AirInNode).MassFlowRate = 0.0;
                    }
                }
            }
            if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {

                //     set water-side mass flow rates
                Real64 mdot = this->MaxCoolCoilFluidFlow;
                PlantUtilities::SetComponentFlowRate(mdot,
                                                     this->CoolCoilFluidInletNode,
                                                     this->CoolCoilFluidOutletNodeNum,
                                                     this->CoolCoilLoopNum,
                                                     this->CoolCoilLoopSide,
                                                     this->CoolCoilBranchNum,
                                                     this->CoolCoilCompNum);
                //     simulate water coil to find operating capacity
                WaterCoils::SimulateWaterCoilComponents(
                    this->m_CoolingCoilName, FirstHVACIteration, this->m_CoolingCoilIndex, initUnitarySystemsQActual);
                this->m_DesignCoolingCapacity = initUnitarySystemsQActual;

            } // from IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWater .OR. Coil_CoolingWaterDetailed
            if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {

                //     set water-side mass flow rates
                Real64 mdot = this->MaxHeatCoilFluidFlow;
                PlantUtilities::SetComponentFlowRate(mdot,
                                                     this->HeatCoilFluidInletNode,
                                                     this->HeatCoilFluidOutletNodeNum,
                                                     this->HeatCoilLoopNum,
                                                     this->HeatCoilLoopSide,
                                                     this->HeatCoilBranchNum,
                                                     this->HeatCoilCompNum);
                //     simulate water coil to find operating capacity
                WaterCoils::SimulateWaterCoilComponents(
                    this->m_HeatingCoilName, FirstHVACIteration, this->m_HeatingCoilIndex, initUnitarySystemsQActual);
                this->m_DesignHeatingCapacity = initUnitarySystemsQActual;

            } // from IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN

            if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {

                //     set water-side mass flow rates
                Real64 mdot = this->MaxHeatCoilFluidFlow;
                PlantUtilities::SetComponentFlowRate(mdot,
                                                     this->HeatCoilFluidInletNode,
                                                     this->HeatCoilFluidOutletNodeNum,
                                                     this->HeatCoilLoopNum,
                                                     this->HeatCoilLoopSide,
                                                     this->HeatCoilBranchNum,
                                                     this->HeatCoilCompNum);
                //     simulate steam coil to find operating capacity
                SteamCoils::SimulateSteamCoilComponents(
                    this->m_HeatingCoilName,
                    FirstHVACIteration,
                    this->m_HeatingCoilIndex,
                    1.0,
                    initUnitarySystemsQActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
                this->m_DesignHeatingCapacity = SteamCoils::GetCoilCapacity(
                    DataHVACGlobals::cAllCoilTypes(this->m_HeatingCoilType_Num), this->m_HeatingCoilName, initUnitarySystemsErrorsFound);

            } // from IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN
            if (this->m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {

                //     set steam-side mass flow rates
                Real64 mdot = this->m_MaxSuppCoilFluidFlow;
                PlantUtilities::SetComponentFlowRate(mdot,
                                                     this->m_SuppCoilFluidInletNode,
                                                     this->m_SuppCoilFluidOutletNodeNum,
                                                     this->m_SuppCoilLoopNum,
                                                     this->m_SuppCoilLoopSide,
                                                     this->m_SuppCoilBranchNum,
                                                     this->m_SuppCoilCompNum);
                //     simulate water coil to find operating capacity
                if (mdot > 0.0) { // not sure why this is here and not used for other coil types, wouldn't capacity be 0 if water flow = 0? Maybe a
                                  // speed issue where coil doesn't need to be simulation if mdot=0.
                    WaterCoils::SimulateWaterCoilComponents(
                        this->m_SuppHeatCoilName, FirstHVACIteration, this->m_SuppHeatCoilIndex, initUnitarySystemsQActual);
                    this->m_DesignSuppHeatingCapacity = initUnitarySystemsQActual;
                } else {
                    this->m_DesignSuppHeatingCapacity = 0.0;
                }

            } // from IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN

            if (this->m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {

                //     set air-side and steam-side mass flow rates
                Real64 mdot = this->m_MaxSuppCoilFluidFlow;
                PlantUtilities::SetComponentFlowRate(mdot,
                                                     this->m_SuppCoilFluidInletNode,
                                                     this->m_SuppCoilFluidOutletNodeNum,
                                                     this->m_SuppCoilLoopNum,
                                                     this->m_SuppCoilLoopSide,
                                                     this->m_SuppCoilBranchNum,
                                                     this->m_SuppCoilCompNum);
                //     simulate steam coil to find operating capacity
                SteamCoils::SimulateSteamCoilComponents(
                    this->m_SuppHeatCoilName,
                    FirstHVACIteration,
                    this->m_SuppHeatCoilIndex,
                    1.0,
                    initUnitarySystemsQActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
                this->m_DesignSuppHeatingCapacity =
                    SteamCoils::GetCoilCapacity("Coil:Heating:Steam", this->m_SuppHeatCoilName, initUnitarySystemsErrorsFound);

            } // from IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
        }     // from IF( FirstHVACIteration ) THEN

        this->m_IterationCounter += 1;

        if (this->m_MySetPointCheckFlag) {
            if (!DataGlobals::SysSizingCalc && DataHVACGlobals::DoSetPointTest) {

                if (this->m_CoolCoilExists) {
                    int ControlNode = this->m_SystemCoolControlNodeNum;
                    if (ControlNode > 0) {
                        this->checkNodeSetPoint(AirLoopNum, ControlNode, UnitarySystems::CoolingCoil, OAUCoilOutTemp);
                    }
                }

                if (this->m_HeatCoilExists) {
                    int ControlNode = this->m_SystemHeatControlNodeNum;
                    if (ControlNode > 0) {
                        this->checkNodeSetPoint(AirLoopNum, ControlNode, UnitarySystems::HeatingCoil, OAUCoilOutTemp);
                    }
                }

                if (this->m_SuppCoilExists) {
                    int ControlNode = this->m_SuppHeatControlNodeNum;
                    if (ControlNode > 0) {
                        this->checkNodeSetPoint(AirLoopNum, ControlNode, UnitarySystems::SuppHeatCoil, OAUCoilOutTemp);
                    }
                }

                this->m_MySetPointCheckFlag = false;
            }
        }

        if (m_setFaultModelInput) {
            if ((!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {

                // check FaultsManager if connection exists
                FaultsManager::SetFaultyCoilSATSensor(this->UnitType, this->Name, this->m_FaultyCoilSATFlag, this->m_FaultyCoilSATIndex);
                if (this->m_FaultyCoilSATFlag) {
                    if (this->m_ControlType != ControlType::Setpoint) {
                        ShowWarningError(FaultsManager::FaultsCoilSATSensor(this->m_FaultyCoilSATIndex).FaultType + ": " +
                                         FaultsManager::FaultsCoilSATSensor(this->m_FaultyCoilSATIndex).Name);
                        ShowContinueError("For : " + this->UnitType + ": " + this->Name);
                        ShowContinueError("The specified unitary system is not controlled on leaving air temperature. The coil SAT sensor "
                                          "fault model will not be applied.");
                        this->m_FaultyCoilSATFlag = false;
                    }
                }
                m_setFaultModelInput = false;
            }
        }

        this->m_CoolingPartLoadFrac = 0.0;
        this->m_HeatingPartLoadFrac = 0.0;
        this->m_SuppHeatPartLoadFrac = 0.0;
        this->m_CoolingCycRatio = 0.0;
        this->m_CoolingSpeedRatio = 0.0;
        this->m_CoolingSpeedNum = 0;
        this->m_HeatingCycRatio = 0.0;
        this->m_HeatingSpeedRatio = 0.0;
        this->m_HeatingSpeedNum = 0;
        this->m_HeatingCoilSensDemand = 0.0;
        this->m_CoolingCoilSensDemand = 0.0;
        this->m_CoolingCoilLatentDemand = 0.0;
        this->m_DehumidInducedHeatingDemandRate = 0.0;
        this->CoolCoilWaterFlowRatio = 0.0;
        this->HeatCoilWaterFlowRatio = 0.0;

        // water/steam coil initialization
        if (this->CoolCoilFluidInletNode > 0) {
            Real64 mdot = 0.0;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->CoolCoilFluidInletNode,
                                                 this->CoolCoilFluidOutletNodeNum,
                                                 this->CoolCoilLoopNum,
                                                 this->CoolCoilLoopSide,
                                                 this->CoolCoilBranchNum,
                                                 this->CoolCoilCompNum);
        }
        if (this->HeatCoilFluidInletNode > 0) {
            Real64 mdot = 0.0;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->HeatCoilFluidInletNode,
                                                 this->HeatCoilFluidOutletNodeNum,
                                                 this->HeatCoilLoopNum,
                                                 this->HeatCoilLoopSide,
                                                 this->HeatCoilBranchNum,
                                                 this->HeatCoilCompNum);
        }
        if (this->m_SuppCoilFluidInletNode > 0) {
            Real64 mdot = 0.0;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->m_SuppCoilFluidInletNode,
                                                 this->m_SuppCoilFluidOutletNodeNum,
                                                 this->m_SuppCoilLoopNum,
                                                 this->m_SuppCoilLoopSide,
                                                 this->m_SuppCoilBranchNum,
                                                 this->m_SuppCoilCompNum);
        }

        this->m_InitHeatPump = true;
        m_massFlow1 = 0.0;
        m_massFlow2 = 0.0;
        m_runTimeFraction1 = 0.0;
        m_runTimeFraction2 = 0.0;
    }

    void UnitarySys::checkNodeSetPoint(int const AirLoopNum,       // number of the current air loop being simulated
                                       int const ControlNode,      // Node to test for set point
                                       int const CoilType,         // True if cooling coil, then test for HumRatMax set point
                                       Real64 const OAUCoilOutTemp // the coil inlet temperature of OutdoorAirUnit
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   March 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine checks for proper set point at control node.

        // METHODOLOGY EMPLOYED:
        // Uses the control node to test for set point.

        if (AirLoopNum == -1) {                                            // Outdoor Air Unit
            DataLoopNode::Node(ControlNode).TempSetPoint = OAUCoilOutTemp; // Set the coil outlet temperature
            if (this->m_ISHundredPercentDOASDXCoil) {
                this->frostControlSetPointLimit(this->m_DesiredOutletTemp,
                                                DataLoopNode::Node(ControlNode).HumRatMax,
                                                DataEnvironment::OutBaroPress,
                                                this->DesignMinOutletTemp,
                                                1);
            }
        } else if (AirLoopNum != -1) { // Not an Outdoor air unit

            bool SetPointErrorFlag = false;
            if (DataLoopNode::Node(ControlNode).TempSetPoint == DataLoopNode::SensedNodeFlagValue && this->m_ControlType == ControlType::Setpoint) {
                if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                    ShowSevereError(this->UnitType + ": Missing temperature setpoint for unitary system = " + this->Name);
                    ShowContinueError("  use a Setpoint Manager to establish a setpoint at the coil control node.");
                    SetPointErrorFlag = true;
                } else {
                    EMSManager::CheckIfNodeSetPointManagedByEMS(ControlNode, EMSManager::iTemperatureSetPoint, SetPointErrorFlag);
                    if (SetPointErrorFlag) {
                        ShowSevereError(this->UnitType + ": Missing temperature setpoint for unitary system = " + this->Name);
                        ShowContinueError("  use a Setpoint Manager to establish a setpoint at the coil control node.");
                        ShowContinueError("  or use an EMS actuator to establish a temperature setpoint at the coil control node.");
                    }
                }
            }
            if ((this->m_DehumidControlType_Num != DehumCtrlType::None) &&
                (DataLoopNode::Node(ControlNode).HumRatMax == DataLoopNode::SensedNodeFlagValue) && this->m_ControlType == ControlType::Setpoint &&
                CoilType == CoolingCoil) {
                if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                    ShowSevereError(this->UnitType + ": Missing humidity ratio setpoint (HUMRATMAX) for unitary system = " + this->Name);
                    ShowContinueError("  use a Setpoint Manager to establish a setpoint at the coil control node.");
                    SetPointErrorFlag = true;
                } else {
                    EMSManager::CheckIfNodeSetPointManagedByEMS(ControlNode, EMSManager::iHumidityRatioMaxSetPoint, SetPointErrorFlag);
                    if (SetPointErrorFlag) {
                        ShowSevereError(this->UnitType + ": Missing maximum humidity ratio setpoint (HUMRATMAX) for unitary system = " + this->Name);
                        ShowContinueError("  use a Setpoint Manager to establish a setpoint at the coil control node.");
                        ShowContinueError("  or use an EMS actuator to establish a maximum humidity ratio setpoint.");
                    }
                }
            }
        }
    }

    void UnitarySys::frostControlSetPointLimit(Real64 &TempSetPoint,       // temperature setpoint of the sensor node
                                               Real64 &HumRatSetPoint,     // humidity ratio setpoint of the sensor node
                                               Real64 const BaroPress,     // baromtric pressure, Pa [N/m^2]
                                               Real64 const TfrostControl, // minimum temperature limit for forst control
                                               int const ControlMode       // temperature or humidity control mode
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC
        //       DATE WRITTEN   January 2013

        // PURPOSE OF THIS SUBROUTINE:
        // Controls the forst formation condition based on user specified minimum DX coil outlet
        // air temperature. Resets the cooling setpoint based on the user specified limiting
        // temperature for frost control.
        // METHODOLOGY EMPLOYED:
        // Based on FrostControlSetPointLimit by Bereket Nigusse in HVACDXSystem

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        int const RunOnSensible(1); // identifier for temperature (sensible load) control
        int const RunOnLatent(2);   // identifier for humidity (latent load) control
        static std::string const routineName("FrostControlSetPointLimit");

        Real64 AirMassFlow = DataLoopNode::Node(this->CoolCoilInletNodeNum).MassFlowRate;
        if (ControlMode == RunOnSensible && AirMassFlow > MinAirMassFlow && TempSetPoint < DataLoopNode::Node(this->CoolCoilInletNodeNum).Temp) {
            if (TempSetPoint < TfrostControl) {
                TempSetPoint = TfrostControl;
                this->m_FrostControlStatus = 1;
            }
        } else if (ControlMode == RunOnLatent && AirMassFlow > MinAirMassFlow &&
                   HumRatSetPoint < DataLoopNode::Node(this->CoolCoilInletNodeNum).HumRat) {
            Real64 HumRatioSat = Psychrometrics::PsyWFnTdpPb(TfrostControl, BaroPress, routineName);
            if (HumRatioSat > HumRatSetPoint) {
                HumRatSetPoint = HumRatioSat;
                this->m_FrostControlStatus = 2;
            }
        } else {
            this->m_FrostControlStatus = 0;
        }
    }

    void UnitarySys::getUnitarySystemInput(std::string const &objectName, bool const ZoneEquipment, int const ZoneOAUnitNum)
    {

        bool errorsFound(false);

        UnitarySys::getUnitarySystemInputData(objectName, ZoneEquipment, ZoneOAUnitNum, errorsFound);

        if (errorsFound) {
            ShowFatalError("getUnitarySystemInputData: previous errors cause termination. Check inputs");
        }
    }

    void UnitarySys::sizeUnitarySystem(bool const FirstHVACIteration, int const AirLoopNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing unitary system components for which nominal cpacities
        // and flow rates have not been specified in the input. Coil sizing is preformed in the coil module.
        // Future modifications will size coils here and "push" this info to the specific coil.

        // METHODOLOGY EMPLOYED:
        // Obtains heating capacities and flow rates from the zone or system sizing arrays.
        // NOTE: In UNITARYSYSTEM:HEATPUMP:AIRTOAIR we are sizing the heating capacity to be
        // equal to the cooling capacity.  Thus the cooling and
        // and heating capacities of a DX heat pump system will be identical. In real life the ARI
        // heating and cooling capacities are close but not identical.

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeUnitarySystem");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Iter;                  // iteration count
        int MSHPIndex;             // Index to design Specification object
        int BranchNum;             // Index to branch on air loop
        Real64 SystemFlow;         // AirloopHVAC flow rate [m3/s]
        Real64 BranchFanFlow;      // branch fan flow rate [m3/s]
        bool ErrFound;             // logical error flag
        std::string FanType;       // fan type
        std::string m_FanName;     // fan name
        std::string SystemType;    // type of air loop equipment
        std::string HXCoilName;    // cooling coil name in HXAssisted parent
        int ActualCoolCoilType;    // cooling coil type in HXAssisted parent
        int SaveCurDuctType;       // used during sizing to save the current duct type
        Real64 QActual;            // water coil output [W]
        Real64 capacityMultiplier; // used for ASHRAE model sizing

        Real64 TempSize;  // DataSizing::AutoSized value of input field
        int FieldNum = 2; // IDD numeric field number where input field description is found
        int SizingMethod; // Integer representation of sizing method (e.g., DataHVACGlobals::CoolingAirflowSizing, DataSizing::HeatingCapacitySizing,
                          // etc.)
        bool PrintFlag;   // TRUE when sizing information is reported in the eio file
        bool SizingDesRunThisSys;         // checks if sizing was performed
        int NumAirLoopZones(0);           // number of zone inlet nodes in an air loop
        int ZoneInSysIndex(0);            // number of zone inlet nodes counter in an airloop
        Real64 SumOfMassFlowRateMax(0.0); // the sum of zone inlet mass flow rates
        int ZoneInletNodeNum(0);          // zone inlet nodes node number
        Real64 minNoLoadFlow;             // used for sizing MaxNoCoolHeatVolFlow for SingleZoneVAV method
        //////////// hoisted into namespace ////////////////////////////////////////////////
        // static int NumUnitarySystemsSized( 0 ); // counter used to delete UnitarySystemNumericFields array after last system is sized
        ////////////////////////////////////////////////////////////////////////////////////
        // References
        DataSizing::ZoneEqSizingData *select_EqSizing(nullptr);

        // sweep specific data into one pointer to avoid if statements throughout this subroutine
        if (DataSizing::CurOASysNum > 0) {
            select_EqSizing = &DataSizing::OASysEqSizing(DataSizing::CurOASysNum);
        } else if (DataSizing::CurSysNum > 0) {
            select_EqSizing = &DataSizing::UnitarySysEqSizing(DataSizing::CurSysNum);
        } else if (DataSizing::CurZoneEqNum > 0) {
            select_EqSizing = &DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum);
            DataSizing::ZoneEqUnitarySys = true;
        } else {
            assert(false);
        }
        // Object Data, points to specific array
        DataSizing::ZoneEqSizingData &EqSizing(*select_EqSizing);

        // can't hurt to initialize these going in, problably redundant
        EqSizing.AirFlow = false;
        EqSizing.CoolingAirFlow = false;
        EqSizing.HeatingAirFlow = false;
        EqSizing.AirVolFlow = 0.0;
        EqSizing.CoolingAirVolFlow = 0.0;
        EqSizing.HeatingAirVolFlow = 0.0;
        EqSizing.Capacity = false;
        EqSizing.CoolingCapacity = false;
        EqSizing.HeatingCapacity = false;
        EqSizing.DesCoolingLoad = 0.0;
        EqSizing.DesHeatingLoad = 0.0;
        EqSizing.OAVolFlow = 0.0; // UnitarySys doesn't have OA

        bool anyEMSRan;
        EMSManager::ManageEMS(DataGlobals::emsCallFromUnitarySystemSizing, anyEMSRan); // calling point

        std::string SizingString("");
        std::string CompName = this->Name;
        std::string CompType = this->UnitType;
        int CoolingSAFlowMethod = this->m_CoolingSAFMethod;
        int HeatingSAFlowMethod = this->m_HeatingSAFMethod;
        // can't reset this to 0 for systems where DX heating coil is in downstream unit and DX cooling coil is in upstream unit
        //		DXCoolCap = 0.0;
        DataSizing::UnitaryHeatCap = 0.0;
        DataSizing::SuppHeatCap = 0.0;
        bool TempCoolingLoad = CoolingLoad;
        bool TempHeatingLoad = HeatingLoad;
        CoolingLoad = true;
        HeatingLoad = false;
        DataSizing::ZoneCoolingOnlyFan = false;
        DataSizing::ZoneHeatingOnlyFan = false;
        bool IsAutoSize = false;
        Real64 SysCoolingFlow = 0.0;
        Real64 SysHeatingFlow = 0.0;
        Real64 CoolCapAtPeak = 0.0;
        Real64 HeatCapAtPeak = 0.0;

        if (DataSizing::CurSysNum > 0 && DataSizing::CurOASysNum == 0 && this->m_FanExists) {
            if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).supFanVecIndex = this->m_FanIndex;
                DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).supFanModelTypeEnum = DataAirSystems::objectVectorOOFanSystemModel;
                DataSizing::DataFanEnumType = DataAirSystems::objectVectorOOFanSystemModel;
                DataSizing::DataFanIndex = this->m_FanIndex;
            } else {
                DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).SupFanNum = this->m_FanIndex;
                DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).supFanModelTypeEnum = DataAirSystems::structArrayLegacyFanModels;
                DataSizing::DataFanEnumType = DataAirSystems::structArrayLegacyFanModels;
                DataSizing::DataFanIndex = this->m_FanIndex;
            }
            if (this->m_FanPlace == FanPlace::BlowThru) {
                DataAirSystems::PrimaryAirSystem(AirLoopNum).supFanLocation = DataAirSystems::fanPlacement::BlowThru;
            } else if (this->m_FanPlace == FanPlace::DrawThru) {
                DataAirSystems::PrimaryAirSystem(AirLoopNum).supFanLocation = DataAirSystems::fanPlacement::DrawThru;
            }
        } else if (DataSizing::CurZoneEqNum > 0 && this->m_FanExists) {
            if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                DataSizing::DataFanEnumType = DataAirSystems::objectVectorOOFanSystemModel;
            } else {
                DataSizing::DataFanEnumType = DataAirSystems::structArrayLegacyFanModels;
            }
            DataSizing::DataFanIndex = this->m_FanIndex;
            if (this->m_FanPlace == FanPlace::BlowThru) {
                DataSizing::DataFanPlacement = DataSizing::zoneFanPlacement::zoneBlowThru;
            } else if (this->m_FanPlace == FanPlace::DrawThru) {
                DataSizing::DataFanPlacement = DataSizing::zoneFanPlacement::zoneDrawThru;
            }
        }

        if (this->ATMixerExists && DataSizing::CurZoneEqNum > 0) { // set up ATMixer conditions for scalable capacity sizing
            SingleDuct::setATMixerSizingProperties(this->m_ATMixerIndex, this->ControlZoneNum, DataSizing::CurZoneEqNum);
        }

        PrintFlag = false;
        // STEP 1: find the DataSizing::AutoSized cooling air flow rate and capacity
        if (this->m_CoolCoilExists) {
            if (!this->m_HeatCoilExists) DataSizing::ZoneCoolingOnlyFan = true;
            FieldNum = 3; // N3 , \field Cooling Supply Air Flow Rate
            SizingMethod = DataHVACGlobals::CoolingAirflowSizing;
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [m3/s]";
            TempSize = this->m_MaxCoolAirVolFlow;
            SaveCurDuctType = DataSizing::CurDuctType;
            DataSizing::CurDuctType = DataHVACGlobals::Cooling;
            if ((CoolingSAFlowMethod == SupplyAirFlowRate) || (CoolingSAFlowMethod == None)) {
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysCoolingFlow = TempSize;
            } else if (CoolingSAFlowMethod == FlowPerFloorArea) {
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysCoolingFlow = TempSize;
                this->m_MaxCoolAirVolFlow = DataSizing::AutoSize;
            } else if (CoolingSAFlowMethod == FractionOfAutoSizedCoolingValue) {
                TempSize = DataSizing::AutoSize;
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysCoolingFlow = TempSize * this->m_MaxCoolAirVolFlow;
                this->m_MaxCoolAirVolFlow = DataSizing::AutoSize;
            } else if (CoolingSAFlowMethod == FlowPerCoolingCapacity) {
                if (this->m_DesignCoolingCapacity == DataSizing::AutoSize) {
                    TempSize = DataSizing::AutoSize;
                    ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                    SizingMethod = DataHVACGlobals::CoolingCapacitySizing;
                    DataSizing::DataFlowUsedForSizing = TempSize;
                    TempSize = DataSizing::AutoSize;
                    if (this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed ||
                        this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling ||
                        this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed ||
                        this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                        DataSizing::DataTotCapCurveIndex = DXCoils::GetDXCoilCapFTCurveIndex(this->m_CoolingCoilIndex, ErrFound);
                        DataSizing::DataIsDXCoil = true;
                    }
                    ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                    CoolCapAtPeak = TempSize;
                    SysCoolingFlow = TempSize * this->m_MaxCoolAirVolFlow;
                    DataSizing::DataTotCapCurveIndex = 0;
                    EqSizing.CoolingCapacity = true;
                    EqSizing.DesCoolingLoad = CoolCapAtPeak;
                } else {
                    SysCoolingFlow = this->m_DesignCoolingCapacity * this->m_MaxCoolAirVolFlow;
                    CoolCapAtPeak = this->m_DesignCoolingCapacity;
                    DataSizing::DXCoolCap = CoolCapAtPeak;
                }
                this->m_MaxCoolAirVolFlow = DataSizing::AutoSize;
            } else {
                // should never happen
                ShowSevereError(RoutineName + ": " + CompType + " = " + CompName);
                ShowContinueError("Illegal entry for Cooling Supply Air Flow Rate Method.");
            }

            DataSizing::CurDuctType = SaveCurDuctType;
            EqSizing.CoolingAirFlow = true;
            EqSizing.CoolingAirVolFlow = SysCoolingFlow;

            // Cooling airflow should be known at this point. Now find DataSizing::AutoSized design cooling capacity.
            if (CoolingSAFlowMethod != FlowPerCoolingCapacity && this->m_DesignCoolingCapacity < 0.0) {
                SizingMethod = DataHVACGlobals::CoolingCapacitySizing;
                DataSizing::DataFlowUsedForSizing = EqSizing.CoolingAirVolFlow;
                TempSize = DataSizing::AutoSize;
                if (this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed ||
                    this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling ||
                    this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed ||
                    this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                    DataSizing::DataTotCapCurveIndex = DXCoils::GetDXCoilCapFTCurveIndex(this->m_CoolingCoilIndex, ErrFound);
                    DataSizing::DataIsDXCoil = true;
                }
                if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    DataSizing::DataTotCapCurveIndex = VariableSpeedCoils::GetVSCoilCapFTCurveIndex(this->m_CoolingCoilIndex, ErrFound);
                    DataSizing::DataIsDXCoil = true;
                }
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                CoolCapAtPeak = TempSize;
                DataSizing::DXCoolCap = CoolCapAtPeak;
                EqSizing.CoolingCapacity = true;
                EqSizing.DesCoolingLoad = CoolCapAtPeak;
            } else {
                CoolCapAtPeak = this->m_DesignCoolingCapacity;
            }
            DataSizing::DataIsDXCoil = false;
            DataSizing::DataTotCapCurveIndex = 0;
            DataSizing::DataFlowUsedForSizing = 0.0;
        }

        // STEP 2: find the DataSizing::AutoSized heating air flow rate and capacity
        if (this->m_HeatCoilExists) {
            if (!this->m_CoolCoilExists) DataSizing::ZoneHeatingOnlyFan = true;
            FieldNum = 7; // N7 , \field Heating Supply Air Flow Rate
            SizingMethod = DataHVACGlobals::HeatingAirflowSizing;
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [m3/s]";
            TempSize = this->m_MaxHeatAirVolFlow;
            SaveCurDuctType = DataSizing::CurDuctType;
            DataSizing::CurDuctType = DataHVACGlobals::Heating;
            if ((HeatingSAFlowMethod == SupplyAirFlowRate) || (HeatingSAFlowMethod == None)) {
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysHeatingFlow = TempSize;
            } else if (HeatingSAFlowMethod == FlowPerFloorArea) {
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysHeatingFlow = TempSize;
                this->m_MaxHeatAirVolFlow = DataSizing::AutoSize;
            } else if (HeatingSAFlowMethod == FractionOfAutoSizedHeatingValue) {
                TempSize = DataSizing::AutoSize;
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysHeatingFlow = TempSize * this->m_MaxHeatAirVolFlow;
                this->m_MaxHeatAirVolFlow = DataSizing::AutoSize;
            } else if (HeatingSAFlowMethod == FlowPerHeatingCapacity) {
                TempSize = DataSizing::AutoSize;
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SizingMethod = DataHVACGlobals::HeatingCapacitySizing;
                DataSizing::DataFlowUsedForSizing = TempSize;
                TempSize = DataSizing::AutoSize;
                DataSizing::DataFracOfAutosizedCoolingCapacity = 1.0;
                DataSizing::DataHeatSizeRatio = this->m_HeatingSizingRatio;
                if (this->m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                    this->m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                    DataSizing::DataTotCapCurveIndex = DXCoils::GetDXCoilCapFTCurveIndex(this->m_HeatingCoilIndex, ErrFound);
                    DataSizing::DataIsDXCoil = true;
                }
                if (DataSizing::CurSysNum > 0)
                    DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating =
                        false; // set to false to allow calculation of actual heating capacity
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                if (DataSizing::CurSysNum > 0) DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating = true;
                HeatCapAtPeak = TempSize;
                SysHeatingFlow = TempSize * this->m_MaxHeatAirVolFlow;
                this->m_MaxHeatAirVolFlow = DataSizing::AutoSize;
                EqSizing.HeatingCapacity = true;
                EqSizing.DesHeatingLoad = HeatCapAtPeak;
            } else {
                // should never happen
                ShowSevereError(RoutineName + ": " + CompType + " = " + CompName);
                ShowContinueError("Illegal entry for Heating Supply Air Flow Rate Method.");
            }

            DataSizing::CurDuctType = SaveCurDuctType;
            EqSizing.HeatingAirFlow = true;
            EqSizing.HeatingAirVolFlow = SysHeatingFlow;

            // Heating airflow should be known at this point. Now find DataSizing::AutoSized design heating capacity.
            if (HeatingSAFlowMethod != FlowPerHeatingCapacity && this->m_DesignHeatingCapacity == DataSizing::AutoSize) {
                SizingMethod = DataHVACGlobals::HeatingCapacitySizing;
                DataSizing::DataFlowUsedForSizing = EqSizing.HeatingAirVolFlow;
                TempSize = DataSizing::AutoSize;
                DataSizing::DataHeatSizeRatio = this->m_HeatingSizingRatio;
                if (this->m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical ||
                    this->m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                    DataSizing::DataTotCapCurveIndex = DXCoils::GetDXCoilCapFTCurveIndex(this->m_HeatingCoilIndex, ErrFound);
                    DataSizing::DataIsDXCoil = true;
                }
                if (DataSizing::CurSysNum > 0)
                    DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating =
                        false; // set to false to allow calculation of actual heating capacity
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                if (DataSizing::CurSysNum > 0) DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating = true;
                HeatCapAtPeak = TempSize;
                EqSizing.HeatingCapacity = true;
                EqSizing.DesHeatingLoad = HeatCapAtPeak;
            } else {
                HeatCapAtPeak = this->m_DesignHeatingCapacity;
            }
            //			if ( ! UnitarySystem( UnitarySysNum ).CoolCoilExists )DXCoolCap = HeatCapAtPeak;
            DataSizing::DataIsDXCoil = false;
            DataSizing::DataTotCapCurveIndex = 0;
            DataSizing::DataFlowUsedForSizing = 0.0;
        }

        // STEP 3: use the greater of cooling and heating air flow rates for system flow
        // previous version of E+ used maximum flow rate for unitary systems. Keep this methodology for now.
        // Delete next 2 lines and uncomment 2 lines inside next if (HeatPump) statement to allow non-heat pump systems to operate at different flow
        // rates (might require additional change to if block logic).
        EqSizing.CoolingAirVolFlow = max(EqSizing.CoolingAirVolFlow, EqSizing.HeatingAirVolFlow);
        EqSizing.HeatingAirVolFlow = EqSizing.CoolingAirVolFlow;

        // STEP 4: set heat pump coil capacities equal to greater of cooling or heating capacity
        if (this->m_HeatPump) { // if a heat pump, use maximum values and set main air flow and capacity variables
            EqSizing.AirFlow = true;
            EqSizing.AirVolFlow = max(EqSizing.CoolingAirVolFlow, EqSizing.HeatingAirVolFlow);
            //			EqSizing.CoolingAirVolFlow = EqSizing.AirVolFlow;
            //			EqSizing.HeatingAirVolFlow = EqSizing.AirVolFlow;
            EqSizing.Capacity = true;
            EqSizing.DesCoolingLoad = max(EqSizing.DesCoolingLoad, EqSizing.DesHeatingLoad);
            EqSizing.DesHeatingLoad = EqSizing.DesCoolingLoad;
            DataSizing::DXCoolCap = EqSizing.DesCoolingLoad;
        } else if (!this->m_CoolCoilExists && DataSizing::CurZoneEqNum > 0) {
            DataSizing::DXCoolCap = EqSizing.DesHeatingLoad;
        }

        if (this->m_OKToPrintSizing) PrintFlag = true;
        // STEP 5: report system parameters (e.g., air flow rates, capacities, etc.)
        if (this->m_FanExists) {

            SizingMethod = DataHVACGlobals::SystemAirflowSizing;
            EqSizing.SystemAirFlow = true;
            EqSizing.AirVolFlow = max(EqSizing.CoolingAirVolFlow, EqSizing.HeatingAirVolFlow);
            if (this->m_DesignFanVolFlowRate <= 0.0) { // attempt to catch any missed logic in GetUnitarySystem
                this->m_DesignFanVolFlowRate = DataSizing::AutoSize;
            }
            DataSizing::DataEMSOverrideON = this->m_DesignFanVolFlowRateEMSOverrideOn;
            DataSizing::DataEMSOverride = this->m_DesignFanVolFlowRateEMSOverrideValue;
            TempSize = this->m_DesignFanVolFlowRate;
            SizingString = "Supply Air Flow Rate [m3/s]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->m_DesignFanVolFlowRate = TempSize;
            DataSizing::DataEMSOverrideON = false;
            EqSizing.SystemAirFlow = false;
        }

        // not sure what to do if UnitarySystem has only 1 coil type and flow needs to occur when present coil is off
        // how does constant fan operating mode pertain here?
        if (this->m_HeatCoilExists && !this->m_CoolCoilExists) {
            if (this->m_MaxCoolAirVolFlow == DataSizing::AutoSize) this->m_MaxCoolAirVolFlow = EqSizing.HeatingAirVolFlow;
        } else if (this->m_CoolCoilExists && !this->m_HeatCoilExists) {
            if (this->m_MaxHeatAirVolFlow == DataSizing::AutoSize) this->m_MaxHeatAirVolFlow = EqSizing.CoolingAirVolFlow;
        }

        if (this->m_HeatCoilExists) {

            SizingMethod = DataHVACGlobals::HeatingAirflowSizing;
            if (this->m_MaxHeatAirVolFlow <= 0.0) { // attempt to catch any missed logic in GetUnitarySystem
                this->m_MaxHeatAirVolFlow = DataSizing::AutoSize;
            }
            FieldNum = 7; // N7 , \field Heating Supply Air Flow Rate
            DataSizing::DataEMSOverrideON = this->m_MaxHeatAirVolFlowEMSOverrideOn;
            DataSizing::DataEMSOverride = this->m_MaxHeatAirVolFlowEMSOverrideValue;
            TempSize = this->m_MaxHeatAirVolFlow;
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [m3/s]";
            SizingString = "Heating Supply Air Flow Rate [m3/s]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->m_MaxHeatAirVolFlow = TempSize;
            DataSizing::DataEMSOverrideON = false;
            DataSizing::DataConstantUsedForSizing = 0.0;
        }

        if (this->m_CoolCoilExists) {

            SizingMethod = DataHVACGlobals::CoolingAirflowSizing;
            if (this->m_MaxCoolAirVolFlow <= 0.0) { // attempt to catch any missed logic in GetUnitarySystem
                this->m_MaxCoolAirVolFlow = DataSizing::AutoSize;
            }
            FieldNum = 3; // N3 , \field Cooling Supply Air Flow Rate
            DataSizing::DataEMSOverrideON = this->m_MaxCoolAirVolFlowEMSOverrideOn;
            DataSizing::DataEMSOverride = this->m_MaxCoolAirVolFlowEMSOverrideValue;
            TempSize = this->m_MaxCoolAirVolFlow;
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [m3/s]";
            SizingString = "Cooling Supply Air Flow Rate [m3/s]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->m_MaxCoolAirVolFlow = TempSize;
            DataSizing::DataEMSOverrideON = false;
            DataSizing::DataConstantUsedForSizing = 0.0;
        }

        // If not set, set DesignFanVolFlowRate as greater of cooling and heating to make sure this value > 0.
        // If fan is hard-sized, use that value, otherwise the fan will size to DesignFanVolFlowRate
        if (this->m_DesignFanVolFlowRate <= 0.0) {
            this->m_DesignFanVolFlowRate = max(this->m_MaxCoolAirVolFlow, this->m_MaxHeatAirVolFlow);
            if (this->m_ActualFanVolFlowRate > 0.0) this->m_DesignFanVolFlowRate = this->m_ActualFanVolFlowRate;
            if (this->m_DesignFanVolFlowRate <= 0.0) {
                ShowWarningError(RoutineName + ": " + CompType + " = " + CompName);
                ShowFatalError("Unable to determine fan air flow rate.");
            }
        }
        if (!this->m_FanExists) this->m_ActualFanVolFlowRate = this->m_DesignFanVolFlowRate;

        if (this->m_CoolCoilExists || this->m_HeatCoilExists || this->m_SuppCoilExists) {

            SizingMethod = DataHVACGlobals::SystemAirflowSizing;

            if (this->m_NoCoolHeatSAFMethod <= SupplyAirFlowRate && this->m_ControlType == ControlType::CCMASHRAE) {
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                if (this->m_MaxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                    DataSizing::DataConstantUsedForSizing = max(this->m_MaxCoolAirVolFlow, this->m_MaxHeatAirVolFlow);
                    if (this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed ||
                        this->m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                        minNoLoadFlow = 0.6667;
                    } else {
                        if (this->m_NoLoadAirFlowRateRatio < 1.0) {
                            minNoLoadFlow = this->m_NoLoadAirFlowRateRatio;
                        } else {
                            minNoLoadFlow = 0.5;
                        }
                    }
                    if (this->m_MaxCoolAirVolFlow >= this->m_MaxHeatAirVolFlow) {
                        DataSizing::DataFractionUsedForSizing = min(minNoLoadFlow, (this->m_MaxHeatAirVolFlow / this->m_MaxCoolAirVolFlow) - 0.01);
                    } else {
                        DataSizing::DataFractionUsedForSizing = min(minNoLoadFlow, (this->m_MaxCoolAirVolFlow / this->m_MaxHeatAirVolFlow) - 0.01);
                    }
                } else {
                    DataSizing::DataConstantUsedForSizing = this->m_MaxNoCoolHeatAirVolFlow;
                    DataSizing::DataFractionUsedForSizing = 1.0;
                }
            } else if (this->m_NoCoolHeatSAFMethod == FractionOfAutoSizedCoolingValue) {
                this->m_MaxNoCoolHeatAirVolFlow *= EqSizing.CoolingAirVolFlow;
                DataSizing::DataConstantUsedForSizing = this->m_MaxNoCoolHeatAirVolFlow;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->m_MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
            } else if (this->m_NoCoolHeatSAFMethod == FractionOfAutoSizedHeatingValue) {
                this->m_MaxNoCoolHeatAirVolFlow *= EqSizing.HeatingAirVolFlow;
                DataSizing::DataConstantUsedForSizing = this->m_MaxNoCoolHeatAirVolFlow;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->m_MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
            } else if (this->m_NoCoolHeatSAFMethod == FlowPerCoolingCapacity) {
                this->m_MaxNoCoolHeatAirVolFlow *= EqSizing.DesCoolingLoad;
                DataSizing::DataConstantUsedForSizing = this->m_MaxNoCoolHeatAirVolFlow;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->m_MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
            } else if (this->m_NoCoolHeatSAFMethod == FlowPerHeatingCapacity) {
                this->m_MaxNoCoolHeatAirVolFlow *= EqSizing.DesHeatingLoad;
                DataSizing::DataConstantUsedForSizing = this->m_MaxNoCoolHeatAirVolFlow;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->m_MaxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
            } else {
                DataSizing::DataFractionUsedForSizing = this->m_NoLoadAirFlowRateRatio;
            }

            FieldNum = 11; // N11 , \field No Load Supply Air Flow Rate
            DataSizing::DataEMSOverrideON = this->m_MaxNoCoolHeatAirVolFlowEMSOverrideOn;
            DataSizing::DataEMSOverride = this->m_MaxNoCoolHeatAirVolFlowEMSOverrideValue;
            TempSize = this->m_MaxNoCoolHeatAirVolFlow;
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [m3/s]";
            SizingString = "No Load Supply Air Flow Rate [m3/s]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->m_MaxNoCoolHeatAirVolFlow = TempSize;
            DataSizing::DataEMSOverrideON = false;
            DataSizing::DataConstantUsedForSizing = 0.0;
            DataSizing::DataFractionUsedForSizing = 0.0;
        }

        if (this->m_MaxCoolAirVolFlow > 0.0) {
            this->LowSpeedCoolFanRatio = this->m_MaxNoCoolHeatAirVolFlow / this->m_MaxCoolAirVolFlow;
        }
        if (this->m_MaxHeatAirVolFlow > 0.0) {
            this->LowSpeedHeatFanRatio = this->m_MaxNoCoolHeatAirVolFlow / this->m_MaxHeatAirVolFlow;
        }

        if (this->ATMixerExists && DataSizing::CurZoneEqNum > 0) { // set up ATMixer conditions for use in component sizing
            SingleDuct::setATMixerSizingProperties(this->m_ATMixerIndex, this->ControlZoneNum, DataSizing::CurZoneEqNum);
        }

        // Change the Volume Flow Rates to Mass Flow Rates
        this->m_DesignMassFlowRate = this->m_DesignFanVolFlowRate * DataEnvironment::StdRhoAir;
        this->MaxCoolAirMassFlow = this->m_MaxCoolAirVolFlow * DataEnvironment::StdRhoAir;
        this->MaxHeatAirMassFlow = this->m_MaxHeatAirVolFlow * DataEnvironment::StdRhoAir;
        this->MaxNoCoolHeatAirMassFlow = this->m_MaxNoCoolHeatAirVolFlow * DataEnvironment::StdRhoAir;

        // initialize idle air flow rate variables in case these are needed in multi-speed heating coil sizing when no multi-speed cooling coil exists
        // the multi-speed coils will overwrite this data and these variables are only used for multi-speed coils in function SetOnOffMassFlowRate
        if (this->m_MultiOrVarSpeedCoolCoil || this->m_MultiOrVarSpeedHeatCoil) {
            if (this->m_DesignFanVolFlowRate > 0.0) {
                this->m_IdleVolumeAirRate = this->m_DesignFanVolFlowRate;
            } else {
                this->m_IdleVolumeAirRate = max(this->m_MaxCoolAirVolFlow, this->m_MaxHeatAirVolFlow);
            }
            this->m_IdleMassFlowRate = this->m_IdleVolumeAirRate * DataEnvironment::StdRhoAir;
            this->m_IdleSpeedRatio = 1.0;
        }

        // initialize multi-speed coils
        if ((this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) ||
            (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed)) {

            if (this->m_NumOfSpeedCooling > 0) {
                if (this->m_CoolVolumeFlowRate.size() == 0) this->m_CoolVolumeFlowRate.resize(this->m_NumOfSpeedCooling + 1);
                if (this->m_CoolMassFlowRate.size() == 0) this->m_CoolMassFlowRate.resize(this->m_NumOfSpeedCooling + 1);
                if (this->m_MSCoolingSpeedRatio.size() == 0) this->m_MSCoolingSpeedRatio.resize(this->m_NumOfSpeedCooling + 1);
            }

            MSHPIndex = this->m_DesignSpecMSHPIndex;
            if (MSHPIndex > -1) {
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedCooling; Iter >= 1; --Iter) {
                    if (designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter - 1] == DataSizing::AutoSize) {
                        designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter - 1] = double(Iter) / double(designSpecMSHP[MSHPIndex].numOfSpeedCooling);
                    }
                }
            }

            VariableSpeedCoils::SimVariableSpeedCoils(blankString,
                                                      this->m_CoolingCoilIndex,
                                                      0,
                                                      this->m_MaxONOFFCyclesperHour,
                                                      this->m_HPTimeConstant,
                                                      this->m_FanDelayTime,
                                                      0,
                                                      0.0,
                                                      1,
                                                      0.0,
                                                      0.0,
                                                      0.0,
                                                      0.0); // conduct the sizing operation in the VS WSHP
            if (this->m_NumOfSpeedCooling != VariableSpeedCoils::VarSpeedCoil(this->m_CoolingCoilIndex).NumOfSpeeds) {
                ShowWarningError(RoutineName + ": " + CompType + " = " + CompName);
                ShowContinueError("Number of cooling speeds does not match coil object.");
                ShowFatalError("Cooling coil = " + VariableSpeedCoils::VarSpeedCoil(this->m_CoolingCoilIndex).VarSpeedCoilType + ": " +
                               VariableSpeedCoils::VarSpeedCoil(this->m_CoolingCoilIndex).Name);
            }
            DataSizing::DXCoolCap = VariableSpeedCoils::GetCoilCapacityVariableSpeed(
                DataHVACGlobals::cAllCoilTypes(this->m_CoolingCoilType_Num), this->m_CoolingCoilName, ErrFound);
            EqSizing.DesCoolingLoad = DataSizing::DXCoolCap;
            EqSizing.DesHeatingLoad = DataSizing::DXCoolCap;

            for (Iter = 1; Iter <= this->m_NumOfSpeedCooling; ++Iter) {
                this->m_CoolVolumeFlowRate[Iter] = VariableSpeedCoils::VarSpeedCoil(this->m_CoolingCoilIndex).MSRatedAirVolFlowRate(Iter);
                this->m_CoolMassFlowRate[Iter] = this->m_CoolVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                this->m_MSCoolingSpeedRatio[Iter] = this->m_CoolVolumeFlowRate[Iter] / this->m_DesignFanVolFlowRate;
            }

            if (MSHPIndex > 0) {
                this->m_IdleVolumeAirRate = this->m_MaxCoolAirVolFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
                this->m_IdleMassFlowRate = this->m_IdleVolumeAirRate * DataEnvironment::StdRhoAir;
                this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
            } else if (this->m_CoolVolumeFlowRate.size() == 0) {
                this->m_IdleVolumeAirRate = this->m_MaxNoCoolHeatAirVolFlow;
                this->m_IdleMassFlowRate = this->MaxNoCoolHeatAirMassFlow;
                this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
            }

        } else if (this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

            if (this->m_NumOfSpeedCooling > 0) {
                if (this->m_CoolVolumeFlowRate.size() == 0) this->m_CoolVolumeFlowRate.resize(this->m_NumOfSpeedCooling + 1);
                if (this->m_CoolMassFlowRate.size() == 0) this->m_CoolMassFlowRate.resize(this->m_NumOfSpeedCooling + 1);
                if (this->m_MSCoolingSpeedRatio.size() == 0) this->m_MSCoolingSpeedRatio.resize(this->m_NumOfSpeedCooling + 1);
            }

            // set the multi-speed high flow rate variable in case a non-zero air flow rate resides on the coil inlet during sizing (e.g., upstream
            // system ran prior to this one)
            DataHVACGlobals::MSHPMassFlowRateHigh =
                EqSizing.CoolingAirVolFlow *
                DataEnvironment::StdRhoAir; // doesn't matter what this value is since only coil size is needed and CompOn = 0 here
            DXCoils::SimDXCoilMultiSpeed(blankString, 1.0, 1.0, this->m_CoolingCoilIndex, 0, 0, 0);
            DataSizing::DXCoolCap = DXCoils::GetCoilCapacityByIndexType(this->m_CoolingCoilIndex, this->m_CoolingCoilType_Num, ErrFound);
            EqSizing.DesCoolingLoad = DataSizing::DXCoolCap;
            MSHPIndex = this->m_DesignSpecMSHPIndex;

            if (MSHPIndex > -1) {
                // use reverse order since we divide by CoolVolumeFlowRate(max)
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedCooling; Iter > 0; --Iter) {
                    if (designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter - 1] == DataSizing::AutoSize)
                        designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter - 1] = double(Iter) / double(designSpecMSHP[MSHPIndex].numOfSpeedCooling);
                    this->m_CoolVolumeFlowRate[Iter] = this->m_MaxCoolAirVolFlow * designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter - 1];
                    this->m_CoolMassFlowRate[Iter] = this->m_CoolVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                    this->m_MSCoolingSpeedRatio[Iter] = this->m_CoolVolumeFlowRate[Iter] / this->m_DesignFanVolFlowRate;
                }
                this->m_IdleVolumeAirRate = this->m_MaxCoolAirVolFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
                this->m_IdleMassFlowRate = this->m_IdleVolumeAirRate * DataEnvironment::StdRhoAir;
                this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
            } else if (this->m_CoolVolumeFlowRate.size() == 0) {
                this->m_IdleVolumeAirRate = this->m_MaxNoCoolHeatAirVolFlow;
                this->m_IdleMassFlowRate = this->MaxNoCoolHeatAirMassFlow;
                this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
            }
        } else if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                   this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {

            if (this->m_NumOfSpeedCooling > 0) {
                if (this->m_CoolVolumeFlowRate.size() == 0) this->m_CoolVolumeFlowRate.resize(this->m_NumOfSpeedCooling + 1);
                if (this->m_CoolMassFlowRate.size() == 0) this->m_CoolMassFlowRate.resize(this->m_NumOfSpeedCooling + 1);
                if (this->m_MSCoolingSpeedRatio.size() == 0) this->m_MSCoolingSpeedRatio.resize(this->m_NumOfSpeedCooling + 1);
            }
            MSHPIndex = this->m_DesignSpecMSHPIndex;

            if (MSHPIndex > -1) {
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedCooling; Iter > 0; --Iter) {
                    if (designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter - 1] == DataSizing::AutoSize)
                        designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter - 1] = double(Iter) / double(designSpecMSHP[MSHPIndex].numOfSpeedCooling);
                    this->m_CoolVolumeFlowRate[Iter] = this->m_MaxCoolAirVolFlow * designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter - 1];
                    this->m_CoolMassFlowRate[Iter] = this->m_CoolVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                    this->m_MSCoolingSpeedRatio[Iter] = this->m_CoolVolumeFlowRate[Iter] / this->m_DesignFanVolFlowRate;
                }
                this->m_IdleVolumeAirRate = this->m_MaxCoolAirVolFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
                this->m_IdleMassFlowRate = this->m_IdleVolumeAirRate * DataEnvironment::StdRhoAir;
                this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
            } else if (this->m_CoolVolumeFlowRate.size() == 0) {
                this->m_IdleVolumeAirRate = this->m_MaxNoCoolHeatAirVolFlow;
                this->m_IdleMassFlowRate = this->MaxNoCoolHeatAirMassFlow;
                this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
            }
        }

        if (this->m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
            this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
            this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {

            if (this->m_NumOfSpeedHeating > 0) {
                if (this->m_HeatVolumeFlowRate.size() == 0) this->m_HeatVolumeFlowRate.resize(this->m_NumOfSpeedHeating + 1);
                if (this->m_HeatMassFlowRate.size() == 0) this->m_HeatMassFlowRate.resize(this->m_NumOfSpeedHeating + 1);
                if (this->m_MSHeatingSpeedRatio.size() == 0) this->m_MSHeatingSpeedRatio.resize(this->m_NumOfSpeedHeating + 1);
            }

            MSHPIndex = this->m_DesignSpecMSHPIndex;

            if (MSHPIndex > -1) {
                // use reverse order since we divide by HeatVolumeFlowRate(max)
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedHeating; Iter > 0; --Iter) {
                    if (designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter - 1] == DataSizing::AutoSize) {
                        if (this->m_ControlType == ControlType::Setpoint &&
                            (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                             this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {
                            designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter - 1] = 1.0;
                        } else {
                            designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter - 1] =
                                double(Iter) / double(designSpecMSHP[MSHPIndex].numOfSpeedHeating);
                        }
                    } else {
                        if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                            this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {
                            if (designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter - 1] < 1.0 && this->m_ControlType == ControlType::Setpoint) {
                                ShowWarningError(RoutineName + ": " + CompType + " = " + CompName);
                                ShowContinueError("Design specification object = " + designSpecMSHP[MSHPIndex].name);
                                ShowContinueError("When control type = SetPointBased the outlet air temperature must change with coil capacity, if "
                                                  "air flow also changes outlet air temperature will be relatively constant.");
                                ShowContinueError("Speed " + General::TrimSigDigits(Iter) +
                                                  " Supply Air Flow Ratio During Heating Operation will be set = 1.0 and the simulation continues");
                                designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter - 1] = 1.0;
                            }
                        }
                    }
                    this->m_HeatVolumeFlowRate[Iter] = this->m_MaxHeatAirVolFlow * designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter - 1];
                    this->m_HeatMassFlowRate[Iter] = this->m_HeatVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                    this->m_MSHeatingSpeedRatio[Iter] = this->m_HeatVolumeFlowRate[Iter] / this->m_DesignFanVolFlowRate;
                }
                if (this->m_CoolCoilExists) {
                    if (this->m_CoolVolumeFlowRate.size() > 0 && MSHPIndex > -1) {
                        this->m_IdleVolumeAirRate =
                            min(this->m_IdleVolumeAirRate, this->m_MaxHeatAirVolFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio);
                        this->m_IdleMassFlowRate =
                            min(this->m_IdleMassFlowRate, this->MaxHeatAirMassFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio);
                        this->m_IdleSpeedRatio = min(this->m_IdleSpeedRatio, this->m_MaxNoCoolHeatAirVolFlow / this->m_DesignFanVolFlowRate);
                    } else {
                        this->m_IdleVolumeAirRate = this->m_MaxNoCoolHeatAirVolFlow;
                        this->m_IdleMassFlowRate = this->MaxNoCoolHeatAirMassFlow;
                        this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
                    }
                } else if (MSHPIndex > 0) {
                    this->m_IdleVolumeAirRate = this->m_MaxHeatAirVolFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
                    this->m_IdleMassFlowRate = this->m_IdleVolumeAirRate * DataEnvironment::StdRhoAir;
                    this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
                } else {
                    this->m_IdleVolumeAirRate = this->m_MaxNoCoolHeatAirVolFlow;
                    this->m_IdleMassFlowRate = this->MaxNoCoolHeatAirMassFlow;
                    this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
                }
            }
        } else if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                   this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {

            MSHPIndex = this->m_DesignSpecMSHPIndex;
            if (MSHPIndex > -1) {
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedHeating; Iter > 0; --Iter) {
                    if (designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter - 1] == DataSizing::AutoSize) {
                        designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter - 1] = double(Iter) / double(designSpecMSHP[MSHPIndex].numOfSpeedHeating);
                    }
                }
            }

            VariableSpeedCoils::SimVariableSpeedCoils(blankString,
                                                      this->m_HeatingCoilIndex,
                                                      0,
                                                      this->m_MaxONOFFCyclesperHour,
                                                      this->m_HPTimeConstant,
                                                      this->m_FanDelayTime,
                                                      0,
                                                      0.0,
                                                      1,
                                                      0.0,
                                                      0.0,
                                                      0.0,
                                                      0.0); // conduct the sizing operation in the VS WSHP

            if (this->m_NumOfSpeedHeating != VariableSpeedCoils::VarSpeedCoil(this->m_HeatingCoilIndex).NumOfSpeeds) {
                ShowWarningError(RoutineName + ": " + CompType + " = " + CompName);
                ShowContinueError("Number of heating speeds does not match coil object.");
                ShowFatalError("Heating coil = " + VariableSpeedCoils::VarSpeedCoil(this->m_HeatingCoilIndex).VarSpeedCoilType + ": " +
                               VariableSpeedCoils::VarSpeedCoil(this->m_HeatingCoilIndex).Name);
            }

            if (this->m_NumOfSpeedHeating > 0) {
                if (this->m_HeatVolumeFlowRate.size() == 0) this->m_HeatVolumeFlowRate.resize(this->m_NumOfSpeedHeating + 1);
                if (this->m_HeatMassFlowRate.size() == 0) this->m_HeatMassFlowRate.resize(this->m_NumOfSpeedHeating + 1);
                if (this->m_MSHeatingSpeedRatio.size() == 0) this->m_MSHeatingSpeedRatio.resize(this->m_NumOfSpeedHeating + 1);
            }

            for (Iter = 1; Iter <= this->m_NumOfSpeedHeating; ++Iter) {
                this->m_HeatVolumeFlowRate[Iter] = VariableSpeedCoils::VarSpeedCoil(this->m_HeatingCoilIndex).MSRatedAirVolFlowRate(Iter);
                this->m_HeatMassFlowRate[Iter] = this->m_HeatVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                if (this->m_DesignFanVolFlowRate > 0.0 && this->m_FanExists) {
                    this->m_MSHeatingSpeedRatio[Iter] = this->m_HeatVolumeFlowRate[Iter] / this->m_DesignFanVolFlowRate;
                } else {
                    this->m_MSHeatingSpeedRatio[Iter] = this->m_HeatVolumeFlowRate[Iter] / this->m_HeatVolumeFlowRate[this->m_NumOfSpeedHeating];
                }
            }

            if (this->m_CoolCoilExists && this->m_NumOfSpeedHeating > 0) {
                if (this->m_CoolVolumeFlowRate.size() > 0 && MSHPIndex > 0) {
                    this->m_IdleVolumeAirRate =
                        min(this->m_IdleVolumeAirRate, this->m_MaxHeatAirVolFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio);
                    this->m_IdleMassFlowRate =
                        min(this->m_IdleMassFlowRate, this->MaxHeatAirMassFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio);
                    this->m_IdleSpeedRatio = min(this->m_IdleSpeedRatio, this->m_MaxNoCoolHeatAirVolFlow / this->m_DesignFanVolFlowRate);
                } else if (this->m_CoolVolumeFlowRate.size() == 0 && MSHPIndex > 0) {
                    this->m_IdleVolumeAirRate = this->m_MaxHeatAirVolFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
                    this->m_IdleMassFlowRate = this->MaxHeatAirMassFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
                    this->m_IdleSpeedRatio =
                        this->m_MSHeatingSpeedRatio[this->m_NumOfSpeedHeating] * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
                } else if (this->m_CoolVolumeFlowRate.size() > 0) {
                    this->m_IdleVolumeAirRate = min(this->m_IdleVolumeAirRate, this->m_MaxNoCoolHeatAirVolFlow);
                    this->m_IdleMassFlowRate = min(this->m_IdleMassFlowRate, this->MaxNoCoolHeatAirMassFlow);
                    this->m_IdleSpeedRatio = min(this->m_IdleSpeedRatio, this->m_MaxNoCoolHeatAirVolFlow / this->m_DesignFanVolFlowRate);
                } else {
                    this->m_IdleVolumeAirRate = this->m_MaxNoCoolHeatAirVolFlow;
                    this->m_IdleMassFlowRate = this->MaxNoCoolHeatAirMassFlow;
                    this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
                }
            } else if (MSHPIndex > 0) {
                this->m_IdleVolumeAirRate = this->m_MaxHeatAirVolFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
                this->m_IdleMassFlowRate = this->m_IdleVolumeAirRate * DataEnvironment::StdRhoAir;
                this->m_IdleSpeedRatio = this->m_MSHeatingSpeedRatio[this->m_NumOfSpeedHeating] * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
            } else {
                this->m_IdleVolumeAirRate = this->m_MaxNoCoolHeatAirVolFlow;
                this->m_IdleMassFlowRate = this->MaxNoCoolHeatAirMassFlow;
                this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
            }
        }
        if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {

            if (this->m_NumOfSpeedHeating > 0) {
                if (this->m_HeatVolumeFlowRate.size() == 0) this->m_HeatVolumeFlowRate.resize(this->m_NumOfSpeedHeating + 1);
                if (this->m_HeatMassFlowRate.size() == 0) this->m_HeatMassFlowRate.resize(this->m_NumOfSpeedHeating + 1);
                if (this->m_MSHeatingSpeedRatio.size() == 0) this->m_MSHeatingSpeedRatio.resize(this->m_NumOfSpeedHeating + 1);
            }

            MSHPIndex = this->m_DesignSpecMSHPIndex;
            if (MSHPIndex > -1) {
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedHeating; Iter > 0; --Iter) {
                    if (designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter - 1] == DataSizing::AutoSize) {
                        designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter - 1] = double(Iter) / double(designSpecMSHP[MSHPIndex].numOfSpeedHeating);
                    }
                    this->m_HeatVolumeFlowRate[Iter] = this->m_MaxHeatAirVolFlow * designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter - 1];
                    this->m_HeatMassFlowRate[Iter] = this->m_HeatVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                    this->m_MSHeatingSpeedRatio[Iter] = this->m_HeatVolumeFlowRate[Iter] / this->m_DesignFanVolFlowRate;
                }
                if (this->m_CoolCoilExists) {
                    if (this->m_CoolVolumeFlowRate.size() > 0 && MSHPIndex > 0) {
                        this->m_IdleVolumeAirRate =
                            min(this->m_IdleVolumeAirRate, this->m_MaxHeatAirVolFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio);
                        this->m_IdleMassFlowRate =
                            min(this->m_IdleMassFlowRate, this->MaxHeatAirMassFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio);
                        this->m_IdleSpeedRatio = min(this->m_IdleSpeedRatio, this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate);
                    } else {
                        this->m_IdleVolumeAirRate = min(this->m_IdleVolumeAirRate, this->m_MaxNoCoolHeatAirVolFlow);
                        this->m_IdleMassFlowRate = min(this->m_IdleMassFlowRate, this->MaxNoCoolHeatAirMassFlow);
                        this->m_IdleSpeedRatio = min(this->m_IdleSpeedRatio, (this->m_MaxNoCoolHeatAirVolFlow / this->m_DesignFanVolFlowRate));
                    }
                } else if (MSHPIndex > 0) {
                    this->m_IdleVolumeAirRate = this->m_MaxHeatAirVolFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
                    this->m_IdleMassFlowRate = this->MaxHeatAirMassFlow * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
                    this->m_IdleSpeedRatio =
                        this->m_MSHeatingSpeedRatio[this->m_NumOfSpeedHeating] * designSpecMSHP[MSHPIndex].noLoadAirFlowRateRatio;
                } else {
                    this->m_IdleVolumeAirRate = this->m_MaxNoCoolHeatAirVolFlow;
                    this->m_IdleMassFlowRate = this->MaxNoCoolHeatAirMassFlow;
                    this->m_IdleSpeedRatio = this->m_IdleVolumeAirRate / this->m_DesignFanVolFlowRate;
                }
            }
        }

        // Not sure if this may be needed for special cases
        if (this->m_CoolCoilExists && this->m_MaxCoolAirVolFlow < 0.0) {
            if (!DataSizing::SysSizingRunDone) {
                BranchNum = BranchInputManager::GetAirBranchIndex("AirloopHVAC:UnitarySystem", this->Name);
                FanType = "";
                m_FanName = "";
                BranchFanFlow = 0.0;
                if (BranchNum > 0.0) BranchInputManager::GetBranchFanTypeName(BranchNum, FanType, m_FanName, ErrFound);
                if (!ErrFound && BranchNum > 0) {
                    if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        BranchFanFlow = HVACFan::fanObjs[this->m_FanIndex]->designAirVolFlowRate;
                    } else {
                        BranchFanFlow = Fans::GetFanDesignVolumeFlowRate(FanType, m_FanName, ErrFound);
                    }
                }
                if (BranchFanFlow > 0.0) {
                    this->m_MaxCoolAirVolFlow = BranchFanFlow;
                } else {
                    SystemFlow = 0.0;
                    if (AirLoopNum > 0.0) SystemFlow = DataAirSystems::PrimaryAirSystem(AirLoopNum).DesignVolFlowRate;
                    if (SystemFlow > 0.0) {
                        this->m_MaxCoolAirVolFlow = SystemFlow;
                    } else {
                        // what do I do?
                    }
                }
            }
        }

        // why is this here?
        this->m_SenLoadLoss = 0.0;
        if (this->m_Humidistat) {
            this->m_LatLoadLoss = 0.0;
        }

        if (this->m_CoolCoilExists) {

            SizingMethod = DataHVACGlobals::CoolingCapacitySizing;
            // water coils must report their size to parent objects (or split out sizing routines for water coils so they can be call from here)
            if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {
                WaterCoils::SimulateWaterCoilComponents(
                    this->m_CoolingCoilName, FirstHVACIteration, this->m_CoolingCoilIndex, QActual, this->m_FanOpMode, 1.0);
                DataSizing::DataConstantUsedForSizing = WaterCoils::GetWaterCoilCapacity(
                    UtilityRoutines::MakeUPPERCase(DataHVACGlobals::cAllCoilTypes(this->m_CoolingCoilType_Num)), this->m_CoolingCoilName, ErrFound);
                EqSizing.DesCoolingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->m_DesignCoolingCapacity = DataSizing::AutoSize;
            } else if (this->m_CoolingCoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted) {
                HXCoilName = HVACHXAssistedCoolingCoil::GetHXDXCoilName(
                    DataHVACGlobals::cAllCoilTypes(this->m_CoolingCoilType_Num), this->m_CoolingCoilName, ErrFound);
                ActualCoolCoilType = HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(
                    DataHVACGlobals::cAllCoilTypes(this->m_CoolingCoilType_Num), this->m_CoolingCoilName, ErrFound, true);
                HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(blankString, true, On, 1.0, this->m_CoolingCoilIndex, 1, false, 1.0, false);
                DataSizing::DataConstantUsedForSizing = WaterCoils::GetWaterCoilCapacity(
                    UtilityRoutines::MakeUPPERCase(DataHVACGlobals::cAllCoilTypes(ActualCoolCoilType)), HXCoilName, ErrFound);
                EqSizing.DesCoolingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->m_DesignCoolingCapacity = DataSizing::AutoSize;
            } else if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {
                WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                this->m_CoolingCoilIndex,
                                                                this->m_CoolingCoilSensDemand,
                                                                this->m_CoolingCoilLatentDemand,
                                                                0,
                                                                0.0,
                                                                this->m_MaxONOFFCyclesperHour,
                                                                this->m_HPTimeConstant,
                                                                this->m_FanDelayTime,
                                                                0,
                                                                0.0,
                                                                FirstHVACIteration);
                DataSizing::DataConstantUsedForSizing = WaterToAirHeatPumpSimple::GetCoilCapacity(
                    DataHVACGlobals::cAllCoilTypes(this->m_CoolingCoilType_Num), this->m_CoolingCoilName, ErrFound);
                EqSizing.DesCoolingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->m_DesignCoolingCapacity = DataSizing::AutoSize;
                if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                    this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP)
                    EqSizing.DesHeatingLoad = DataSizing::DataConstantUsedForSizing;
            } else if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP) {
                WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                    this->m_CoolingCoilIndex,
                                                    this->MaxCoolAirMassFlow,
                                                    this->m_FanOpMode,
                                                    FirstHVACIteration,
                                                    0.0,
                                                    this->m_MaxONOFFCyclesperHour,
                                                    this->m_HPTimeConstant,
                                                    this->m_FanDelayTime,
                                                    this->m_InitHeatPump,
                                                    0.0,
                                                    0.0,
                                                    0,
                                                    0.0);
                DataSizing::DataConstantUsedForSizing = WaterToAirHeatPump::GetCoilCapacity(
                    DataHVACGlobals::cAllCoilTypes(this->m_CoolingCoilType_Num), this->m_CoolingCoilName, ErrFound);
                EqSizing.DesCoolingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                    this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple)
                    EqSizing.DesHeatingLoad = DataSizing::DataConstantUsedForSizing;
            } else if (this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {
                PackagedThermalStorageCoil::SimTESCoil(this->m_CoolingCoilName, this->m_CoolingCoilIndex, this->m_FanOpMode, this->m_TESOpMode, 0.0);
                PackagedThermalStorageCoil::GetTESCoilCoolingCapacity(
                    this->m_CoolingCoilName, DataSizing::DataConstantUsedForSizing, ErrFound, CompType);
                EqSizing.DesCoolingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
            }

            TempSize = this->m_DesignCoolingCapacity;
            DataSizing::DataFlowUsedForSizing = this->m_MaxCoolAirVolFlow;
            SizingString = "Nominal Cooling Capacity [W]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->m_DesignCoolingCapacity = TempSize;
            DataSizing::DataConstantUsedForSizing = 0.0;
            DataSizing::DataFractionUsedForSizing = 0.0;
            DataSizing::DataFlowUsedForSizing = 0.0;
        }

        if (this->m_HeatCoilExists) {

            SizingMethod = DataHVACGlobals::HeatingCapacitySizing;

            // water coils must report their size to parent objects (or split out sizing routines for water coils so they can be call from here)
            if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                WaterCoils::SimulateWaterCoilComponents(
                    this->m_HeatingCoilName, FirstHVACIteration, this->m_HeatingCoilIndex, QActual, this->m_FanOpMode, 1.0);
                DataSizing::DataConstantUsedForSizing = WaterCoils::GetWaterCoilCapacity(
                    UtilityRoutines::MakeUPPERCase(DataHVACGlobals::cAllCoilTypes(this->m_HeatingCoilType_Num)), this->m_HeatingCoilName, ErrFound);
                EqSizing.DesHeatingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->m_DesignHeatingCapacity = DataSizing::AutoSize;
            }

            TempSize = this->m_DesignHeatingCapacity;
            SizingString = "Nominal Heating Capacity [W]";
            if (DataSizing::CurSysNum > 0)
                DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating =
                    false; // set to false to allow calculation of parent object heating capacity
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) DataSizing::DXCoolCap = TempSize;
            if (DataSizing::CurSysNum > 0) DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating = true;
            this->m_DesignHeatingCapacity = TempSize;
            DataSizing::DataConstantUsedForSizing = 0.0;
            DataSizing::DataFractionUsedForSizing = 0.0;
            DataSizing::DataHeatSizeRatio = 1.0;
        }

        DataSizing::UnitaryHeatCap = this->m_DesignHeatingCapacity;

        if ((this->m_HeatCoilExists || this->m_SuppCoilExists) && this->m_ControlType != ControlType::CCMASHRAE) {

            SizingMethod = DataHVACGlobals::MaxHeaterOutletTempSizing;
            TempSize = this->DesignMaxOutletTemp;
            FieldNum = 17; // N17, \field Maximum Supply Air Temperature
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [C]";
            SizingString = "Maximum Supply Air Temperature [C]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->DesignMaxOutletTemp = TempSize;
        }

        if (this->m_SuppCoilExists) {

            SizingMethod = DataHVACGlobals::HeatingCapacitySizing;

            PrintFlag = false;
            TempSize = this->m_DesignSuppHeatingCapacity;
            SizingString = "Supplemental Heating Coil Nominal Capacity [W]";
            if (TempSize == DataSizing::AutoSize) {
                IsAutoSize = true;
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                this->m_DesignSuppHeatingCapacity = TempSize;
            }

            if (this->m_Humidistat && this->m_DehumidControlType_Num == DehumCtrlType::CoolReheat && IsAutoSize) {
                DataSizing::DataConstantUsedForSizing = max(this->m_DesignSuppHeatingCapacity, this->m_DesignCoolingCapacity);
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                TempSize = DataSizing::AutoSize;
            }

            if (this->m_OKToPrintSizing) PrintFlag = true;
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->m_DesignSuppHeatingCapacity = TempSize;
            IsAutoSize = false;
            DataSizing::DataConstantUsedForSizing = 0.0;
            DataSizing::DataFractionUsedForSizing = 0.0;

            DataSizing::SuppHeatCap = this->m_DesignSuppHeatingCapacity;
        }

        // register plant flow rate. Not sure this has ever been tested.
        if (this->m_HeatRecActive) {
            PlantUtilities::RegisterPlantCompDesignFlow(this->m_HeatRecoveryInletNodeNum, this->m_DesignHRWaterVolumeFlow);
        }

        // Set flow rate for unitary system with no fan
        if (DataSizing::CurOASysNum == 0 && DataSizing::CurZoneEqNum == 0 && this->m_DesignFanVolFlowRate <= 0.0) {
            SystemFlow = 0;
            if (AirLoopNum > 0) SystemFlow = DataAirSystems::PrimaryAirSystem(AirLoopNum).DesignVolFlowRate;
            if (SystemFlow > 0.0) {
                this->m_DesignFanVolFlowRate = SystemFlow;
            } else {
                this->m_DesignFanVolFlowRate = max(this->m_MaxCoolAirVolFlow, this->m_MaxHeatAirVolFlow);
            }
            this->m_DesignMassFlowRate = this->m_DesignFanVolFlowRate * DataEnvironment::StdRhoAir;
        }

        // Moved from InitLoadBasedControl
        // Find the number of zones (zone Inlet Nodes) attached to an air loop from the air loop number
        if (this->m_AirLoopEquipment && this->m_ControlType != ControlType::Setpoint) {
            if (allocated(DataAirLoop::AirToZoneNodeInfo))
                NumAirLoopZones =
                    DataAirLoop::AirToZoneNodeInfo(AirLoopNum).NumZonesCooled + DataAirLoop::AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
            if (allocated(DataAirLoop::AirToZoneNodeInfo)) {
                initLoadBasedControlFlowFracFlagReady = true;
                for (ZoneInSysIndex = 1; ZoneInSysIndex <= NumAirLoopZones; ++ZoneInSysIndex) {
                    // zone inlet nodes for cooling
                    if (DataAirLoop::AirToZoneNodeInfo(AirLoopNum).NumZonesCooled > 0) {
                        if (DataAirLoop::AirToZoneNodeInfo(AirLoopNum).TermUnitCoolInletNodes(ZoneInSysIndex) == -999) {
                            // the data structure for the zones inlet nodes has not been filled
                            initLoadBasedControlFlowFracFlagReady = false;
                        }
                    }
                    // zone inlet nodes for heating
                    if (DataAirLoop::AirToZoneNodeInfo(AirLoopNum).NumZonesHeated > 0) {
                        if (DataAirLoop::AirToZoneNodeInfo(AirLoopNum).TermUnitHeatInletNodes(ZoneInSysIndex) == -999) {
                            // the data structure for the zones inlet nodes has not been filled
                            initLoadBasedControlFlowFracFlagReady = false;
                        }
                    }
                }
            }
            if (allocated(DataAirLoop::AirToZoneNodeInfo) && initLoadBasedControlFlowFracFlagReady) {
                SumOfMassFlowRateMax = 0.0; // initialize the sum of the maximum flows
                for (ZoneInSysIndex = 1; ZoneInSysIndex <= NumAirLoopZones; ++ZoneInSysIndex) {
                    ZoneInletNodeNum = DataAirLoop::AirToZoneNodeInfo(AirLoopNum).TermUnitCoolInletNodes(ZoneInSysIndex);
                    SumOfMassFlowRateMax += DataLoopNode::Node(ZoneInletNodeNum).MassFlowRateMax;
                    if (DataAirLoop::AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZoneInSysIndex) == this->ControlZoneNum) {
                        initLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax = DataLoopNode::Node(ZoneInletNodeNum).MassFlowRateMax;
                    }
                }
                if (SumOfMassFlowRateMax != 0.0) {
                    if (initLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax >= DataHVACGlobals::SmallAirVolFlow) {
                        this->ControlZoneMassFlowFrac = initLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax / SumOfMassFlowRateMax;
                    } else {
                        ShowSevereError(this->UnitType + " = " + this->Name);
                        ShowContinueError(" The Fraction of Supply Air Flow That Goes Through the Controlling Zone is set to 1.");
                        this->ControlZoneMassFlowFrac = 1.0;
                    }
                    ReportSizingManager::ReportSizingOutput(this->UnitType,
                                                            this->Name,
                                                            "Fraction of Supply Air Flow That Goes Through the Controlling Zone",
                                                            this->ControlZoneMassFlowFrac);
                }
            }
        } else {
            this->ControlZoneMassFlowFrac = 1.0;
        }

        if (this->m_ControlType == ControlType::CCMASHRAE) {

            SizingDesRunThisSys = false;
            DataSizing::DataZoneUsedForSizing = this->ControlZoneNum;
            CheckThisZoneForSizing(DataSizing::DataZoneUsedForSizing, SizingDesRunThisSys);

            SizingMethod = DataHVACGlobals::ASHRAEMinSATCoolingSizing;
            capacityMultiplier = 0.5; // one-half of design zone load
            if (SizingDesRunThisSys) {
                DataSizing::DataCapacityUsedForSizing = DataSizing::FinalZoneSizing(this->ControlZoneNum).DesCoolLoad * capacityMultiplier;
            } else {
                DataSizing::DataCapacityUsedForSizing = this->m_DesignCoolingCapacity * capacityMultiplier;
            }
            DataSizing::DataCapacityUsedForSizing /= this->ControlZoneMassFlowFrac;
            DataSizing::DataFlowUsedForSizing = this->m_MaxNoCoolHeatAirVolFlow;
            FieldNum = 2; // Minimum Supply Air Temperature in Cooling Mode
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [C]";
            SizingString = "Minimum Supply Air Temperature [C]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, this->DesignMinOutletTemp, PrintFlag, RoutineName);

            SizingMethod = DataHVACGlobals::ASHRAEMaxSATHeatingSizing;
            FieldNum = 17; // Maximum Supply Air Temperature in Heating Mode
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [C]";
            SizingString = "Maximum Supply Air Temperature [C]";
            if (SizingDesRunThisSys) {
                DataSizing::DataCapacityUsedForSizing = DataSizing::FinalZoneSizing(this->ControlZoneNum).DesHeatLoad * capacityMultiplier;
            } else {
                DataSizing::DataCapacityUsedForSizing = this->m_DesignHeatingCapacity * capacityMultiplier;
            }
            DataSizing::DataCapacityUsedForSizing /= this->ControlZoneMassFlowFrac;
            DataSizing::DataFlowUsedForSizing = this->m_MaxNoCoolHeatAirVolFlow;
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, this->DesignMaxOutletTemp, PrintFlag, RoutineName);

            DataSizing::DataCapacityUsedForSizing = 0.0; // reset so other routines don't use this inadvertently
            DataSizing::DataFlowUsedForSizing = 0.0;
            DataSizing::DataZoneUsedForSizing = 0;

            // check that MaxNoCoolHeatAirVolFlow is less than both MaxCoolAirVolFlow and MaxHeatAirVolFlow
            if (this->m_MaxNoCoolHeatAirVolFlow >= this->m_MaxCoolAirVolFlow || this->m_MaxNoCoolHeatAirVolFlow >= this->m_MaxHeatAirVolFlow) {
                ShowSevereError(this->UnitType + " = " + this->Name);
                ShowContinueError(" For SingleZoneVAV control the No Load Supply Air Flow Rate must be less than both the cooling and heating supply "
                                  "air flow rates.");
                this->m_MaxNoCoolHeatAirVolFlow = min(this->m_MaxCoolAirVolFlow, this->m_MaxHeatAirVolFlow) - 0.01;
                ShowContinueError(" The SingleZoneVAV control No Load Supply Air Flow Rate is reset to " +
                                  General::TrimSigDigits(this->m_MaxNoCoolHeatAirVolFlow, 5) + " and the simulation continues.");
            }
        }

        CoolingLoad = TempCoolingLoad;
        HeatingLoad = TempHeatingLoad;
        // if (++NumUnitarySystemsSized == NumUnitarySystem)
        //    UnitarySystemNumericFields.deallocate(); // remove temporary array for field names at end of sizing
    }

    void UnitarySys::getUnitarySystemInputData(std::string const &objectName, bool const ZoneEquipment, int const ZoneOAUnitNum, bool &errorsFound)
    {

        static std::string const getUnitarySystemInput("getUnitarySystemInputData");
        static std::string const unitarySysHeatPumpPerformanceObjectType("UnitarySystemPerformance:Multispeed");

        std::string cCurrentModuleObject = "AirLoopHVAC:UnitarySystem";

        auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == inputProcessor->epJSON.end()) {
            ShowSevereError("getUnitarySystemInputData: did not find AirLoopHVAC:UnitarySystem object in input file. Check inputs");
            errorsFound = true;
        } else {
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                auto const &thisObjectName = UtilityRoutines::MakeUPPERCase(instance.key());
                // only get the current data once all data has been read in and vector unitarySys has been initialized
                // when UnitarySystems::getInputOnceFlag is true read all unitary systems, otherwise read just the curren object
                if (!UtilityRoutines::SameString(objectName, thisObjectName) && !UnitarySystems::getInputOnceFlag) continue;

                int sysNum = getUnitarySystemIndex(thisObjectName);
                UnitarySys thisSys;
                if (sysNum == -1) {
                    ++numUnitarySystems;
                } else {
                    thisSys = unitarySys[sysNum];
                    // *************** used only to eliminate unused object warning when using only Json type getInput **********
                    int TotalArgs = 0;
                    int NumAlphas = 0;
                    int NumNumbers = 0;
                    inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);
                    int IOStatus = 0;
                    Array1D_string Alphas(NumAlphas);
                    Array1D<Real64> Numbers(NumNumbers, 0.0);
                    Array1D_bool lNumericBlanks(NumNumbers, true);
                    Array1D_bool lAlphaBlanks(NumAlphas, true);
                    Array1D_string cAlphaFields(NumAlphas);
                    Array1D_string cNumericFields(NumNumbers);
                    inputProcessor->getObjectItem(cCurrentModuleObject,
                                                  sysNum + 1,
                                                  Alphas,
                                                  NumAlphas,
                                                  Numbers,
                                                  NumNumbers,
                                                  IOStatus,
                                                  lNumericBlanks,
                                                  lAlphaBlanks,
                                                  cAlphaFields,
                                                  cNumericFields);
                    // **********************************************************************************************************
                }

                auto const &fields = instance.value();
                thisSys.UnitType = cCurrentModuleObject;
                thisSys.m_unitarySystemType_Num = DataHVACGlobals::UnitarySys_AnyCoilType;
                thisSys.Name = UtilityRoutines::MakeUPPERCase(thisObjectName);

                std::string loc_AirInNodeName = UtilityRoutines::MakeUPPERCase(fields.at("air_inlet_node_name")); // required field

                if (UnitarySystems::getInputOnceFlag)
                    thisSys.AirInNode = NodeInputManager::GetOnlySingleNode(loc_AirInNodeName,
                                                                            errorsFound,
                                                                            cCurrentModuleObject,
                                                                            thisObjectName,
                                                                            DataLoopNode::NodeType_Air,
                                                                            DataLoopNode::NodeConnectionType_Inlet,
                                                                            1,
                                                                            DataLoopNode::ObjectIsParent);

                std::string loc_AirOutNodeName = UtilityRoutines::MakeUPPERCase(fields.at("air_outlet_node_name")); // required field

                if (UnitarySystems::getInputOnceFlag)
                    thisSys.AirOutNode = NodeInputManager::GetOnlySingleNode(loc_AirOutNodeName,
                                                                             errorsFound,
                                                                             cCurrentModuleObject,
                                                                             thisObjectName,
                                                                             DataLoopNode::NodeType_Air,
                                                                             DataLoopNode::NodeConnectionType_Outlet,
                                                                             1,
                                                                             DataLoopNode::ObjectIsParent);

                // Early calls to ATMixer don't have enough info to pass GetInput. Need push_back here, and protect against the next time through.
                if (sysNum == -1 || !DataZoneEquipment::ZoneEquipInputsFilled) {
                    if (sysNum == -1) unitarySys.push_back(thisSys);
                    continue;
                }

                if (ZoneEquipment) {
                    thisSys.UnitarySystemType_Num = DataZoneEquipment::ZoneUnitarySys_Num;
                    thisSys.m_OKToPrintSizing = true;
                } else {
                    thisSys.UnitarySystemType_Num = SimAirServingZones::UnitarySystemModel;
                }

                thisSys.m_IterationMode.resize(21);

                std::string loc_heatingCoilType("");
                if (fields.find("heating_coil_object_type") != fields.end()) { // not required field
                    loc_heatingCoilType = UtilityRoutines::MakeUPPERCase(fields.at("heating_coil_object_type"));
                }

                std::string loc_m_HeatingCoilName("");
                if (fields.find("heating_coil_name") != fields.end()) { // not required field
                    loc_m_HeatingCoilName = UtilityRoutines::MakeUPPERCase(fields.at("heating_coil_name"));
                }

                Real64 loc_m_HeatingSizingRatio(1.0);
                if (fields.find("dx_heating_coil_sizing_ratio") != fields.end()) { // not required field, has default
                    loc_m_HeatingSizingRatio = fields.at("dx_heating_coil_sizing_ratio");
                }

                std::string loc_coolingCoilType("");
                if (fields.find("cooling_coil_object_type") != fields.end()) { // not required field
                    loc_coolingCoilType = UtilityRoutines::MakeUPPERCase(fields.at("cooling_coil_object_type"));
                }

                std::string loc_m_CoolingCoilName("");
                if (fields.find("cooling_coil_name") != fields.end()) { // not required field
                    loc_m_CoolingCoilName = UtilityRoutines::MakeUPPERCase(fields.at("cooling_coil_name"));
                }

                std::string loc_m_ISHundredPercentDOASDXCoil("No");
                if (fields.find("use_doas_dx_cooling_coil") != fields.end()) { // not required field, has default
                    loc_m_ISHundredPercentDOASDXCoil = UtilityRoutines::MakeUPPERCase(fields.at("use_doas_dx_cooling_coil"));
                }

                Real64 loc_DesignMinOutletTemp(2.0);
                if (fields.find("minimum_supply_air_temperature") != fields.end()) { // not required field, has default
                    loc_DesignMinOutletTemp = fields.at("minimum_supply_air_temperature");
                }

                std::string loc_latentControlFlag("SensibleOnlyLoadControl");
                if (fields.find("latent_load_control") != fields.end()) { // not required field, has default
                    loc_latentControlFlag = UtilityRoutines::MakeUPPERCase(fields.at("latent_load_control"));
                }

                std::string loc_suppHeatCoilType("");
                if (fields.find("supplemental_heating_coil_object_type") != fields.end()) { // not required field
                    loc_suppHeatCoilType = UtilityRoutines::MakeUPPERCase(fields.at("supplemental_heating_coil_object_type"));
                }

                std::string loc_m_SuppHeatCoilName("");
                if (fields.find("supplemental_heating_coil_name") != fields.end()) { // not required field
                    loc_m_SuppHeatCoilName = UtilityRoutines::MakeUPPERCase(fields.at("supplemental_heating_coil_name"));
                }

                std::string loc_m_CoolingSAFMethod("");
                if (fields.find("cooling_supply_air_flow_rate_method") != fields.end()) { // not required field
                    loc_m_CoolingSAFMethod = UtilityRoutines::MakeUPPERCase(fields.at("cooling_supply_air_flow_rate_method"));
                }

                Real64 loc_m_CoolingSAFMethod_SAFlow(-999.0);
                if (fields.find("cooling_supply_air_flow_rate") != fields.end()) { // not required field, autosizable
                    auto tempFieldVal = fields.at("cooling_supply_air_flow_rate");
                    if (tempFieldVal == "Autosize") {
                        loc_m_CoolingSAFMethod_SAFlow = DataSizing::AutoSize;
                    } else {
                        loc_m_CoolingSAFMethod_SAFlow = fields.at("cooling_supply_air_flow_rate");
                    }
                }

                Real64 loc_m_CoolingSAFMethod_SAFlowPerFloorArea(-999.0);
                if (fields.find("cooling_supply_air_flow_rate_per_floor_area") != fields.end()) { // not required field
                    loc_m_CoolingSAFMethod_SAFlowPerFloorArea = fields.at("cooling_supply_air_flow_rate_per_floor_area");
                }

                Real64 loc_m_CoolingSAFMethod_FracOfAutosizedCoolingSAFlow(-999.0);
                if (fields.find("cooling_fraction_of_autosized_cooling_supply_air_flow") != fields.end()) { // not required field
                    loc_m_CoolingSAFMethod_FracOfAutosizedCoolingSAFlow = fields.at("cooling_fraction_of_autosized_cooling_supply_air_flow");
                }

                Real64 loc_m_CoolingSAFMethod_FlowPerCoolingCapacity(-999.0);
                if (fields.find("cooling_supply_air_flow_rate_per_unit_of_capacity") != fields.end()) { // not required field
                    loc_m_CoolingSAFMethod_FlowPerCoolingCapacity = fields.at("cooling_supply_air_flow_rate_per_unit_of_capacity");
                }

                std::string loc_m_HeatingSAFMethod("");
                if (fields.find("heating_supply_air_flow_rate_method") != fields.end()) { // not required field
                    loc_m_HeatingSAFMethod = UtilityRoutines::MakeUPPERCase(fields.at("heating_supply_air_flow_rate_method"));
                }

                Real64 loc_m_HeatingSAFMethod_SAFlow(-999.0);
                if (fields.find("heating_supply_air_flow_rate") != fields.end()) { // not required field
                    auto tempFieldVal = fields.at("heating_supply_air_flow_rate");
                    if (tempFieldVal == "Autosize") {
                        loc_m_HeatingSAFMethod_SAFlow = DataSizing::AutoSize;
                    } else {
                        loc_m_HeatingSAFMethod_SAFlow = fields.at("heating_supply_air_flow_rate");
                    }
                }

                Real64 loc_m_HeatingSAFMethod_SAFlowPerFloorArea(-999.0);
                if (fields.find("heating_supply_air_flow_rate_per_floor_area") != fields.end()) { // not required field
                    loc_m_HeatingSAFMethod_SAFlowPerFloorArea = fields.at("heating_supply_air_flow_rate_per_floor_area");
                }

                Real64 loc_m_HeatingSAFMethod_FracOfAutosizedHeatingSAFlow(-999.0);
                if (fields.find("heating_fraction_of_autosized_heating_supply_air_flow") != fields.end()) { // not required field
                    loc_m_HeatingSAFMethod_FracOfAutosizedHeatingSAFlow = fields.at("heating_fraction_of_autosized_heating_supply_air_flow");
                }

                Real64 loc_m_HeatingSAFMethod_FlowPerHeatingCapacity(-999.0);
                if (fields.find("heating_supply_air_flow_rate_per_unit_of_capacity") != fields.end()) { // not required field
                    loc_m_HeatingSAFMethod_FlowPerHeatingCapacity = fields.at("heating_supply_air_flow_rate_per_unit_of_capacity");
                }

                std::string loc_m_NoCoolHeatSAFMethod("");
                if (fields.find("no_load_supply_air_flow_rate_method") != fields.end()) { // not required field
                    loc_m_NoCoolHeatSAFMethod = UtilityRoutines::MakeUPPERCase(fields.at("no_load_supply_air_flow_rate_method"));
                }

                Real64 loc_m_NoCoolHeatSAFMethod_SAFlow(-999.0);
                if (fields.find("no_load_supply_air_flow_rate") != fields.end()) { // not required field
                    auto tempFieldVal = fields.at("no_load_supply_air_flow_rate");
                    if (tempFieldVal == "Autosize") {
                        loc_m_NoCoolHeatSAFMethod_SAFlow = DataSizing::AutoSize;
                    } else {
                        loc_m_NoCoolHeatSAFMethod_SAFlow = fields.at("no_load_supply_air_flow_rate");
                    }
                }

                Real64 loc_m_NoCoolHeatSAFMethod_SAFlowPerFloorArea(-999.0);
                if (fields.find("no_load_supply_air_flow_rate_per_floor_area") != fields.end()) { // not required field
                    loc_m_NoCoolHeatSAFMethod_SAFlowPerFloorArea = fields.at("no_load_supply_air_flow_rate_per_floor_area");
                }

                Real64 loc_m_NoCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow(-999.0);
                if (fields.find("no_load_fraction_of_autosized_cooling_supply_air_flow") != fields.end()) { // not required field
                    loc_m_NoCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow = fields.at("no_load_fraction_of_autosized_cooling_supply_air_flow");
                }

                Real64 loc_m_NoCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow(-999.0);
                if (fields.find("no_load_fraction_of_autosized_heating_supply_air_flow") != fields.end()) { // not required field
                    loc_m_NoCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow = fields.at("no_load_fraction_of_autosized_heating_supply_air_flow");
                }

                Real64 loc_m_NoCoolHeatSAFMethod_FlowPerCoolingCapacity(-999.0);
                if (fields.find("no_load_supply_air_flow_rate_per_unit_of_capacity_during_cooling_operation") != fields.end()) { // not required field
                    loc_m_NoCoolHeatSAFMethod_FlowPerCoolingCapacity =
                        fields.at("no_load_supply_air_flow_rate_per_unit_of_capacity_during_cooling_operation");
                }

                Real64 loc_m_NoCoolHeatSAFMethod_FlowPerHeatingCapacity(-999.0);
                if (fields.find("heating_supply_air_flow_rate_per_unit_of_capacity_during_heating_operation") != fields.end()) { // not required field
                    loc_m_NoCoolHeatSAFMethod_FlowPerHeatingCapacity =
                        fields.at("heating_supply_air_flow_rate_per_unit_of_capacity_during_heating_operation");
                }

                Real64 loc_DesignMaxOutletTemp(80.0);
                if (fields.find("maximum_supply_air_temperature") != fields.end()) { // not required field, has default of 80 C
                    auto tempFieldVal = fields.at("maximum_supply_air_temperature");
                    if (tempFieldVal == "Autosize") {
                        loc_DesignMaxOutletTemp = DataSizing::AutoSize;
                    } else {
                        loc_DesignMaxOutletTemp = fields.at("maximum_supply_air_temperature");
                    }
                }

                Real64 loc_m_MaxOATSuppHeat(21.0);
                if (fields.find("maximum_outdoor_dry_bulb_temperature_for_supplemental_heater_operation") !=
                    fields.end()) { // not required field, has default
                    loc_m_MaxOATSuppHeat = fields.at("maximum_outdoor_dry_bulb_temperature_for_supplemental_heater_operation");
                }

                std::string loc_condenserInletNodeName = "";
                if (fields.find("outdoor_dry_bulb_temperature_sensor_node_name") != fields.end()) { // not required field
                    loc_condenserInletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("outdoor_dry_bulb_temperature_sensor_node_name"));
                }

                Real64 loc_m_MaxONOFFCyclesperHour(2.5);
                if (fields.find("maximum_cycling_rate") != fields.end()) { // not required field, has default
                    loc_m_MaxONOFFCyclesperHour = fields.at("maximum_cycling_rate");
                }

                Real64 loc_m_HPTimeConstant(60.0);
                if (fields.find("heat_pump_time_constant") != fields.end()) { // not required field, has default
                    loc_m_HPTimeConstant = fields.at("heat_pump_time_constant");
                }

                Real64 loc_m_OnCyclePowerFraction(0.01);
                if (fields.find("fraction_of_on_cycle_power_use") != fields.end()) { // not required field, has default
                    loc_m_OnCyclePowerFraction = fields.at("fraction_of_on_cycle_power_use");
                }

                Real64 loc_m_FanDelayTime(60.0);
                if (fields.find("heat_pump_fan_delay_time") != fields.end()) { // not required field, has default
                    loc_m_FanDelayTime = fields.at("heat_pump_fan_delay_time");
                }

                Real64 loc_m_AncillaryOnPower(0.0);
                if (fields.find("ancillary_on_cycle_electric_power") != fields.end()) { // not required field, has default
                    loc_m_AncillaryOnPower = fields.at("ancillary_on_cycle_electric_power");
                }

                Real64 loc_m_AncillaryOffPower(0.0);
                if (fields.find("ancillary_off_cycle_electric_power") != fields.end()) { // not required field, has default
                    loc_m_AncillaryOffPower = fields.at("ancillary_off_cycle_electric_power");
                }

                Real64 loc_m_DesignHRWaterVolumeFlow(0.0);
                if (fields.find("design_heat_recovery_water_flow_rate") != fields.end()) { // not required field, has default
                    loc_m_DesignHRWaterVolumeFlow = fields.at("design_heat_recovery_water_flow_rate");
                }

                Real64 loc_m_MaxHROutletWaterTemp(80.0);
                if (fields.find("maximum_temperature_for_heat_recovery") != fields.end()) { // not required field, has default
                    loc_m_MaxHROutletWaterTemp = fields.at("maximum_temperature_for_heat_recovery");
                }

                std::string loc_heatRecoveryInletNodeName = "";
                if (fields.find("heat_recovery_water_inlet_node_name") != fields.end()) { // not required field
                    loc_heatRecoveryInletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("heat_recovery_water_inlet_node_name"));
                }

                std::string loc_heatRecoveryOutletNodeName = "";
                if (fields.find("heat_recovery_water_outlet_node_name") != fields.end()) { // not required field
                    loc_heatRecoveryOutletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("heat_recovery_water_outlet_node_name"));
                }

                std::string loc_m_DesignSpecMultispeedHPType = "";
                if (fields.find("design_specification_multispeed_object_type") != fields.end()) { // not required field
                    loc_m_DesignSpecMultispeedHPType = UtilityRoutines::MakeUPPERCase(fields.at("design_specification_multispeed_object_type"));
                }

                std::string loc_m_DesignSpecMultispeedHPName = "";
                if (fields.find("design_specification_multispeed_object_name") != fields.end()) { // not required field
                    loc_m_DesignSpecMultispeedHPName = UtilityRoutines::MakeUPPERCase(fields.at("design_specification_multispeed_object_name"));
                }

                int FanInletNode = 0;
                int FanOutletNode = 0;
                Real64 FanVolFlowRate = 0.0;
                int CoolingCoilInletNode = 0;
                int CoolingCoilOutletNode = 0;
                int HeatingCoilInletNode = 0;
                int HeatingCoilOutletNode = 0;
                int SupHeatCoilInletNode = 0;
                int SupHeatCoilOutletNode = 0;

                bool errFlag = false;
                bool isNotOK = false;

                std::string loc_sysAvailSched("");
                if (fields.find("availability_schedule_name") != fields.end()) { // not required field
                    loc_sysAvailSched = UtilityRoutines::MakeUPPERCase(fields.at("availability_schedule_name"));
                    thisSys.m_SysAvailSchedPtr = ScheduleManager::GetScheduleIndex(loc_sysAvailSched);
                } else {
                    thisSys.m_SysAvailSchedPtr = DataGlobals::ScheduleAlwaysOn;
                }

                std::string loc_m_ControlType = fields.at("control_type");

                if (UtilityRoutines::SameString(loc_m_ControlType, "Load")) {
                    thisSys.m_ControlType = ControlType::Load;
                } else if (UtilityRoutines::SameString(loc_m_ControlType, "SetPoint")) {
                    thisSys.m_ControlType = ControlType::Setpoint;
                } else if (UtilityRoutines::SameString(loc_m_ControlType, "SingleZoneVAV")) {
                    thisSys.m_ControlType = ControlType::CCMASHRAE;
                    thisSys.m_ValidASHRAECoolCoil = true;
                    thisSys.m_ValidASHRAEHeatCoil = true;
                } else {
                    ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                    ShowContinueError("Invalid Control Type = " + loc_m_ControlType);
                    errorsFound = true;
                }

                std::string loc_controlZoneName("");
                if (fields.find("controlling_zone_or_thermostat_location") != fields.end()) { // not required field
                    loc_controlZoneName = UtilityRoutines::MakeUPPERCase(fields.at("controlling_zone_or_thermostat_location"));
                } else if (thisSys.m_ControlType == ControlType::Load || thisSys.m_ControlType == ControlType::CCMASHRAE) {
                    ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                    ShowContinueError("Controlling Zone or Thermostat Location cannot be blank when Control Type = Load or SingleZoneVAV");
                    errorsFound = true;
                }
                if (loc_controlZoneName != "") thisSys.ControlZoneNum = UtilityRoutines::FindItemInList(loc_controlZoneName, DataHeatBalance::Zone);
                //// check that control zone name is valid for load based control
                // if (UnitarySystem(UnitarySysNum).ControlType == LoadBased || UnitarySystem(UnitarySysNum).ControlType == CCM_ASHRAE) {
                //    if (UnitarySystem(UnitarySysNum).ControlZoneNum == 0) {
                //        ShowSevereError(CurrentModuleObject + ": " + UnitarySystem(UnitarySysNum).Name);
                //        ShowContinueError("When " + cAlphaFields(iControlTypeAlphaNum) + " = " + Alphas(iControlTypeAlphaNum));
                //        ShowContinueError(cAlphaFields(iControlZoneAlphaNum) + " must be a valid zone name, zone name = " +
                //        Alphas(iControlZoneAlphaNum)); ErrorsFound = true;
                //    }
                //}

                std::string loc_dehumm_ControlType("");
                if (fields.find("dehumidification_control_type") != fields.end()) { // not required field, has default
                    loc_dehumm_ControlType = UtilityRoutines::MakeUPPERCase(fields.at("dehumidification_control_type"));
                } else {
                    loc_dehumm_ControlType = "NONE"; // default value
                }

                if (UtilityRoutines::SameString(loc_dehumm_ControlType, "None")) {
                    thisSys.m_DehumidControlType_Num = DehumCtrlType::None;
                    thisSys.m_Humidistat = false;
                } else if (UtilityRoutines::SameString(loc_dehumm_ControlType, "CoolReheat")) {
                    thisSys.m_DehumidControlType_Num = DehumCtrlType::CoolReheat;
                    thisSys.m_Humidistat = true;
                } else if (UtilityRoutines::SameString(loc_dehumm_ControlType, "Multimode")) {
                    thisSys.m_DehumidControlType_Num = DehumCtrlType::Multimode;
                    thisSys.m_Humidistat = true;
                } else {
                    ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                    ShowContinueError("Illegal dehumidification control type = " + loc_dehumm_ControlType);
                    thisSys.m_Humidistat = false;
                    errorsFound = true;
                }
                if (thisSys.m_Humidistat && thisSys.m_ControlType == ControlType::Load) {
                    bool AirNodeFound = false;
                    for (int HStatZoneNum = 1; HStatZoneNum <= DataZoneControls::NumHumidityControlZones; ++HStatZoneNum) {
                        if (DataZoneControls::HumidityControlZone(HStatZoneNum).ActualZoneNum != thisSys.ControlZoneNum) continue;
                        AirNodeFound = true;
                    }
                    if (!AirNodeFound && thisSys.ControlZoneNum > 0) {
                        ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                        ShowContinueError("Did not find Air Node (Zone with Humidistat).");
                        ShowContinueError("specified Control Zone Name = " + loc_controlZoneName);
                        errorsFound = true;
                    }
                }

                Real64 TotalFloorAreaOnAirLoop = 0.0;
                int AirLoopNumber = 0;
                bool AirNodeFound = false;
                bool AirLoopFound = false;
                bool OASysFound = false;
                bool ZoneEquipmentFound = false;
                bool ZoneInletNodeFound = false;

                // Get AirTerminal mixer data
                SingleDuct::GetATMixer(thisObjectName,
                                       thisSys.m_ATMixerName,
                                       thisSys.m_ATMixerIndex,
                                       thisSys.ATMixerType,
                                       thisSys.m_ATMixerPriNode,
                                       thisSys.m_ATMixerSecNode,
                                       thisSys.ATMixerOutNode,
                                       thisSys.AirOutNode);
                if (thisSys.ATMixerType == DataHVACGlobals::ATMixer_InletSide || thisSys.ATMixerType == DataHVACGlobals::ATMixer_SupplySide) {
                    thisSys.ATMixerExists = true;
                }

                // if part of ZoneHVAC:OutdoorAirUnit bypass most checks for connection to air loop or OASystem
                if (ZoneOAUnitNum > 0) OASysFound = true;

                // check if the UnitarySystem is connected as zone equipment
                if (!thisSys.ATMixerExists && !AirLoopFound && !OASysFound) {
                    for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                        for (int ZoneExhNum = 1; ZoneExhNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes; ++ZoneExhNum) {
                            if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum) != thisSys.AirInNode) continue;
                            ZoneEquipmentFound = true;
                            //               Find the controlled zone number for the specified thermostat location
                            thisSys.NodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                            TotalFloorAreaOnAirLoop =
                                DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum).FloorArea;
                            thisSys.m_AirLoopEquipment = false;
                            thisSys.m_ZoneInletNode = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum);
                            if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex > 0) {
                                for (int EquipNum = 1; EquipNum <= DataZoneEquipment::ZoneEquipList(
                                                                       DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                                       .NumOfEquipTypes;
                                     ++EquipNum) {
                                    if ((DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                             .EquipType_Num(EquipNum) != DataZoneEquipment::ZoneUnitarySys_Num) ||
                                        DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .EquipName(EquipNum) != thisObjectName)
                                        continue;
                                    thisSys.m_ZoneSequenceCoolingNum =
                                        DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                            .CoolingPriority(EquipNum);
                                    thisSys.m_ZoneSequenceHeatingNum =
                                        DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                            .HeatingPriority(EquipNum);
                                }
                            }
                            thisSys.ControlZoneNum = ControlledZoneNum;
                            break;
                        }
                        if (ZoneEquipmentFound) {
                            for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                 ++ZoneInletNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.AirOutNode) continue;
                                ZoneInletNodeFound = true;
                                break;
                            }
                        }
                    }
                    if (!ZoneInletNodeFound) {
                        for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                            for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                 ++ZoneInletNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.AirOutNode) continue;
                                ZoneInletNodeFound = true;
                                ZoneEquipmentFound = true;
                                break;
                            }
                        }
                        if (!ZoneInletNodeFound && ZoneEquipmentFound) {
                            ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                            ShowContinueError("Incorrect or misspelled Air Outlet Node Name = " + loc_AirOutNodeName);
                            // ShowContinueError("Incorrect or misspelled " + cAlphaFields(iAirOutletNodeNameAlphaNum) + " = " +
                            //                  Alphas(iAirOutletNodeNameAlphaNum));
                            ShowContinueError("Node name does not match any controlled zone inlet node name. Check ZoneHVAC:EquipmentConnections "
                                              "object inputs.");
                            errorsFound = true;
                        }
                    }
                }

                // check if the UnitarySystem is connected as zone equipment
                if (thisSys.ATMixerExists && thisSys.ATMixerType == DataHVACGlobals::ATMixer_InletSide) {

                    if (!AirLoopFound && !OASysFound) {
                        for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                            for (int ZoneExhNum = 1; ZoneExhNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes;
                                 ++ZoneExhNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum) != thisSys.m_ATMixerSecNode)
                                    continue;
                                ZoneEquipmentFound = true;
                                //               Find the controlled zone number for the specified thermostat location
                                thisSys.NodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                                TotalFloorAreaOnAirLoop =
                                    DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum).FloorArea;
                                thisSys.m_AirLoopEquipment = false;
                                thisSys.m_ZoneInletNode = thisSys.AirOutNode;
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex > 0) {
                                    for (int EquipNum = 1; EquipNum <= DataZoneEquipment::ZoneEquipList(
                                                                           DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                                           .NumOfEquipTypes;
                                         ++EquipNum) {
                                        if ((DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                 .EquipType_Num(EquipNum) != DataZoneEquipment::ZoneUnitarySys_Num) ||
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                    .EquipName(EquipNum) != thisObjectName)
                                            continue;
                                        thisSys.m_ZoneSequenceCoolingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .CoolingPriority(EquipNum);
                                        thisSys.m_ZoneSequenceHeatingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .HeatingPriority(EquipNum);
                                    }
                                }
                                break;
                            }
                            if (ZoneEquipmentFound) {
                                for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                     ++ZoneInletNum) {
                                    if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.AirOutNode) continue;
                                    ZoneInletNodeFound = true;
                                    break;
                                }
                            }
                        }
                        if (!ZoneInletNodeFound) {
                            for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                                for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                     ++ZoneInletNum) {
                                    if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.AirOutNode) continue;
                                    ZoneInletNodeFound = true;
                                    ZoneEquipmentFound = true;
                                    break;
                                }
                            }
                            if (!ZoneInletNodeFound && ZoneEquipmentFound) {
                                ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                                ShowContinueError("Incorrect or misspelled Air Outlet Node Name = " + loc_AirOutNodeName);
                                // ShowContinueError("Incorrect or misspelled " + cAlphaFields(iAirOutletNodeNameAlphaNum) + " = " +
                                //                  Alphas(iAirOutletNodeNameAlphaNum));
                                ShowContinueError("Node name does not match any controlled zone inlet node name. Check ZoneHVAC:EquipmentConnections "
                                                  "object inputs.");
                                errorsFound = true;
                            }
                        }
                    }
                }

                if (thisSys.ATMixerExists && thisSys.ATMixerType == DataHVACGlobals::ATMixer_SupplySide) {

                    if (!AirLoopFound && !OASysFound) {
                        for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                            for (int ZoneExhNum = 1; ZoneExhNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes;
                                 ++ZoneExhNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum) != thisSys.AirInNode) continue;
                                ZoneEquipmentFound = true;
                                //               Find the controlled zone number for the specified thermostat location
                                thisSys.NodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                                TotalFloorAreaOnAirLoop =
                                    DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum).FloorArea;
                                thisSys.m_AirLoopEquipment = false;
                                thisSys.m_ZoneInletNode = thisSys.ATMixerOutNode;
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex > 0) {
                                    for (int EquipNum = 1; EquipNum <= DataZoneEquipment::ZoneEquipList(
                                                                           DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                                           .NumOfEquipTypes;
                                         ++EquipNum) {
                                        if ((DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                 .EquipType_Num(EquipNum) != DataZoneEquipment::ZoneUnitarySys_Num) ||
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                    .EquipName(EquipNum) != thisObjectName)
                                            continue;
                                        thisSys.m_ZoneSequenceCoolingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .CoolingPriority(EquipNum);
                                        thisSys.m_ZoneSequenceHeatingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .HeatingPriority(EquipNum);
                                    }
                                }
                                break;
                            }
                            if (ZoneEquipmentFound) {
                                for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                     ++ZoneInletNum) {
                                    if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.ATMixerOutNode)
                                        continue;
                                    ZoneInletNodeFound = true;
                                    break;
                                }
                            }
                        }
                        if (!ZoneInletNodeFound) {
                            for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                                for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                     ++ZoneInletNum) {
                                    if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.ATMixerOutNode)
                                        continue;
                                    ZoneInletNodeFound = true;
                                    ZoneEquipmentFound = true;
                                    break;
                                }
                            }
                            if (!ZoneInletNodeFound && ZoneEquipmentFound) {
                                ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                                ShowContinueError("Incorrect or misspelled Air Outlet Node Name = " + loc_AirOutNodeName);
                                // ShowContinueError("Incorrect or misspelled " + cAlphaFields(iAirOutletNodeNameAlphaNum) + " = " +
                                //                  Alphas(iAirOutletNodeNameAlphaNum));
                                ShowContinueError("Node name does not match any air terminal mixer secondary air inlet node. Check "
                                                  "AirTerminal:SingleDuct:Mixer object inputs.");
                                errorsFound = true;
                            }
                        }
                    }
                }

                if (!ZoneEquipmentFound) {
                    // check if the UnitarySystem is connected to an air loop
                    for (int AirLoopNum = 1; AirLoopNum <= DataHVACGlobals::NumPrimaryAirSys; ++AirLoopNum) {
                        for (int BranchNum = 1; BranchNum <= DataAirSystems::PrimaryAirSystem(AirLoopNum).NumBranches; ++BranchNum) {
                            for (int CompNum = 1; CompNum <= DataAirSystems::PrimaryAirSystem(AirLoopNum).Branch(BranchNum).TotalComponents;
                                 ++CompNum) {
                                if (UtilityRoutines::SameString(DataAirSystems::PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name,
                                                                thisObjectName) &&
                                    UtilityRoutines::SameString(DataAirSystems::PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(CompNum).TypeOf,
                                                                cCurrentModuleObject)) {
                                    AirLoopNumber = AirLoopNum;
                                    AirLoopFound = true;
                                    for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                                        if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum != thisSys.ControlZoneNum) continue;
                                        //             Find the controlled zone number for the specified thermostat location
                                        thisSys.NodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                                        //             Determine if system is on air loop served by the thermostat location specified
                                        for (int zoneInNode = 1; zoneInNode <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                             ++zoneInNode) {
                                            if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode) ==
                                                AirLoopNumber) {
                                                thisSys.m_ZoneInletNode = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(zoneInNode);
                                                TotalFloorAreaOnAirLoop +=
                                                    DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum)
                                                        .FloorArea;
                                            }
                                        }
                                        // if (thisSys.m_ZoneInletNode == 0) AirLoopFound = false;
                                        for (int TstatZoneNum = 1; TstatZoneNum <= DataZoneControls::NumTempControlledZones; ++TstatZoneNum) {
                                            if (DataZoneControls::TempControlledZone(TstatZoneNum).ActualZoneNum != thisSys.ControlZoneNum) continue;
                                            AirNodeFound = true;
                                        }
                                        for (int TstatZoneNum = 1; TstatZoneNum <= DataZoneControls::NumComfortControlledZones; ++TstatZoneNum) {
                                            if (DataZoneControls::ComfortControlledZone(TstatZoneNum).ActualZoneNum != thisSys.ControlZoneNum)
                                                continue;
                                            AirNodeFound = true;
                                        }
                                        if (!AirNodeFound && thisSys.ControlZoneNum > 0) {
                                            ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                                            ShowContinueError("Did not find Air Node (Zone with Thermostat or Thermal Comfort Thermostat).");
                                            ShowContinueError("specified Control Zone Name = " + loc_controlZoneName);
                                            errorsFound = true;
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    // check if the UnitarySystem is connected to an outside air system
                    if (!AirLoopFound && DataSizing::CurOASysNum > 0) {
                        for (int OASysNum = 1; OASysNum <= DataAirLoop::NumOASystems; ++OASysNum) {
                            for (int OACompNum = 1; OACompNum <= DataAirLoop::OutsideAirSys(OASysNum).NumComponents; ++OACompNum) {
                                if (!UtilityRoutines::SameString(DataAirLoop::OutsideAirSys(OASysNum).ComponentName(OACompNum), thisObjectName) ||
                                    !UtilityRoutines::SameString(DataAirLoop::OutsideAirSys(OASysNum).ComponentType(OACompNum), cCurrentModuleObject))
                                    continue;
                                AirLoopNumber = OASysNum;
                                OASysFound = true;
                                break;
                            }
                        }
                    }
                }

                // if (!AirLoopFound && !ZoneEquipmentFound && !OASysFound && !DataHVACGlobals::GetAirPathDataDone) {
                if (!AirLoopFound && !ZoneEquipmentFound && !OASysFound) {
                    // Unsucessful attempt. Need to push_back here, and protect against the next time through.
                    // If no push back, then memory will be reallocated next time factory is called with next push_back
                    if (sysNum == -1) unitarySys.push_back(thisSys);
                    continue; // will this do the trick and avoid protecting everything below here?
                } else {
                    if (AirLoopFound && (thisSys.m_ZoneInletNode > 0 || thisSys.m_ControlType == ControlType::Setpoint)) {
                        thisSys.m_OKToPrintSizing = true;
                        thisSys.m_ThisSysInputShouldBeGotten = false;
                    } else if (ZoneEquipmentFound) {
                        thisSys.m_OKToPrintSizing = true;
                        thisSys.m_ThisSysInputShouldBeGotten = false;
                    } else if (OASysFound) {
                        thisSys.m_OKToPrintSizing = true;
                        thisSys.m_ThisSysInputShouldBeGotten = false;
                    }
                }

                if (AirLoopNumber == 0 && !ZoneEquipmentFound &&
                    (thisSys.m_ControlType == ControlType::Load || thisSys.m_ControlType == ControlType::CCMASHRAE)) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    ShowContinueError("Did not find proper connection for AirLoopHVAC or ZoneHVAC system.");
                    // ShowContinueError("specified " + cAlphaFields(iControlZoneAlphaNum) + " = " + Alphas(iControlZoneAlphaNum));
                    if (!AirNodeFound && !ZoneEquipmentFound) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("Did not find air node (zone with thermostat).");
                        // ShowContinueError("specified " + cAlphaFields(iControlZoneAlphaNum) + " = " + Alphas(iControlZoneAlphaNum));
                        ShowContinueError(
                            "Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object must be specified for this zone.");
                    }
                    errorsFound = true;
                }

                if (!ZoneEquipmentFound)
                    BranchNodeConnections::TestCompSet(
                        cCurrentModuleObject, UtilityRoutines::MakeUPPERCase(thisObjectName), loc_AirInNodeName, loc_AirOutNodeName, "Air Nodes");

                std::string loc_fanType("");
                if (fields.find("supply_fan_object_type") != fields.end()) { // not required field
                    loc_fanType = UtilityRoutines::MakeUPPERCase(fields.at("supply_fan_object_type"));
                }

                std::string loc_m_FanName("");
                if (fields.find("supply_fan_name") != fields.end()) { // not required field
                    loc_m_FanName = UtilityRoutines::MakeUPPERCase(fields.at("supply_fan_name"));
                }

                if (loc_m_FanName != "" && loc_fanType != "") {
                    if (UtilityRoutines::SameString(loc_fanType, "Fan:SystemModel")) {
                        if (!HVACFan::checkIfFanNameIsAFanSystem(loc_m_FanName)) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {
                            thisSys.m_FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                            isNotOK = false;
                            ValidateComponent(loc_fanType, loc_m_FanName, isNotOK, cCurrentModuleObject);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            } else {                                                                  // mine data from fan object
                                HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(loc_m_FanName)); // call constructor
                                thisSys.m_FanIndex = HVACFan::getFanObjectVectorIndex(loc_m_FanName);
                                if (thisSys.m_FanIndex > -1) {
                                    FanInletNode = HVACFan::fanObjs[thisSys.m_FanIndex]->inletNodeNum;
                                    FanOutletNode = HVACFan::fanObjs[thisSys.m_FanIndex]->outletNodeNum;
                                    thisSys.m_FanAvailSchedPtr = HVACFan::fanObjs[thisSys.m_FanIndex]->availSchedIndex;
                                    FanVolFlowRate = HVACFan::fanObjs[thisSys.m_FanIndex]->designAirVolFlowRate;
                                } else {
                                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                    ShowContinueError("Unable to access fan data.");
                                    ShowContinueError("Fan Type = " + loc_fanType + ", Fan name = " + loc_m_FanName);
                                    errorsFound = true;
                                }
                                if (FanVolFlowRate == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                                thisSys.m_ActualFanVolFlowRate = FanVolFlowRate;
                                thisSys.m_DesignFanVolFlowRate = FanVolFlowRate;
                            }
                        }
                    } else {
                        Fans::GetFanType(loc_m_FanName, thisSys.m_FanType_Num, isNotOK, cCurrentModuleObject, loc_m_FanName);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {
                            isNotOK = false;
                            ValidateComponent(loc_fanType, loc_m_FanName, isNotOK, cCurrentModuleObject);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            } else { // mine data from fan object
                                // Get the fan index
                                Fans::GetFanIndex(loc_m_FanName, thisSys.m_FanIndex, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Design Fan Volume Flow Rate
                                errFlag = false;
                                FanVolFlowRate = Fans::GetFanDesignVolumeFlowRate(loc_fanType, loc_m_FanName, errFlag);
                                if (FanVolFlowRate == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                                thisSys.m_ActualFanVolFlowRate = FanVolFlowRate;
                                thisSys.m_DesignFanVolFlowRate = FanVolFlowRate;
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Fan Inlet Node
                                errFlag = false;
                                FanInletNode = Fans::GetFanInletNode(loc_fanType, loc_m_FanName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Fan Outlet Node
                                errFlag = false;
                                FanOutletNode = Fans::GetFanOutletNode(loc_fanType, loc_m_FanName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the fan's availability schedule
                                errFlag = false;
                                thisSys.m_FanAvailSchedPtr = Fans::GetFanAvailSchPtr(loc_fanType, loc_m_FanName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                            } // IF (IsNotOK) THEN
                        }
                    }
                    thisSys.m_FanExists = true;
                    thisSys.m_FanName = loc_m_FanName;
                } else {
                    if ((loc_m_FanName == "" && loc_fanType != "") || (loc_m_FanName != "" && loc_fanType == "")) {
                        ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                        ShowContinueError("Invalid Fan Type or Name: Fan Name = " + loc_m_FanName + ", Fan Type = " + loc_fanType);
                        errorsFound = true;
                    }
                }

                // Add fan to component sets array
                if (thisSys.m_FanExists && thisSys.m_FanCompNotSetYet) {
                    BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                         thisObjectName,
                                                         loc_fanType,
                                                         loc_m_FanName,
                                                         DataLoopNode::NodeID(FanInletNode),
                                                         DataLoopNode::NodeID(FanOutletNode));
                    thisSys.m_FanCompNotSetYet = false;
                }

                std::string loc_supFanPlace("");
                if (fields.find("fan_placement") != fields.end()) { // not required field
                    loc_supFanPlace = UtilityRoutines::MakeUPPERCase(fields.at("fan_placement"));
                }
                if (UtilityRoutines::SameString(loc_supFanPlace, "BlowThrough")) thisSys.m_FanPlace = FanPlace::BlowThru;
                if (UtilityRoutines::SameString(loc_supFanPlace, "DrawThrough")) thisSys.m_FanPlace = FanPlace::DrawThru;
                if (thisSys.m_FanPlace == FanPlace::NotYetSet && thisSys.m_FanExists) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    ShowContinueError("Illegal Fan Placement = " + loc_supFanPlace);
                    errorsFound = true;
                }

                std::string loc_supFanOpMode("");
                if (fields.find("supply_air_fan_operating_mode_schedule_name") != fields.end()) { // not required field
                    loc_supFanOpMode = UtilityRoutines::MakeUPPERCase(fields.at("supply_air_fan_operating_mode_schedule_name"));
                }

                thisSys.m_FanOpModeSchedPtr = ScheduleManager::GetScheduleIndex(loc_supFanOpMode);
                if (loc_supFanOpMode != "" && thisSys.m_FanOpModeSchedPtr == 0) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    ShowContinueError("Illegal Fan Operating Mode Schedule Name = " + loc_supFanOpMode);
                    // ShowContinueError("Illegal " + cAlphaFields(iFanSchedAlphaNum) + " = " + Alphas(iFanSchedAlphaNum));
                    errorsFound = true;
                } else if (loc_supFanOpMode == "") {
                    if (thisSys.m_ControlType == ControlType::Setpoint) {
                        // Fan operating mode must be constant fan so that the coil outlet temp is proportional to PLR
                        // Cycling fan always outputs the full load outlet air temp so should not be used with set point based control
                        thisSys.m_FanOpMode = DataHVACGlobals::ContFanCycCoil;
                    } else {
                        thisSys.m_FanOpMode = DataHVACGlobals::CycFanCycCoil;
                        if (thisSys.m_FanType_Num != DataHVACGlobals::FanType_SimpleOnOff &&
                            thisSys.m_FanType_Num != DataHVACGlobals::FanType_SystemModelObject && thisSys.m_FanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError(cAlphaFields(iFanTypeAlphaNum) + " = " + Alphas(iFanTypeAlphaNum));
                            // ShowContinueError("Fan type must be Fan:OnOff of Fan:SystemModel when " + cAlphaFields(iFanSchedAlphaNum) + " =
                            // Blank.");
                            ShowContinueError(
                                "Fan type must be Fan:OnOff or Fan:SystemModel when Supply Air Fan Operating Mode Schedule Name is blank.");
                            errorsFound = true;
                        }
                    }
                } else if (loc_supFanOpMode != "" && thisSys.m_FanOpMode > 0 && thisSys.m_ControlType == ControlType::Setpoint) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.m_FanOpModeSchedPtr, ">", 0.0, "<=", 1.0)) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("For " + loc_fanType + " = " + loc_m_FanName);
                        ShowContinueError("Fan operating mode must be continuous (fan operating mode schedule values > 0).");
                        // ShowContinueError("Error found in " + cAlphaFields(iFanSchedAlphaNum) + " = " + Alphas(iFanSchedAlphaNum));
                        ShowContinueError("...schedule values must be (>0., <=1.)");
                        errorsFound = true;
                    }
                }

                // Check fan's schedule for cycling fan operation IF constant volume fan is used
                if (thisSys.m_FanOpModeSchedPtr > 0 && thisSys.m_FanType_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.m_FanOpModeSchedPtr, ">", 0.0, "<=", 1.0)) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("For " + cAlphaFields(iFanTypeAlphaNum) + " = " + Alphas(iFanTypeAlphaNum));
                        ShowContinueError("Fan operating mode must be continuous (fan operating mode schedule values > 0).");
                        // ShowContinueError("Error found in " + cAlphaFields(iFanSchedAlphaNum) + " = " + Alphas(iFanSchedAlphaNum));
                        ShowContinueError("...schedule values must be (>0., <=1.)");
                        errorsFound = true;
                    }
                }

                bool PrintMessage = true;
                // Get coil data
                thisSys.m_HeatingSizingRatio = loc_m_HeatingSizingRatio;
                int HeatingCoilPLFCurveIndex = 0;
                thisSys.m_HeatingCoilName = loc_m_HeatingCoilName;
                thisSys.m_HeatingCoilTypeName = loc_heatingCoilType; //  for coil selection report
                if (loc_heatingCoilType != "") {
                    thisSys.m_HeatCoilExists = true;
                    PrintMessage = false;
                } else {
                    thisSys.m_ValidASHRAEHeatCoil = false;
                }

                if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:DX:VariableSpeed")) {
                    thisSys.m_HeatingCoilType_Num = DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:DX:MultiSpeed")) {
                    thisSys.m_HeatingCoilType_Num = DataHVACGlobals::CoilDX_MultiSpeedHeating;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Water")) {
                    thisSys.m_HeatingCoilType_Num = DataHVACGlobals::Coil_HeatingWater;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Steam")) {
                    thisSys.m_HeatingCoilType_Num = DataHVACGlobals::Coil_HeatingSteam;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:WaterToAirHeatPump:EquationFit")) {
                    thisSys.m_HeatingCoilType_Num = DataHVACGlobals::Coil_HeatingWaterToAirHPSimple;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:WaterToAirHeatPump:ParameterEstimation")) {
                    thisSys.m_HeatingCoilType_Num = DataHVACGlobals::Coil_HeatingWaterToAirHP;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit")) {
                    thisSys.m_HeatingCoilType_Num = DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Electric:MultiStage")) {
                    thisSys.m_HeatingCoilType_Num = DataHVACGlobals::Coil_HeatingElectric_MultiStage;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Gas:MultiStage")) {
                    thisSys.m_HeatingCoilType_Num = DataHVACGlobals::Coil_HeatingGas_MultiStage;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Fuel") ||
                           UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Electric") ||
                           UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Desuperheater")) {
                    thisSys.m_HeatingCoilType_Num = HeatingCoils::GetHeatingCoilTypeNum(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:UserDefined")) {
                    thisSys.m_HeatingCoilType_Num = DataHVACGlobals::Coil_UserDefined;
                } else if (thisSys.m_HeatCoilExists) {
                    thisSys.m_HeatingCoilType_Num = DXCoils::GetCoilTypeNum(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag, PrintMessage);
                }

                if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {

                    thisSys.m_DXHeatingCoil = true;
                    errFlag = false;

                    ValidateComponent(loc_heatingCoilType, loc_m_HeatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;

                    } else { // mine data from DX heating coil

                        // Get DX heating coil index
                        DXCoils::GetDXCoilIndex(loc_m_HeatingCoilName, thisSys.m_HeatingCoilIndex, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.m_HeatingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get DX heating coil capacity
                        thisSys.m_DesignHeatingCapacity = DXCoils::GetCoilCapacity(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (thisSys.m_DesignHeatingCapacity == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get DX coil air flow rate.
                        thisSys.m_MaxHeatAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (thisSys.m_MaxHeatAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Nodes
                        HeatingCoilInletNode = DXCoils::GetCoilInletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        HeatingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        DXCoils::SetDXCoolingCoilData(
                            thisSys.m_HeatingCoilIndex, errorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, loc_m_HeatingSizingRatio);
                    }

                } else if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                           thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) {

                    thisSys.m_DXHeatingCoil = true;
                    errFlag = false;

                    ValidateComponent(loc_heatingCoilType, loc_m_HeatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else {

                        thisSys.m_HeatingCoilIndex =
                            VariableSpeedCoils::GetCoilIndexVariableSpeed(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.m_NumOfSpeedHeating = VariableSpeedCoils::GetVSCoilNumOfSpeeds(loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.m_HeatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;

                        thisSys.m_MaxHeatAirVolFlow =
                            VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (thisSys.m_MaxHeatAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        HeatingCoilInletNode = VariableSpeedCoils::GetCoilInletNodeVariableSpeed(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        HeatingCoilOutletNode =
                            VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                        // Get DX heating coil capacity
                        thisSys.m_DesignHeatingCapacity =
                            VariableSpeedCoils::GetCoilCapacityVariableSpeed(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (thisSys.m_DesignHeatingCapacity == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }
                } else if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                    thisSys.m_DXHeatingCoil = true;
                    errFlag = false;
                    DXCoils::GetDXCoilIndex(loc_m_HeatingCoilName, thisSys.m_HeatingCoilIndex, errFlag, loc_heatingCoilType);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    thisSys.m_HeatingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);

                    // Get DX coil air flow rate.
                    thisSys.m_MaxHeatAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                    if (thisSys.m_MaxHeatAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    HeatingCoilInletNode = DXCoils::GetCoilInletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }
                    HeatingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    thisSys.m_DesignHeatingCapacity = DXCoils::GetCoilCapacity(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);

                } else if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                           thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {

                    errFlag = false;
                    HeatingCoils::GetCoilIndex(loc_m_HeatingCoilName, thisSys.m_HeatingCoilIndex, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }
                    HeatingCoilInletNode = HeatingCoils::GetCoilInletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }
                    HeatingCoilOutletNode = HeatingCoils::GetCoilOutletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    thisSys.m_HeatingCoilAvailSchPtr = HeatingCoils::GetCoilAvailScheduleIndex(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);

                    thisSys.m_DesignHeatingCapacity = HeatingCoils::GetCoilCapacity(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);

                    if (thisSys.m_DesignHeatingCapacity == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;

                } else if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingGasOrOtherFuel ||
                           thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric ||
                           thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingDesuperheater) {
                    errFlag = false;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    } else {

                        ValidateComponent(loc_heatingCoilType, loc_m_HeatingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from heating coil

                            // Get heating coil index
                            errFlag = false;
                            HeatingCoils::GetCoilIndex(loc_m_HeatingCoilName, thisSys.m_HeatingCoilIndex, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the design heating capacity
                            thisSys.m_DesignHeatingCapacity = HeatingCoils::GetCoilCapacity(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                            if (thisSys.m_DesignHeatingCapacity == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.m_HeatingCoilAvailSchPtr =
                                HeatingCoils::GetCoilAvailScheduleIndex(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);

                            // Get the Heating Coil Inlet Node
                            HeatingCoilInletNode = HeatingCoils::GetCoilInletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Heating Coil Outlet Node
                            HeatingCoilOutletNode = HeatingCoils::GetCoilOutletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Heating Coil PLF Curve Index
                            HeatingCoilPLFCurveIndex = HeatingCoils::GetHeatingCoilPLFCurveIndex(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                            // These heating coil types do not have an air flow input field
                            if (thisSys.m_RequestAutoSize) {
                                thisSys.m_MaxHeatAirVolFlow = DataSizing::AutoSize;
                            }
                        } // IF (IsNotOK) THEN
                    }

                } else if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                    ValidateComponent(loc_heatingCoilType, loc_m_HeatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.m_HeatingCoilAvailSchPtr =
                            WaterCoils::GetWaterCoilAvailScheduleIndex(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.m_HeatingCoilIndex = WaterCoils::GetWaterCoilIndex("COIL:HEATING:WATER", loc_m_HeatingCoilName, errFlag);
                        if (thisSys.m_HeatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil water Inlet or control Node number
                        thisSys.HeatCoilFluidInletNode = WaterCoils::GetCoilWaterInletNode("Coil:Heating:Water", loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil hot water max volume flow rate
                        thisSys.MaxHeatCoilFluidFlow = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", loc_m_HeatingCoilName, errFlag);
                        if (thisSys.MaxHeatCoilFluidFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;

                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Inlet Node
                        HeatingCoilInletNode = WaterCoils::GetCoilInletNode("Coil:Heating:Water", loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = WaterCoils::GetCoilOutletNode("Coil:Heating:Water", loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                    ValidateComponent(loc_heatingCoilType, loc_m_HeatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.m_HeatingCoilAvailSchPtr =
                            SteamCoils::GetSteamCoilAvailScheduleIndex(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.m_HeatingCoilIndex = SteamCoils::GetSteamCoilIndex("COIL:HEATING:STEAM", loc_m_HeatingCoilName, errFlag);
                        if (thisSys.m_HeatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil steam inlet node number
                        errFlag = false;
                        thisSys.HeatCoilFluidInletNode = SteamCoils::GetCoilSteamInletNode("Coil:Heating:Steam", loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil steam max volume flow rate
                        thisSys.MaxHeatCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(thisSys.m_HeatingCoilIndex, errFlag);
                        if (thisSys.MaxHeatCoilFluidFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;

                        if (thisSys.MaxHeatCoilFluidFlow > 0.0) {
                            int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            Real64 TempSteamIn = 100.0;
                            Real64 SteamDensity =
                                FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getUnitarySystemInput);
                            thisSys.MaxHeatCoilFluidFlow *= SteamDensity;
                            errFlag = false;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        HeatingCoilInletNode = SteamCoils::GetCoilAirInletNode(thisSys.m_HeatingCoilIndex, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = SteamCoils::GetCoilAirOutletNode(thisSys.m_HeatingCoilIndex, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        if (thisSys.m_RequestAutoSize) {
                            thisSys.m_MaxHeatAirVolFlow = DataSizing::AutoSize;
                        }
                    }

                } else if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) {
                    thisSys.m_DXHeatingCoil = true;
                    ValidateComponent(loc_heatingCoilType, loc_m_HeatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.m_HeatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.m_HeatingCoilIndex = WaterToAirHeatPumpSimple::GetCoilIndex(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (thisSys.m_HeatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.m_DesignHeatingCapacity =
                            WaterToAirHeatPumpSimple::GetCoilCapacity(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                        errFlag = false;
                        thisSys.m_MaxHeatAirVolFlow =
                            WaterToAirHeatPumpSimple::GetCoilAirFlowRate(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (thisSys.m_MaxHeatAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        HeatingCoilInletNode = WaterToAirHeatPumpSimple::GetCoilInletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = WaterToAirHeatPumpSimple::GetCoilOutletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP) {
                    thisSys.m_DXHeatingCoil = true;
                    ValidateComponent(loc_heatingCoilType, loc_m_HeatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.m_HeatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.m_HeatingCoilIndex = WaterToAirHeatPump::GetCoilIndex(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (thisSys.m_HeatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.m_DesignHeatingCapacity = WaterToAirHeatPump::GetCoilCapacity(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        HeatingCoilInletNode = WaterToAirHeatPump::GetCoilInletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = WaterToAirHeatPump::GetCoilOutletNode(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                    ValidateComponent(loc_heatingCoilType, loc_m_HeatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from Heating coil object

                        errFlag = false;
                        thisSys.m_HeatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        UserDefinedComponents::GetUserDefinedCoilIndex(
                            loc_m_HeatingCoilName, thisSys.m_HeatingCoilIndex, errFlag, cCurrentModuleObject);
                        if (thisSys.m_HeatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // **** How to get this info ****
                        // UnitarySystem( UnitarySysNum ).DesignHeatingCapacity =
                        //     GetWtoAHPCoilCapacity(CoolingCoilType,  loc_m_CoolingCoilName,  errFlag );
                        // if ( errFlag ) {
                        //    ShowContinueError( "Occurs in " + CurrentModuleObject + " = " +
                        //         UnitarySystem(UnitarySysNum.Name);
                        //    ErrorsFound = true;
                        //    errFlag = false;
                        //  }

                        // Get the Cooling Coil Inlet Node
                        errFlag = false;
                        UserDefinedComponents::GetUserDefinedCoilAirInletNode(
                            loc_m_HeatingCoilName, HeatingCoilInletNode, errFlag, cCurrentModuleObject);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Cooling Coil Outlet Node
                        UserDefinedComponents::GetUserDefinedCoilAirOutletNode(
                            loc_m_HeatingCoilName, HeatingCoilOutletNode, errFlag, cCurrentModuleObject);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.m_HeatCoilExists) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilTypeAlphaNum) + " = " + Alphas(iHeatingCoilTypeAlphaNum));
                    errorsFound = true;
                } // IF (thisSys%m_HeatingCoilType_Num == Coil_HeatingGasOrOtherFuel .OR. &, etc.

                if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {
                    thisSys.m_MultiSpeedHeatingCoil = true;
                } else if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                           thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                    thisSys.m_VarSpeedHeatingCoil = true;
                }

                // coil outlet node set point has priority, IF not exist, then use system outlet node
                if (SetPointManager::NodeHasSPMCtrlVarType(thisSys.AirOutNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.m_SystemHeatControlNodeNum = thisSys.AirOutNode;
                if (SetPointManager::NodeHasSPMCtrlVarType(HeatingCoilOutletNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.m_SystemHeatControlNodeNum = HeatingCoilOutletNode;

                thisSys.HeatCoilInletNodeNum = HeatingCoilInletNode;
                thisSys.HeatCoilOutletNodeNum = HeatingCoilOutletNode;
                thisSys.m_HeatingCoilName = loc_m_HeatingCoilName;

                // Add heating coil to component sets array
                if (thisSys.m_HeatCoilExists && thisSys.m_HeatCompNotSetYet) {
                    if (thisSys.m_HeatingCoilType_Num != DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                        BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                             thisObjectName,
                                                             loc_heatingCoilType,
                                                             loc_m_HeatingCoilName,
                                                             DataLoopNode::NodeID(HeatingCoilInletNode),
                                                             DataLoopNode::NodeID(HeatingCoilOutletNode));
                    } else {
                        BranchNodeConnections::SetUpCompSets(
                            cCurrentModuleObject, thisObjectName, loc_heatingCoilType, loc_m_HeatingCoilName, "UNDEFINED", "UNDEFINED");
                    }
                    thisSys.m_HeatCompNotSetYet = false;
                }

                // Get Cooling Coil Information IF available
                if (loc_coolingCoilType != "" && loc_m_CoolingCoilName != "") {
                    thisSys.m_CoolCoilExists = true;

                    //       Find the type of coil. do not print message since this may not be the correct coil type.
                    errFlag = false;
                    if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:VariableSpeed")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:MultiSpeed")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::CoilDX_MultiSpeedCooling;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:Water")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingWater;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:Water:DetailedGeometry")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterDetailed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:TwoStageWithHumidityControlMode")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
                        thisSys.m_CoolingCoilType_Num =
                            HVACHXAssistedCoolingCoil::GetCoilGroupTypeNum(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag, PrintMessage);
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
                        thisSys.m_CoolingCoilType_Num =
                            HVACHXAssistedCoolingCoil::GetCoilGroupTypeNum(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag, PrintMessage);
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:WaterToAirHeatPump:EquationFit")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterToAirHPSimple;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterToAirHP;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:SingleSpeed")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::CoilDX_CoolingSingleSpeed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:TwoSpeed")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::CoilDX_CoolingTwoSpeed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:UserDefined")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::Coil_UserDefined;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:SingleSpeed:ThermalStorage")) {
                        thisSys.m_CoolingCoilType_Num = DataHVACGlobals::CoilDX_PackagedThermalStorageCooling;
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilTypeAlphaNum) + " = " + Alphas(iCoolingCoilTypeAlphaNum));
                    }

                    if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed ||
                        thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                        ValidateComponent(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from DX cooling coil

                            if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) thisSys.m_NumOfSpeedCooling = 2;

                            // Get DX cooling coil index
                            DXCoils::GetDXCoilIndex(loc_m_CoolingCoilName, thisSys.m_CoolingCoilIndex, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            thisSys.m_CoolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);

                            // Get DX cooling coil capacity
                            errFlag = false;
                            thisSys.m_DesignCoolingCapacity = DXCoils::GetCoilCapacity(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_DesignCoolingCapacity == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get DX coil air flow rate. Latter fields will overwrite this IF input field is present
                            errFlag = false;
                            thisSys.m_MaxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = DXCoils::GetCoilInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            CoolingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get Outdoor condenser node from DX coil object
                            errFlag = false;
                            thisSys.m_CondenserNodeNum = DXCoils::GetCoilCondenserInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            if (thisSys.m_FanExists) {
                                errFlag = false;
                                DXCoils::SetDXCoolingCoilData(
                                    thisSys.m_CoolingCoilIndex, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, loc_m_FanName);
                                DXCoils::SetDXCoolingCoilData(
                                    thisSys.m_CoolingCoilIndex, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, thisSys.m_FanIndex);
                                DXCoils::SetDXCoolingCoilData(thisSys.m_CoolingCoilIndex,
                                                              errFlag,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              thisSys.m_FanType_Num);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }
                            }
                            if (thisSys.m_HeatCoilExists) {
                                if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                    thisSys.m_HeatPump = true;
                                }

                                // set fan info for heating coils
                                if (thisSys.m_FanExists) {
                                    if (thisSys.m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.m_HeatingCoilName,
                                                                                     thisSys.m_HeatingCoilTypeName,
                                                                                     thisSys.m_FanName,
                                                                                     DataAirSystems::objectVectorOOFanSystemModel,
                                                                                     thisSys.m_FanIndex);
                                    } else {
                                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.m_HeatingCoilName,
                                                                                     thisSys.m_HeatingCoilTypeName,
                                                                                     thisSys.m_FanName,
                                                                                     DataAirSystems::structArrayLegacyFanModels,
                                                                                     thisSys.m_FanIndex);
                                    }
                                }
                            }

                        } // IF (IsNotOK) THEN

                        // Push heating coil PLF curve index to DX coil
                        if (HeatingCoilPLFCurveIndex > 0) {
                            DXCoils::SetDXCoolingCoilData(thisSys.m_CoolingCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                        }

                    } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                        ValidateComponent(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from DX cooling coil

                            // Get DX cooling coil index
                            DXCoils::GetDXCoilIndex(loc_m_CoolingCoilName, thisSys.m_CoolingCoilIndex, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            thisSys.m_CoolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);

                            // Get DX cooling coil capacity
                            errFlag = false;
                            thisSys.m_DesignCoolingCapacity = DXCoils::GetCoilCapacity(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_DesignCoolingCapacity == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                            errFlag = false;
                            thisSys.m_MaxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = DXCoils::GetCoilInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            CoolingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get Outdoor condenser node from DX coil object
                            errFlag = false;
                            thisSys.m_CondenserNodeNum = DXCoils::GetCoilCondenserInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                        } // IF (IsNotOK) THEN

                        // Push heating coil PLF curve index to DX coil
                        if (HeatingCoilPLFCurveIndex > 0) {
                            DXCoils::SetDXCoolingCoilData(thisSys.m_CoolingCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                        }

                        if (thisSys.m_HeatCoilExists) {
                            if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.m_HeatPump = true;
                            }
                        }

                    } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) {
                        ValidateComponent(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from heat exchanger assisted cooling coil

                            // Get DX heat exchanger assisted cooling coil index
                            errFlag = false;
                            HVACHXAssistedCoolingCoil::GetHXDXCoilIndex(loc_m_CoolingCoilName, thisSys.m_CoolingCoilIndex, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            std::string ChildCoolingCoilName =
                                HVACHXAssistedCoolingCoil::GetHXDXCoilName(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK);
                            std::string ChildCoolingCoilType =
                                HVACHXAssistedCoolingCoil::GetHXDXCoilType(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            if (UtilityRoutines::SameString(ChildCoolingCoilType, "COIL:COOLING:DX:SINGLESPEED")) {

                                errFlag = false;
                                thisSys.m_CoolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(ChildCoolingCoilType, ChildCoolingCoilName, errFlag);
                                if (isNotOK) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                                errFlag = false;
                                thisSys.m_MaxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(ChildCoolingCoilType, ChildCoolingCoilName, errFlag);
                                if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get Outdoor condenser node from heat exchanger assisted DX coil object
                                errFlag = false;
                                thisSys.m_CondenserNodeNum = DXCoils::GetCoilCondenserInletNode(
                                    "COIL:COOLING:DX:SINGLESPEED",
                                    HVACHXAssistedCoolingCoil::GetHXDXCoilName(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag),
                                    errFlag);

                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                            } else if (UtilityRoutines::SameString(ChildCoolingCoilType, "COIL:COOLING:DX:VARIABLESPEED")) {
                                thisSys.m_CoolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                                errFlag = false;
                                thisSys.m_MaxCoolAirVolFlow =
                                    VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed(ChildCoolingCoilType, ChildCoolingCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }
                                thisSys.m_CondenserNodeNum = VariableSpeedCoils::GetVSCoilCondenserInletNode(ChildCoolingCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }
                            }

                            // Get DX cooling coil capacity
                            errFlag = false;
                            thisSys.m_DesignCoolingCapacity =
                                HVACHXAssistedCoolingCoil::GetCoilCapacity(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_DesignCoolingCapacity == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = HVACHXAssistedCoolingCoil::GetCoilInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            CoolingCoilOutletNode = HVACHXAssistedCoolingCoil::GetCoilOutletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Push heating coil PLF curve index to DX coil
                            if (HeatingCoilPLFCurveIndex > 0) {
                                // get the actual index to the DX cooling coil object
                                int DXCoilIndex =
                                    HVACHXAssistedCoolingCoil::GetActualDXCoilIndex(loc_coolingCoilType, loc_m_CoolingCoilName, errorsFound);
                                thisSys.m_ActualDXCoilIndexForHXAssisted = DXCoilIndex;
                                int ActualCoolCoilType =
                                    HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag, true);
                                if (ActualCoolCoilType == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                                    DXCoils::SetDXCoolingCoilData(DXCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                                }
                                // what could we do for VS coil here? odd thing here
                            }

                            if (thisSys.m_HeatCoilExists) {
                                if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                    thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                    thisSys.m_HeatPump = true;
                                }
                            }

                        } // IF (IsNotOK) THEN
                    } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted) {
                        ValidateComponent(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from heat exchanger assisted cooling coil

                            errFlag = false;
                            int ActualCoolCoilType =
                                HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag, true);
                            std::string HXCoilName = HVACHXAssistedCoolingCoil::GetHXDXCoilName(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);

                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get DX heat exchanger assisted cooling coil index
                            errFlag = false;
                            HVACHXAssistedCoolingCoil::GetHXDXCoilIndex(loc_m_CoolingCoilName, thisSys.m_CoolingCoilIndex, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            thisSys.m_CoolingCoilAvailSchPtr =
                                WaterCoils::GetWaterCoilAvailScheduleIndex(DataHVACGlobals::cAllCoilTypes(ActualCoolCoilType), HXCoilName, errFlag);
                            thisSys.MaxCoolCoilFluidFlow =
                                WaterCoils::GetCoilMaxWaterFlowRate(DataHVACGlobals::cAllCoilTypes(ActualCoolCoilType), HXCoilName, errFlag);
                            // Get the Cooling Coil water Inlet Node number
                            thisSys.CoolCoilFluidInletNode =
                                WaterCoils::GetCoilWaterInletNode(DataHVACGlobals::cAllCoilTypes(ActualCoolCoilType), HXCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = HVACHXAssistedCoolingCoil::GetCoilInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            CoolingCoilOutletNode = HVACHXAssistedCoolingCoil::GetCoilOutletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            thisSys.m_MaxCoolAirVolFlow =
                                HVACHXAssistedCoolingCoil::GetHXCoilAirFlowRate(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            thisSys.m_CondenserNodeNum = 0;

                            // Push heating coil PLF curve index to DX coil
                            if (HeatingCoilPLFCurveIndex > 0) {
                                // get the actual index to the DX cooling coil object
                                int DXCoilIndex =
                                    HVACHXAssistedCoolingCoil::GetActualDXCoilIndex(loc_coolingCoilType, loc_m_CoolingCoilName, errorsFound);
                                thisSys.m_ActualDXCoilIndexForHXAssisted = DXCoilIndex;
                                int ActualCoolCoilType =
                                    HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag, true);
                                if (ActualCoolCoilType == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                                    DXCoils::SetDXCoolingCoilData(DXCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                                }
                                // VS coil issue here
                            }

                        } // IF (IsNotOK) THEN
                    } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed ||
                               thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) {
                        ValidateComponent(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {
                            errFlag = false;
                            thisSys.m_CoolingCoilIndex =
                                VariableSpeedCoils::GetCoilIndexVariableSpeed(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            CoolingCoilInletNode =
                                VariableSpeedCoils::GetCoilInletNodeVariableSpeed(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            CoolingCoilOutletNode =
                                VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.m_CondenserNodeNum = VariableSpeedCoils::GetVSCoilCondenserInletNode(loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.m_CoolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;

                            thisSys.m_NumOfSpeedCooling = VariableSpeedCoils::GetVSCoilNumOfSpeeds(loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            errFlag = false;
                            thisSys.m_DesignCoolingCapacity =
                                VariableSpeedCoils::GetCoilCapacityVariableSpeed(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_DesignCoolingCapacity == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            thisSys.m_MaxCoolAirVolFlow =
                                VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Set fan info
                            if (thisSys.m_FanExists) {
                                VariableSpeedCoils::setVarSpeedFanInfo(
                                    thisSys.m_CoolingCoilIndex, loc_m_FanName, thisSys.m_FanIndex, thisSys.m_FanType_Num);
                            }
                        }

                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        if (thisSys.m_HeatCoilExists) {
                            if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.m_HeatPump = true;
                            }
                        }

                    } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                        errFlag = false;
                        DXCoils::GetDXCoilIndex(loc_m_CoolingCoilName, thisSys.m_CoolingCoilIndex, errFlag, loc_coolingCoilType);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.m_CoolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);

                        errFlag = false;
                        CoolingCoilInletNode = DXCoils::GetCoilInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        errFlag = false;
                        CoolingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        errFlag = false;
                        thisSys.m_DesignCoolingCapacity = DXCoils::GetCoilCapacity(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                        if (thisSys.m_DesignCoolingCapacity == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                        errFlag = false;
                        thisSys.m_MaxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                        if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        if (thisSys.m_HeatCoilExists) {
                            if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.m_HeatPump = true;
                            }
                        }

                    } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                               thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {

                        ValidateComponent(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.m_CoolingCoilAvailSchPtr =
                                WaterCoils::GetWaterCoilAvailScheduleIndex(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.m_CoolingCoilIndex = WaterCoils::GetWaterCoilIndex(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_CoolingCoilIndex == 0) {
                                // ShowSevereError(cCurrentModuleObject + " illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " +
                                // HeatingCoilName);
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // call for air flow rate not valid for other water coil types
                            if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater) {
                                thisSys.m_MaxCoolAirVolFlow = WaterCoils::GetWaterCoilDesAirFlow(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                    errFlag = false;
                                }
                            }

                            // Get the Cooling Coil water Inlet Node number
                            thisSys.CoolCoilFluidInletNode = WaterCoils::GetCoilWaterInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            bool InletNodeNotControlled = true;
                            //  CALL CheckCoilWaterInletNode(thisSys%CoolCoilFluidInletNode,InletNodeNotControlled)
                            if (!InletNodeNotControlled) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError(DataHVACControllers::ControllerTypes(DataHVACControllers::ControllerSimple_Type) + " found for " +
                                                  loc_coolingCoilType + " = \"" + loc_m_CoolingCoilName + ".\"");
                                ShowContinueError("...water coil controllers are not used with " + thisSys.UnitType);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil chilled water max volume flow rate
                            errFlag = false;
                            thisSys.MaxCoolCoilFluidFlow = WaterCoils::GetCoilMaxWaterFlowRate(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.MaxCoolCoilFluidFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Inlet Node
                            CoolingCoilInletNode = WaterCoils::GetCoilInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            CoolingCoilOutletNode = WaterCoils::GetCoilOutletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }
                    } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {
                        ValidateComponent(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.m_CoolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.m_CoolingCoilIndex = WaterToAirHeatPumpSimple::GetCoilIndex(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_CoolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_m_CoolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.m_DesignCoolingCapacity =
                                WaterToAirHeatPumpSimple::GetCoilCapacity(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                            errFlag = false;
                            thisSys.m_MaxCoolAirVolFlow =
                                WaterToAirHeatPumpSimple::GetCoilAirFlowRate(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            CoolingCoilInletNode = WaterToAirHeatPumpSimple::GetCoilInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            CoolingCoilOutletNode = WaterToAirHeatPumpSimple::GetCoilOutletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                        if (thisSys.m_HeatCoilExists) {
                            if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.m_HeatPump = true;
                            }
                        }

                    } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP) {
                        ValidateComponent(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.m_CoolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.m_CoolingCoilIndex = WaterToAirHeatPump::GetCoilIndex(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (thisSys.m_CoolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_m_CoolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.m_DesignCoolingCapacity =
                                WaterToAirHeatPump::GetCoilCapacity(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            CoolingCoilInletNode = WaterToAirHeatPump::GetCoilInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            CoolingCoilOutletNode = WaterToAirHeatPump::GetCoilOutletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                        if (thisSys.m_HeatCoilExists) {
                            if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.m_HeatPump = true;
                            }
                        }

                    } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                        ValidateComponent(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.m_CoolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            UserDefinedComponents::GetUserDefinedCoilIndex(
                                loc_m_CoolingCoilName, thisSys.m_CoolingCoilIndex, errFlag, cCurrentModuleObject);
                            if (thisSys.m_CoolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_m_CoolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // **** How to get this info ****
                            //						UnitarySystem( UnitarySysNum ).DesignCoolingCapacity =
                            // GetWtoAHPCoilCapacity(
                            // CoolingCoilType, loc_m_CoolingCoilName, errFlag ); 						if ( errFlag ) {
                            //							ShowContinueError( "Occurs in " + CurrentModuleObject + " = "
                            //+
                            // UnitarySystem( UnitarySysNum ).Name ); 							ErrorsFound = true;
                            //							errFlag = false;
                            //						}

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            UserDefinedComponents::GetUserDefinedCoilAirInletNode(
                                loc_m_CoolingCoilName, CoolingCoilInletNode, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            UserDefinedComponents::GetUserDefinedCoilAirOutletNode(
                                loc_m_CoolingCoilName, CoolingCoilOutletNode, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                    } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {
                        ValidateComponent(loc_coolingCoilType, loc_m_CoolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.m_CoolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            PackagedThermalStorageCoil::GetTESCoilIndex(
                                loc_m_CoolingCoilName, thisSys.m_CoolingCoilIndex, errFlag, cCurrentModuleObject);
                            if (thisSys.m_CoolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_m_CoolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            PackagedThermalStorageCoil::GetTESCoilCoolingAirFlowRate(
                                loc_m_CoolingCoilName, thisSys.m_MaxCoolAirVolFlow, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            PackagedThermalStorageCoil::GetTESCoilCoolingCapacity(
                                loc_m_CoolingCoilName, thisSys.m_DesignCoolingCapacity, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            PackagedThermalStorageCoil::GetTESCoilAirInletNode(
                                loc_m_CoolingCoilName, CoolingCoilInletNode, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            PackagedThermalStorageCoil::GetTESCoilAirOutletNode(
                                loc_m_CoolingCoilName, CoolingCoilOutletNode, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                    } else { // IF(.NOT. lAlphaBlanks(16))THEN
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilTypeAlphaNum) + " = " + Alphas(iCoolingCoilTypeAlphaNum));
                        errorsFound = true;
                    }

                    if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                        thisSys.m_MultiSpeedCoolingCoil = true;
                    } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit ||
                               thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                        thisSys.m_VarSpeedCoolingCoil = true;
                    }

                    if (SetPointManager::NodeHasSPMCtrlVarType(thisSys.AirOutNode, SetPointManager::iCtrlVarType_Temp))
                        thisSys.m_SystemCoolControlNodeNum = thisSys.AirOutNode;
                    if (SetPointManager::NodeHasSPMCtrlVarType(CoolingCoilOutletNode, SetPointManager::iCtrlVarType_Temp))
                        thisSys.m_SystemCoolControlNodeNum = CoolingCoilOutletNode;

                    thisSys.CoolCoilInletNodeNum = CoolingCoilInletNode;
                    thisSys.CoolCoilOutletNodeNum = CoolingCoilOutletNode;
                    thisSys.m_CoolingCoilName = loc_m_CoolingCoilName;

                } else {
                    thisSys.m_ValidASHRAECoolCoil = false;
                }

                if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple &&
                    thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {
                    thisSys.m_WaterCyclingMode = DataHVACGlobals::WaterCycling;
                    WaterToAirHeatPumpSimple::SetSimpleWSHPData(
                        thisSys.m_CoolingCoilIndex, errorsFound, thisSys.m_WaterCyclingMode, _, thisSys.m_HeatingCoilIndex);
                }

                if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit &&
                    thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) {
                    VariableSpeedCoils::SetVarSpeedCoilData(thisSys.m_CoolingCoilIndex, errorsFound, _, thisSys.m_HeatingCoilIndex);
                }

                // Add cooling coil to component sets array
                if (thisSys.m_CoolCoilExists && thisSys.m_CoolCompNotSetYet) {
                    if (thisSys.m_CoolingCoilType_Num != DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                        BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                             thisObjectName,
                                                             loc_coolingCoilType,
                                                             loc_m_CoolingCoilName,
                                                             DataLoopNode::NodeID(CoolingCoilInletNode),
                                                             DataLoopNode::NodeID(CoolingCoilOutletNode));
                    } else {
                        BranchNodeConnections::SetUpCompSets(
                            cCurrentModuleObject, thisObjectName, loc_coolingCoilType, loc_m_CoolingCoilName, "UNDEFINED", "UNDEFINED");
                    }
                    thisSys.m_CoolCompNotSetYet = false;
                }
                // Run as 100% DOAS DX coil
                if (!UtilityRoutines::SameString(loc_m_ISHundredPercentDOASDXCoil, "Yes")) {
                    thisSys.m_ISHundredPercentDOASDXCoil = false;
                } else {
                    if (UtilityRoutines::SameString(loc_m_ISHundredPercentDOASDXCoil, "Yes")) {
                        thisSys.m_ISHundredPercentDOASDXCoil = true;
                        if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                            ShowWarningError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Invalid entry for " + cAlphaFields(iDOASDXCoilAlphaNum) + " :" + Alphas(iDOASDXCoilAlphaNum));
                            ShowContinueError("Variable DX Cooling Coil is not supported as 100% DOAS DX coil.");
                            ShowContinueError("Variable DX Cooling Coil is reset as a regular DX coil and the simulation continues.");
                            thisSys.m_ISHundredPercentDOASDXCoil = false;
                        }
                    } else if (UtilityRoutines::SameString(loc_m_ISHundredPercentDOASDXCoil, "")) {
                        thisSys.m_ISHundredPercentDOASDXCoil = false;
                    } else if (UtilityRoutines::SameString(loc_m_ISHundredPercentDOASDXCoil, "No")) {
                        thisSys.m_ISHundredPercentDOASDXCoil = false;
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iDOASDXCoilAlphaNum) + " :" + Alphas(iDOASDXCoilAlphaNum));
                        ShowContinueError("Must be Yes or No.");
                        errorsFound = true;
                    }
                }

                // considered as as 100% DOAS DX cooling coil
                if (thisSys.m_ISHundredPercentDOASDXCoil) {
                    // set the system DX Coil application type to the child DX coil
                    if (!(thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed ||
                          thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {
                        DXCoils::SetDXCoilTypeData(thisSys.m_CoolingCoilName);
                    }
                }
                // DOAS DX Cooling Coil Leaving Minimum Air Temperature
                // if (NumNumbers > 0) {
                // if (!lNumericBlanks(iDOASDXMinTempNumericNum)) {
                thisSys.DesignMinOutletTemp = loc_DesignMinOutletTemp;
                if (thisSys.m_ControlType != ControlType::CCMASHRAE && thisSys.DesignMinOutletTemp == DataSizing::AutoSize) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Invalid entry for " + cNumericFields(iDOASDXMinTempNumericNum) + " =DataSizing::AutoSize.");
                    // ShowContinueError("AutoSizing not allowed when " + cAlphaFields(im_ControlTypeAlphaNum) + " = " +
                    //                  Alphas(im_ControlTypeAlphaNum));
                    errorsFound = true;
                }
                if (thisSys.m_ControlType != ControlType::CCMASHRAE && thisSys.DesignMinOutletTemp > 7.5) {
                    ShowWarningError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Invalid entry for " + cNumericFields(iDOASDXMinTempNumericNum) + " = " +
                    //                  TrimSigDigits(Numbers(iDOASDXMinTempNumericNum), 3));
                    ShowContinueError("The minimum supply air temperature will be limited to 7.5C and the simulation continues.");
                    thisSys.DesignMinOutletTemp = 7.5;
                }
                //}
                //}

                // Get Latent Load Control flag
                if (loc_latentControlFlag != "") {
                    if (UtilityRoutines::SameString(loc_latentControlFlag, "SensibleOnlyLoadControl")) {
                        thisSys.m_RunOnSensibleLoad = true;
                        thisSys.m_RunOnLatentLoad = false;
                    } else if (UtilityRoutines::SameString(loc_latentControlFlag, "LatentOnlyLoadControl")) {
                        thisSys.m_RunOnSensibleLoad = false;
                        thisSys.m_RunOnLatentLoad = true;
                    } else if (UtilityRoutines::SameString(loc_latentControlFlag, "LatentOrSensibleLoadControl")) {
                        thisSys.m_RunOnSensibleLoad = true;
                        thisSys.m_RunOnLatentLoad = true;
                    } else if (UtilityRoutines::SameString(loc_latentControlFlag, "LatentWithSensibleLoadControl")) {
                        thisSys.m_RunOnSensibleLoad = true;
                        thisSys.m_RunOnLatentLoad = true;
                        thisSys.m_RunOnLatentOnlyWithSensible = true;
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iRunOnLatentLoadAlphaNum) + " :" +
                        // Alphas(iRunOnLatentLoadAlphaNum));
                        ShowContinueError("Must be SensibleOnlyLoadControl, LatentOnlyLoadControl, LatentOrSensibleLoadControl, or "
                                          "LatentWithSensibleLoadControl.");
                    }
                }

                // Get reheat coil data if humidistat is used
                thisSys.m_SuppHeatCoilName = loc_m_SuppHeatCoilName;
                thisSys.m_SuppHeatCoilTypeName = loc_suppHeatCoilType;
                errFlag = false;

                if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Water")) {
                    thisSys.m_SuppHeatCoilType_Num = DataHVACGlobals::Coil_HeatingWater;
                } else if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Steam")) {
                    thisSys.m_SuppHeatCoilType_Num = DataHVACGlobals::Coil_HeatingSteam;
                } else if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Fuel") ||
                           UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Electric") ||
                           UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:DesuperHeater")) {
                    thisSys.m_SuppHeatCoilType_Num = HeatingCoils::GetHeatingCoilTypeNum(loc_suppHeatCoilType, loc_m_SuppHeatCoilName, errFlag);
                } else if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:UserDefined")) {
                    thisSys.m_SuppHeatCoilType_Num = DataHVACGlobals::Coil_UserDefined;
                }

                if (loc_suppHeatCoilType != "" && loc_m_SuppHeatCoilName != "") {
                    thisSys.m_SuppCoilExists = true;

                    if (thisSys.m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingGasOrOtherFuel ||
                        thisSys.m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingElectric ||
                        thisSys.m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingDesuperheater) {

                        thisSys.m_SuppHeatCoilType_Num = HeatingCoils::GetHeatingCoilTypeNum(loc_suppHeatCoilType, loc_m_SuppHeatCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {

                            ValidateComponent(loc_suppHeatCoilType, loc_m_SuppHeatCoilName, isNotOK, cCurrentModuleObject);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;

                            } else { // mine data from reheat coil

                                // Get the heating coil index
                                thisSys.m_SuppHeatCoilIndex =
                                    HeatingCoils::GetHeatingCoilIndex(loc_suppHeatCoilType, loc_m_SuppHeatCoilName, isNotOK);
                                if (isNotOK) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the design supplemental heating capacity
                                errFlag = false;
                                thisSys.m_DesignSuppHeatingCapacity =
                                    HeatingCoils::GetCoilCapacity(loc_suppHeatCoilType, loc_m_SuppHeatCoilName, errFlag);
                                if (thisSys.m_DesignSuppHeatingCapacity == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;

                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Reheat Coil Inlet Node
                                errFlag = false;
                                SupHeatCoilInletNode = HeatingCoils::GetCoilInletNode(loc_suppHeatCoilType, loc_m_SuppHeatCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Reheat Coil Outlet Node
                                errFlag = false;
                                SupHeatCoilOutletNode = HeatingCoils::GetCoilOutletNode(loc_suppHeatCoilType, loc_m_SuppHeatCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                            } // IF (IsNotOK) THEN
                        }

                        thisSys.m_SuppCoilAirInletNode = SupHeatCoilInletNode;
                        thisSys.m_SuppCoilAirOutletNode = SupHeatCoilOutletNode;

                    } else if (thisSys.m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {

                        ValidateComponent(loc_suppHeatCoilType, loc_m_SuppHeatCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from heating coil object

                            // Get the Heating Coil water Inlet or control Node number
                            errFlag = false;
                            thisSys.m_SuppCoilFluidInletNode =
                                WaterCoils::GetCoilWaterInletNode("Coil:Heating:Water", loc_m_SuppHeatCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the ReHeat Coil hot water max volume flow rate
                            errFlag = false;
                            thisSys.m_MaxSuppCoilFluidFlow =
                                WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", loc_m_SuppHeatCoilName, errFlag);
                            if (thisSys.m_MaxSuppCoilFluidFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;

                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the ReHeat Coil Inlet Node
                            errFlag = false;
                            SupHeatCoilInletNode = WaterCoils::GetCoilInletNode("Coil:Heating:Water", loc_m_SuppHeatCoilName, errFlag);
                            thisSys.m_SuppCoilAirInletNode = SupHeatCoilInletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the ReHeat Coil Outlet Node
                            errFlag = false;
                            SupHeatCoilOutletNode = WaterCoils::GetCoilOutletNode("Coil:Heating:Water", loc_m_SuppHeatCoilName, errFlag);
                            thisSys.m_SuppCoilAirOutletNode = SupHeatCoilOutletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }
                        }

                    } else if (thisSys.m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {

                        ValidateComponent(loc_suppHeatCoilType, loc_m_SuppHeatCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from heating coil object

                            errFlag = false;
                            thisSys.m_SuppHeatCoilIndex = SteamCoils::GetSteamCoilIndex("COIL:HEATING:STEAM", loc_m_SuppHeatCoilName, errFlag);
                            if (thisSys.m_SuppHeatCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowSevereError("Illegal " + cAlphaFields(iSuppHeatCoilNameAlphaNum) + " = " + SuppHeatCoilName);
                                errorsFound = true;
                            }

                            // Get the Heating Coil steam inlet node number
                            errFlag = false;
                            thisSys.m_SuppCoilFluidInletNode =
                                SteamCoils::GetCoilSteamInletNode("Coil:Heating:Steam", loc_m_SuppHeatCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }
                            // Get the Heating Coil steam max volume flow rate
                            thisSys.m_MaxSuppCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(thisSys.m_SuppHeatCoilIndex, errFlag);
                            if (thisSys.m_MaxSuppCoilFluidFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;

                            if (thisSys.m_MaxSuppCoilFluidFlow > 0.0) {
                                int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                                Real64 TempSteamIn = 100.0;
                                Real64 SteamDensity =
                                    FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getUnitarySystemInput);
                                thisSys.m_MaxSuppCoilFluidFlow =
                                    SteamCoils::GetCoilMaxSteamFlowRate(thisSys.m_SuppHeatCoilIndex, errFlag) * SteamDensity;
                            }

                            // Get the Heating Coil Inlet Node
                            errFlag = false;
                            SupHeatCoilInletNode = SteamCoils::GetCoilAirInletNode(thisSys.m_SuppHeatCoilIndex, loc_m_SuppHeatCoilName, errFlag);
                            thisSys.m_SuppCoilAirInletNode = SupHeatCoilInletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Heating Coil Outlet Node
                            errFlag = false;
                            SupHeatCoilOutletNode = SteamCoils::GetCoilAirOutletNode(thisSys.m_SuppHeatCoilIndex, loc_m_SuppHeatCoilName, errFlag);
                            thisSys.m_SuppCoilAirOutletNode = SupHeatCoilOutletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }
                        }

                    } else if (thisSys.m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                        ValidateComponent(loc_suppHeatCoilType, loc_m_SuppHeatCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Heating coil object

                            //						errFlag = false;
                            //						UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr =
                            // ScheduleAlwaysOn; 						if ( errFlag ) {
                            //							ShowContinueError( "Occurs in " + CurrentModuleObject + " = "
                            //+
                            // UnitarySystem(  UnitarySysNum ).Name ); 							ErrorsFound = true;
                            //							errFlag = false;
                            //						}

                            errFlag = false;
                            UserDefinedComponents::GetUserDefinedCoilIndex(
                                loc_m_SuppHeatCoilName, thisSys.m_SuppHeatCoilIndex, errFlag, cCurrentModuleObject);
                            if (thisSys.m_SuppHeatCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iSuppHeatCoilNameAlphaNum) + " = " + SuppHeatCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the supplemental heating Coil Inlet Node
                            errFlag = false;
                            UserDefinedComponents::GetUserDefinedCoilAirInletNode(
                                loc_m_SuppHeatCoilName, SupHeatCoilInletNode, errFlag, cCurrentModuleObject);
                            thisSys.m_SuppCoilAirInletNode = SupHeatCoilInletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the supplemenatal heating Coil Outlet Node
                            UserDefinedComponents::GetUserDefinedCoilAirOutletNode(
                                loc_m_SuppHeatCoilName, SupHeatCoilOutletNode, errFlag, cCurrentModuleObject);
                            thisSys.m_SuppCoilAirOutletNode = SupHeatCoilOutletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                    } else { // Illegal reheating coil type
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Illegal " + cAlphaFields(iSuppHeatCoilTypeAlphaNum) + " = " + Alphas(iSuppHeatCoilTypeAlphaNum));
                        errorsFound = true;
                    } // IF (thisSys%SuppHeatCoilType_Num == Coil_HeatingGasOrOtherFuel .OR. &, etc.

                } // IF(.NOT. lAlphaBlanks(iSuppHeatCoilTypeAlphaNum))THEN

                if (SetPointManager::NodeHasSPMCtrlVarType(thisSys.AirOutNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.m_SuppHeatControlNodeNum = thisSys.AirOutNode;
                if (SetPointManager::NodeHasSPMCtrlVarType(SupHeatCoilOutletNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.m_SuppHeatControlNodeNum = SupHeatCoilOutletNode;

                // Add supplemental heating coil to component sets array
                if (thisSys.m_SuppCoilExists && thisSys.m_SuppCompNotSetYet) {
                    BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                         thisObjectName,
                                                         loc_suppHeatCoilType,
                                                         loc_m_SuppHeatCoilName,
                                                         DataLoopNode::NodeID(SupHeatCoilInletNode),
                                                         DataLoopNode::NodeID(SupHeatCoilOutletNode));
                    thisSys.m_SuppCompNotSetYet = false;
                }

                // set fan info for heating coils
                if (thisSys.m_SuppCoilExists && thisSys.m_FanExists) {
                    if (thisSys.m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.m_SuppHeatCoilName,
                                                                     thisSys.m_SuppHeatCoilTypeName,
                                                                     thisSys.m_FanName,
                                                                     DataAirSystems::objectVectorOOFanSystemModel,
                                                                     thisSys.m_FanIndex);
                    } else {
                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.m_SuppHeatCoilName,
                                                                     thisSys.m_SuppHeatCoilTypeName,
                                                                     thisSys.m_FanName,
                                                                     DataAirSystems::structArrayLegacyFanModels,
                                                                     thisSys.m_FanIndex);
                    }
                }

                // Users may not provide SA flow input fields (below) and leave them blank. Check if other coil isDataSizing::AutoSized first to
                // alieviate input requirements. check if coil has no air flow input (VolFlow = 0) and other coil isDataSizing::AutoSized. If so,
                // useDataSizing::AutoSize for coil with 0 air flow rate. This means that the coils MUST mine the air flow rate if it exists
                if (thisSys.m_CoolCoilExists && thisSys.m_HeatCoilExists) {
                    if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize && thisSys.m_MaxHeatAirVolFlow == 0 && loc_m_HeatingSAFMethod == "") {
                        thisSys.m_MaxHeatAirVolFlow = DataSizing::AutoSize;
                    } else if (thisSys.m_MaxCoolAirVolFlow == 0 && thisSys.m_MaxHeatAirVolFlow == DataSizing::AutoSize &&
                               loc_m_CoolingSAFMethod == "") {
                        thisSys.m_MaxCoolAirVolFlow = DataSizing::AutoSize;
                    }
                }

                // Determine supply air flow rate sizing method for cooling mode
                if (UtilityRoutines::SameString(loc_m_CoolingSAFMethod, "SupplyAirFlowRate")) {
                    thisSys.m_CoolingSAFMethod = SupplyAirFlowRate;

                    if (loc_m_CoolingSAFMethod_SAFlow != -999.0) {
                        thisSys.m_MaxCoolAirVolFlow = loc_m_CoolingSAFMethod_SAFlow;
                        if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;

                        if ((thisSys.m_MaxCoolAirVolFlow < 0.0 && thisSys.m_MaxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.m_MaxCoolAirVolFlow == 0.0 && thisSys.m_CoolCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cNumericFields(iMaxCoolAirVolFlowNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iMaxCoolAirVolFlowNumericNum), 7));
                            errorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iMaxCoolAirVolFlowNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_CoolingSAFMethod, "FlowPerFloorArea")) {

                    thisSys.m_CoolingSAFMethod = FlowPerFloorArea;
                    if (loc_m_CoolingSAFMethod_SAFlowPerFloorArea != -999.0) {
                        thisSys.m_MaxCoolAirVolFlow = loc_m_CoolingSAFMethod_SAFlowPerFloorArea;
                        if ((thisSys.m_MaxCoolAirVolFlow < 0.0 && thisSys.m_MaxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.m_MaxCoolAirVolFlow == 0.0 && thisSys.m_CoolCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFloorAreaNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iCoolFlowPerFloorAreaNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFloorAreaNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.m_MaxCoolAirVolFlow *= TotalFloorAreaOnAirLoop;
                            thisSys.m_RequestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iCoolFlowPerFloorAreaNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_CoolingSAFMethod, "FractionOfAutosizedCoolingValue")) {

                    thisSys.m_CoolingSAFMethod = FractionOfAutoSizedCoolingValue;
                    if (loc_m_CoolingSAFMethod_FracOfAutosizedCoolingSAFlow != -999.0) {
                        thisSys.m_MaxCoolAirVolFlow = loc_m_CoolingSAFMethod_FracOfAutosizedCoolingSAFlow;
                        if ((thisSys.m_MaxCoolAirVolFlow < 0.0 && thisSys.m_MaxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.m_MaxCoolAirVolFlow == 0.0 && thisSys.m_CoolCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFracCoolNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iCoolFlowPerFracCoolNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFracCoolNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.m_RequestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iCoolFlowPerFracCoolNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_CoolingSAFMethod, "FlowPerCoolingCapacity")) {

                    thisSys.m_CoolingSAFMethod = FlowPerCoolingCapacity;
                    if (loc_m_CoolingSAFMethod_FlowPerCoolingCapacity != -999.0) {
                        thisSys.m_MaxCoolAirVolFlow = loc_m_CoolingSAFMethod_FlowPerCoolingCapacity;
                        if ((thisSys.m_MaxCoolAirVolFlow < 0.0 && thisSys.m_MaxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.m_MaxCoolAirVolFlow == 0.0 && thisSys.m_CoolCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerCoolCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iCoolFlowPerCoolCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.m_MaxCoolAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerCoolCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.m_RequestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iCoolFlowPerCoolCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_CoolingSAFMethod, "None") || loc_m_CoolingSAFMethod == "") {
                    thisSys.m_CoolingSAFMethod = None;
                    //          thisSys%RequestAutosize = .TRUE. ! ??
                    if (thisSys.m_CoolCoilExists && thisSys.m_MaxCoolAirVolFlow == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " is blank and relates to " + loc_coolingCoilType +
                        // " = " +
                        //                  loc_m_CoolingCoilName);
                        if (thisSys.m_HeatCoilExists) {
                            ShowContinueError(
                                "Blank field not allowed for this coil type when heating coil air flow rate is notDataSizing::AutoSized.");
                        } else {
                            ShowContinueError("Blank field not allowed for this type of cooling coil.");
                        }
                        errorsFound = true;
                    }
                } else {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                    ShowContinueError("Valid entries are: SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingValue, "
                                      "FlowPerCoolingCapacity, or None ");
                    errorsFound = true;
                }

                // Determine supply air flow rate sizing method for heating mode
                if (UtilityRoutines::SameString(loc_m_HeatingSAFMethod, "SupplyAirFlowRate")) {
                    thisSys.m_HeatingSAFMethod = SupplyAirFlowRate;
                    if (loc_m_HeatingSAFMethod_SAFlow != -999.0) {
                        thisSys.m_MaxHeatAirVolFlow = loc_m_HeatingSAFMethod_SAFlow;
                        if (thisSys.m_MaxHeatAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;

                        if ((thisSys.m_MaxHeatAirVolFlow < 0.0 && thisSys.m_MaxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.m_MaxHeatAirVolFlow == 0.0 && thisSys.m_HeatCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cNumericFields(iMaxHeatAirVolFlowNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iMaxHeatAirVolFlowNumericNum), 7));
                            errorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iMaxHeatAirVolFlowNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_HeatingSAFMethod, "FlowPerFloorArea")) {
                    thisSys.m_HeatingSAFMethod = FlowPerFloorArea;
                    if (loc_m_HeatingSAFMethod_SAFlowPerFloorArea != -999.0) {
                        thisSys.m_MaxHeatAirVolFlow = loc_m_HeatingSAFMethod_SAFlowPerFloorArea;
                        if ((thisSys.m_MaxHeatAirVolFlow < 0.0 && thisSys.m_MaxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.m_MaxHeatAirVolFlow == 0.0 && thisSys.m_HeatCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFloorAreaNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iHeatFlowPerFloorAreaNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.m_MaxHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFloorAreaNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.m_MaxHeatAirVolFlow *= TotalFloorAreaOnAirLoop;
                            thisSys.m_RequestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatFlowPerFloorAreaNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_HeatingSAFMethod, "FractionOfAutosizedHeatingValue")) {
                    thisSys.m_HeatingSAFMethod = FractionOfAutoSizedHeatingValue;
                    if (loc_m_HeatingSAFMethod_FracOfAutosizedHeatingSAFlow != -999.0) {
                        thisSys.m_MaxHeatAirVolFlow = loc_m_HeatingSAFMethod_FracOfAutosizedHeatingSAFlow;
                        if ((thisSys.m_MaxHeatAirVolFlow < 0.0 && thisSys.m_MaxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.m_MaxHeatAirVolFlow == 0.0 && thisSys.m_HeatCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFracCoolNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iHeatFlowPerFracCoolNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.m_MaxHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFracCoolNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.m_RequestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatFlowPerFracCoolNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_HeatingSAFMethod, "FlowPerHeatingCapacity")) {
                    thisSys.m_HeatingSAFMethod = FlowPerHeatingCapacity;
                    if (loc_m_HeatingSAFMethod_FlowPerHeatingCapacity != -999.0) {
                        thisSys.m_MaxHeatAirVolFlow = loc_m_HeatingSAFMethod_FlowPerHeatingCapacity;
                        if ((thisSys.m_MaxHeatAirVolFlow < 0.0 && thisSys.m_MaxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.m_MaxHeatAirVolFlow == 0.0 && thisSys.m_HeatCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerHeatCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iHeatFlowPerHeatCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.m_MaxHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerHeatCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.m_RequestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatFlowPerHeatCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_HeatingSAFMethod, "None") || loc_m_HeatingSAFMethod == "") {
                    thisSys.m_HeatingSAFMethod = None;
                    //          thisSys%RequestAutosize = .TRUE. ! ??
                    if (thisSys.m_HeatCoilExists && thisSys.m_MaxHeatAirVolFlow == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " is blank and relates to " + loc_heatingCoilType +
                        // " = " +
                        //                  loc_m_HeatingCoilName);
                        if (thisSys.m_CoolCoilExists) {
                            ShowContinueError(
                                "Blank field not allowed for this coil type when cooling coil air flow rate is notDataSizing::AutoSized.");
                        } else {
                            ShowContinueError("Blank field not allowed for this type of heating coil.");
                        }
                        errorsFound = true;
                    }
                } else {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                    ShowContinueError("Valid entries are: SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedHeatingValue, "
                                      "FlowPerHeatingCapacity, or None ");
                    errorsFound = true;
                }

                // Determine supply air flow rate sizing method when cooling or heating is not needed
                if (UtilityRoutines::SameString(loc_m_NoCoolHeatSAFMethod, "SupplyAirFlowRate")) {
                    thisSys.m_NoCoolHeatSAFMethod = SupplyAirFlowRate;
                    if (loc_m_NoCoolHeatSAFMethod_SAFlow != -999.0) {
                        thisSys.m_MaxNoCoolHeatAirVolFlow = loc_m_NoCoolHeatSAFMethod_SAFlow;
                        if (thisSys.m_MaxNoCoolHeatAirVolFlow == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;

                        if (thisSys.m_MaxNoCoolHeatAirVolFlow < 0.0 && thisSys.m_MaxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cNumericFields(iMaxNoCoolHeatAirVolFlowNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iMaxNoCoolHeatAirVolFlowNumericNum), 7));
                            errorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iMaxNoCoolHeatAirVolFlowNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_NoCoolHeatSAFMethod, "FlowPerFloorArea")) {
                    thisSys.m_NoCoolHeatSAFMethod = FlowPerFloorArea;
                    if (loc_m_NoCoolHeatSAFMethod_SAFlowPerFloorArea != -999.0) {
                        thisSys.m_MaxNoCoolHeatAirVolFlow = loc_m_NoCoolHeatSAFMethod_SAFlowPerFloorArea;
                        if (thisSys.m_MaxNoCoolHeatAirVolFlow < 0.0 && thisSys.m_MaxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerFloorAreaNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.m_MaxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.m_MaxNoCoolHeatAirVolFlow *= TotalFloorAreaOnAirLoop;
                            thisSys.m_RequestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_NoCoolHeatSAFMethod, "FractionOfAutosizedCoolingValue")) {
                    thisSys.m_NoCoolHeatSAFMethod = FractionOfAutoSizedCoolingValue;
                    if (loc_m_NoCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow != -999.0) {
                        thisSys.m_MaxNoCoolHeatAirVolFlow = loc_m_NoCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow;
                        if (thisSys.m_MaxNoCoolHeatAirVolFlow < 0.0 && thisSys.m_MaxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerFracCoolNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.m_MaxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.m_RequestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_NoCoolHeatSAFMethod, "FractionOfAutosizedHeatingValue")) {
                    thisSys.m_NoCoolHeatSAFMethod = FractionOfAutoSizedHeatingValue;
                    if (loc_m_NoCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow != -999.0) {
                        thisSys.m_MaxNoCoolHeatAirVolFlow = loc_m_NoCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow;
                        if (thisSys.m_MaxNoCoolHeatAirVolFlow < 0.0 && thisSys.m_MaxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerFracHeatNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.m_MaxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.m_RequestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_NoCoolHeatSAFMethod, "FlowPerCoolingCapacity")) {
                    thisSys.m_NoCoolHeatSAFMethod = FlowPerCoolingCapacity;
                    if (loc_m_NoCoolHeatSAFMethod_FlowPerCoolingCapacity != -999.0) {
                        thisSys.m_MaxNoCoolHeatAirVolFlow = loc_m_NoCoolHeatSAFMethod_FlowPerCoolingCapacity;
                        if (thisSys.m_MaxNoCoolHeatAirVolFlow < 0.0 && thisSys.m_MaxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerCoolCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.m_MaxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.m_RequestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_NoCoolHeatSAFMethod, "FlowPerHeatingCapacity")) {
                    thisSys.m_NoCoolHeatSAFMethod = FlowPerHeatingCapacity;
                    if (loc_m_NoCoolHeatSAFMethod_FlowPerHeatingCapacity != -999.0) {
                        thisSys.m_MaxNoCoolHeatAirVolFlow = loc_m_NoCoolHeatSAFMethod_FlowPerHeatingCapacity;
                        if (thisSys.m_MaxNoCoolHeatAirVolFlow < 0.0 && thisSys.m_MaxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerHeatCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.m_MaxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.m_RequestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_m_NoCoolHeatSAFMethod, "None") || loc_m_NoCoolHeatSAFMethod == "") {
                    thisSys.m_NoCoolHeatSAFMethod = None;
                    //          thisSys%RequestAutosize = .TRUE. ! ??
                } else {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                    ShowContinueError("Valid entries are: SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingValue, "
                                      "FractionOfAutosizedHeatingValue, FlowPerCoolingCapacity, FlowPerHeatingCapacity, or None ");
                    errorsFound = true;
                }

                //       Fan operating mode (cycling or constant) schedule. IF constant fan, then set AirFlowControl
                if (thisSys.m_FanOpModeSchedPtr > 0) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.m_FanOpModeSchedPtr, ">=", 0.0, "<=", 0.0)) {
                        //           set fan operating mode to continuous so sizing can set VS coil data
                        thisSys.m_FanOpMode = DataHVACGlobals::ContFanCycCoil;
                        //           set air flow control mode:
                        //             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
                        //             UseCompressorOffFlow = operate at value specified by user
                        //           AirFlowControl only valid if fan opmode = ContFanCycComp
                        if (thisSys.m_MaxNoCoolHeatAirVolFlow == 0.0) {
                            thisSys.m_AirFlowControl = UseCompFlow::UseCompressorOnFlow;
                        } else {
                            thisSys.m_AirFlowControl = UseCompFlow::UseCompressorOffFlow;
                        }
                    }
                }

                if (thisSys.m_CoolingCoilType_Num != DataHVACGlobals::CoilDX_CoolingHXAssisted &&
                    thisSys.m_CoolingCoilType_Num != DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl &&
                    thisSys.m_DehumidControlType_Num == DehumCtrlType::Multimode) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iDehumidControlAlphaNum) + " = " + Alphas(iDehumidControlAlphaNum));
                    ShowContinueError("Multimode control must be used with a Heat Exchanger Assisted or Multimode Cooling Coil.");
                    if (loc_m_SuppHeatCoilName == "" && loc_suppHeatCoilType == "") {
                    } else {
                        if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                            ShowContinueError("Dehumidification control type is assumed to be None and the simulation continues.");
                            thisSys.m_DehumidControlType_Num = DehumCtrlType::None;
                        } else {
                            ShowContinueError("Dehumidification control type is assumed to be CoolReheat and the simulation continues.");
                            thisSys.m_DehumidControlType_Num = DehumCtrlType::CoolReheat;
                        }
                    }
                }

                //       Check placement of cooling coil with respect to fan placement and dehumidification control type

                if (thisSys.m_FanExists) {
                    if (thisSys.m_FanPlace == FanPlace::BlowThru) {
                        if (FanOutletNode == HeatingCoilInletNode && thisSys.m_DehumidControlType_Num != DehumCtrlType::CoolReheat) {
                            thisSys.m_CoolingCoilUpstream = false;
                        }
                    } else if (thisSys.m_FanPlace == FanPlace::DrawThru) {
                        if (HeatingCoilOutletNode == CoolingCoilInletNode && thisSys.m_DehumidControlType_Num != DehumCtrlType::CoolReheat) {
                            thisSys.m_CoolingCoilUpstream = false;
                        }
                    }
                } else {
                    if (HeatingCoilOutletNode == CoolingCoilInletNode && thisSys.m_DehumidControlType_Num != DehumCtrlType::CoolReheat) {
                        thisSys.m_CoolingCoilUpstream = false;
                    }
                    if (ZoneEquipmentFound) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("ZoneHVAC equipment must contain a fan object.");
                        // ShowContinueError("specified " + cAlphaFields(iFanTypeAlphaNum) + " = " + Alphas(iFanTypeAlphaNum));
                        // ShowContinueError("specified " + cAlphaFields(im_FanNameAlphaNum) + " = " + Alphas(im_FanNameAlphaNum));
                        errorsFound = true;
                    }
                }

                // check node connections
                if (thisSys.m_FanPlace == FanPlace::BlowThru) {

                    if (FanInletNode != thisSys.AirInNode) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("When a blow through fan is specified, the fan inlet node name must be the same as the unitary system "
                                          "inlet node name.");
                        ShowContinueError("...Fan inlet node name           = " + DataLoopNode::NodeID(FanInletNode));
                        ShowContinueError("...UnitarySystem inlet node name = " + DataLoopNode::NodeID(thisSys.AirInNode));
                        errorsFound = true;
                    }
                    if (thisSys.m_CoolingCoilUpstream) {
                        if (FanOutletNode != CoolingCoilInletNode && thisSys.m_CoolCoilExists && thisSys.m_FanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a blow through fan is specified, the fan outlet node name must be the same as the cooling coil "
                                              "inlet node name.");
                            ShowContinueError("...Fan outlet node name         = " + DataLoopNode::NodeID(FanOutletNode));
                            ShowContinueError("...Cooling coil inlet node name = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != HeatingCoilInletNode && thisSys.m_CoolCoilExists && thisSys.m_HeatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                            ShowContinueError("...Cooling coil outlet node name = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...Heating coil inlet node name  = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            errorsFound = true;
                        }
                        if (thisSys.m_SuppCoilExists) {
                            if (SupHeatCoilOutletNode != thisSys.AirOutNode) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError("The reheat coil outlet node name must be the same as the unitary system outlet node name.");
                                ShowContinueError("...Reheat coil outlet node name   = " + DataLoopNode::NodeID(SupHeatCoilOutletNode));
                                ShowContinueError("...UnitarySystem outlet node name = " + DataLoopNode::NodeID(thisSys.AirOutNode));
                                //                ErrorsFound=.TRUE.
                            }
                        } else { // IF((thisSys%m_Humidistat ...
                            // Heating coil outlet node name must be the same as the Unitary system outlet node name
                            if (thisSys.m_HeatCoilExists && HeatingCoilOutletNode != thisSys.AirOutNode) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError("When a blow through fan is specified, the heating coil outlet node name must be the same as the "
                                                  "unitary system outlet node name.");
                                ShowContinueError("...Heating coil outlet node name  = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                                ShowContinueError("...Unitary system outlet node name = " + DataLoopNode::NodeID(thisSys.AirOutNode));
                                errorsFound = true;
                            }
                        }
                    } else { // IF(thisSys%CoolingCoilUpstream)THEN
                        if (FanOutletNode != HeatingCoilInletNode && thisSys.m_FanExists && thisSys.m_HeatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a blow through fan is specified, the fan outlet node name must be the same as the heating coil "
                                              "inlet node name.");
                            ShowContinueError("...Fan outlet node name         = " + DataLoopNode::NodeID(FanOutletNode));
                            ShowContinueError("...Heating coil inlet node name = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            errorsFound = true;
                        }
                        if (HeatingCoilOutletNode != CoolingCoilInletNode && thisSys.m_CoolCoilExists && thisSys.m_HeatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The heating coil outlet node name must be the same as the cooling coil inlet node name.");
                            ShowContinueError("...Heating coil outlet node name = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                            ShowContinueError("...Cooling coil inlet node name  = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != thisSys.AirOutNode && thisSys.m_CoolCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(
                                "When a blow through fan is specified, the cooling coil outlet node name must be the same as the unitary "
                                "system outlet node name.");
                            ShowContinueError("...Cooling coil outlet node name   = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...UnitarySystem outlet node name  = " + DataLoopNode::NodeID(thisSys.AirOutNode));
                            errorsFound = true;
                        }
                    }

                } else if (thisSys.m_FanPlace == FanPlace::DrawThru) { // ELSE from IF(thisSys%FanPlace .EQ. BlowThru)THEN

                    if (thisSys.m_CoolingCoilUpstream) {
                        if (CoolingCoilInletNode != thisSys.AirInNode && CoolingCoilInletNode != 0 && thisSys.m_FanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(
                                "When a draw through fan is specified, the cooling coil inlet node name must be the same as the unitary "
                                "system inlet node name.");
                            ShowContinueError("...Cooling coil inlet node name  = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            ShowContinueError("...UnitarySystem inlet node name = " + DataLoopNode::NodeID(thisSys.AirInNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != HeatingCoilInletNode && thisSys.m_CoolCoilExists && thisSys.m_HeatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                            ShowContinueError("...Cooling coil outlet node name = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...Heating coil inlet node name  = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            errorsFound = true;
                        }
                        if (HeatingCoilOutletNode != FanInletNode && thisSys.m_HeatCoilExists && thisSys.m_FanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a draw through fan is specified, the heating coil outlet node name must be the same as the fan "
                                              "inlet node name.");
                            ShowContinueError("...Heating coil outlet node name = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                            ShowContinueError("...Fan inlet node name           = " + DataLoopNode::NodeID(FanInletNode));
                            errorsFound = true;
                        }
                        if (thisSys.m_SuppCoilExists) {
                            if (FanOutletNode != SupHeatCoilInletNode && thisSys.m_FanExists) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError(
                                    "When a draw through fan is specified, the fan outlet node name must be the same as the reheat coil "
                                    "inlet node name.");
                                ShowContinueError("...Fan outlet node name        = " + DataLoopNode::NodeID(FanOutletNode));
                                ShowContinueError("...Reheat coil inlet node name = " + DataLoopNode::NodeID(SupHeatCoilInletNode));
                                //                ErrorsFound=.TRUE.
                            }
                            if (SupHeatCoilOutletNode != thisSys.AirOutNode) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError("The reheat coil outlet node name must be the same as the unitary system outlet node name.");
                                ShowContinueError("...Reheat coil outlet node name   = " + DataLoopNode::NodeID(SupHeatCoilOutletNode));
                                ShowContinueError("...UnitarySystem outlet node name = " + DataLoopNode::NodeID(thisSys.AirOutNode));
                            }
                        } else {
                            if (FanOutletNode != thisSys.AirOutNode && thisSys.m_FanExists) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError(
                                    "When a draw through fan is specified, the fan outlet node name must be the same as the unitary system "
                                    "outlet node name.");
                                ShowContinueError("...Fan outlet node name        = " + DataLoopNode::NodeID(FanOutletNode));
                                ShowContinueError("...Unitary system outlet node name = " + DataLoopNode::NodeID(thisSys.AirOutNode));
                                errorsFound = true;
                            }
                        }
                    } else { // IF(thisSys%CoolingCoilUpstream)THEN
                        if (HeatingCoilInletNode != thisSys.AirInNode && HeatingCoilInletNode != 0 && thisSys.m_FanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(
                                "When a draw through fan is specified, the heating coil inlet node name must be the same as the unitary "
                                "system inlet node name.");
                            ShowContinueError("...Heating coil inlet node name  = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            ShowContinueError("...UnitarySystem inlet node name = " + DataLoopNode::NodeID(thisSys.AirInNode));
                            errorsFound = true;
                        }
                        if (HeatingCoilOutletNode != CoolingCoilInletNode && thisSys.m_HeatCoilExists && thisSys.m_CoolCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The heating coil outlet node name must be the same as the cooling coil inlet node name.");
                            ShowContinueError("...Heating coil outlet node name = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                            ShowContinueError("...Cooling coil inlet node name  = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != FanInletNode && thisSys.m_CoolCoilExists && thisSys.m_FanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a draw through fan is specified, the cooling coil outlet node name must be the same as the fan "
                                              "inlet node name.");
                            ShowContinueError("...Cooling coil outlet node name = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...Fan inlet node name           = " + DataLoopNode::NodeID(FanInletNode));
                            errorsFound = true;
                        }
                        if (FanOutletNode != thisSys.AirOutNode && thisSys.m_FanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a draw through fan is specified, the fan outlet node name must be the same as the unitary system "
                                              "outlet node name.");
                            ShowContinueError("...Fan outlet node name           = " + DataLoopNode::NodeID(FanOutletNode));
                            ShowContinueError("...UnitarySystem outlet node name = " + DataLoopNode::NodeID(thisSys.AirOutNode));
                            errorsFound = true;
                        }
                    }
                } // ELSE from IF(thisSys%FanPlace .EQ. BlowThru)THEN

                // Set the unitary system supplemental heater max outlet temperature
                // this field will be 0 if the input is not specified (included) in the input file
                // someone may use a default other than what we intended, allow it to be used
                // so if this field is blank, and the input field is included, read the default, otherwise use 80
                // if (!lNumericBlanks(iDesignMaxOutletTempNumericNum) && NumNumbers > (iDesignMaxOutletTempNumericNum - 1)) {
                thisSys.DesignMaxOutletTemp = loc_DesignMaxOutletTemp;
                if (thisSys.DesignMaxOutletTemp == DataSizing::AutoSize) thisSys.m_RequestAutoSize = true;
                //}

                // Set maximum Outdoor air temperature for supplemental heating coil operation
                // this field will be 0 if the input is not specified (included) in the input file
                // someone may use a default other than what we intended, allow it to be used
                // so if this field is blank, and the input field is included, read the default, otherwise use 9999
                // if (!lNumericBlanks(iMaxOATSuppHeatNumericNum) && NumNumbers > (iMaxOATSuppHeatNumericNum - 1)) {
                thisSys.m_MaxOATSuppHeat = loc_m_MaxOATSuppHeat;
                // Can't let MaxOATSuppHeat default to 21C if using cool reheat since it would shut off supp heater when dehumidifying
                // this may also allow supplemental heater to operate when in heating mode when it should not
                if (thisSys.m_MaxOATSuppHeat == 21.0 && thisSys.m_DehumidControlType_Num == DehumCtrlType::CoolReheat) {
                    thisSys.m_MaxOATSuppHeat = 999.0;
                }

                if (thisSys.m_MaxCoolAirVolFlow > 0.0 && thisSys.m_MaxHeatAirVolFlow > 0.0 && thisSys.m_MaxNoCoolHeatAirVolFlow >= 0.0 &&
                    !thisSys.m_RequestAutoSize) {
                    thisSys.m_DesignFanVolFlowRate = max(thisSys.m_MaxCoolAirVolFlow, thisSys.m_MaxHeatAirVolFlow, thisSys.m_MaxNoCoolHeatAirVolFlow);
                } else if (thisSys.m_MaxCoolAirVolFlow > 0.0 && thisSys.m_MaxNoCoolHeatAirVolFlow >= 0.0 && !thisSys.m_RequestAutoSize) {
                    thisSys.m_DesignFanVolFlowRate = max(thisSys.m_MaxCoolAirVolFlow, thisSys.m_MaxNoCoolHeatAirVolFlow);
                } else if (thisSys.m_MaxHeatAirVolFlow > 0.0 && thisSys.m_MaxNoCoolHeatAirVolFlow >= 0.0 && !thisSys.m_RequestAutoSize) {
                    thisSys.m_DesignFanVolFlowRate = max(thisSys.m_MaxHeatAirVolFlow, thisSys.m_MaxNoCoolHeatAirVolFlow);
                } else {
                    if (thisSys.m_FanExists && thisSys.m_DesignFanVolFlowRate == 0.0) {
                        thisSys.m_DesignFanVolFlowRate = DataSizing::AutoSize;
                    }
                    // need more of this type of warning when flow cannot be determined
                    if (thisSys.m_MaxHeatAirVolFlow == 0.0 && thisSys.m_HeatCoilExists) {
                        if (thisSys.m_FanExists) {
                            if (thisSys.m_CoolCoilExists && thisSys.m_MaxCoolAirVolFlow != DataSizing::AutoSize) {
                                if (thisSys.m_MaxCoolAirVolFlow == 0.0) {
                                    thisSys.m_MaxHeatAirVolFlow = thisSys.m_DesignFanVolFlowRate;
                                }
                            }
                        } else if (thisSys.m_CoolCoilExists) {
                            thisSys.m_MaxHeatAirVolFlow = thisSys.m_MaxCoolAirVolFlow;
                        } else {
                            if (thisSys.m_HeatingCoilType_Num != DataHVACGlobals::CoilDX_HeatingEmpirical &&
                                thisSys.m_HeatingCoilType_Num != DataHVACGlobals::CoilDX_MultiSpeedHeating &&
                                thisSys.m_HeatingCoilType_Num != DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("When non-DX heating coils are specified, the heating air flow rate must be entered in " +
                                //                  cAlphaFields(iHeatSAFMAlphaNum));
                                errorsFound = true;
                            }
                        }
                    } else if (thisSys.m_MaxHeatAirVolFlow == 0.0 && !thisSys.m_FanExists && !thisSys.m_CoolCoilExists) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("When non-DX heating coils are specified, the heating air flow rate must be entered in " +
                        //                  cAlphaFields(iHeatSAFMAlphaNum));
                    }
                }

                if (FanVolFlowRate != DataSizing::AutoSize && thisSys.m_FanExists) {
                    if (FanVolFlowRate < thisSys.m_MaxCoolAirVolFlow && thisSys.m_MaxCoolAirVolFlow != DataSizing::AutoSize &&
                        thisSys.m_CoolCoilExists) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... air flow rate = " + General::TrimSigDigits(FanVolFlowRate, 7) + " in fan object " + thisSys.m_FanName +
                                          " is less than the maximum HVAC system air flow rate in cooling mode.");
                        // ShowContinueError(" The " + cNumericFields(iMaxCoolAirVolFlowNumericNum) +
                        //                  " is reset to the fan flow rate and the simulation continues.");
                        thisSys.m_MaxCoolAirVolFlow = FanVolFlowRate;
                        thisSys.m_DesignFanVolFlowRate = FanVolFlowRate;
                    }
                    if (FanVolFlowRate < thisSys.m_MaxHeatAirVolFlow && thisSys.m_MaxHeatAirVolFlow != DataSizing::AutoSize &&
                        thisSys.m_HeatCoilExists) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... air flow rate = " + General::TrimSigDigits(FanVolFlowRate, 7) + " in fan object " + thisSys.m_FanName +
                                          " is less than the maximum HVAC system air flow rate in heating mode.");
                        // ShowContinueError(" The " + cNumericFields(3) + " is reset to the fan flow rate and the simulation continues.");
                        thisSys.m_MaxHeatAirVolFlow = FanVolFlowRate;
                        thisSys.m_DesignFanVolFlowRate = FanVolFlowRate;
                    }
                }

                if (thisSys.m_FanOpModeSchedPtr > 0) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.m_FanOpModeSchedPtr, ">=", 0.0, "<=", 0.0)) {
                        //           set air flow control mode:
                        //             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
                        //             UseCompressorOffFlow = operate at value specified by user
                        //           AirFlowControl only valid IF fan opmode = ContFanCycComp
                        if (thisSys.m_MaxNoCoolHeatAirVolFlow == 0.0) {
                            thisSys.m_AirFlowControl = UseCompFlow::UseCompressorOnFlow;
                        } else {
                            thisSys.m_AirFlowControl = UseCompFlow::UseCompressorOffFlow;
                        }
                    }
                }

                // Set minimum OAT for heat pump compressor operation in cooling mode
                // get from coil module
                errFlag = false;
                if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                    thisSys.m_MinOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                    thisSys.m_MinOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                    thisSys.m_MinOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                    thisSys.m_MinOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    thisSys.m_MinOATCompressorCooling = VariableSpeedCoils::GetVSCoilMinOATCompressor(loc_m_CoolingCoilName, errFlag);
                } else {
                    thisSys.m_MinOATCompressorCooling = -1000.0;
                }
                if (errFlag) {
                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                    errorsFound = true;
                }

                // Set minimum OAT for heat pump compressor operation in heating mode
                // get from coil module
                errFlag = false;
                if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                    thisSys.m_MinOATCompressorHeating = VariableSpeedCoils::GetVSCoilMinOATCompressor(loc_m_HeatingCoilName, errFlag);
                } else if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical ||
                           thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                    thisSys.m_MinOATCompressorHeating = DXCoils::GetMinOATCompressor(loc_heatingCoilType, loc_m_HeatingCoilName, errFlag);
                    //       ELSEIF  ***... make sure we catch all possbile coil types here ...***
                } else {
                    thisSys.m_MinOATCompressorHeating = -1000.0;
                }
                if (errFlag) {
                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                    errorsFound = true;
                }

                //       Mine heatpump Outdoor condenser node from DX coil object
                errFlag = false;
                if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                    thisSys.m_CondenserNodeNum = DXCoils::GetCoilCondenserInletNode(loc_coolingCoilType, loc_m_CoolingCoilName, errFlag);
                } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    thisSys.m_CondenserNodeNum = VariableSpeedCoils::GetVSCoilCondenserInletNode(loc_m_CoolingCoilName, errFlag);
                } else if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) {
                    // already filled
                    // UnitarySystem( UnitarySysNum ).CondenserNodeNum = GetDXCoilCondenserInletNode( "Coil:Cooling:DX:SingleSpeed",
                    // GetHXDXCoilName( CoolingCoilType, loc_m_CoolingCoilName, errFlag ), errFlag );

                } else {
                    if (loc_condenserInletNodeName != "") {
                        thisSys.m_CondenserNodeNum = NodeInputManager::GetOnlySingleNode(loc_condenserInletNodeName,
                                                                                         errFlag,
                                                                                         cCurrentModuleObject,
                                                                                         thisObjectName,
                                                                                         DataLoopNode::NodeType_Air,
                                                                                         DataLoopNode::NodeConnectionType_Inlet,
                                                                                         1,
                                                                                         DataLoopNode::ObjectIsParent);
                    } else {
                        // do nothing?
                    }
                }
                if (errFlag) {
                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                    errorsFound = true;
                }

                // Set the heatpump cycling rate
                thisSys.m_MaxONOFFCyclesperHour = loc_m_MaxONOFFCyclesperHour;
                // if (NumNumbers < iMaxONOFFCycPerHourNumericNum) {
                //    thisSys.m_MaxONOFFCyclesperHour = 2.5;
                //}

                // Set the heat pump time constant
                thisSys.m_HPTimeConstant = loc_m_HPTimeConstant;
                // if (NumNumbers < im_HPTimeConstantNumericNum) {
                //    thisSys.m_HPTimeConstant = 60.0;
                //}

                // Set the heat pump on-cycle power use fraction
                thisSys.m_OnCyclePowerFraction = loc_m_OnCyclePowerFraction;
                // if (NumNumbers < iOnCyclePowerFracNumericNum) {
                //    thisSys.OnCyclePowerFraction = 0.01;
                //}

                // Set the heat pump fan delay time
                thisSys.m_FanDelayTime = loc_m_FanDelayTime;
                // if (NumNumbers < iFanDelayTimeNumericNum) {
                //    thisSys.FanDelayTime = 60.0;
                //}

                thisSys.m_AncillaryOnPower = loc_m_AncillaryOnPower;
                thisSys.m_AncillaryOffPower = loc_m_AncillaryOffPower;

                thisSys.m_DesignHRWaterVolumeFlow = loc_m_DesignHRWaterVolumeFlow;
                thisSys.m_MaxHROutletWaterTemp = loc_m_MaxHROutletWaterTemp;

                if (thisSys.m_DesignHRWaterVolumeFlow > 0.0) {
                    thisSys.m_HeatRecActive = true;
                    errFlag = false;
                    if (loc_heatRecoveryInletNodeName != "" && loc_heatRecoveryOutletNodeName != "") {
                        thisSys.m_HeatRecoveryInletNodeNum = NodeInputManager::GetOnlySingleNode(loc_heatRecoveryInletNodeName,
                                                                                                 errFlag,
                                                                                                 cCurrentModuleObject,
                                                                                                 thisObjectName,
                                                                                                 DataLoopNode::NodeType_Water,
                                                                                                 DataLoopNode::NodeConnectionType_Inlet,
                                                                                                 3,
                                                                                                 DataLoopNode::ObjectIsNotParent);
                        thisSys.m_HeatRecoveryOutletNodeNum = NodeInputManager::GetOnlySingleNode(loc_heatRecoveryOutletNodeName,
                                                                                                  errFlag,
                                                                                                  cCurrentModuleObject,
                                                                                                  thisObjectName,
                                                                                                  DataLoopNode::NodeType_Water,
                                                                                                  DataLoopNode::NodeConnectionType_Outlet,
                                                                                                  3,
                                                                                                  DataLoopNode::ObjectIsNotParent);

                        BranchNodeConnections::TestCompSet(cCurrentModuleObject,
                                                           thisObjectName,
                                                           loc_heatRecoveryInletNodeName,
                                                           loc_heatRecoveryOutletNodeName,
                                                           "Unitary System Heat Recovery Nodes");

                        if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                            DXCoils::SetMSHPDXCoilHeatRecoveryFlag(thisSys.m_CoolingCoilIndex);
                        }
                        if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                            DXCoils::SetMSHPDXCoilHeatRecoveryFlag(thisSys.m_HeatingCoilIndex);
                        }
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Illegal " + cAlphaFields(iHRWaterInletNodeAlphaNum) + " = " + Alphas(iHRWaterInletNodeAlphaNum));
                        // ShowContinueError("Illegal " + cAlphaFields(iHRWaterOutletNodeAlphaNum) + " = " + Alphas(iHRWaterOutletNodeAlphaNum));
                        // ShowContinueError("... heat recovery nodes must be specified when " + cNumericFields(iDesignHRWaterVolFlowNumericNum) +
                        //                  " is greater than 0.");
                        // ShowContinueError("... " + cNumericFields(iDesignHRWaterVolFlowNumericNum) + " = " +
                        //                  TrimSigDigits(thisSys.DesignHRWaterVolumeFlow, 7));
                        errorsFound = true;
                    }
                }

                if (loc_m_DesignSpecMultispeedHPType != "" && loc_m_DesignSpecMultispeedHPName != "") {
                    thisSys.m_DesignSpecMultispeedHPType = loc_m_DesignSpecMultispeedHPType;
                    thisSys.m_DesignSpecMultispeedHPName = loc_m_DesignSpecMultispeedHPName;
                    int designSpecType_Num = 1;

                    DesignSpecMSHP thisDesignSpec;
                    thisSys.m_CompPointerMSHP = thisDesignSpec.factory(designSpecType_Num, loc_m_DesignSpecMultispeedHPName);
                    thisSys.m_DesignSpecMSHPIndex = getDesignSpecMSHPIndex(thisSys.m_DesignSpecMultispeedHPName);

                    if (thisSys.m_DesignSpecMSHPIndex > -1) {

                        thisSys.m_NoLoadAirFlowRateRatio = thisSys.m_CompPointerMSHP->noLoadAirFlowRateRatio;

                        if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                            thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                            thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {
                            thisSys.m_NumOfSpeedHeating = thisSys.m_CompPointerMSHP->numOfSpeedHeating;
                            thisSys.m_HeatMassFlowRate.resize(thisSys.m_NumOfSpeedHeating + 1);
                            thisSys.m_HeatVolumeFlowRate.resize(thisSys.m_NumOfSpeedHeating + 1);
                            thisSys.m_MSHeatingSpeedRatio.resize(thisSys.m_NumOfSpeedHeating + 1);
                            for (int i = 1; i <= thisSys.m_NumOfSpeedHeating; ++i) {
                                thisSys.m_HeatMassFlowRate[i] = 0.0;
                                thisSys.m_HeatVolumeFlowRate[i] = 0.0;
                                thisSys.m_MSHeatingSpeedRatio[i] = 1.0;
                            }
                        }

                        if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                            thisSys.m_NumOfSpeedCooling = thisSys.m_CompPointerMSHP->numOfSpeedCooling;
                            thisSys.m_CoolMassFlowRate.resize(thisSys.m_NumOfSpeedCooling + 1);
                            thisSys.m_CoolVolumeFlowRate.resize(thisSys.m_NumOfSpeedCooling + 1);
                            thisSys.m_MSCoolingSpeedRatio.resize(thisSys.m_NumOfSpeedCooling + 1);
                            for (int i = 1; i <= thisSys.m_NumOfSpeedCooling; ++i) {
                                thisSys.m_CoolMassFlowRate[i] = 0.0;
                                thisSys.m_CoolVolumeFlowRate[i] = 0.0;
                                thisSys.m_MSCoolingSpeedRatio[i] = 1.0;
                            }
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... one or both of the following inputs are invalid.");
                        // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPTypeAlphaNum) + " = " + Alphas(iDesignSpecMSHPTypeAlphaNum));
                        // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPNameAlphaNum) + " = " + Alphas(iDesignSpecMSHPNameAlphaNum));
                        errorsFound = true;
                    }
                } else if ((loc_m_DesignSpecMultispeedHPType == "" && loc_m_DesignSpecMultispeedHPName != "") ||
                           (loc_m_DesignSpecMultispeedHPType != "" && loc_m_DesignSpecMultispeedHPName == "")) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    ShowContinueError("... one or both of the following inputs are invalid.");
                    // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPTypeAlphaNum) + " = " + Alphas(iDesignSpecMSHPTypeAlphaNum));
                    // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPNameAlphaNum) + " = " + Alphas(iDesignSpecMSHPNameAlphaNum));
                    errorsFound = true;
                    //} else if (thisSys.m_NumOfSpeedHeating > 0) { // how do these last 2 get called?
                    //    int m_NumOfSpeedHeating = thisSys.m_NumOfSpeedHeating;

                    //    thisSys.m_HeatMassFlowRate.allocate(m_NumOfSpeedHeating);
                    //    thisSys.m_HeatVolumeFlowRate.allocate(m_NumOfSpeedHeating);
                    //    thisSys.m_MSHeatingSpeedRatio.allocate(m_NumOfSpeedHeating);
                    //    thisSys.m_MSHeatingSpeedRatio = 1.0;

                    //} else if (thisSys.m_NumOfSpeedCooling > 0) {
                    //    int m_NumOfSpeedCooling = thisSys.m_NumOfSpeedCooling;

                    //    thisSys.m_CoolMassFlowRate.allocate(m_NumOfSpeedCooling);
                    //    thisSys.m_CoolVolumeFlowRate.allocate(m_NumOfSpeedCooling);
                    //    thisSys.m_m_MSCoolingSpeedRatio.allocate(m_NumOfSpeedCooling);
                    //    thisSys.m_m_MSCoolingSpeedRatio = 1.0;
                }

                if (thisSys.m_MultiSpeedCoolingCoil) {

                    // int designSpecIndex = thisSys.m_DesignSpecMSHPIndex;
                    // if (designSpecIndex > 0) thisSys.m_NumOfSpeedCooling = DesignSpecMSHP.numOfSpeedCooling;

                    if (thisSys.m_NumOfSpeedCooling == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... Cooling coil object type requires valid " + unitarySysHeatPumpPerformanceObjectType +
                                          " for cooling to be specified with number of speeds > 0");
                        errorsFound = true;
                    }
                }
                if (thisSys.m_MultiSpeedHeatingCoil) {

                    if (thisSys.m_DesignSpecMSHPIndex > -1) thisSys.m_NumOfSpeedHeating = thisSys.m_CompPointerMSHP->numOfSpeedHeating;

                    if (thisSys.m_NumOfSpeedHeating == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... Heating coil object type requires valid " + unitarySysHeatPumpPerformanceObjectType +
                                          " for heating to be specified with number of speeds > 0");
                        errorsFound = true;
                    }
                }

                if ((thisSys.m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating &&
                     thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) ||
                    (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingGasOrOtherFuel &&
                     thisSys.m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling)) {
                    if (thisSys.m_DesignSpecMSHPIndex > -1) {
                        if (thisSys.m_CompPointerMSHP->m_SingleModeFlag) {
                            thisSys.m_SingleMode = 1;
                        }
                    }
                } else {
                    if (thisSys.m_DesignSpecMSHPIndex > -1) {
                        if (thisSys.m_CompPointerMSHP->m_SingleModeFlag) {
                            ShowSevereError(cCurrentModuleObject + ": " + thisObjectName);
                            ShowContinueError(
                                "In order to perform Single Mode Operation, the valid cooling coil type is Coil:Cooling:DX:MultiSpeed and "
                                "the valid heating is Coil:Heating:DX:MultiSpeed or Coil:Heating:Fuel.");
                            // ShowContinueError("The input cooling coil type = " + Alphas(iCoolingCoilTypeAlphaNum) +
                            //                  " and the input heating coil type = " + Alphas(iHeatingCoilTypeAlphaNum));
                        }
                    }
                }

                if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    VariableSpeedCoils::SetVarSpeedCoilData(thisSys.m_CoolingCoilIndex, errorsFound, _, _, thisSys.m_DesignSpecMSHPIndex);
                }

                if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                    VariableSpeedCoils::SetVarSpeedCoilData(thisSys.m_HeatingCoilIndex, errorsFound, _, _, thisSys.m_DesignSpecMSHPIndex);
                }

                // set global logicals that denote coil type
                if (thisSys.m_MultiSpeedHeatingCoil || thisSys.m_VarSpeedHeatingCoil) {
                    thisSys.m_MultiOrVarSpeedHeatCoil = true;
                }
                if (thisSys.m_MultiSpeedCoolingCoil || thisSys.m_VarSpeedCoolingCoil) {
                    thisSys.m_MultiOrVarSpeedCoolCoil = true;
                }

                // set global variables for multi-stage chilled and hot water coils
                if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                    thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {
                    if (thisSys.m_DesignSpecMSHPIndex > -1) {
                        thisSys.m_NumOfSpeedCooling = thisSys.m_CompPointerMSHP->numOfSpeedCooling;
                        if (thisSys.m_NumOfSpeedCooling > 1) {
                            thisSys.m_MultiSpeedCoolingCoil = true;
                            thisSys.m_MultiOrVarSpeedCoolCoil = true;
                        }
                    }
                }
                if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                    // designSpecIndex = thisSys.m_DesignSpecMSHPIndex;
                    if (thisSys.m_DesignSpecMSHPIndex > -1) {
                        thisSys.m_NumOfSpeedHeating = thisSys.m_CompPointerMSHP->numOfSpeedHeating;
                        if (thisSys.m_NumOfSpeedHeating > 1) {
                            thisSys.m_MultiSpeedHeatingCoil = true;
                            thisSys.m_MultiOrVarSpeedHeatCoil = true;
                        }
                    }
                }

                // check for specific input requirements for ASHRAE90.1 model
                if (thisSys.m_ControlType == ControlType::CCMASHRAE) {

                    // only allowed for water and DX cooling coils at this time
                    if (thisSys.m_CoolCoilExists && thisSys.m_CoolingCoilType_Num != DataHVACGlobals::Coil_CoolingWater &&
                        thisSys.m_CoolingCoilType_Num != DataHVACGlobals::Coil_CoolingWaterDetailed &&
                        thisSys.m_CoolingCoilType_Num != DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                        if (DataGlobals::DisplayExtraWarnings) {
                            ShowWarningError(cCurrentModuleObject + ": " + thisObjectName);
                            ShowContinueError("ASHRAE90.1 control method requires specific cooling coil types.");
                            ShowContinueError("Valid cooling coil types are Coil:Cooling:Water, Coil:Cooling:Water:DetailedGeometry and "
                                              "Coil:Cooling:DX:SingleSpeed.");
                            // ShowContinueError("The input cooling coil type = " + Alphas(iCoolingCoilTypeAlphaNum) +
                            //                  ". This coil will not be modeled using the ASHRAE 90.1 algorithm.");
                        }
                        // mark this coil as non-ASHRAE90 type
                        thisSys.m_ValidASHRAECoolCoil = false;
                    }
                    // only allow for water, fuel, or electric at this time
                    if (thisSys.m_HeatCoilExists && thisSys.m_HeatingCoilType_Num != DataHVACGlobals::Coil_HeatingWater &&
                        thisSys.m_HeatingCoilType_Num != DataHVACGlobals::Coil_HeatingGasOrOtherFuel &&
                        thisSys.m_HeatingCoilType_Num != DataHVACGlobals::Coil_HeatingElectric &&
                        thisSys.m_HeatingCoilType_Num != DataHVACGlobals::CoilDX_HeatingEmpirical) {
                        if (DataGlobals::DisplayExtraWarnings) {
                            ShowWarningError(cCurrentModuleObject + ": " + thisObjectName);
                            ShowContinueError("ASHRAE90.1 control method requires specific heating coil types.");
                            ShowContinueError("Valid heating coil types are Coil:Heating:Water, Coil:Heating:Fuel, Coil:Heating:Electric and "
                                              "Coil:Heating:DX:SingleSpeed.");
                            // ShowContinueError("The input heating coil type = " + Alphas(iHeatingCoilTypeAlphaNum) +
                            //                  ". This coil will not be modeled using the ASHRAE 90.1 algorithm.");
                        }
                        // mark this coil as non-ASHRAE90 type
                        thisSys.m_ValidASHRAEHeatCoil = false;
                    }
                    if (thisSys.m_DehumidControlType_Num == DehumCtrlType::Multimode ||
                        thisSys.m_DehumidControlType_Num == DehumCtrlType::CoolReheat) {
                        ShowWarningError(cCurrentModuleObject + ": " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iDehumidControlAlphaNum) + " = " +
                        // Alphas(iDehumidControlAlphaNum));
                        ShowContinueError(
                            "ASHRAE90.1 control method does not support dehumidification at this time. Dehumidification control type is "
                            "assumed to be None.");
                        thisSys.m_DehumidControlType_Num = DehumCtrlType::None;
                    }
                    if (thisSys.m_RunOnLatentLoad) {
                        ShowWarningError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iRunOnLatentLoadAlphaNum) + " :" +
                        // Alphas(iRunOnLatentLoadAlphaNum));
                        ShowContinueError(
                            "ASHRAE90.1 control method does not support latent load control at this time. This input must be selected as "
                            "SensibleOnlyLoadControl.");
                        thisSys.m_RunOnSensibleLoad = true;
                        thisSys.m_RunOnLatentLoad = false;
                        thisSys.m_RunOnLatentOnlyWithSensible = false;
                    }
                }

                if (sysNum == -1) {
                    unitarySys.push_back(thisSys);
                } else {
                    unitarySys[sysNum] = thisSys;
                }

                if (sysNum == -1) sysNum = getUnitarySystemIndex(thisObjectName);

                // Setup Report variables for the Unitary System that are not reported in the components themselves
                //                if (GetUnitarySystemDoOnlyOnceFlag) {
                //                    for (UnitarySysNum = 1; UnitarySysNum <= NumUnitarySystem; ++UnitarySysNum) {
                SetupOutputVariable("Unitary System Part Load Ratio",
                                    OutputProcessor::Unit::None,
                                    unitarySys[sysNum].m_PartLoadFrac,
                                    "System",
                                    "Average",
                                    unitarySys[sysNum].Name);
                SetupOutputVariable("Unitary System Total Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    unitarySys[sysNum].m_TotCoolEnergyRate,
                                    "System",
                                    "Average",
                                    unitarySys[sysNum].Name);
                SetupOutputVariable("Unitary System Sensible Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    unitarySys[sysNum].m_SensCoolEnergyRate,
                                    "System",
                                    "Average",
                                    unitarySys[sysNum].Name);
                SetupOutputVariable("Unitary System Latent Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    unitarySys[sysNum].m_LatCoolEnergyRate,
                                    "System",
                                    "Average",
                                    unitarySys[sysNum].Name);
                SetupOutputVariable("Unitary System Total Heating Rate",
                                    OutputProcessor::Unit::W,
                                    unitarySys[sysNum].m_TotHeatEnergyRate,
                                    "System",
                                    "Average",
                                    unitarySys[sysNum].Name);
                SetupOutputVariable("Unitary System Sensible Heating Rate",
                                    OutputProcessor::Unit::W,
                                    unitarySys[sysNum].m_SensHeatEnergyRate,
                                    "System",
                                    "Average",
                                    unitarySys[sysNum].Name);
                SetupOutputVariable("Unitary System Latent Heating Rate",
                                    OutputProcessor::Unit::W,
                                    unitarySys[sysNum].m_LatHeatEnergyRate,
                                    "System",
                                    "Average",
                                    unitarySys[sysNum].Name);
                SetupOutputVariable("Unitary System Ancillary Electric Power",
                                    OutputProcessor::Unit::W,
                                    unitarySys[sysNum].m_TotalAuxElecPower,
                                    "System",
                                    "Average",
                                    unitarySys[sysNum].Name);

                // report predicted load as determined by Unitary System for load control only
                if (unitarySys[sysNum].m_ControlType != ControlType::Setpoint) {
                    SetupOutputVariable("Unitary System Predicted Sensible Load to Setpoint Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        unitarySys[sysNum].m_SensibleLoadPredicted,
                                        "System",
                                        "Average",
                                        unitarySys[sysNum].Name);
                    SetupOutputVariable("Unitary System Predicted Moisture Load to Setpoint Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        unitarySys[sysNum].m_MoistureLoadPredicted,
                                        "System",
                                        "Average",
                                        unitarySys[sysNum].Name);
                }

                //        IF(UnitarySystem(UnitarySysNum)%m_DehumidControlType_Num .EQ. dehumidm_ControlType::CoolReheat)THEN
                SetupOutputVariable("Unitary System Dehumidification Induced Heating Demand Rate",
                                    OutputProcessor::Unit::W,
                                    unitarySys[sysNum].m_DehumidInducedHeatingDemandRate,
                                    "System",
                                    "Average",
                                    unitarySys[sysNum].Name);
                //        END IF

                if (unitarySys[sysNum].m_FanExists) {
                    SetupOutputVariable("Unitary System Fan Part Load Ratio",
                                        OutputProcessor::Unit::None,
                                        unitarySys[sysNum].FanPartLoadRatio,
                                        "System",
                                        "Average",
                                        unitarySys[sysNum].Name);
                }

                SetupOutputVariable("Unitary System Compressor Part Load Ratio",
                                    OutputProcessor::Unit::None,
                                    unitarySys[sysNum].m_CompPartLoadRatio,
                                    "System",
                                    "Average",
                                    unitarySys[sysNum].Name);

                SetupOutputVariable("Unitary System Frost Control Status",
                                    OutputProcessor::Unit::None,
                                    unitarySys[sysNum].m_FrostControlStatus,
                                    "System",
                                    "Average",
                                    unitarySys[sysNum].Name);

                if (unitarySys[sysNum].m_HeatCoilExists) {
                    SetupOutputVariable("Unitary System Heating Ancillary Electric Energy",
                                        OutputProcessor::Unit::J,
                                        unitarySys[sysNum].m_HeatingAuxElecConsumption,
                                        "System",
                                        "Sum",
                                        unitarySys[sysNum].Name,
                                        _,
                                        "Electric",
                                        "Heating",
                                        _,
                                        "System");
                }

                {
                    auto const SELECT_CASE_var(unitarySys[sysNum].m_CoolingCoilType_Num);
                    if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                        SetupOutputVariable("Unitary System Cycling Ratio",
                                            OutputProcessor::Unit::None,
                                            unitarySys[sysNum].m_CycRatio,
                                            "System",
                                            "Average",
                                            unitarySys[sysNum].Name);
                        SetupOutputVariable("Unitary System Compressor Speed Ratio",
                                            OutputProcessor::Unit::None,
                                            unitarySys[sysNum].m_SpeedRatio,
                                            "System",
                                            "Average",
                                            unitarySys[sysNum].Name);
                    } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                        SetupOutputVariable("Unitary System Cooling Ancillary Electric Energy",
                                            OutputProcessor::Unit::J,
                                            unitarySys[sysNum].m_CoolingAuxElecConsumption,
                                            "System",
                                            "Sum",
                                            unitarySys[sysNum].Name,
                                            _,
                                            "Electric",
                                            "Cooling",
                                            _,
                                            "System");
                        SetupOutputVariable("Unitary System Electric Power",
                                            OutputProcessor::Unit::W,
                                            unitarySys[sysNum].m_ElecPower,
                                            "System",
                                            "Average",
                                            unitarySys[sysNum].Name);
                        SetupOutputVariable("Unitary System Electric Energy",
                                            OutputProcessor::Unit::J,
                                            unitarySys[sysNum].m_ElecPowerConsumption,
                                            "System",
                                            "Sum",
                                            unitarySys[sysNum].Name);
                        if (unitarySys[sysNum].m_HeatRecActive) {
                            SetupOutputVariable("Unitary System Heat Recovery Rate",
                                                OutputProcessor::Unit::W,
                                                unitarySys[sysNum].m_HeatRecoveryRate,
                                                "System",
                                                "Average",
                                                unitarySys[sysNum].Name);
                            SetupOutputVariable("Unitary System Heat Recovery Inlet Temperature",
                                                OutputProcessor::Unit::C,
                                                unitarySys[sysNum].m_HeatRecoveryInletTemp,
                                                "System",
                                                "Average",
                                                unitarySys[sysNum].Name);
                            SetupOutputVariable("Unitary System Heat Recovery Outlet Temperature",
                                                OutputProcessor::Unit::C,
                                                unitarySys[sysNum].m_HeatRecoveryOutletTemp,
                                                "System",
                                                "Average",
                                                unitarySys[sysNum].Name);
                            SetupOutputVariable("Unitary System Heat Recovery Fluid Mass Flow Rate",
                                                OutputProcessor::Unit::kg_s,
                                                unitarySys[sysNum].m_HeatRecoveryMassFlowRate,
                                                "System",
                                                "Average",
                                                unitarySys[sysNum].Name);
                            SetupOutputVariable("Unitary System Heat Recovery Energy",
                                                OutputProcessor::Unit::J,
                                                unitarySys[sysNum].m_HeatRecoveryEnergy,
                                                "System",
                                                "Sum",
                                                unitarySys[sysNum].Name);
                        }
                    } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHP)) {
                        SetupOutputVariable("Unitary System Requested Sensible Cooling Rate",
                                            OutputProcessor::Unit::W,
                                            unitarySys[sysNum].m_CoolingCoilSensDemand,
                                            "System",
                                            "Average",
                                            unitarySys[sysNum].Name);
                        SetupOutputVariable("Unitary System Requested Latent Cooling Rate",
                                            OutputProcessor::Unit::W,
                                            unitarySys[sysNum].m_CoolingCoilLatentDemand,
                                            "System",
                                            "Average",
                                            unitarySys[sysNum].Name);
                    } else {
                    }
                }

                {
                    auto const SELECT_CASE_var(unitarySys[sysNum].m_HeatingCoilType_Num);
                    if ((SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) ||
                        (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) ||
                        (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {
                    } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHP)) {
                        SetupOutputVariable("Unitary System Requested Heating Rate",
                                            OutputProcessor::Unit::W,
                                            unitarySys[sysNum].m_HeatingCoilSensDemand,
                                            "System",
                                            "Average",
                                            unitarySys[sysNum].Name);
                    } else {
                    }
                }

                if (unitarySys[sysNum].m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling ||
                    unitarySys[sysNum].m_HeatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                    unitarySys[sysNum].m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                    unitarySys[sysNum].m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {
                    SetupOutputVariable("Unitary System DX Coil Cycling Ratio",
                                        OutputProcessor::Unit::None,
                                        unitarySys[sysNum].m_CycRatio,
                                        "System",
                                        "Average",
                                        unitarySys[sysNum].Name);
                    SetupOutputVariable("Unitary System DX Coil Speed Ratio",
                                        OutputProcessor::Unit::None,
                                        unitarySys[sysNum].m_SpeedRatio,
                                        "System",
                                        "Average",
                                        unitarySys[sysNum].Name);
                    SetupOutputVariable("Unitary System DX Coil Speed Level",
                                        OutputProcessor::Unit::None,
                                        unitarySys[sysNum].m_SpeedNum,
                                        "System",
                                        "Average",
                                        unitarySys[sysNum].Name);
                }

                if (((unitarySys[sysNum].m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                      unitarySys[sysNum].m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) &&
                     unitarySys[sysNum].m_MultiSpeedCoolingCoil) ||
                    (unitarySys[sysNum].m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater && unitarySys[sysNum].m_MultiSpeedHeatingCoil)) {
                    SetupOutputVariable("Unitary System Water Coil Cycling Ratio",
                                        OutputProcessor::Unit::None,
                                        unitarySys[sysNum].m_CycRatio,
                                        "System",
                                        "Average",
                                        unitarySys[sysNum].Name);
                    SetupOutputVariable("Unitary System Water Coil Speed Ratio",
                                        OutputProcessor::Unit::None,
                                        unitarySys[sysNum].m_SpeedRatio,
                                        "System",
                                        "Average",
                                        unitarySys[sysNum].Name);
                    SetupOutputVariable("Unitary System Water Coil Speed Level",
                                        OutputProcessor::Unit::None,
                                        unitarySys[sysNum].m_SpeedNum,
                                        "System",
                                        "Average",
                                        unitarySys[sysNum].Name);
                }

                if (DataGlobals::AnyEnergyManagementSystemInModel) {
                    SetupEMSActuator("UnitarySystem",
                                     unitarySys[sysNum].Name,
                                     "Autosized Supply Air Flow Rate",
                                     "[m3/s]",
                                     unitarySys[sysNum].m_DesignFanVolFlowRateEMSOverrideOn,
                                     unitarySys[sysNum].m_DesignFanVolFlowRateEMSOverrideValue);
                    SetupEMSActuator("UnitarySystem",
                                     unitarySys[sysNum].Name,
                                     "Autosized Supply Air Flow Rate During Cooling Operation",
                                     "[m3/s]",
                                     unitarySys[sysNum].m_MaxCoolAirVolFlowEMSOverrideOn,
                                     unitarySys[sysNum].m_MaxCoolAirVolFlowEMSOverrideValue);
                    SetupEMSActuator("UnitarySystem",
                                     unitarySys[sysNum].Name,
                                     "Autosized Supply Air Flow Rate During Heating Operation",
                                     "[m3/s]",
                                     unitarySys[sysNum].m_MaxHeatAirVolFlowEMSOverrideOn,
                                     unitarySys[sysNum].m_MaxHeatAirVolFlowEMSOverrideValue);
                    SetupEMSActuator("UnitarySystem",
                                     unitarySys[sysNum].Name,
                                     "Autosized Supply Air Flow Rate During No Heating or Cooling Operation",
                                     "[m3/s]",
                                     unitarySys[sysNum].m_MaxNoCoolHeatAirVolFlowEMSOverrideOn,
                                     unitarySys[sysNum].m_MaxNoCoolHeatAirVolFlowEMSOverrideValue);

                    SetupEMSInternalVariable(
                        "Unitary System Control Zone Mass Flow Fraction", unitarySys[sysNum].Name, "[]", unitarySys[sysNum].ControlZoneMassFlowFrac);
                }
                //                    }
                if (DataGlobals::AnyEnergyManagementSystemInModel) {
                    //                        for (UnitarySysNum = 1; UnitarySysNum <= NumUnitarySystem; ++UnitarySysNum) {
                    SetupEMSInternalVariable(
                        "Unitary HVAC Design Heating Capacity", unitarySys[sysNum].Name, "[W]", unitarySys[sysNum].m_DesignHeatingCapacity);
                    SetupEMSInternalVariable(
                        "Unitary HVAC Design Cooling Capacity", unitarySys[sysNum].Name, "[W]", unitarySys[sysNum].m_DesignCoolingCapacity);
                    SetupEMSActuator("Unitary HVAC",
                                     unitarySys[sysNum].Name,
                                     "Sensible Load Request",
                                     "[W]",
                                     unitarySys[sysNum].m_EMSOverrideSensZoneLoadRequest,
                                     unitarySys[sysNum].m_EMSSensibleZoneLoadValue);
                    SetupEMSActuator("Unitary HVAC",
                                     unitarySys[sysNum].Name,
                                     "Moisture Load Request",
                                     "[W]",
                                     unitarySys[sysNum].m_EMSOverrideMoistZoneLoadRequest,
                                     unitarySys[sysNum].m_EMSMoistureZoneLoadValue);
                    //                        }
                }
                // can this be called each time a system is gottem?
                bool anyEMSRan;
                EMSManager::ManageEMS(DataGlobals::emsCallFromComponentGetInput, anyEMSRan);
            }
        }
    }

    void UnitarySys::calcUnitarySuppSystemToSP(bool const FirstHVACIteration // True when first HVAC iteration
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages supplemental heater component simulation for setpoint based operation scheme.

        // Locals
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 QActual;

        std::string CompName = this->m_SuppHeatCoilName;
        int CoilType_Num = this->m_SuppHeatCoilType_Num;

        if ((CoilType_Num == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) || (CoilType_Num == DataHVACGlobals::Coil_HeatingElectric)) {
            HeatingCoils::SimulateHeatingCoilComponents(
                CompName, FirstHVACIteration, _, this->m_SuppHeatCoilIndex, _, _, this->m_FanOpMode, this->m_SuppHeatPartLoadFrac);

        } else if (CoilType_Num == DataHVACGlobals::Coil_HeatingDesuperheater) {
            HeatingCoils::SimulateHeatingCoilComponents(
                CompName, FirstHVACIteration, _, this->m_SuppHeatCoilIndex, _, _, this->m_FanOpMode, this->m_SuppHeatPartLoadFrac);

        } else if (CoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
            WaterCoils::SimulateWaterCoilComponents(
                CompName, FirstHVACIteration, this->m_SuppHeatCoilIndex, QActual, this->m_FanOpMode, this->m_SuppHeatPartLoadFrac);

        } else if (CoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
            SteamCoils::SimulateSteamCoilComponents(CompName,
                                                    FirstHVACIteration,
                                                    this->m_SuppHeatCoilIndex,
                                                    this->m_DesignSuppHeatingCapacity * this->m_SuppHeatPartLoadFrac,
                                                    _,
                                                    this->m_FanOpMode,
                                                    this->m_SuppHeatPartLoadFrac);

        } else {
        }
    }

    void UnitarySys::controlUnitarySystemtoLoad(int const AirLoopNum,          // Primary air loop number
                                                bool const FirstHVACIteration, // True when first HVAC iteration
                                                int &CompOn,                   // Determines if compressor is on or off
                                                Real64 const OAUCoilOutTemp,   // the coil inlet temperature of OutdoorAirUnit
                                                bool HXUnitOn                  // Flag to control HX for HXAssisted Cooling Coil
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        Real64 SuppPLR = 0.0;       // supplemental heating part-load ratio
        Real64 ZoneLoad = 0.0;      // zone load (W)
        Real64 SupHeaterLoad = 0.0; // additional heating required by supplemental heater (W)
        Real64 OnOffAirFlowRatio = 1.0;
        this->updateUnitarySystemControl(AirLoopNum,
                                         this->CoolCoilOutletNodeNum,
                                         this->m_SystemCoolControlNodeNum,
                                         OnOffAirFlowRatio,
                                         FirstHVACIteration,
                                         OAUCoilOutTemp,
                                         ZoneLoad,
                                         this->DesignMaxOutletTemp);

        // will not be running supplemental heater on this CALL (simulate with supplemental heater off)
        Real64 FullSensibleOutput = 0.0;
        // using furnace module logic
        // first check to see if cycling fan with economizer can meet the load
        if (AirLoopNum > 0) {
            if (this->m_CoolCoilExists && this->m_HeatCoilExists &&
                this->m_CoolingCoilType_Num != DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed &&
                this->m_HeatingCoilType_Num != DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed && !FirstHVACIteration &&
                this->m_FanOpMode == DataHVACGlobals::CycFanCycCoil && CoolingLoad && DataAirLoop::AirLoopControlInfo(AirLoopNum).EconoActive) {
                CompOn = 0;
                this->controlUnitarySystemOutput(AirLoopNum, FirstHVACIteration, OnOffAirFlowRatio, ZoneLoad, FullSensibleOutput, HXUnitOn, CompOn);
                if (this->m_CoolingPartLoadFrac >= 1.0 || this->m_HeatingPartLoadFrac >= 1.0 ||
                    (this->m_CoolingPartLoadFrac <= 0.0 && this->m_HeatingPartLoadFrac <= 0.0)) {
                    CompOn = 1;
                    this->controlUnitarySystemOutput(
                        AirLoopNum, FirstHVACIteration, OnOffAirFlowRatio, ZoneLoad, FullSensibleOutput, HXUnitOn, CompOn);
                }
            } else {
                CompOn = 1;
                this->controlUnitarySystemOutput(AirLoopNum, FirstHVACIteration, OnOffAirFlowRatio, ZoneLoad, FullSensibleOutput, HXUnitOn, CompOn);
            }
        } else {
            CompOn = 1;
            this->controlUnitarySystemOutput(AirLoopNum, FirstHVACIteration, OnOffAirFlowRatio, ZoneLoad, FullSensibleOutput, HXUnitOn, CompOn);
        }
        Real64 CoolPLR = this->m_CoolingPartLoadFrac;
        Real64 HeatPLR = this->m_HeatingPartLoadFrac;
        Real64 HeatCoilLoad = HeatPLR * this->m_DesignHeatingCapacity;
        Real64 SensOutput = 0.0;
        Real64 LatOutput = 0.0;

        if (this->CoolCoilFluidInletNode > 0) {
            PlantUtilities::SetComponentFlowRate(DataLoopNode::Node(this->CoolCoilFluidInletNode).MassFlowRate,
                                                 this->CoolCoilFluidInletNode,
                                                 this->CoolCoilFluidOutletNodeNum,
                                                 this->CoolCoilLoopNum,
                                                 this->CoolCoilLoopSide,
                                                 this->CoolCoilBranchNum,
                                                 this->CoolCoilCompNum);
        }
        if (this->HeatCoilFluidInletNode > 0) {
            PlantUtilities::SetComponentFlowRate(DataLoopNode::Node(this->HeatCoilFluidInletNode).MassFlowRate,
                                                 this->HeatCoilFluidInletNode,
                                                 this->HeatCoilFluidOutletNodeNum,
                                                 this->HeatCoilLoopNum,
                                                 this->HeatCoilLoopSide,
                                                 this->HeatCoilBranchNum,
                                                 this->HeatCoilCompNum);
        }

        if (this->m_SuppCoilExists && (HeatingLoad || CoolingLoad || MoistureLoad < 0.0)) {
            if ((FullSensibleOutput < (QToHeatSetPt - DataHVACGlobals::SmallLoad)) && !FirstHVACIteration) {
                SupHeaterLoad = max(0.0, QToHeatSetPt - FullSensibleOutput);
                this->m_SupHeaterLoad = 0.0;
                // what does this line even do? I know we want the supplemental heater on only if there is a dehum load,
                // but for HP's the supp heater should also run if the heating coil can't turn on
                // (i.e., this line calc's a supp heater load, then next line also calc's it?)
                if (MoistureLoad < 0.0) this->m_SupHeaterLoad = SupHeaterLoad;
                // so it look's like this next line should only be valid for HP's.
                if (this->m_DesignSuppHeatingCapacity > 0.0) {
                    this->m_SuppHeatPartLoadFrac = min(1.0, SupHeaterLoad / this->m_DesignSuppHeatingCapacity);
                }
            } else {
                SupHeaterLoad = 0.0;
                this->m_SuppHeatPartLoadFrac = 0.0;
            }
        } else {
            SupHeaterLoad = 0.0;
            this->m_SuppHeatPartLoadFrac = 0.0;
        }

        this->calcUnitarySystemToLoad(AirLoopNum,
                                      FirstHVACIteration,
                                      CoolPLR,
                                      HeatPLR,
                                      OnOffAirFlowRatio,
                                      SensOutput,
                                      LatOutput,
                                      HXUnitOn,
                                      HeatCoilLoad,
                                      SupHeaterLoad,
                                      CompOn);

        // check supplemental heating coil outlet temp based on maximum allowed
        if (this->m_SuppCoilExists) {
            SuppPLR = this->m_SuppHeatPartLoadFrac;
            // only need to test for high supply air temp if supplemental coil is operating
            if (SuppPLR > 0.0) {
                this->calcUnitarySystemToLoad(AirLoopNum,
                                              FirstHVACIteration,
                                              CoolPLR,
                                              HeatPLR,
                                              OnOffAirFlowRatio,
                                              SensOutput,
                                              LatOutput,
                                              HXUnitOn,
                                              HeatCoilLoad,
                                              SupHeaterLoad,
                                              CompOn);
                if (this->m_DesignSuppHeatingCapacity > 0.0) {
                    this->m_SuppHeatPartLoadFrac = SupHeaterLoad / this->m_DesignSuppHeatingCapacity;
                } else {
                    this->m_SuppHeatPartLoadFrac = 0.0;
                }
            }
        }

        if (this->m_SuppCoilFluidInletNode > 0) {
            PlantUtilities::SetComponentFlowRate(DataLoopNode::Node(this->m_SuppCoilFluidInletNode).MassFlowRate,
                                                 this->m_SuppCoilFluidInletNode,
                                                 this->m_SuppCoilFluidOutletNodeNum,
                                                 this->m_SuppCoilLoopNum,
                                                 this->m_SuppCoilLoopSide,
                                                 this->m_SuppCoilBranchNum,
                                                 this->m_SuppCoilCompNum);
        }

        if (this->m_HeatRecActive) {
            PlantUtilities::SetComponentFlowRate(DataLoopNode::Node(this->m_HeatRecoveryInletNodeNum).MassFlowRate,
                                                 this->m_HeatRecoveryInletNodeNum,
                                                 this->m_HeatRecoveryOutletNodeNum,
                                                 this->m_HRLoopNum,
                                                 this->m_HRLoopSideNum,
                                                 this->m_HRBranchNum,
                                                 this->m_HRCompNum);
        }
    }

    void UnitarySys::controlUnitarySystemtoSP(int const AirLoopNum,          // Primary air loop number
                                              bool const FirstHVACIteration, // True when first HVAC iteration
                                              int &CompOn,                   // compressor on/off control
                                              Real64 const OAUCoilOutTemp,   // the coil inlet temperature of OutdoorAirUnit
                                              bool HXUnitOn                  // Flag to control HX for HXAssisted Cooling Coil
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages component simulation.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 PartLoadRatio = 0.0;     // coil operating part-load ratio
        Real64 OnOffAirFlowRatio = 1.0; // Setpoint based coil control does not use this variable
        Real64 CoilCoolHeatRat = 1.0;   // ratio of cooling to heating PLR for cycling fan RH control
        Real64 ZoneLoad = 0.0;
        Real64 HeatCoilLoad = -999.0;

        // CALL the series of components that simulate a Unitary System
        if (this->ATMixerExists) {
            // There is an air terminal mixer
            if (this->ATMixerType == DataHVACGlobals::ATMixer_InletSide) { // if there is an inlet side air terminal mixer
                                                                           // set the primary air inlet mass flow rate
                DataLoopNode::Node(this->m_ATMixerPriNode).MassFlowRate =
                    min(DataLoopNode::Node(this->m_ATMixerPriNode).MassFlowRateMaxAvail, DataLoopNode::Node(this->AirInNode).MassFlowRate);
                // now calculate the the mixer outlet conditions (and the secondary air inlet flow rate)
                // the mixer outlet flow rate has already been set above (it is the "inlet" node flow rate)
                SingleDuct::SimATMixer(this->m_ATMixerName, FirstHVACIteration, this->m_ATMixerIndex);
            }
        }

        if (this->m_FanExists && this->m_FanPlace == FanPlace::BlowThru) {
            if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->m_FanIndex]->simulate(_, _, _, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->m_FanIndex, FanSpeedRatio);
            }
        }

        if (this->m_CoolingCoilUpstream) {

            if (this->m_CoolCoilExists) {
                this->updateUnitarySystemControl(AirLoopNum,
                                                 this->CoolCoilOutletNodeNum,
                                                 this->m_SystemCoolControlNodeNum,
                                                 OnOffAirFlowRatio,
                                                 FirstHVACIteration,
                                                 OAUCoilOutTemp,
                                                 ZoneLoad,
                                                 this->DesignMaxOutletTemp);
                this->controlCoolingSystemToSP(AirLoopNum, FirstHVACIteration, HXUnitOn, CompOn);
                PartLoadRatio = this->m_CoolingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) {
                    CompOn = 1;
                    this->m_LastMode = CoolingMode;
                }
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }
            if (this->m_HeatCoilExists) {
                this->updateUnitarySystemControl(AirLoopNum,
                                                 this->HeatCoilOutletNodeNum,
                                                 this->m_SystemHeatControlNodeNum,
                                                 OnOffAirFlowRatio,
                                                 FirstHVACIteration,
                                                 OAUCoilOutTemp,
                                                 ZoneLoad,
                                                 this->DesignMaxOutletTemp);
                this->controlHeatingSystemToSP(AirLoopNum, FirstHVACIteration, CompOn, HeatCoilLoad);
                PartLoadRatio = this->m_HeatingPartLoadFrac;
                int CompOn = 0;
                if (PartLoadRatio > 0.0) {
                    CompOn = 1;
                    this->m_LastMode = HeatingMode;
                }
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, HeatCoilLoad);
            }

        } else {

            if (this->m_HeatCoilExists) {
                this->updateUnitarySystemControl(AirLoopNum,
                                                 this->HeatCoilOutletNodeNum,
                                                 this->m_SystemHeatControlNodeNum,
                                                 OnOffAirFlowRatio,
                                                 FirstHVACIteration,
                                                 OAUCoilOutTemp,
                                                 ZoneLoad,
                                                 this->DesignMaxOutletTemp);
                this->controlHeatingSystemToSP(AirLoopNum, FirstHVACIteration, CompOn, HeatCoilLoad);
                PartLoadRatio = this->m_HeatingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) {
                    CompOn = 1;
                    this->m_LastMode = HeatingMode;
                }
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, HeatCoilLoad);
            }
            if (this->m_CoolCoilExists) {
                this->updateUnitarySystemControl(AirLoopNum,
                                                 this->CoolCoilOutletNodeNum,
                                                 this->m_SystemCoolControlNodeNum,
                                                 OnOffAirFlowRatio,
                                                 FirstHVACIteration,
                                                 OAUCoilOutTemp,
                                                 ZoneLoad,
                                                 this->DesignMaxOutletTemp);
                this->controlCoolingSystemToSP(AirLoopNum, FirstHVACIteration, HXUnitOn, CompOn);
                PartLoadRatio = this->m_CoolingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) {
                    CompOn = 1;
                    this->m_LastMode = CoolingMode;
                }
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }
        }

        if (this->m_FanExists && this->m_FanPlace == FanPlace::DrawThru) {
            if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->m_FanIndex]->simulate(_, _, _, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->m_FanIndex, FanSpeedRatio);
            }
        }

        if (this->m_SuppCoilExists) {
            SuppHeatingCoilFlag = true;
            this->updateUnitarySystemControl(AirLoopNum,
                                             this->m_SuppCoilAirOutletNode,
                                             this->m_SuppHeatControlNodeNum,
                                             OnOffAirFlowRatio,
                                             FirstHVACIteration,
                                             OAUCoilOutTemp,
                                             ZoneLoad,
                                             this->DesignMaxOutletTemp);
            this->controlSuppHeatSystemToSP(AirLoopNum, FirstHVACIteration);
            this->calcUnitarySuppSystemToSP(FirstHVACIteration);
            SuppHeatingCoilFlag = false;
        }

        // If there is a supply side air terminal mixer, calculate its output
        if (this->ATMixerExists) {
            if (this->ATMixerType == DataHVACGlobals::ATMixer_SupplySide) {
                SingleDuct::SimATMixer(this->m_ATMixerName, FirstHVACIteration, this->m_ATMixerIndex);
            }
        }

        this->m_InitHeatPump = false;
    }

    void UnitarySys::updateUnitarySystemControl(int const AirLoopNum,  // number of the current air loop being simulated
                                                int const OutNode,     // coil outlet node number
                                                int const ControlNode, // control node number
                                                Real64 &OnOffAirFlowRatio,
                                                bool const FirstHVACIteration,
                                                Real64 const OAUCoilOutletTemp, // "ONLY" for zoneHVAC:OutdoorAirUnit
                                                Real64 &ZoneLoad,
                                                Real64 const MaxOutletTemp // limits heating coil outlet temp [C]
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing unitary systems.

        // METHODOLOGY EMPLOYED:
        // Either CALL the coil model to get the size or size coil.
        // Current method is to use same methodology as is used in coil objects.
        // Future changes will include a common sizing algorithm and push the calculated
        // size to the coil object prior to first call (so the coil will not DataSizing::AutoSize).

        // These initializations are done every iteration

        {
            Real64 MoistureLoad = 0.0;
            auto const SELECT_CASE_var(this->m_ControlType);
            if (SELECT_CASE_var == ControlType::Load || SELECT_CASE_var == ControlType::CCMASHRAE) {
                if (AirLoopNum == -1) { // This IF-THEN routine is just for ZoneHVAC:OutdoorAirUnit
                    ShowWarningError(this->UnitType + " \"" + this->Name + "\"");
                    ShowFatalError("...Load based control is not allowed when used with ZoneHVAC:OutdoorAirUnit");
                }

                // here we need to deal with sequenced zone equip
                HeatingLoad = false;
                CoolingLoad = false;
                if (this->m_ZoneSequenceCoolingNum > 0 && this->m_ZoneSequenceHeatingNum > 0) {
                    QToCoolSetPt = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->ControlZoneNum)
                                       .SequencedOutputRequiredToCoolingSP(this->m_ZoneSequenceCoolingNum);
                    QToHeatSetPt = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->ControlZoneNum)
                                       .SequencedOutputRequiredToHeatingSP(this->m_ZoneSequenceHeatingNum);
                    if (QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0 &&
                        DataHeatBalFanSys::TempControlType(this->ControlZoneNum) != DataHVACGlobals::SingleCoolingSetPoint) {
                        ZoneLoad = QToHeatSetPt;
                        HeatingLoad = true;
                    } else if (QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0 &&
                               DataHeatBalFanSys::TempControlType(this->ControlZoneNum) == DataHVACGlobals::SingleCoolingSetPoint) {
                        ZoneLoad = 0.0;
                    } else if (QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0 &&
                               DataHeatBalFanSys::TempControlType(this->ControlZoneNum) != DataHVACGlobals::SingleHeatingSetPoint) {
                        ZoneLoad = QToCoolSetPt;
                        CoolingLoad = true;
                    } else if (QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0 &&
                               DataHeatBalFanSys::TempControlType(this->ControlZoneNum) == DataHVACGlobals::SingleHeatingSetPoint) {
                        ZoneLoad = 0.0;
                    } else if (QToHeatSetPt <= 0.0 && QToCoolSetPt >= 0.0) {
                        ZoneLoad = 0.0;
                    }
                    MoistureLoad = DataZoneEnergyDemands::ZoneSysMoistureDemand(this->ControlZoneNum)
                                       .SequencedOutputRequiredToDehumidSP(this->m_ZoneSequenceCoolingNum);
                } else {
                    ZoneLoad = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->ControlZoneNum).RemainingOutputRequired;
                    QToCoolSetPt = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->ControlZoneNum).OutputRequiredToCoolingSP;
                    QToHeatSetPt = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->ControlZoneNum).OutputRequiredToHeatingSP;
                    MoistureLoad = DataZoneEnergyDemands::ZoneSysMoistureDemand(this->ControlZoneNum).OutputRequiredToDehumidifyingSP;
                }

                if (this->m_DehumidControlType_Num != DehumCtrlType::None) {
                    Real64 H2OHtOfVap = Psychrometrics::PsyHfgAirFnWTdb(DataLoopNode::Node(this->NodeNumOfControlledZone).HumRat,
                                                                        DataLoopNode::Node(this->NodeNumOfControlledZone).Temp);

                    // positive MoistureLoad means no dehumidification load
                    MoistureLoad = min(0.0, MoistureLoad * H2OHtOfVap);
                } else {
                    MoistureLoad = 0.0;
                }

                this->initLoadBasedControl(AirLoopNum, FirstHVACIteration, OnOffAirFlowRatio, ZoneLoad);

                // *** the location of this EMS override looks suspect. If EMS is active the load will be changed but the CoolingLoad and HeatingLoad
                // flags are not updated. suggest this be moved up above InitLoadBasedControl function on previous line so the EMS loads are used in
                // that routine EMS override point
                if (this->m_EMSOverrideSensZoneLoadRequest) ZoneLoad = this->m_EMSSensibleZoneLoadValue;
                if (this->m_EMSOverrideMoistZoneLoadRequest) MoistureLoad = this->m_EMSMoistureZoneLoadValue;

                this->m_SimASHRAEModel = false; // flag used to envoke ASHRAE 90.1 model calculations
                // allows non-ASHSRAE compliant coil types to be modeled using non-ASHAR90 method. Constant fan operating mode is required.
                if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                    if (CoolingLoad) {
                        if (this->m_ValidASHRAECoolCoil) this->m_SimASHRAEModel = true;
                    } else if (HeatingLoad) {
                        if (this->m_ValidASHRAEHeatCoil) this->m_SimASHRAEModel = true;
                    }
                }

            } else if (SELECT_CASE_var == ControlType::Setpoint) {
                if (AirLoopNum == -1) { // This IF-THEN routine is just for ZoneHVAC:OutdoorAIRUNIT

                    if (ControlNode == 0) {
                        this->m_DesiredOutletTemp = OAUCoilOutletTemp;
                        this->m_DesiredOutletHumRat = 1.0;
                    } else if (ControlNode == OutNode) {
                        this->m_DesiredOutletTemp = OAUCoilOutletTemp;
                    }
                    // If the unitary system is an equipment of Outdoor Air Unit, the desired coil outlet humidity level is set to zero
                    this->m_DesiredOutletHumRat = 1.0;

                } else { // Not Outdoor Air Unit or zone equipment
                    if (AirLoopNum > 0) economizerFlag = DataAirLoop::AirLoopControlInfo(AirLoopNum).EconoActive;
                    if (ControlNode == 0) {
                        this->m_DesiredOutletTemp = 0.0;
                        this->m_DesiredOutletHumRat = 1.0;
                    } else if (ControlNode == OutNode) {
                        if (this->m_ISHundredPercentDOASDXCoil && this->m_RunOnSensibleLoad) {
                            this->frostControlSetPointLimit(DataLoopNode::Node(ControlNode).TempSetPoint,
                                                            DataLoopNode::Node(ControlNode).HumRatMax,
                                                            DataEnvironment::OutBaroPress,
                                                            this->DesignMinOutletTemp,
                                                            1);
                        }
                        this->m_DesiredOutletTemp = DataLoopNode::Node(ControlNode).TempSetPoint;
                        //  IF HumRatMax is zero, then there is no request from SetpointManager:SingleZone:Humidity:Maximum
                        if ((this->m_DehumidControlType_Num != DehumCtrlType::None) && (DataLoopNode::Node(ControlNode).HumRatMax > 0.0)) {
                            if (this->m_ISHundredPercentDOASDXCoil && this->m_RunOnLatentLoad) {
                                this->frostControlSetPointLimit(DataLoopNode::Node(ControlNode).TempSetPoint,
                                                                DataLoopNode::Node(ControlNode).HumRatMax,
                                                                DataEnvironment::OutBaroPress,
                                                                this->DesignMinOutletTemp,
                                                                2);
                            }
                            this->m_DesiredOutletHumRat = DataLoopNode::Node(ControlNode).HumRatMax;
                        } else {
                            this->m_DesiredOutletHumRat = 1.0;
                        }
                    } else {
                        if (this->m_ISHundredPercentDOASDXCoil && this->m_RunOnSensibleLoad) {
                            this->frostControlSetPointLimit(DataLoopNode::Node(ControlNode).TempSetPoint,
                                                            DataLoopNode::Node(ControlNode).HumRatMax,
                                                            DataEnvironment::OutBaroPress,
                                                            this->DesignMinOutletTemp,
                                                            1);
                        }
                        this->m_DesiredOutletTemp =
                            DataLoopNode::Node(ControlNode).TempSetPoint - (DataLoopNode::Node(ControlNode).Temp - DataLoopNode::Node(OutNode).Temp);
                        if (this->m_DehumidControlType_Num != DehumCtrlType::None) {
                            if (this->m_ISHundredPercentDOASDXCoil && this->m_RunOnLatentLoad) {
                                this->frostControlSetPointLimit(DataLoopNode::Node(ControlNode).TempSetPoint,
                                                                DataLoopNode::Node(ControlNode).HumRatMax,
                                                                DataEnvironment::OutBaroPress,
                                                                this->DesignMinOutletTemp,
                                                                2);
                            }
                            this->m_DesiredOutletHumRat = DataLoopNode::Node(ControlNode).HumRatMax -
                                                          (DataLoopNode::Node(ControlNode).HumRat - DataLoopNode::Node(OutNode).HumRat);
                        } else {
                            this->m_DesiredOutletHumRat = 1.0;
                        }
                    }
                }
                this->m_DesiredOutletTemp = min(this->m_DesiredOutletTemp, MaxOutletTemp);

            } else {
            }
        }
    }

    void UnitarySys::controlUnitarySystemOutput(int const AirLoopNum,          // Index to air loop
                                                bool const FirstHVACIteration, // True when first HVAC iteration
                                                Real64 &OnOffAirFlowRatio,     // ratio of heating PLR to cooling PLR (is this correct?)
                                                Real64 const ZoneLoad,
                                                Real64 &FullSensibleOutput,
                                                bool HXUnitOn, // Flag to control HX for HXAssisted Cooling Coil
                                                int CompOn)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines operating PLR and calculates the load based system output.

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIter(100); // maximum number of iterations

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::vector<Real64> Par(17);      // parameters passed to RegulaFalsi function
        int SpeedNum;                     // multi-speed coil speed number
        Real64 SensOutputOn;              // sensible output at PLR = 1 [W]
        Real64 LatOutputOn;               // latent output at PLR = 1 [W]
        Real64 TempLoad;                  // represents either a sensible or latent load [W]
        Real64 TempSysOutput;             // represents either a sensible or latent capacity [W]
        Real64 TempSensOutput;            // iterative sensible capacity [W]
        Real64 TempLatOutput;             // iterative latent capacity [W]
        Real64 TempMinPLR;                // iterative minimum PLR
        Real64 TempMaxPLR;                // iterative maximum PLR
        Real64 CoolingOnlySensibleOutput; // use to calculate dehumidification induced heating [W]
        Real64 CpAir;                     // specific heat of air [J/kg_C]
        Real64 FullLoadAirOutletTemp;     // saved full load outlet air temperature [C]
        Real64 FullLoadAirOutletHumRat;   // saved full load outlet air humidity ratio [kg/kg]
        Real64 NoLoadOutletTemp;          // outlet temp of system with coils off [C]

        std::string CompName = this->Name;
        int OutletNode = this->AirOutNode;

        if (ScheduleManager::GetCurrentScheduleValue(this->m_SysAvailSchedPtr) <= 0.0) {
            return;
        }

        Real64 PartLoadRatio = 0.0; // Get no load result
        // fan and coil PLR are disconnected when using ASHRAE model, don't confuse these for other models
        this->FanPartLoadRatio = 0.0;
        int SolFlag = 0;    // # of iterations IF positive, -1 means failed to converge, -2 means bounds are incorrect
        int SolFlagLat = 0; // # of iterations IF positive, -1 means failed to converge, -2 means bounds are incorrect

        Real64 SensOutputOff = 0.0;
        Real64 LatOutputOff = 0.0;
        Real64 CoolPLR = 0.0;
        Real64 HeatPLR = 0.0;
        int CompressorONFlag = 0;
        Real64 HeatCoilLoad = 0.0;
        Real64 SupHeaterLoad = 0.0;

        this->m_WSHPRuntimeFrac = 0.0;

        this->setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);

        if (!HeatingLoad && !CoolingLoad && MoistureLoad >= 0.0) return;

        this->calcUnitarySystemToLoad(AirLoopNum,
                                      FirstHVACIteration,
                                      CoolPLR,
                                      HeatPLR,
                                      OnOffAirFlowRatio,
                                      SensOutputOff,
                                      LatOutputOff,
                                      HXUnitOn,
                                      HeatCoilLoad,
                                      SupHeaterLoad,
                                      CompressorONFlag);
        FullSensibleOutput = SensOutputOff;
        NoLoadOutletTemp = DataLoopNode::Node(OutletNode).Temp;

        if (!HeatingLoad && !CoolingLoad) {
            // no load
            if (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff) return;
            // Dehumcontrol_Multimode only controls RH if there is a sensible load
            if (this->m_DehumidControlType_Num == DehumCtrlType::Multimode) return;
        }

        // determine if PLR=0 meets the load
        {
            auto const SELECT_CASE_var(DataHeatBalFanSys::TempControlType(this->ControlZoneNum));
            if (SELECT_CASE_var == DataHVACGlobals::SingleHeatingSetPoint) {
                if (HeatingLoad && SensOutputOff > ZoneLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
                if (!HeatingLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
            } else if (SELECT_CASE_var == DataHVACGlobals::SingleCoolingSetPoint) {
                if (CoolingLoad && SensOutputOff < ZoneLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
                if (!CoolingLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
            } else if ((SELECT_CASE_var == DataHVACGlobals::SingleHeatCoolSetPoint) ||
                       (SELECT_CASE_var == DataHVACGlobals::DualSetPointWithDeadBand)) {
                if (HeatingLoad && SensOutputOff > ZoneLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
                if (CoolingLoad && SensOutputOff < ZoneLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
                if (!HeatingLoad && !CoolingLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
            } else {
                // should never get here
            }
        }

        // if a variable speed unit, the SensOutputOff at SpeedNum=1 must be checked to see if it exceeds the ZoneLoad
        // This is still no load but at the first speed above idle
        if ((HeatingLoad && this->m_NumOfSpeedHeating > 0) || (CoolingLoad && this->m_NumOfSpeedCooling > 0)) {
            if (this->m_Staged) {
                if (HeatingLoad) {
                    this->m_HeatingSpeedNum = this->m_StageNum;
                } else {
                    this->m_CoolingSpeedNum = std::abs(this->m_StageNum);
                }
            } else {
                if (HeatingLoad) {
                    this->m_HeatingSpeedNum = 1;
                } else {
                    this->m_CoolingSpeedNum = 1;
                }
            }
            this->setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
            this->calcUnitarySystemToLoad(AirLoopNum,
                                          FirstHVACIteration,
                                          CoolPLR,
                                          HeatPLR,
                                          OnOffAirFlowRatio,
                                          SensOutputOff,
                                          LatOutputOff,
                                          HXUnitOn,
                                          HeatCoilLoad,
                                          SupHeaterLoad,
                                          CompressorONFlag);
            FullSensibleOutput = SensOutputOff;

            {
                auto const SELECT_CASE_var(DataHeatBalFanSys::TempControlType(this->ControlZoneNum));
                if (SELECT_CASE_var == DataHVACGlobals::SingleHeatingSetPoint) {
                    if (HeatingLoad && SensOutputOff > ZoneLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
                    if (!HeatingLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
                } else if (SELECT_CASE_var == DataHVACGlobals::SingleCoolingSetPoint) {
                    if (CoolingLoad && SensOutputOff < ZoneLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
                    if (!CoolingLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
                } else if ((SELECT_CASE_var == DataHVACGlobals::SingleHeatCoolSetPoint) ||
                           (SELECT_CASE_var == DataHVACGlobals::DualSetPointWithDeadBand)) {
                    if (HeatingLoad && SensOutputOff > ZoneLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
                    if (CoolingLoad && SensOutputOff < ZoneLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
                    if (!HeatingLoad && !CoolingLoad && (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOff)) return;
                } else {
                    // should never get here
                }
            }
        }

        PartLoadRatio = 1.0; // Get full load result
        this->FanPartLoadRatio = 1.0;
        CompressorONFlag = CompOn;

        if (HeatingLoad) {
            CoolPLR = 0.0;
            HeatPLR = 1.0;
            this->m_HeatingCoilSensDemand = ZoneLoad;
            this->m_WSHPRuntimeFrac = HeatPLR;
            if (this->m_NumOfSpeedHeating > 0) {
                this->m_HeatingSpeedRatio = 1.0;
                this->m_HeatingCycRatio = 1.0;
                this->m_HeatingSpeedNum = this->m_NumOfSpeedHeating;
            }
            if (this->m_Staged && this->m_StageNum > 0) {
                if (this->m_NumOfSpeedHeating > 0) {
                    this->m_HeatingSpeedNum = min(this->m_StageNum, this->m_NumOfSpeedHeating);
                    this->m_HeatingSpeedRatio = 0.0;
                }
                this->setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
                this->calcUnitarySystemToLoad(AirLoopNum,
                                              FirstHVACIteration,
                                              CoolPLR,
                                              HeatPLR,
                                              OnOffAirFlowRatio,
                                              SensOutputOff,
                                              LatOutputOff,
                                              HXUnitOn,
                                              HeatCoilLoad,
                                              SupHeaterLoad,
                                              CompressorONFlag);
                if (SensOutputOff > ZoneLoad) return;
                if (this->m_NumOfSpeedHeating > 0) this->m_HeatingSpeedRatio = 1.0;
            }
        } else if (CoolingLoad || MoistureLoad < LatOutputOff) {
            CoolPLR = 1.0;
            HeatPLR = 0.0;
            if (CoolingLoad) {
                this->m_CoolingCoilSensDemand = std::abs(ZoneLoad);
            } else {
                this->m_CoolingCoilSensDemand = 0.0;
            }
            this->m_CoolingCoilLatentDemand = std::abs(MoistureLoad);
            this->m_WSHPRuntimeFrac = CoolPLR;
            if (this->m_NumOfSpeedCooling > 0) {
                this->m_CoolingSpeedRatio = 1.0;
                this->m_CoolingCycRatio = 1.0;
                this->m_CoolingSpeedNum = this->m_NumOfSpeedCooling;
            }
            if (this->m_Staged && this->m_StageNum < 0) {
                if (this->m_NumOfSpeedCooling > 0) this->m_CoolingSpeedNum = min(std::abs(this->m_StageNum), this->m_NumOfSpeedCooling);
                this->setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);
                this->m_CoolingSpeedRatio = 0.0;
                this->calcUnitarySystemToLoad(AirLoopNum,
                                              FirstHVACIteration,
                                              CoolPLR,
                                              HeatPLR,
                                              OnOffAirFlowRatio,
                                              SensOutputOff,
                                              LatOutputOff,
                                              HXUnitOn,
                                              HeatCoilLoad,
                                              SupHeaterLoad,
                                              CompressorONFlag);
                if (SensOutputOff < ZoneLoad) return;
                if (this->m_NumOfSpeedCooling > 0) this->m_CoolingSpeedRatio = 1.0;
            }
        } else {
            // will return here when no cooling or heating load and MoistureLoad > LatOutputOff (i.e., PLR=0)
            return;
        }

        this->setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadRatio);

        this->calcUnitarySystemToLoad(AirLoopNum,
                                      FirstHVACIteration,
                                      CoolPLR,
                                      HeatPLR,
                                      OnOffAirFlowRatio,
                                      SensOutputOn,
                                      LatOutputOn,
                                      HXUnitOn,
                                      HeatCoilLoad,
                                      SupHeaterLoad,
                                      CompressorONFlag);
        FullSensibleOutput = SensOutputOn;
        FullLoadAirOutletTemp = DataLoopNode::Node(OutletNode).Temp;
        FullLoadAirOutletHumRat = DataLoopNode::Node(OutletNode).HumRat;

        // turn on HX if dehumidm_ControlType::Multimode
        if (this->m_DehumidControlType_Num == DehumCtrlType::Multimode && MoistureLoad < 0.0 && MoistureLoad < LatOutputOn && CoolingLoad) {
            HXUnitOn = true;
            this->calcUnitarySystemToLoad(AirLoopNum,
                                          FirstHVACIteration,
                                          CoolPLR,
                                          HeatPLR,
                                          OnOffAirFlowRatio,
                                          SensOutputOn,
                                          LatOutputOn,
                                          HXUnitOn,
                                          HeatCoilLoad,
                                          SupHeaterLoad,
                                          CompressorONFlag);
            FullSensibleOutput = SensOutputOn;
        }

        // test to see if full capacity is less than load, if so set to PLR=1 and RETURN if no moisture load
        if ((HeatingLoad && this->m_NumOfSpeedHeating <= 1) || (CoolingLoad && this->m_NumOfSpeedCooling <= 1)) {
            {
                auto const SELECT_CASE_var(DataHeatBalFanSys::TempControlType(this->ControlZoneNum));
                if (SELECT_CASE_var == DataHVACGlobals::SingleHeatingSetPoint) {
                    if (HeatingLoad && SensOutputOn < ZoneLoad) {
                        this->m_HeatingPartLoadFrac = 1.0;
                        this->m_WSHPRuntimeFrac = 1.0;
                        if (MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn) return;
                    }
                    if (!HeatingLoad && (MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn)) return;
                } else if (SELECT_CASE_var == DataHVACGlobals::SingleCoolingSetPoint) {
                    if (CoolingLoad && SensOutputOn > ZoneLoad) {
                        this->m_CoolingPartLoadFrac = 1.0;
                        this->m_WSHPRuntimeFrac = 1.0;
                        if (MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn) return;
                    }
                    if (!CoolingLoad && (MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn)) return;
                } else if ((SELECT_CASE_var == DataHVACGlobals::SingleHeatCoolSetPoint) ||
                           (SELECT_CASE_var == DataHVACGlobals::DualSetPointWithDeadBand)) {
                    if (HeatingLoad && SensOutputOn < ZoneLoad) {
                        this->m_HeatingPartLoadFrac = 1.0;
                        this->m_WSHPRuntimeFrac = 1.0;
                        if (MoistureLoad >= 0.0 || MoistureLoad > LatOutputOn) return;
                    }
                    if (CoolingLoad && SensOutputOn > ZoneLoad) {
                        this->m_CoolingPartLoadFrac = 1.0;
                        this->m_WSHPRuntimeFrac = 1.0;
                        return;
                    }
                    if (!HeatingLoad && !CoolingLoad && (MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn)) {
                        return;
                    }
                } else {
                    // no other choices for thermostat control
                }
            }
        }
        // will find speed for multispeed coils here and then RegulaFalsi on PLR at a fixed speed

        // Do the non-variable or non-multispeed coils have a NumOfSpeed = 0 ? We don't need to do this for single speed coils.
        // Check to see which speed to meet the load
        this->m_HeatingSpeedNum = 0;
        this->m_CoolingSpeedNum = 0;
        if (!this->m_Staged) {
            if (HeatingLoad) {
                for (SpeedNum = 1; SpeedNum <= this->m_NumOfSpeedHeating; ++SpeedNum) {
                    CoolPLR = 0.0;
                    HeatPLR = 1.0;
                    if (SpeedNum == 1) {
                        this->m_HeatingSpeedRatio = 0.0;
                    } else {
                        this->m_HeatingSpeedRatio = 1.0;
                    }
                    this->m_HeatingCycRatio = 1.0;
                    this->m_HeatingSpeedNum = SpeedNum;
                    this->calcUnitarySystemToLoad(AirLoopNum,
                                                  FirstHVACIteration,
                                                  CoolPLR,
                                                  HeatPLR,
                                                  OnOffAirFlowRatio,
                                                  SensOutputOn,
                                                  LatOutputOn,
                                                  HXUnitOn,
                                                  HeatCoilLoad,
                                                  SupHeaterLoad,
                                                  CompressorONFlag);
                    if (this->m_HeatingCoilType_Num != DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit &&
                        (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater && !this->m_MultiSpeedHeatingCoil)) {
                        this->m_HeatingSpeedRatio = 0.0;
                        this->m_HeatingSpeedNum = SpeedNum - 1;
                        if (this->m_HeatingSpeedNum == 0) {
                            this->m_HeatingCycRatio = 0.0;
                            HeatPLR = 0.0;
                        } else {
                            this->m_HeatingCycRatio = 1.0;
                            HeatPLR = 1.0;
                        }
                        this->calcUnitarySystemToLoad(AirLoopNum,
                                                      FirstHVACIteration,
                                                      CoolPLR,
                                                      HeatPLR,
                                                      OnOffAirFlowRatio,
                                                      SensOutputOff,
                                                      LatOutputOff,
                                                      HXUnitOn,
                                                      HeatCoilLoad,
                                                      SupHeaterLoad,
                                                      CompressorONFlag);
                        this->m_HeatingSpeedNum = SpeedNum;
                    }
                    if (ZoneLoad <= SensOutputOn) {
                        break;
                    }
                }
            } else { // Cooling or moisture load
                for (SpeedNum = 1; SpeedNum <= this->m_NumOfSpeedCooling; ++SpeedNum) {
                    CoolPLR = 1.0;
                    HeatPLR = 0.0;
                    if (SpeedNum == 1) {
                        this->m_CoolingSpeedRatio = 0.0;
                    } else {
                        this->m_CoolingSpeedRatio = 1.0;
                    }
                    this->m_CoolingCycRatio = 1.0;
                    this->m_CoolingSpeedNum = SpeedNum;
                    this->calcUnitarySystemToLoad(AirLoopNum,
                                                  FirstHVACIteration,
                                                  CoolPLR,
                                                  HeatPLR,
                                                  OnOffAirFlowRatio,
                                                  SensOutputOn,
                                                  LatOutputOn,
                                                  HXUnitOn,
                                                  HeatCoilLoad,
                                                  SupHeaterLoad,
                                                  CompressorONFlag);

                    if ((this->m_CoolingCoilType_Num != DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) &&
                        ((this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                          this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) &&
                         !this->m_MultiSpeedCoolingCoil)) {
                        this->m_CoolingSpeedRatio = 0.0;
                        this->m_CoolingSpeedNum = SpeedNum - 1;
                        if (this->m_CoolingSpeedNum == 0) {
                            this->m_CoolingCycRatio = 0.0;
                            CoolPLR = 0.0;
                        } else {
                            this->m_CoolingCycRatio = 1.0;
                            this->m_CoolingSpeedRatio = 0.0;
                            if (this->m_SingleMode == 1) {
                                CoolPLR = 1.0;
                            }
                        }

                        this->calcUnitarySystemToLoad(AirLoopNum,
                                                      FirstHVACIteration,
                                                      CoolPLR,
                                                      HeatPLR,
                                                      OnOffAirFlowRatio,
                                                      SensOutputOff,
                                                      LatOutputOff,
                                                      HXUnitOn,
                                                      HeatCoilLoad,
                                                      SupHeaterLoad,
                                                      CompressorONFlag);
                        this->m_CoolingSpeedNum = SpeedNum;
                    }
                    if (ZoneLoad >= SensOutputOn) {
                        break;
                    }
                }
            }
        } else { // IF (.NOT. UnitarySystem(UnitarySysNum)%Staged) THEN
            // Staged control
            if (HeatingLoad) {
                CoolPLR = 0.0;
                HeatPLR = 1.0;
                SpeedNum = this->m_StageNum;
                if (SpeedNum == 1) {
                    this->m_HeatingSpeedRatio = 0.0;
                } else {
                    this->m_HeatingSpeedRatio = 1.0;
                    SpeedNum = min(this->m_StageNum, this->m_NumOfSpeedHeating);
                }
                this->m_HeatingCycRatio = 1.0;
                this->m_HeatingSpeedNum = SpeedNum;
                this->calcUnitarySystemToLoad(AirLoopNum,
                                              FirstHVACIteration,
                                              CoolPLR,
                                              HeatPLR,
                                              OnOffAirFlowRatio,
                                              SensOutputOn,
                                              LatOutputOn,
                                              HXUnitOn,
                                              HeatCoilLoad,
                                              SupHeaterLoad,
                                              CompressorONFlag);
                if (this->m_HeatingCoilType_Num != DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) {
                    this->m_HeatingSpeedRatio = 0.0;
                    this->m_HeatingSpeedNum = SpeedNum - 1;
                    if (this->m_HeatingSpeedNum == 0) {
                        this->m_HeatingCycRatio = 0.0;
                        HeatPLR = 0.0;
                    } else {
                        this->m_HeatingCycRatio = 1.0;
                        HeatPLR = 1.0;
                    }
                    this->calcUnitarySystemToLoad(AirLoopNum,
                                                  FirstHVACIteration,
                                                  CoolPLR,
                                                  HeatPLR,
                                                  OnOffAirFlowRatio,
                                                  SensOutputOff,
                                                  LatOutputOff,
                                                  HXUnitOn,
                                                  HeatCoilLoad,
                                                  SupHeaterLoad,
                                                  CompressorONFlag);
                    this->m_HeatingSpeedNum = SpeedNum;
                }
                if (ZoneLoad <= SensOutputOn) {
                    //        EXIT ????????????
                }
            } else { // Cooling or moisture load
                CoolPLR = 1.0;
                HeatPLR = 0.0;
                SpeedNum = std::abs(this->m_StageNum);
                if (SpeedNum == 1) {
                    this->m_CoolingSpeedRatio = 0.0;
                } else {
                    this->m_CoolingSpeedRatio = 1.0;
                    SpeedNum = min(std::abs(this->m_StageNum), this->m_NumOfSpeedCooling);
                }
                this->m_CoolingCycRatio = 1.0;
                this->m_CoolingSpeedNum = SpeedNum;
                this->calcUnitarySystemToLoad(AirLoopNum,
                                              FirstHVACIteration,
                                              CoolPLR,
                                              HeatPLR,
                                              OnOffAirFlowRatio,
                                              SensOutputOn,
                                              LatOutputOn,
                                              HXUnitOn,
                                              HeatCoilLoad,
                                              SupHeaterLoad,
                                              CompressorONFlag);

                if (this->m_CoolingCoilType_Num != DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) {
                    this->m_CoolingSpeedRatio = 0.0;
                    this->m_CoolingSpeedNum = SpeedNum - 1;
                    if (this->m_CoolingSpeedNum == 0) {
                        this->m_CoolingCycRatio = 0.0;
                        CoolPLR = 0.0;
                    } else {
                        this->m_CoolingCycRatio = 1.0;
                        CoolPLR = 1.0;
                    }
                    this->calcUnitarySystemToLoad(AirLoopNum,
                                                  FirstHVACIteration,
                                                  CoolPLR,
                                                  HeatPLR,
                                                  OnOffAirFlowRatio,
                                                  SensOutputOff,
                                                  LatOutputOff,
                                                  HXUnitOn,
                                                  HeatCoilLoad,
                                                  SupHeaterLoad,
                                                  CompressorONFlag);
                    this->m_CoolingSpeedNum = SpeedNum;
                }
                if (ZoneLoad >= SensOutputOn) {
                    //        EXIT ???????????
                }
            }
        } // IF (.NOT. UnitarySystem(UnitarySysNum)%Staged) THEN

        FullSensibleOutput = SensOutputOn;

        if (!HeatingLoad && !CoolingLoad && (MoistureLoad >= 0.0 || MoistureLoad < LatOutputOn)) {
            // if no load, or only a moisture load which can't be met at PLR=1, RETURN
            return;
        }

        // use the ASHRAE 90.1 method of reduced fan speed at low loads
        if (this->m_SimASHRAEModel) {

            // check to make sure unit has the capacity to meet the load
            if ((HeatingLoad && ZoneLoad < SensOutputOn) || (CoolingLoad && ZoneLoad > SensOutputOn)) {
                UnitarySys &SZVAVModel(unitarySys[this->m_UnitarySysNum]);
                SZVAVModel::calcSZVAVModel(SZVAVModel,
                                           this->m_UnitarySysNum,
                                           FirstHVACIteration,
                                           CoolingLoad,
                                           HeatingLoad,
                                           ZoneLoad,
                                           OnOffAirFlowRatio,
                                           HXUnitOn,
                                           AirLoopNum,
                                           PartLoadRatio,
                                           CompressorONFlag);
            }

        } else { // not ASHRAE model

            // must test to see if load is bounded by capacity before calling RegulaFalsi
            if ((HeatingLoad && ZoneLoad < SensOutputOn) || (CoolingLoad && ZoneLoad > SensOutputOn)) {
                if ((HeatingLoad && ZoneLoad > SensOutputOff) || (CoolingLoad && ZoneLoad < SensOutputOff)) {
                    Par[1] = double(this->m_UnitarySysNum);
                    Par[2] = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                    if (FirstHVACIteration) Par[2] = 1.0;
                    Par[3] = double(this->m_FanOpMode);
                    Par[4] = CompressorONFlag; // CompOp
                    Par[5] = ZoneLoad;
                    Par[6] = 0.0; // FLAG, 0.0 if heating load, 1.0 IF cooling or moisture load
                    if (CoolingLoad) Par[6] = 1.0;
                    Par[7] = 1.0;               // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
                    Par[8] = OnOffAirFlowRatio; // Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
                    Par[9] = 0.0;               // HXUnitOn is always false for HX
                    Par[10] = this->m_HeatingPartLoadFrac;
                    Par[11] = double(AirLoopNum);

                    //     Tolerance is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
                    General::SolveRoot(0.001, MaxIter, SolFlag, PartLoadRatio, &this->calcUnitarySystemLoadResidual, 0.0, 1.0, Par);

                    if (SolFlag == -1) {
                        if (HeatingLoad) {
                            // IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                            // This does cause a problem when coil cannot turn on when OAT < min allowed or scheduled off
                            // If max iteration limit is exceeded, how do we know if the heating coil is operating?
                            TempMaxPLR = -0.1;
                            TempSensOutput = SensOutputOff;
                            while ((TempSensOutput - ZoneLoad) < 0.0 && TempMaxPLR < 1.0) {
                                // find upper limit of HeatingPLR
                                TempMaxPLR += 0.1;

                                // SUBROUTINE SetSpeedVariables(UnitarySysNum, SensibleLoad, PartLoadRatio)
                                this->setSpeedVariables(true, TempMaxPLR);
                                this->calcUnitarySystemToLoad(AirLoopNum,
                                                              FirstHVACIteration,
                                                              CoolPLR,
                                                              TempMaxPLR,
                                                              OnOffAirFlowRatio,
                                                              TempSensOutput,
                                                              TempLatOutput,
                                                              HXUnitOn,
                                                              HeatCoilLoad,
                                                              SupHeaterLoad,
                                                              CompressorONFlag);
                            }
                            TempMinPLR = TempMaxPLR;
                            while ((TempSensOutput - ZoneLoad) > 0.0 && TempMinPLR > 0.0) {
                                // pull upper limit of HeatingPLR down to last valid limit (i.e. heat output still exceeds SystemSensibleLoad)
                                TempMaxPLR = TempMinPLR;
                                // find minimum limit of HeatingPLR
                                TempMinPLR -= 0.01;
                                this->setSpeedVariables(true, TempMinPLR);
                                this->calcUnitarySystemToLoad(AirLoopNum,
                                                              FirstHVACIteration,
                                                              CoolPLR,
                                                              TempMinPLR,
                                                              OnOffAirFlowRatio,
                                                              TempSensOutput,
                                                              TempLatOutput,
                                                              HXUnitOn,
                                                              HeatCoilLoad,
                                                              SupHeaterLoad,
                                                              CompressorONFlag);
                            }
                            // Now solve again with tighter PLR limits
                            General::SolveRoot(0.001, MaxIter, SolFlag, HeatPLR, &this->calcUnitarySystemLoadResidual, TempMinPLR, TempMaxPLR, Par);
                            this->calcUnitarySystemToLoad(AirLoopNum,
                                                          FirstHVACIteration,
                                                          CoolPLR,
                                                          HeatPLR,
                                                          OnOffAirFlowRatio,
                                                          TempSensOutput,
                                                          TempLatOutput,
                                                          HXUnitOn,
                                                          HeatCoilLoad,
                                                          SupHeaterLoad,
                                                          CompressorONFlag);
                        } else if (CoolingLoad) {
                            // RegulaFalsi may not find cooling PLR when the latent degradation model is used.
                            // IF iteration limit is exceeded (SolFlag = -1), find tighter boundary of solution and repeat RegulaFalsi
                            TempMaxPLR = -0.1;
                            TempSysOutput = SensOutputOff;
                            TempLoad = ZoneLoad;
                            while ((TempSysOutput - TempLoad) > 0.0 &&
                                   TempMaxPLR < 0.95) { // avoid PLR > 1 by limiting TempMaxPLR to 1 (i.e., TempMaxPLR += 0.1)
                                // find upper limit of HeatingPLR
                                TempMaxPLR += 0.1;
                                if (TempMaxPLR > 0.95 && TempMaxPLR < 1.05) {
                                    TempMaxPLR = 1.0; // enforce a perfect 1.0 at the top end
                                }
                                this->setSpeedVariables(true, TempMaxPLR);
                                this->calcUnitarySystemToLoad(AirLoopNum,
                                                              FirstHVACIteration,
                                                              TempMaxPLR,
                                                              HeatPLR,
                                                              OnOffAirFlowRatio,
                                                              TempSensOutput,
                                                              TempLatOutput,
                                                              HXUnitOn,
                                                              HeatCoilLoad,
                                                              SupHeaterLoad,
                                                              CompressorONFlag);
                                TempSysOutput = TempSensOutput;
                            }
                            TempMinPLR = TempMaxPLR;
                            while ((TempSysOutput - TempLoad) < 0.0 && TempMinPLR > 0.05) {
                                // pull upper limit of HeatingPLR down to last valid limit (i.e. heat output still exceeds SystemSensibleLoad)
                                TempMaxPLR = TempMinPLR;
                                // find minimum limit of HeatingPLR
                                TempMinPLR -= 0.01;
                                this->setSpeedVariables(true, TempMinPLR);
                                this->calcUnitarySystemToLoad(AirLoopNum,
                                                              FirstHVACIteration,
                                                              TempMinPLR,
                                                              HeatPLR,
                                                              OnOffAirFlowRatio,
                                                              TempSensOutput,
                                                              TempLatOutput,
                                                              HXUnitOn,
                                                              HeatCoilLoad,
                                                              SupHeaterLoad,
                                                              CompressorONFlag);
                                TempSysOutput = TempSensOutput;
                            }
                            // Now solve again with tighter PLR limits
                            General::SolveRoot(0.001, MaxIter, SolFlag, CoolPLR, &this->calcUnitarySystemLoadResidual, TempMinPLR, TempMaxPLR, Par);
                            this->calcUnitarySystemToLoad(AirLoopNum,
                                                          FirstHVACIteration,
                                                          CoolPLR,
                                                          HeatPLR,
                                                          OnOffAirFlowRatio,
                                                          TempSensOutput,
                                                          TempLatOutput,
                                                          HXUnitOn,
                                                          HeatCoilLoad,
                                                          SupHeaterLoad,
                                                          CompressorONFlag);
                        } // IF(HeatingLoad)THEN
                        if (SolFlag == -1) {
                            if (std::abs(ZoneLoad - TempSensOutput) > DataHVACGlobals::SmallLoad) {
                                if (this->MaxIterIndex == 0) {
                                    ShowWarningMessage("Coil control failed to converge for " + this->UnitType + ':' + this->Name);
                                    ShowContinueError("  Iteration limit exceeded in calculating system sensible part-load ratio.");
                                    ShowContinueErrorTimeStamp("Sensible load to be met = " + General::TrimSigDigits(ZoneLoad, 2) +
                                                               " (watts), sensible output = " + General::TrimSigDigits(TempSensOutput, 2) +
                                                               " (watts), and the simulation continues.");
                                }
                                ShowRecurringWarningErrorAtEnd(this->UnitType + " \"" + this->Name +
                                                                   "\" - Iteration limit exceeded in calculating sensible part-load ratio error "
                                                                   "continues. Sensible load statistics:",
                                                               this->MaxIterIndex,
                                                               ZoneLoad,
                                                               ZoneLoad);
                            }
                        } else if (SolFlag == -2) {
                            if (this->RegulaFalsiFailedIndex == 0) {
                                ShowWarningMessage("Coil control failed for " + this->UnitType + ':' + this->Name);
                                ShowContinueError("  sensible part-load ratio determined to be outside the range of 0-1.");
                                ShowContinueErrorTimeStamp("Sensible load to be met = " + General::TrimSigDigits(ZoneLoad, 2) +
                                                           " (watts), and the simulation continues.");
                            }
                            ShowRecurringWarningErrorAtEnd(
                                this->UnitType + " \"" + this->Name +
                                    "\" - sensible part-load ratio out of range error continues. Sensible load statistics:",
                                this->RegulaFalsiFailedIndex,
                                ZoneLoad,
                                ZoneLoad);
                        }
                    } else if (SolFlag == -2) {
                        if (this->RegulaFalsiFailedIndex == 0) {
                            ShowWarningMessage("Coil control failed for " + this->UnitType + ':' + this->Name);
                            ShowContinueError("  sensible part-load ratio determined to be outside the range of 0-1.");
                            ShowContinueErrorTimeStamp("Sensible load to be met = " + General::TrimSigDigits(ZoneLoad, 2) +
                                                       " (watts), and the simulation continues.");
                        }
                        ShowRecurringWarningErrorAtEnd(this->UnitType + " \"" + this->Name +
                                                           "\" - sensible part-load ratio out of range error continues. Sensible load statistics:",
                                                       this->RegulaFalsiFailedIndex,
                                                       ZoneLoad,
                                                       ZoneLoad);
                    }    // IF (SolFlag == -1) THEN
                } else { // load is not bounded by capacity. Leave PLR=1 or turn off unit?
                    this->m_CoolingPartLoadFrac = 0.0;
                    this->m_HeatingPartLoadFrac = 0.0;
                    CoolPLR = 0.0;
                    HeatPLR = 0.0;
                    PartLoadRatio = 0.0;
                } // IF((HeatingLoad .AND. ZoneLoad > SensOutputOff) .OR. (CoolingLoad .AND. ZoneLoad < SensOutputOff))THEN
            }     // IF((HeatingLoad .AND. ZoneLoad < SensOutputOn) .OR. (CoolingLoad .AND. ZoneLoad > SensOutputOn))THEN
        }

        if (HeatingLoad && (this->m_MultiSpeedHeatingCoil || this->m_VarSpeedHeatingCoil)) {
            if (this->m_HeatingSpeedNum == 1) {
                this->m_HeatingCycRatio = PartLoadRatio;
                this->m_HeatingSpeedRatio = 0.0;
            } else {
                if (this->m_SingleMode == 0) {
                    this->m_HeatingCycRatio = 1.0;
                    this->m_HeatingSpeedRatio = PartLoadRatio;
                } else {
                    this->m_HeatingCycRatio = PartLoadRatio;
                    this->m_HeatingSpeedRatio = 1.0;
                }
            }
            HeatPLR = PartLoadRatio;
            CoolPLR = 0.0;
            this->m_CoolingCycRatio = 0.0;
            this->m_CoolingSpeedRatio = 0.0;
        } else if (CoolingLoad && (this->m_MultiSpeedCoolingCoil || this->m_VarSpeedCoolingCoil)) {
            if (this->m_CoolingSpeedNum == 1) {
                this->m_CoolingCycRatio = PartLoadRatio;
                this->m_CoolingSpeedRatio = 0.0;
            } else {
                if (this->m_SingleMode == 0) {
                    this->m_CoolingCycRatio = 1.0;
                    this->m_CoolingSpeedRatio = PartLoadRatio;
                } else {
                    this->m_CoolingCycRatio = PartLoadRatio;
                    this->m_CoolingSpeedRatio = 1.0;
                }
            }
            this->m_HeatingCycRatio = 0.0;
            this->m_HeatingSpeedRatio = 0.0;
            HeatPLR = 0.0;
            CoolPLR = PartLoadRatio;
        } else {
            HeatPLR = this->m_HeatingPartLoadFrac;
            CoolPLR = this->m_CoolingPartLoadFrac;
        }

        this->calcUnitarySystemToLoad(AirLoopNum,
                                      FirstHVACIteration,
                                      CoolPLR,
                                      HeatPLR,
                                      OnOffAirFlowRatio,
                                      TempSensOutput,
                                      TempLatOutput,
                                      HXUnitOn,
                                      HeatCoilLoad,
                                      SupHeaterLoad,
                                      CompressorONFlag);

        // FullSensibleOutput is used to set supplemental heater PLR in calling routine
        // OnOffAirFlowRatio is used to average air flow between ON and OFF state
        FullSensibleOutput = TempSensOutput;

        // RETURN if the moisture load is met
        if (MoistureLoad >= 0.0 || MoistureLoad >= TempLatOutput) return;
        // Multimode does not meet the latent load, only the sensible load with or without HX active
        if (!CoolingLoad && this->m_DehumidControlType_Num == DehumCtrlType::Multimode) return;
        //  IF(HeatingLoad .AND. UnitarySystem(UnitarySysNum)%m_DehumidControlType_Num .EQ. dehumidm_ControlType::CoolReheat)RETURN

        if ((this->m_DehumidControlType_Num == DehumCtrlType::CoolReheat || this->m_DehumidControlType_Num == DehumCtrlType::Multimode)) {

            // find maximum latent output IF not already calculated
            if (HeatingLoad) {
                CoolPLR = 1.0;
                this->m_CoolingPartLoadFrac = 1.0;
                this->m_CoolingSpeedNum = this->m_NumOfSpeedCooling;
                this->m_CoolingSpeedRatio = 1.0;
                this->m_CoolingCycRatio = 1.0;
                this->m_WSHPRuntimeFrac = CoolPLR;
                if (this->m_CoolingSpeedNum > 0) {
                    this->m_HeatingPartLoadFrac = 0.0;
                    this->m_HeatingSpeedNum = 0;
                    HeatPLR = 0.0;
                    CoolingLoad = true;
                    HeatingLoad = false;
                    this->m_HeatingCoilSensDemand = 0.0;
                    this->m_CoolingCoilLatentDemand = MoistureLoad;
                    this->calcUnitarySystemToLoad(AirLoopNum,
                                                  FirstHVACIteration,
                                                  0.0,
                                                  0.0,
                                                  OnOffAirFlowRatio,
                                                  TempSensOutput,
                                                  TempLatOutput,
                                                  HXUnitOn,
                                                  HeatCoilLoad,
                                                  SupHeaterLoad,
                                                  CompressorONFlag);
                    this->calcUnitarySystemToLoad(AirLoopNum,
                                                  FirstHVACIteration,
                                                  CoolPLR,
                                                  HeatPLR,
                                                  OnOffAirFlowRatio,
                                                  TempSensOutput,
                                                  LatOutputOn,
                                                  HXUnitOn,
                                                  HeatCoilLoad,
                                                  SupHeaterLoad,
                                                  CompressorONFlag);
                } else {
                    this->m_HeatingCoilSensDemand = 0.0;
                    this->m_CoolingCoilLatentDemand = 0.0;
                    this->calcUnitarySystemToLoad(AirLoopNum,
                                                  FirstHVACIteration,
                                                  0.0,
                                                  0.0,
                                                  OnOffAirFlowRatio,
                                                  TempSensOutput,
                                                  TempLatOutput,
                                                  HXUnitOn,
                                                  HeatCoilLoad,
                                                  SupHeaterLoad,
                                                  CompressorONFlag);
                    this->m_CoolingCoilLatentDemand = MoistureLoad;
                    this->calcUnitarySystemToLoad(AirLoopNum,
                                                  FirstHVACIteration,
                                                  CoolPLR,
                                                  HeatPLR,
                                                  OnOffAirFlowRatio,
                                                  TempSensOutput,
                                                  LatOutputOn,
                                                  HXUnitOn,
                                                  HeatCoilLoad,
                                                  SupHeaterLoad,
                                                  CompressorONFlag);
                }
            }

            if (this->m_DehumidControlType_Num == DehumCtrlType::Multimode && MoistureLoad < LatOutputOn) {
                HXUnitOn = true;
                this->calcUnitarySystemToLoad(AirLoopNum,
                                              FirstHVACIteration,
                                              CoolPLR,
                                              HeatPLR,
                                              OnOffAirFlowRatio,
                                              TempSensOutput,
                                              LatOutputOn,
                                              HXUnitOn,
                                              HeatCoilLoad,
                                              SupHeaterLoad,
                                              CompressorONFlag);
                FullSensibleOutput = TempSensOutput;
            }

            //    IF ((HeatingLoad .AND. MoistureLoad < TempLatOutput) .OR. &
            //        (CoolingLoad .AND. MoistureLoad < TempLatOutput .AND. MoistureLoad > LatOutputOn) .OR. &
            //        ((.NOT. HeatingLoad) .AND. (.NOT. CoolingLoad) .AND. MoistureLoad > LatOutputOn)) THEN
            if ((MoistureLoad < TempLatOutput) && (MoistureLoad > LatOutputOn)) { // bounds check for RegulaFalsi

                // save heating PLR
                HeatPLR = this->m_HeatingPartLoadFrac;
                Par[1] = double(this->m_UnitarySysNum);
                Par[2] = 0.0; // FLAG, if 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                if (FirstHVACIteration) Par[2] = 1.0;
                Par[3] = double(this->m_FanOpMode);
                Par[4] = CompressorONFlag; // CompOp
                if (this->m_DehumidControlType_Num == DehumCtrlType::Multimode) {
                    Par[5] = ZoneLoad;
                    Par[7] = 1.0; // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
                } else {
                    Par[5] = MoistureLoad;
                    Par[7] = 0.0; // FLAG, 0.0 if latent load, 1.0 if sensible load to be met
                }
                Par[6] = 1.0; // FLAG, 0.0 if heating load, 1.0 if cooling or moisture load
                //      IF(HeatingLoad)Par(6)  = 0.0d0
                Par[8] = OnOffAirFlowRatio; // Ratio of compressor ON mass flow rate to AVERAGE mass flow rate over time step
                if (HXUnitOn) {
                    Par[9] = 1.0;
                } else {
                    Par[9] = 0.0;
                }
                Par[10] = this->m_HeatingPartLoadFrac;
                Par[11] = double(AirLoopNum);
                // Tolerance is fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
                General::SolveRoot(0.001, MaxIter, SolFlagLat, PartLoadRatio, this->calcUnitarySystemLoadResidual, 0.0, 1.0, Par);
                //      IF (HeatingLoad) THEN
                //        UnitarySystem(UnitarySysNum)%HeatingPartLoadFrac = PartLoadRatio
                //      ELSE
                this->m_CoolingPartLoadFrac = PartLoadRatio;
                //      END IF
                this->m_HeatingPartLoadFrac = HeatPLR;
            } else if (MoistureLoad < LatOutputOn && CoolingLoad) {
                //     Logic below needs further look...what to do if the bounds check for RegulaFalsi fail?
                //     I'm not even sure if this should be done.
                //     It's wrong anyway, since there won't be a cooling load if multimode (see RETURN about 80 lines up).
                if (this->m_DehumidControlType_Num != DehumCtrlType::Multimode) {
                    this->m_CoolingPartLoadFrac = 1.0;
                }
            }
        }

        CoolPLR = this->m_CoolingPartLoadFrac;
        HeatPLR = this->m_HeatingPartLoadFrac;

        this->calcUnitarySystemToLoad(AirLoopNum,
                                      FirstHVACIteration,
                                      CoolPLR,
                                      HeatPLR,
                                      OnOffAirFlowRatio,
                                      TempSensOutput,
                                      TempLatOutput,
                                      HXUnitOn,
                                      HeatCoilLoad,
                                      SupHeaterLoad,
                                      CompressorONFlag);

        if (SolFlagLat == -1) {
            // RegulaFalsi may not find cooling PLR when the latent degradation model is used.
            // IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
            TempMaxPLR = -0.1;
            TempLatOutput = LatOutputOff;
            while ((TempLatOutput - MoistureLoad) > 0.0 && TempMaxPLR < 1.0) {
                // find upper limit of HeatingPLR
                TempMaxPLR += 0.1;
                this->calcUnitarySystemToLoad(AirLoopNum,
                                              FirstHVACIteration,
                                              TempMaxPLR,
                                              HeatPLR,
                                              OnOffAirFlowRatio,
                                              TempSensOutput,
                                              TempLatOutput,
                                              HXUnitOn,
                                              HeatCoilLoad,
                                              SupHeaterLoad,
                                              CompressorONFlag);
            }
            TempMinPLR = TempMaxPLR;
            while ((TempLatOutput - MoistureLoad) < 0.0 && TempMinPLR > 0.0) {
                // pull upper limit of HeatingPLR DOwn to last valid limit (i.e. heat output still exceeds SystemSensibleLoad)
                TempMaxPLR = TempMinPLR;
                // find minimum limit of HeatingPLR
                TempMinPLR -= 0.01;
                this->calcUnitarySystemToLoad(AirLoopNum,
                                              FirstHVACIteration,
                                              TempMinPLR,
                                              HeatPLR,
                                              OnOffAirFlowRatio,
                                              TempSensOutput,
                                              TempLatOutput,
                                              HXUnitOn,
                                              HeatCoilLoad,
                                              SupHeaterLoad,
                                              CompressorONFlag);
            }
            // Now solve again with tighter PLR limits
            General::SolveRoot(0.001, MaxIter, SolFlagLat, CoolPLR, &this->calcUnitarySystemLoadResidual, TempMinPLR, TempMaxPLR, Par);
            this->calcUnitarySystemToLoad(AirLoopNum,
                                          FirstHVACIteration,
                                          CoolPLR,
                                          HeatPLR,
                                          OnOffAirFlowRatio,
                                          TempSensOutput,
                                          TempLatOutput,
                                          HXUnitOn,
                                          HeatCoilLoad,
                                          SupHeaterLoad,
                                          CompressorONFlag);
            if (SolFlagLat == -1) {
                if (std::abs(MoistureLoad - TempLatOutput) > DataHVACGlobals::SmallLoad) {
                    if (this->warnIndex.m_LatMaxIterIndex == 0) {
                        ShowWarningMessage("Coil control failed to converge for " + this->UnitType + ':' + this->Name);
                        ShowContinueError("  Iteration limit exceeded in calculating system Latent part-load ratio.");
                        ShowContinueErrorTimeStamp("Latent load to be met = " + General::TrimSigDigits(MoistureLoad, 2) +
                                                   " (watts), Latent output = " + General::TrimSigDigits(TempLatOutput, 2) +
                                                   " (watts), and the simulation continues.");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        this->UnitType + " \"" + this->Name +
                            "\" - Iteration limit exceeded in calculating Latent part-load ratio error continues. Latent load statistics:",
                        this->warnIndex.m_LatMaxIterIndex,
                        MoistureLoad,
                        MoistureLoad);
                }
            } else if (SolFlagLat == -2) {
                if (this->warnIndex.m_LatRegulaFalsiFailedIndex == 0) {
                    ShowWarningMessage("Coil control failed for " + this->UnitType + ':' + this->Name);
                    ShowContinueError("  Latent part-load ratio determined to be outside the range of 0-1.");
                    ShowContinueErrorTimeStamp("Latent load to be met = " + General::TrimSigDigits(MoistureLoad, 2) +
                                               " (watts), and the simulation continues.");
                }
                ShowRecurringWarningErrorAtEnd(this->UnitType + " \"" + this->Name +
                                                   "\" - Latent part-load ratio out of range error continues. Latent load statistics:",
                                               this->warnIndex.m_LatRegulaFalsiFailedIndex,
                                               MoistureLoad,
                                               MoistureLoad);
            }
        } else if (SolFlagLat == -2) {
            if (this->warnIndex.m_LatRegulaFalsiFailedIndex == 0) {
                ShowWarningMessage("Coil control failed for " + this->UnitType + ':' + this->Name);
                ShowContinueError("  Latent part-load ratio determined to be outside the range of 0-1.");
                ShowContinueErrorTimeStamp("Latent load to be met = " + General::TrimSigDigits(MoistureLoad, 2) +
                                           " (watts), and the simulation continues.");
            }
            ShowRecurringWarningErrorAtEnd(this->UnitType + " \"" + this->Name +
                                               "\" - Latent part-load ratio out of range error continues. Latent load statistics:",
                                           this->warnIndex.m_LatRegulaFalsiFailedIndex,
                                           MoistureLoad,
                                           MoistureLoad);
        }

        FullSensibleOutput = TempSensOutput;

        CpAir = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->CoolCoilInletNodeNum).HumRat,
                                               DataLoopNode::Node(this->CoolCoilInletNodeNum).Temp);
        CoolingOnlySensibleOutput = DataLoopNode::Node(this->CoolCoilInletNodeNum).MassFlowRate * CpAir *
                                    ((DataLoopNode::Node(this->NodeNumOfControlledZone).Temp - DataLoopNode::Node(this->CoolCoilOutletNodeNum).Temp) -
                                     (DataLoopNode::Node(this->HeatCoilOutletNodeNum).Temp - DataLoopNode::Node(this->HeatCoilInletNodeNum).Temp));
        if (QToHeatSetPt < 0.0) {
            //   Calculate the reheat coil load wrt the heating setpoint temperature. Reheat coil picks up
            //   the entire excess sensible cooling (DX cooling coil and impact of outdoor air).
            this->m_DehumidInducedHeatingDemandRate = max(0.0, (CoolingOnlySensibleOutput + QToHeatSetPt));
            //   Heating mode and dehumidification is required
        } else if (QToHeatSetPt >= 0.0) {
            //   Calculate the reheat coil load as the sensible capacity of the DX cooling coil only. Let
            //   the heating coil pick up the load due to outdoor air.
            this->m_DehumidInducedHeatingDemandRate = max(0.0, CoolingOnlySensibleOutput);
        }
    }

    void UnitarySys::initLoadBasedControl(int const AirLoopNum, // number of the current air loop being simulated
                                          bool const FirstHVACIteration,
                                          Real64 &OnOffAirFlowRatio,
                                          Real64 &ZoneLoad)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the load controlled Unitary Systems.

        // METHODOLOGY EMPLOYED:
        // Initialize mass flow rates and speed ratios. Calculate loads and adjust if necessary when using constant fan.

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const Small5WLoad(5.0);
        static std::string const routineName("InitUnitarySystems");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // static Array1D_bool MyEnvrnFlag; // environment flag
        // static Array1D_bool MyFanFlag;   // used for sizing fan inputs one time
        // static Array1D_bool MyCheckFlag; // Used to obtain the zone inlet node number
        // in the controlled zone
        // static Array1D_bool MyStagedFlag; // used for finding on m_Staged thermostat
        //////////// hoisted into namespace ////////////////////////////////////////////////
        // static bool MyOneTimeFlag( true ); // one time allocation flag // InitLoadBasedControlOneTimeFlag
        // static bool MyAirLoopPass( true ); // one time allocation flag // InitLoadBasedControlAirLoopPass
        // static int AirLoopPass( 0 ); // Number of air loop pass // AirLoopPassCounter
        // static bool FlowFracFlagReady( true ); // one time flag for calculating flow fraction // InitLoadBasedControlFlowFracFlagReady
        // static Real64 CntrlZoneTerminalUnitMassFlowRateMax( 0.0 ); // Maximum mass flow rate through controlled zone //
        // InitLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax
        ////////////////////////////////////////////////////////////////////////////////////
        // inlet node and system outlet node
        Real64 MaxTemp = 0.0; // Maximum temperature used in latent loss calculation
        Real64 QZnReq = 0.0;
        Real64 QActual = 0.0;
        Real64 SensOutputOff = 0.0;
        Real64 LatOutputOff = 0.0;
        Real64 HeatCoilLoad = 0.0;
        Real64 SupHeaterLoad = 0.0;
        int CompOn = 0;

        // if (initLoadBasedControlOneTimeFlag) {

        //    // initialize the environment and sizing flags
        //    MyEnvrnFlag.allocate(NumUnitarySystem);
        //    MyFanFlag.allocate(NumUnitarySystem);
        //    MyCheckFlag.allocate(NumUnitarySystem);
        //    MyStagedFlag.allocate(NumUnitarySystem);

        //    MyEnvrnFlag = true;
        //    MyFanFlag = true;
        //    MyCheckFlag = true;
        //    InitLoadBasedControlOneTimeFlag = false;
        //    MyStagedFlag = true;
        //}

        // error flag for mining functions
        bool errorsFound = false;

        // do the Begin Environment initializations
        if (DataGlobals::BeginEnvrnFlag && this->m_MyEnvrnFlag2) {

            // set fluid-side hardware limits
            if (this->HeatCoilFluidInletNode > 0) {

                if (this->MaxHeatCoilFluidFlow == DataSizing::AutoSize) {
                    // IF water coil max water flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                    if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                        WaterCoils::SimulateWaterCoilComponents(this->m_HeatingCoilName, FirstHVACIteration, this->m_HeatingCoilIndex);
                        Real64 CoilMaxVolFlowRate = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->m_HeatingCoilName, errorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HeatCoilLoopNum).FluidName,
                                                                           DataGlobals::CWInitConvTemp,
                                                                           DataPlant::PlantLoop(this->HeatCoilLoopNum).FluidIndex,
                                                                           routineName);
                            this->MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                    }
                    // IF steam coil max steam flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                    if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                        SteamCoils::SimulateSteamCoilComponents(this->m_HeatingCoilName,
                                                                FirstHVACIteration,
                                                                this->m_HeatingCoilIndex,
                                                                1.0,
                                                                QActual); // QCoilReq, simulate any load > 0 to get max capacity
                        Real64 CoilMaxVolFlowRate = SteamCoils::GetCoilMaxSteamFlowRate(this->m_HeatingCoilIndex, errorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            Real64 TempSteamIn = 100.0;
                            Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                            this->MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                    }
                }

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->MaxHeatCoilFluidFlow,
                                                   this->HeatCoilFluidInletNode,
                                                   this->HeatCoilFluidOutletNodeNum,
                                                   this->HeatCoilLoopNum,
                                                   this->HeatCoilLoopSide,
                                                   this->HeatCoilBranchNum,
                                                   this->HeatCoilCompNum);
            }
            if (this->m_SuppCoilFluidInletNode > 0) {
                if (this->m_MaxSuppCoilFluidFlow == DataSizing::AutoSize) {
                    if (this->m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                        // IF water coil max water flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                        WaterCoils::SimulateWaterCoilComponents(this->m_SuppHeatCoilName, FirstHVACIteration, this->m_SuppHeatCoilIndex);
                        Real64 CoilMaxVolFlowRate = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->m_SuppHeatCoilName, errorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->m_SuppCoilLoopNum).FluidName,
                                                                           DataGlobals::CWInitConvTemp,
                                                                           DataPlant::PlantLoop(this->m_SuppCoilLoopNum).FluidIndex,
                                                                           routineName);
                            this->m_MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                    }
                    if (this->m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                        SteamCoils::SimulateSteamCoilComponents(this->m_SuppHeatCoilName,
                                                                FirstHVACIteration,
                                                                this->m_SuppHeatCoilIndex,
                                                                1.0,
                                                                QActual); // QCoilReq, simulate any load > 0 to get max capacity
                        Real64 CoilMaxVolFlowRate = SteamCoils::GetCoilMaxSteamFlowRate(this->m_SuppHeatCoilIndex, errorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            Real64 TempSteamIn = 100.0;
                            Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                            this->m_MaxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                    }
                    PlantUtilities::InitComponentNodes(0.0,
                                                       this->m_MaxSuppCoilFluidFlow,
                                                       this->m_SuppCoilFluidInletNode,
                                                       this->m_SuppCoilFluidOutletNodeNum,
                                                       this->m_SuppCoilLoopNum,
                                                       this->m_SuppCoilLoopSide,
                                                       this->m_SuppCoilBranchNum,
                                                       this->m_SuppCoilCompNum);
                }
            }
            this->m_MyEnvrnFlag2 = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            this->m_MyEnvrnFlag2 = true;
        }

        if (allocated(DataZoneEquipment::ZoneEquipConfig) && this->m_MyCheckFlag) {
            if (this->m_AirLoopEquipment) {
                int zoneNum = DataHeatBalance::Zone(this->ControlZoneNum).ZoneEqNum;
                int zoneInlet = this->m_ZoneInletNode;
                if (zoneInlet == 0) {
                    this->m_ThisSysInputShouldBeGotten = true; // need to find zone inlet node once data is available
                    this->m_MySizingCheckFlag = true;          // need to resize after getInput is read in again
                    this->m_OKToPrintSizing = true;            // hope first time back through finds the data, else multiple prints to the eio
                    this->m_airLoopReturnCounter += 1;
                    if (this->m_airLoopReturnCounter < 3) return;
                }
                int coolingPriority = 0;
                int heatingPriority = 0;
                // setup zone equipment sequence information based on finding matching air terminal
                if (DataZoneEquipment::ZoneEquipConfig(zoneNum).EquipListIndex > 0) {
                    DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(zoneNum).EquipListIndex)
                        .getPrioritiesforInletNode(zoneInlet, coolingPriority, heatingPriority);
                    this->m_ZoneSequenceCoolingNum = coolingPriority;
                    this->m_ZoneSequenceHeatingNum = heatingPriority;
                }
                this->m_MyCheckFlag = false;
                if (this->m_ZoneSequenceCoolingNum == 0) {
                    ShowSevereError(this->UnitType + " \"" + this->Name +
                                    "\": No matching air terminal found in the zone equipment list for zone = " +
                                    DataHeatBalance::Zone(this->ControlZoneNum).Name + ".");
                    ShowFatalError("Subroutine InitLoadBasedControl: Errors found in getting " + this->UnitType +
                                   " input.  Preceding condition(s) causes termination.");
                }
            }
            if (this->m_ZoneInletNode == 0) {
                ShowSevereError(this->UnitType + " \"" + this->Name + "\": The zone inlet node in the controlled zone (" +
                                DataHeatBalance::Zone(this->ControlZoneNum).Name + ") is not found.");
                ShowFatalError("Subroutine InitLoadBasedControl: Errors found in getting " + this->UnitType +
                               " input.  Preceding condition(s) causes termination.");
            }
        }

        // What type of logic is this? Is the point to go through the main IF once? or every other time?
        // RR: This was used with AirflowNetwork to calculate duct losses.
        // RR: AFN counts the number of passes through airloop equipment (same logic in Furnaces and other modules) and resets the counter to 0 on
        // BeginEnvrnFlag. RR: This has been changed in this module and AFN to use AirflowNetworkFanActivated if AirflowNetworkUnitarySystem is seen
        // by AFN. RR: Search for AirflowNetworkFanActivated in this module to see usage. The following lines of code can probably be removed although
        // it would require a AFN input file to test.
        if (DataGlobals::BeginEnvrnFlag && m_initLoadBasedControlAirLoopPass) {
            m_airLoopPassCounter = 0;
            m_initLoadBasedControlAirLoopPass = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) {
            m_initLoadBasedControlAirLoopPass = true;
        }

        ++m_airLoopPassCounter;
        if (m_airLoopPassCounter > 2) m_airLoopPassCounter = 1;

        // reset duct losses from previous iteration
        if (FirstHVACIteration) {
            this->m_SenLoadLoss = 0.0;
            this->m_LatLoadLoss = 0.0;
        }

        // Calcuate air distribution losses
        //  IF (.NOT. FirstHVACIteration .AND. AirLoopPass .EQ. 1 .AND. AirflowNetworkFanActivated) THEN
        if (!FirstHVACIteration && DataAirflowNetwork::AirflowNetworkFanActivated) {
            Real64 DeltaMassRate = 0.0;
            int ZoneInNode = this->m_ZoneInletNode;
            Real64 MinHumRat = DataLoopNode::Node(ZoneInNode).HumRat;
            Real64 MassFlowRate = DataLoopNode::Node(ZoneInNode).MassFlowRate / this->ControlZoneMassFlowFrac;
            if (DataLoopNode::Node(this->AirOutNode).Temp < DataLoopNode::Node(this->NodeNumOfControlledZone).Temp)
                MinHumRat = DataLoopNode::Node(this->AirOutNode).HumRat;
            if (DataAirflowNetwork::SimulateAirflowNetwork > DataAirflowNetwork::AirflowNetworkControlMultizone) {
                DeltaMassRate =
                    DataLoopNode::Node(this->AirOutNode).MassFlowRate - DataLoopNode::Node(ZoneInNode).MassFlowRate / this->ControlZoneMassFlowFrac;
                if (DeltaMassRate < 0.0) DeltaMassRate = 0.0;
            } else {
                MassFlowRate = DataLoopNode::Node(this->AirOutNode).MassFlowRate;
                DeltaMassRate = 0.0;
            }
            this->m_SenLoadLoss = MassFlowRate * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(this->AirOutNode).Temp, MinHumRat) -
                                                  Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(ZoneInNode).Temp, MinHumRat)) +
                                  DeltaMassRate * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(this->AirOutNode).Temp, MinHumRat) -
                                                   Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(this->NodeNumOfControlledZone).Temp, MinHumRat));
            if (std::abs(this->m_SensibleLoadMet) > 0.0) {
                if (std::abs(this->m_SenLoadLoss / this->m_SensibleLoadMet) < 0.001) this->m_SenLoadLoss = 0.0;
            }
            if (this->m_Humidistat) {
                MaxTemp = DataLoopNode::Node(this->NodeNumOfControlledZone).Temp;
                this->m_LatLoadLoss = MassFlowRate * (Psychrometrics::PsyHFnTdbW(MaxTemp, DataLoopNode::Node(this->AirOutNode).HumRat) -
                                                      Psychrometrics::PsyHFnTdbW(MaxTemp, DataLoopNode::Node(ZoneInNode).HumRat)) +
                                      DeltaMassRate * (Psychrometrics::PsyHFnTdbW(MaxTemp, DataLoopNode::Node(this->AirOutNode).HumRat) -
                                                       Psychrometrics::PsyHFnTdbW(MaxTemp, DataLoopNode::Node(this->NodeNumOfControlledZone).HumRat));
                if (std::abs(this->m_LatentLoadMet) > 0.0) {
                    if (std::abs(this->m_LatLoadLoss / this->m_LatentLoadMet) < 0.001) this->m_LatLoadLoss = 0.0;
                }
            }
        }

        if (this->m_FanOpModeSchedPtr > 0) {
            if (ScheduleManager::GetCurrentScheduleValue(this->m_FanOpModeSchedPtr) == 0.0) {
                this->m_FanOpMode = DataHVACGlobals::CycFanCycCoil;
            } else {
                this->m_FanOpMode = DataHVACGlobals::ContFanCycCoil;
                DataHVACGlobals::OnOffFanPartLoadFraction = 1.0;
            }
        }

        //  OpMode = UnitarySystem(UnitarySysNum)%FanOpMode
        if (allocated(DataAirLoop::AirLoopControlInfo) && this->m_AirLoopEquipment) {
            economizerFlag = DataAirLoop::AirLoopControlInfo(AirLoopNum).EconoActive;
        } else {
            economizerFlag = false;
        }

        // System load calculation for cycling fan systems
        if (this->ControlZoneMassFlowFrac > 0.0) {
            QZnReq = ZoneLoad / this->ControlZoneMassFlowFrac;
            MoistureLoad /= this->ControlZoneMassFlowFrac;
            QToCoolSetPt /= this->ControlZoneMassFlowFrac;
            QToHeatSetPt /= this->ControlZoneMassFlowFrac;
            ZoneLoad = QZnReq;
        } else {
            QZnReq = ZoneLoad;
            this->ControlZoneMassFlowFrac = 1.0;
        }

        CoolingLoad = false;
        HeatingLoad = false;

        if (QZnReq > Small5WLoad / this->ControlZoneMassFlowFrac && !DataZoneEnergyDemands::CurDeadBandOrSetback(this->ControlZoneNum)) {
            if (DataHeatBalFanSys::TempControlType(this->ControlZoneNum) != DataHVACGlobals::SingleCoolingSetPoint) {
                HeatingLoad = true;
            }
        } else if (QZnReq < -Small5WLoad / this->ControlZoneMassFlowFrac && !DataZoneEnergyDemands::CurDeadBandOrSetback(this->ControlZoneNum)) {
            if (DataHeatBalFanSys::TempControlType(this->ControlZoneNum) != DataHVACGlobals::SingleHeatingSetPoint) {
                CoolingLoad = true;
            }
        }

        // System load calculation for constant fan systems
        if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
            bool HXUnitOn = false;
            this->FanPartLoadRatio = 0.0; // sets fan to minimum for ASHRAE model
            this->setOnOffMassFlowRate(OnOffAirFlowRatio,
                                       0.0); // CompOnMassFlow and CompOffMassFlow are scalar, reset to this system's values
            this->calcUnitarySystemToLoad(AirLoopNum,
                                          FirstHVACIteration,
                                          0.0,
                                          0.0,
                                          OnOffAirFlowRatio,
                                          SensOutputOff,
                                          LatOutputOff,
                                          HXUnitOn,
                                          HeatCoilLoad,
                                          SupHeaterLoad,
                                          CompOn);
            {
                auto const SELECT_CASE_var(DataHeatBalFanSys::TempControlType(this->ControlZoneNum));
                if (SELECT_CASE_var == DataHVACGlobals::SingleHeatingSetPoint) {
                    CoolingLoad = false;
                    // No heating load and constant fan pushes zone below heating set point
                    if (SensOutputOff < 0.0 && QToHeatSetPt < 0.0 && SensOutputOff - QToHeatSetPt < -DataHVACGlobals::SmallLoad) {
                        HeatingLoad = true;
                        CoolingLoad = false;
                        ZoneLoad = QToHeatSetPt;
                    }
                } else if (SELECT_CASE_var == DataHVACGlobals::SingleCoolingSetPoint) {
                    HeatingLoad = false;
                    // No heating load and constant fan pushes zone above cooling set point
                    if (SensOutputOff > 0.0 && QToCoolSetPt > 0.0 && SensOutputOff - QToCoolSetPt > DataHVACGlobals::SmallLoad) {
                        HeatingLoad = false;
                        CoolingLoad = true;
                        ZoneLoad = QToCoolSetPt;
                    }
                } else if (SELECT_CASE_var == DataHVACGlobals::SingleHeatCoolSetPoint) {
                    // zone temp above cooling and heating set point temps
                    if (QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0) {
                        // zone pushed below heating set point
                        if (SensOutputOff < 0.0 && QToHeatSetPt - SensOutputOff > DataHVACGlobals::SmallLoad) {
                            HeatingLoad = true;
                            CoolingLoad = false;
                            ZoneLoad = QToHeatSetPt;
                        }
                        // zone temp below heating set point temp
                    } else if (QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0) {
                        // zone pushed above cooling set point
                        if (SensOutputOff > 0.0 && QToCoolSetPt - SensOutputOff > DataHVACGlobals::SmallLoad) {
                            HeatingLoad = false;
                            CoolingLoad = true;
                            ZoneLoad = QToCoolSetPt;
                        }
                    }
                } else if (SELECT_CASE_var == DataHVACGlobals::DualSetPointWithDeadBand) {
                    // zone temp above cooling and heating set point temps
                    if (QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0) {
                        // zone pushed into deadband
                        if (SensOutputOff < 0.0 && QToCoolSetPt - SensOutputOff > DataHVACGlobals::SmallLoad) {
                            HeatingLoad = false;
                            CoolingLoad = false;
                            ZoneLoad = 0.0;
                        }
                        // zone pushed below heating set point
                        if (SensOutputOff < 0.0 && QToHeatSetPt - SensOutputOff > DataHVACGlobals::SmallLoad) {
                            HeatingLoad = true;
                            CoolingLoad = false;
                            ZoneLoad = QToHeatSetPt;
                        }
                        // zone temp below heating set point temp
                    } else if (QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0) {
                        // zone pushed into deadband
                        if (SensOutputOff > 0.0 && SensOutputOff - QToHeatSetPt > DataHVACGlobals::SmallLoad) {
                            HeatingLoad = false;
                            CoolingLoad = false;
                            ZoneLoad = 0.0;
                        }
                        // zone pushed above cooling set point
                        if (SensOutputOff > 0.0 && SensOutputOff - QToCoolSetPt > DataHVACGlobals::SmallLoad) {
                            HeatingLoad = false;
                            CoolingLoad = true;
                            ZoneLoad = QToCoolSetPt;
                        }
                        // zone temp between set point temps
                    } else if (QToHeatSetPt < 0.0 && QToCoolSetPt > 0.0) {
                        // zone pushed below heating set point
                        if (SensOutputOff < 0.0 && SensOutputOff - QToHeatSetPt < -DataHVACGlobals::SmallLoad) {
                            HeatingLoad = true;
                            CoolingLoad = false;
                            ZoneLoad = QToHeatSetPt;
                            // zone pushed above cooling set point
                        } else if (SensOutputOff > 0.0 && SensOutputOff - QToCoolSetPt > DataHVACGlobals::SmallLoad) {
                            HeatingLoad = false;
                            CoolingLoad = true;
                            ZoneLoad = QToCoolSetPt;
                        }
                    }
                } else {
                }
            }

            if (CoolingLoad && this->m_IterationCounter <= 20) {
                this->m_IterationMode[this->m_IterationCounter] = CoolingMode;
            } else if (HeatingLoad && this->m_IterationCounter <= 20) {
                this->m_IterationMode[this->m_IterationCounter] = HeatingMode;
            } else if (this->m_IterationCounter <= 20) {
                this->m_IterationMode[this->m_IterationCounter] = NoCoolHeat;
            }
            // IF small loads to meet or not converging, just shut down unit
            if (std::abs(ZoneLoad) < Small5WLoad) {
                ZoneLoad = 0.0;
                CoolingLoad = false;
                HeatingLoad = false;
            } else if (this->m_IterationCounter > 6) {                // attempt to lock output (air flow) if oscillations are detected
                int OperatingMode = this->m_IterationMode[7];         // VS systems can take a few more iterations than single-speed systems
                int OperatingModeMinusOne = this->m_IterationMode[6]; // previously tested 5th iteration, now tests 7th
                int OperatingModeMinusTwo = this->m_IterationMode[5];
                bool Oscillate = true;
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
        }

        // Determine the m_Staged status
        if (allocated(DataZoneControls::StageZoneLogic) && this->m_DesignSpecMSHPIndex > 0) {
            if (DataZoneControls::StageZoneLogic(this->ControlZoneNum)) {
                this->m_Staged = true;
                this->m_StageNum = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->ControlZoneNum).StageNum;
            } else {
                if (this->m_MyStagedFlag) {
                    ShowWarningError("ZoneControl:Thermostat:StagedDualSetpoint is found, but is not applied to this UnitarySystem "
                                     "object with UnitarySystemPerformance:Multispeed type = ");
                    ShowContinueError(this->Name + ". Please make correction. Simulation continues...");
                    this->m_MyStagedFlag = false;
                }
            }
        }

        // Staged control
        if (this->m_Staged) {
            if (this->m_StageNum == 0) {
                HeatingLoad = false;
                CoolingLoad = false;
                QZnReq = 0.0;
            } else {
                QZnReq = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->ControlZoneNum).RemainingOutputRequired / this->ControlZoneMassFlowFrac;
                if (this->m_StageNum > 0) {
                    HeatingLoad = true;
                    CoolingLoad = false;
                } else {
                    HeatingLoad = false;
                    CoolingLoad = true;
                }
            }
        }

        if (this->m_DehumidControlType_Num == DehumCtrlType::Multimode) {
            if (HeatingLoad) MoistureLoad = 0.0;
        }

        // Check load control
        if (this->m_RunOnLatentOnlyWithSensible && ZoneLoad == 0.0) MoistureLoad = 0.0;
        if (!this->m_RunOnSensibleLoad) {
            ZoneLoad = 0.0;
            CoolingLoad = false;
            HeatingLoad = false;
        }
        if (!this->m_RunOnLatentLoad) MoistureLoad = 0.0;

        // Testing heat pump air to air with RH control with CoolReheat dehumidifaction control showed that when there was heating
        // and moisture load, the cooling coil was turning on to meet the moisture load and reheat was then turning on to meet both
        // heating load and excess cooling load caused by cooling coil. Adding the logic below caused the zone temperature,
        // relative humidity, cooling/heating rate to line up for both the orignal and new file with unitary system object.

        if (this->m_SuppCoilExists) {
            if (this->m_DehumidControlType_Num == DehumCtrlType::CoolReheat) {
                if (MoistureLoad < 0.0 && this->m_HeatPump) {
                    HeatingLoad = false;
                    CoolingLoad = true;
                }
            }
        }

        // set report variables for predicted sensible and latent load
        this->m_SensibleLoadPredicted = ZoneLoad;
        this->m_MoistureLoadPredicted = MoistureLoad;
    }

    void UnitarySys::setOnOffMassFlowRate(Real64 &OnOffAirFlowRatio, // ratio of coil on to coil off air flow rate
                                          Real64 const PartLoadRatio // coil part-load ratio
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   May 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the components.

        // METHODOLOGY EMPLOYED:
        // The unitarysystem may have alternate air flow rates
        // in cooling, heating, and when no cooling or heating is needed. Set up the coil (comp) ON and OFF
        // air flow rates. Use these flow rates during the Calc routines to set the average mass flow rates
        // based on PLR.

        // REFERENCES:
        // Based on SetOnOffMassFlowRate by Richard Raustad

        int HeatSpeedNum = 0;
        int CoolSpeedNum = 0;

        CompOffMassFlow = 0.0;
        CompOffFlowRatio = 0.0;
        m_massFlow1 = 0.0;
        m_massFlow2 = 0.0;

        // Set the compressor or coil ON mass flow rate
        if (HeatingLoad) {

            this->m_LastMode = HeatingMode;

            if (this->m_MultiOrVarSpeedHeatCoil) {

                HeatSpeedNum = this->m_HeatingSpeedNum;

                if (HeatSpeedNum == 0) {
                    CompOnMassFlow = this->m_IdleMassFlowRate;
                    CompOnFlowRatio = this->m_IdleSpeedRatio;
                } else if (HeatSpeedNum == 1) {
                    CompOnMassFlow = this->m_HeatMassFlowRate[1];
                    CompOnFlowRatio = this->m_MSHeatingSpeedRatio[1];
                } else if (HeatSpeedNum > 1) {
                    CompOnMassFlow = this->m_HeatMassFlowRate[HeatSpeedNum];
                    CompOnFlowRatio = this->m_MSHeatingSpeedRatio[HeatSpeedNum];
                }
                // Set the compressor or coil OFF mass flow rate based on LOGICAL flag
                // UseCompressorOnFlow is used when the user does not enter a value for no cooling or heating flow rate
                if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                    if (MoistureLoad < 0.0 && this->m_Humidistat && this->m_DehumidControlType_Num == DehumCtrlType::CoolReheat) {
                        if (this->m_MultiOrVarSpeedCoolCoil) {
                            CoolSpeedNum = this->m_CoolingSpeedNum;
                            if (CoolSpeedNum < 1) {
                                CompOnMassFlow = this->m_IdleMassFlowRate;
                                CompOffMassFlow = this->m_IdleMassFlowRate;
                                CompOffFlowRatio = this->m_IdleSpeedRatio;
                            } else if (CoolSpeedNum == 1) {
                                CompOnMassFlow = this->m_CoolMassFlowRate[1];
                                CompOffMassFlow = this->m_CoolMassFlowRate[1];
                                CompOffFlowRatio = this->m_MSCoolingSpeedRatio[1];
                            } else if (CoolSpeedNum > 1) {
                                CompOnMassFlow = this->m_CoolMassFlowRate[CoolSpeedNum];
                                CompOffMassFlow = this->m_CoolMassFlowRate[CoolSpeedNum - 1];
                                CompOffFlowRatio = this->m_MSCoolingSpeedRatio[CoolSpeedNum - 1];
                            }
                        } else {
                            CompOffMassFlow = this->MaxCoolAirMassFlow;
                            CompOffFlowRatio = this->m_CoolingFanSpeedRatio;
                        }
                    } else {
                        if (HeatSpeedNum == 0) {
                            CompOffMassFlow = this->m_IdleMassFlowRate;
                            CompOffFlowRatio = this->m_IdleSpeedRatio;
                        } else if (HeatSpeedNum == 1) {
                            CompOffMassFlow = this->m_HeatMassFlowRate[HeatSpeedNum];
                            CompOffFlowRatio = this->m_HeatMassFlowRate[HeatSpeedNum];
                        } else {
                            CompOffMassFlow = this->m_HeatMassFlowRate[HeatSpeedNum - 1];
                            CompOffFlowRatio = this->m_MSHeatingSpeedRatio[HeatSpeedNum - 1];
                        }
                    }
                } else { // cycling fan mode
                    if (HeatSpeedNum <= 1) {
                        CompOffMassFlow = 0.0; // #5518
                        CompOffFlowRatio = 0.0;
                    } else {
                        CompOffMassFlow = this->m_HeatMassFlowRate[HeatSpeedNum - 1];
                        CompOffFlowRatio = this->m_MSHeatingSpeedRatio[HeatSpeedNum - 1];
                    }
                }
            } else { // IF(MultiOrVarSpeedHeatCoil) THEN
                //   If a heating and moisture load exists, operate at the cooling mass flow rate ELSE operate at the heating flow rate
                if (MoistureLoad < 0.0 && this->m_Humidistat && this->m_DehumidControlType_Num == DehumCtrlType::CoolReheat &&
                    !this->m_DXHeatingCoil) {
                    if (this->m_MultiOrVarSpeedCoolCoil) {
                        CoolSpeedNum = this->m_CoolingSpeedNum;
                        if (CoolSpeedNum < 1) {
                            CompOnMassFlow = this->m_IdleMassFlowRate;
                            CompOnFlowRatio = this->m_IdleSpeedRatio;
                        } else if (CoolSpeedNum == 1) {
                            CompOnMassFlow = this->m_CoolMassFlowRate[1];
                            CompOnFlowRatio = this->m_MSCoolingSpeedRatio[1];
                        } else {
                            CompOnMassFlow = this->m_CoolMassFlowRate[CoolSpeedNum];
                            CompOnFlowRatio = this->m_MSCoolingSpeedRatio[CoolSpeedNum];
                        }
                    } else { // IF (MultiOrVarSpeedCoolCoil) THEN
                        CompOnMassFlow = this->MaxCoolAirMassFlow;
                        CompOnFlowRatio = this->m_CoolingFanSpeedRatio;
                        if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                            CompOffMassFlow = this->MaxNoCoolHeatAirMassFlow;
                            CompOffFlowRatio = this->m_CoolingFanSpeedRatio;
                        }
                    }
                } else { // Heating load but no moisture load
                    CompOnMassFlow = this->MaxHeatAirMassFlow;
                    CompOnFlowRatio = this->m_HeatingFanSpeedRatio;
                    if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                        if (this->m_AirFlowControl == UseCompFlow::UseCompressorOnFlow) {
                            CompOffMassFlow = this->MaxHeatAirMassFlow;
                            CompOffFlowRatio = this->m_HeatingFanSpeedRatio;
                        } else {
                            CompOffMassFlow = this->MaxNoCoolHeatAirMassFlow;
                            CompOffFlowRatio = this->m_HeatingFanSpeedRatio;
                        }
                    }
                }
            }

            // If a cooling load exists, operate at the cooling mass flow rate
        } else if (CoolingLoad) {

            this->m_LastMode = CoolingMode;

            if (this->m_MultiOrVarSpeedCoolCoil) {

                CoolSpeedNum = this->m_CoolingSpeedNum;

                if (CoolSpeedNum == 0) {
                    CompOnMassFlow = this->m_IdleMassFlowRate;
                    CompOnFlowRatio = this->m_IdleSpeedRatio;
                } else if (CoolSpeedNum == 1) {
                    CompOnMassFlow = this->m_CoolMassFlowRate[1];
                    CompOnFlowRatio = this->m_MSCoolingSpeedRatio[1];
                } else if (CoolSpeedNum > 1) {
                    CompOnMassFlow = this->m_CoolMassFlowRate[CoolSpeedNum];
                    CompOnFlowRatio = this->m_MSCoolingSpeedRatio[CoolSpeedNum];
                }
                // Set the compressor or coil OFF mass flow rate based on LOGICAL flag
                // UseCompressorOnFlow is used when the user does not enter a value for no cooling or heating flow rate
                //    IF(UnitarySystem(UnitarySysNum)%FanOpMode == DataHVACGlobals::ContFanCycCoil)THEN
                //      IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
                if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                    if (CoolSpeedNum == 0) {
                        CompOffMassFlow = this->m_IdleMassFlowRate;
                        CompOffFlowRatio = this->m_IdleSpeedRatio;
                    } else if (CoolSpeedNum == 1) {
                        CompOffMassFlow = this->m_CoolMassFlowRate[CoolSpeedNum];
                        CompOffFlowRatio = this->m_CoolMassFlowRate[CoolSpeedNum];
                    } else {
                        CompOffMassFlow = this->m_CoolMassFlowRate[CoolSpeedNum - 1];
                        CompOffFlowRatio = this->m_MSCoolingSpeedRatio[CoolSpeedNum - 1];
                    }
                } else { // cycling fan mode
                    if (CoolSpeedNum <= 1) {
                        CompOffMassFlow = 0.0; // #5518
                        CompOffFlowRatio = 0.0;
                    } else {
                        CompOffMassFlow = this->m_CoolMassFlowRate[CoolSpeedNum - 1];
                        CompOffFlowRatio = this->m_MSCoolingSpeedRatio[CoolSpeedNum - 1];
                    }
                }
            } else { // IF(MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
                CompOnMassFlow = this->MaxCoolAirMassFlow;
                CompOnFlowRatio = this->m_CoolingSpeedRatio;
                if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                    if (this->m_AirFlowControl == UseCompFlow::UseCompressorOnFlow) {
                        CompOffMassFlow = this->MaxCoolAirMassFlow;
                        CompOffFlowRatio = this->m_CoolingFanSpeedRatio;
                    } else {
                        CompOffMassFlow = this->MaxNoCoolHeatAirMassFlow;
                        CompOffFlowRatio = this->m_CoolingFanSpeedRatio;
                    }
                }
            }

        } else { // No load
            // If no load exists, set the compressor on mass flow rate.
            // Set equal the mass flow rate when no heating or cooling is needed If no moisture load exists.
            // If the user has set the off mass flow rate to 0, set according to the last operating mode.

            if (MoistureLoad < 0.0 && this->m_Humidistat && this->m_DehumidControlType_Num == DehumCtrlType::CoolReheat) {
                if (this->m_MultiOrVarSpeedCoolCoil) {
                    CoolSpeedNum = this->m_CoolingSpeedNum;
                    if (CoolSpeedNum < 1) {
                        CompOnMassFlow = this->m_IdleMassFlowRate;
                        CompOnFlowRatio = this->m_IdleSpeedRatio;
                    } else if (CoolSpeedNum == 1) {
                        CompOnMassFlow = this->m_CoolMassFlowRate[1];
                        CompOnFlowRatio = this->m_MSCoolingSpeedRatio[1];
                    } else {
                        CompOnMassFlow = this->m_CoolMassFlowRate[CoolSpeedNum];
                        CompOnFlowRatio = this->m_MSCoolingSpeedRatio[CoolSpeedNum];
                    }

                    if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                        if (this->m_AirFlowControl == UseCompFlow::UseCompressorOnFlow) {
                            if (CoolSpeedNum <= 1) {
                                CompOffMassFlow = this->m_IdleMassFlowRate;
                                CompOffFlowRatio = this->m_IdleSpeedRatio;
                            } else {
                                CompOffMassFlow = this->m_CoolMassFlowRate[CoolSpeedNum - 1];
                                CompOffFlowRatio = this->m_MSCoolingSpeedRatio[CoolSpeedNum - 1];
                            }
                        } else {
                            CompOffMassFlow = this->m_IdleMassFlowRate;
                            CompOffFlowRatio = this->m_IdleSpeedRatio;
                        }
                    }

                } else { // IF (MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
                    CompOnMassFlow = this->MaxCoolAirMassFlow;
                    CompOnFlowRatio = this->m_CoolingFanSpeedRatio;
                    if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                        if (this->m_AirFlowControl == UseCompFlow::UseCompressorOnFlow) {
                            CompOffMassFlow = this->MaxCoolAirMassFlow;
                            CompOffFlowRatio = this->m_CoolingFanSpeedRatio;
                        } else {
                            CompOffMassFlow = this->MaxNoCoolHeatAirMassFlow;
                            CompOffFlowRatio = this->m_CoolingFanSpeedRatio;
                        }
                    }
                }

            } else { // No Moisture Load

                if (this->m_LastMode == HeatingMode) {
                    if (this->m_MultiOrVarSpeedHeatCoil) {
                        CompOnMassFlow = this->m_IdleMassFlowRate;
                        CompOnFlowRatio = this->m_IdleSpeedRatio;
                    } else {
                        CompOnMassFlow = this->MaxNoCoolHeatAirMassFlow;
                        CompOnFlowRatio = 1.0;
                    }
                } else {
                    if (this->m_MultiOrVarSpeedCoolCoil) {
                        CompOnMassFlow = this->m_IdleMassFlowRate;
                        CompOnFlowRatio = this->m_IdleSpeedRatio;
                    } else {
                        CompOnMassFlow = this->MaxNoCoolHeatAirMassFlow;
                        CompOnFlowRatio = 1.0;
                    }
                }
                if (CompOnMassFlow == 0.0) {
                    if (this->m_LastMode == HeatingMode) {
                        if (this->m_MultiOrVarSpeedHeatCoil) {
                            HeatSpeedNum = this->m_HeatingSpeedNum;
                            if (HeatSpeedNum == 0) {
                                CompOnMassFlow = this->m_IdleMassFlowRate;
                                CompOnFlowRatio = this->m_IdleSpeedRatio;
                            } else if (HeatSpeedNum == 1) {
                                CompOnMassFlow = this->m_HeatMassFlowRate[1];
                                CompOnFlowRatio = this->m_MSHeatingSpeedRatio[1];
                            } else if (HeatSpeedNum > 1) {
                                CompOnMassFlow = this->m_HeatMassFlowRate[HeatSpeedNum];
                                CompOnFlowRatio = this->m_MSHeatingSpeedRatio[HeatSpeedNum];
                            }
                        } else { // IF(MultiOrVarSpeedHeatCoil) THEN
                            CompOnMassFlow = this->MaxHeatAirMassFlow;
                            CompOnFlowRatio = this->m_HeatingFanSpeedRatio;
                        }
                    } else { // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
                        if (this->m_MultiOrVarSpeedCoolCoil) {
                            CoolSpeedNum = this->m_CoolingSpeedNum;
                            if (CoolSpeedNum == 0) {
                                CompOnMassFlow = this->m_IdleMassFlowRate;
                                CompOnFlowRatio = this->m_IdleSpeedRatio;
                            } else if (CoolSpeedNum == 1) {
                                CompOnMassFlow = this->m_CoolMassFlowRate[1];
                                CompOnFlowRatio = this->m_MSCoolingSpeedRatio[1];
                            } else if (CoolSpeedNum > 1) {
                                CompOnMassFlow = this->m_CoolMassFlowRate[CoolSpeedNum];
                                CompOnFlowRatio = this->m_MSCoolingSpeedRatio[CoolSpeedNum];
                            }
                        } else { // IF(MultiOrVarSpeedCoolCoil) THEN
                            CompOnMassFlow = this->MaxCoolAirMassFlow;
                            CompOnFlowRatio = this->m_CoolingFanSpeedRatio;
                        } // IF(MultiOrVarSpeedCoolCoil) THEN
                    }     // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
                }         // IF(CompOnMassFlow .EQ. 0.0d0)THEN

                if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                    if (this->m_AirFlowControl == UseCompFlow::UseCompressorOnFlow) {
                        if (this->m_LastMode == HeatingMode) {
                            if (this->m_MultiOrVarSpeedHeatCoil) {
                                HeatSpeedNum = this->m_HeatingSpeedNum;
                                if (HeatSpeedNum < 1) {
                                    CompOffMassFlow = this->m_IdleMassFlowRate;
                                    CompOffFlowRatio = this->m_IdleSpeedRatio;
                                } else if (HeatSpeedNum == 1) {
                                    CompOffMassFlow = this->m_HeatMassFlowRate[1];
                                    CompOffFlowRatio = this->m_MSHeatingSpeedRatio[1];
                                } else {
                                    CompOffMassFlow = this->m_HeatMassFlowRate[HeatSpeedNum - 1];
                                    CompOffFlowRatio = this->m_MSHeatingSpeedRatio[HeatSpeedNum - 1];
                                }
                            } else {
                                CompOffMassFlow = this->MaxHeatAirMassFlow;
                                CompOffFlowRatio = this->m_HeatingFanSpeedRatio;
                            }
                        } else { // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
                            if (this->m_MultiOrVarSpeedCoolCoil) {
                                CoolSpeedNum = this->m_CoolingSpeedNum;
                                if (CoolSpeedNum < 1) {
                                    CompOffMassFlow = this->m_IdleMassFlowRate;
                                    CompOffFlowRatio = this->m_IdleSpeedRatio;
                                } else if (CoolSpeedNum == 1) {
                                    CompOffMassFlow = this->m_CoolMassFlowRate[1];
                                    CompOffFlowRatio = this->m_MSCoolingSpeedRatio[1];
                                } else {
                                    CompOffMassFlow = this->m_CoolMassFlowRate[CoolSpeedNum - 1];
                                    CompOffFlowRatio = this->m_MSCoolingSpeedRatio[CoolSpeedNum - 1];
                                }
                            } else {
                                CompOffMassFlow = this->MaxCoolAirMassFlow;
                                CompOffFlowRatio = this->m_CoolingFanSpeedRatio;
                            }
                        }    // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
                    } else { // IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
                        if (this->m_LastMode == HeatingMode) {
                            if (this->m_MultiOrVarSpeedHeatCoil) {
                                CompOffMassFlow = this->m_IdleMassFlowRate;
                                CompOffFlowRatio = this->m_IdleSpeedRatio;
                            } else {
                                CompOffMassFlow = this->MaxNoCoolHeatAirMassFlow;
                                CompOffFlowRatio = this->m_HeatingFanSpeedRatio;
                            }
                        } else {
                            if (this->m_MultiOrVarSpeedCoolCoil) {
                                CompOffMassFlow = this->m_IdleMassFlowRate;
                                CompOffFlowRatio = this->m_IdleSpeedRatio;
                            } else {
                                CompOffMassFlow = this->MaxNoCoolHeatAirMassFlow;
                                CompOffFlowRatio = this->m_CoolingFanSpeedRatio;
                            }
                        }
                    } // IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
                }     // IF(UnitarySystem(UnitarySysNum)%FanOpMode == DataHVACGlobals::ContFanCycCoil)THEN
            }         // ELSE ! No Moisture Load
        }             // No Heating/Cooling Load

        if (this->m_MultiSpeedHeatingCoil && (HeatingLoad && HeatSpeedNum == 1)) {
            if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                DataHVACGlobals::MSHPMassFlowRateLow = CompOnMassFlow; // #5737
            } else {
                DataHVACGlobals::MSHPMassFlowRateLow = CompOnMassFlow * PartLoadRatio; // proportional to PLR when speed = 1,  #5518
            }
        } else if (this->m_MultiSpeedCoolingCoil && (CoolingLoad && CoolSpeedNum == 1)) {
            if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                DataHVACGlobals::MSHPMassFlowRateLow = CompOnMassFlow; // #5737
            } else {
                DataHVACGlobals::MSHPMassFlowRateLow = CompOnMassFlow * PartLoadRatio; // proportional to PLR when speed = 1,  #5518
            }
        } else {
            DataHVACGlobals::MSHPMassFlowRateLow = CompOffMassFlow; // these need to be set for multi-speed coils
        }
        DataHVACGlobals::MSHPMassFlowRateHigh = CompOnMassFlow; // doesn't hurt to set these if multi-speed coils are not used

        m_massFlow1 = CompOnMassFlow;
        m_massFlow2 = CompOffMassFlow;

        // Set the system mass flow rates
        this->setAverageAirFlow(PartLoadRatio, OnOffAirFlowRatio);
    }

    void UnitarySys::setAverageAirFlow(Real64 const PartLoadRatio, // unit part load ratio
                                       Real64 &OnOffAirFlowRatio   // ratio of compressor ON airflow to AVERAGE airflow over timestep
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   July 2005

        // PURPOSE OF THIS SUBROUTINE:
        // Set the average air mass flow rates using the part-load fraction of the HVAC system for this time step
        // Set OnOffAirFlowRatio to be used by DX coils

        // METHODOLOGY EMPLOYED:
        // The air flow rate in cooling, heating, and no cooling or heating can be dIFferent.
        // Calculate the air flow rate based on initializations.

        Real64 AverageUnitMassFlow = 0.0; // average supply air mass flow rate over time step
        bool FanOn = false;

        m_runTimeFraction1 = 0.0;
        m_runTimeFraction2 = 0.0;

        Real64 FanPartLoadRatio = PartLoadRatio;
        if (this->m_SimASHRAEModel) FanPartLoadRatio = this->FanPartLoadRatio;
        int SpeedNum = max(this->m_CoolingSpeedNum, this->m_HeatingSpeedNum);
        int InletNode = this->AirInNode;

        if (SpeedNum > 1) {
            if ((CoolingLoad && this->m_MultiOrVarSpeedCoolCoil) || (HeatingLoad && this->m_MultiOrVarSpeedHeatCoil)) {
                AverageUnitMassFlow = FanPartLoadRatio * CompOnMassFlow + (1.0 - FanPartLoadRatio) * CompOffMassFlow;
            } else {
                AverageUnitMassFlow = CompOnMassFlow;
            }
        } else {
            AverageUnitMassFlow = (FanPartLoadRatio * CompOnMassFlow) + ((1.0 - FanPartLoadRatio) * CompOffMassFlow);
        }

        if (CompOffFlowRatio > 0.0) {
            if (SpeedNum > 1) {
                if ((CoolingLoad && this->m_MultiOrVarSpeedCoolCoil) || (HeatingLoad && this->m_MultiOrVarSpeedHeatCoil)) {
                    FanSpeedRatio = FanPartLoadRatio * CompOnFlowRatio + (1.0 - FanPartLoadRatio) * CompOffFlowRatio;
                    m_runTimeFraction1 = FanPartLoadRatio;
                    m_runTimeFraction2 = 1.0 - FanPartLoadRatio;
                } else {
                    FanSpeedRatio = CompOnFlowRatio;
                    m_runTimeFraction1 = FanPartLoadRatio;
                    m_runTimeFraction2 = 0.0;
                }
            } else {
                FanSpeedRatio = (FanPartLoadRatio * CompOnFlowRatio) + ((1.0 - FanPartLoadRatio) * CompOffFlowRatio);
                m_runTimeFraction1 = FanPartLoadRatio;
                m_runTimeFraction2 = 1.0 - FanPartLoadRatio;
            }
        } else {
            FanSpeedRatio = CompOnFlowRatio;
            m_runTimeFraction1 = FanPartLoadRatio;
            m_runTimeFraction2 = 0.0;
        }

        if (!(HeatingLoad && this->m_NumOfSpeedHeating == 0)) {
            if (this->m_SingleMode == 1) {
                if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                    AverageUnitMassFlow = CompOnMassFlow;
                    FanSpeedRatio = CompOnFlowRatio;
                    m_runTimeFraction1 = 1.0;
                    m_runTimeFraction2 = 0.0;
                } else {
                    AverageUnitMassFlow = FanPartLoadRatio * CompOnMassFlow;
                    FanSpeedRatio = FanPartLoadRatio * CompOnFlowRatio;
                    m_runTimeFraction1 = FanPartLoadRatio;
                    m_runTimeFraction2 = 0.0;
                }
            }
        }

        // If the unitary system is scheduled on or nightime cycle overrides fan schedule. Uses same logic as fan.
        if (this->m_FanExists) {
            FanOn = false;
            if (ScheduleManager::GetCurrentScheduleValue(this->m_FanAvailSchedPtr) > 0) FanOn = true;
        } else {
            FanOn = true;
        }
        if (ScheduleManager::GetCurrentScheduleValue(this->m_SysAvailSchedPtr) > 0.0 &&
            ((FanOn || DataHVACGlobals::TurnFansOn) && !DataHVACGlobals::TurnFansOff)) {
            if (this->m_ControlType == ControlType::Setpoint) {
                // set point based equipment should use VAV terminal units to set the flow.
                // zone equipment needs to set flow since no other device regulates flow (ZoneHVAC /= AirLoopEquipment)
                if (!this->m_AirLoopEquipment) {
                    DataLoopNode::Node(InletNode).MassFlowRate = AverageUnitMassFlow;
                    DataLoopNode::Node(InletNode).MassFlowRateMaxAvail =
                        AverageUnitMassFlow; // #5531 zone equipment needs MaxAvail set or fan will not turn ON
                }
                if (AverageUnitMassFlow > 0.0) {
                    OnOffAirFlowRatio = 1.0;
                } else {
                    OnOffAirFlowRatio = 0.0;
                }
            } else {
                DataLoopNode::Node(InletNode).MassFlowRate = AverageUnitMassFlow;
                if (!this->m_AirLoopEquipment) {
                    DataLoopNode::Node(InletNode).MassFlowRateMaxAvail =
                        AverageUnitMassFlow; // #5531 zone equipment needs MaxAvail set or fan will not turn ON
                }
                if (AverageUnitMassFlow > 0.0) {
                    OnOffAirFlowRatio = CompOnMassFlow / AverageUnitMassFlow;
                } else {
                    OnOffAirFlowRatio = 0.0;
                }
            }
        } else {
            DataLoopNode::Node(InletNode).MassFlowRate = 0.0;
            OnOffAirFlowRatio = 1.0;
        }
    }

    void UnitarySys::calcUnitarySystemToLoad(int const AirLoopNum,          // index to air loop
                                             bool const FirstHVACIteration, // True when first HVAC iteration
                                             Real64 const CoolPLR,          // operating cooling part-load ratio []
                                             Real64 const HeatPLR,          // operating cooling part-load ratio []
                                             Real64 &OnOffAirFlowRatio,     // ratio of heating PLR to cooling PLR (is this correct?)
                                             Real64 &SensOutput,            // sensible capacity (W)
                                             Real64 &LatOutput,             // latent capacity (W)
                                             bool HXUnitOn,                 // Flag to control HX for HXAssisted Cooling Coil
                                             Real64 HeatCoilLoad,           // Adjusted load to heating coil when SAT exceeds max limit (W)
                                             Real64 SuppCoilLoad,           // Adjusted load to supp heating coil when SAT exceeds max limit (W)
                                             int const CompOn               // Determines if compressor is on or off
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the resulting performance of the unitary system given
        // the operating PLR. System output is calculated with respect to zone condition.

        Real64 CoilCoolHeatRat = 0.0; // ratio of cooling to heating PLR for cycling fan RH control

        int InletNode = this->AirInNode;

        int CoolingCompOn = 0;
        if (CoolPLR > 0) {
            CoolingCompOn = CompOn;
            // for multispeed coils, comp is on IF speed > 1
        } else if (this->m_CoolingSpeedNum > 1) {
            CoolingCompOn = 1;
        }

        int HeatingCompOn = 0;
        if (HeatPLR > 0) {
            HeatingCompOn = CompOn;
            CoilCoolHeatRat = min(1.0, CoolPLR / HeatPLR);
        } else {
            CoilCoolHeatRat = 1.0;
        }
        // for multispeed coils, comp is on at PLR=0 IF speed > 1
        if (this->m_HeatingSpeedNum > 1) HeatingCompOn = 1;

        // set the operating flow rate
        if (this->m_NumOfSpeedCooling > 0 || this->m_NumOfSpeedHeating > 0) {
            this->setOnOffMassFlowRate(OnOffAirFlowRatio, max(CoolPLR, HeatPLR));
        } else {
            this->setAverageAirFlow(max(CoolPLR, HeatPLR), OnOffAirFlowRatio);
        }

        // Call the series of components that simulate a Unitary System
        if (this->ATMixerExists) {
            // There is an air terminal mixer
            if (this->ATMixerType == DataHVACGlobals::ATMixer_InletSide) { // if there is an inlet side air terminal mixer
                // set the primary air inlet mass flow rate
                DataLoopNode::Node(this->m_ATMixerPriNode).MassFlowRate =
                    min(DataLoopNode::Node(this->m_ATMixerPriNode).MassFlowRateMaxAvail, DataLoopNode::Node(InletNode).MassFlowRate);
                // now calculate the the mixer outlet conditions (and the secondary air inlet flow rate)
                // the mixer outlet flow rate has already been set above (it is the "inlet" node flow rate)
                SingleDuct::SimATMixer(this->m_ATMixerName, FirstHVACIteration, this->m_ATMixerIndex);
            }
        }

        if (this->m_FanExists && this->m_FanPlace == FanPlace::BlowThru) {
            if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->m_FanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->m_FanIndex, FanSpeedRatio);
            }
        }

        if (this->m_CoolingCoilUpstream) {

            if (this->m_CoolCoilExists) {
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }
            if (this->m_HeatCoilExists) {
                // operate the heating coil without regard to coil outlet temperature
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad);
                if (DataLoopNode::Node(this->HeatCoilOutletNodeNum).Temp > this->DesignMaxOutletTemp && !this->m_SimASHRAEModel) {
                    Real64 MDotAir = DataLoopNode::Node(this->HeatCoilInletNodeNum).MassFlowRate;
                    Real64 CpAirIn = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->HeatCoilInletNodeNum).HumRat,
                                                                    DataLoopNode::Node(this->HeatCoilInletNodeNum).Temp);
                    Real64 CpAirOut = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->HeatCoilOutletNodeNum).HumRat,
                                                                     DataLoopNode::Node(this->HeatCoilOutletNodeNum).Temp);
                    Real64 CpAir = (CpAirIn + CpAirOut) / 2;
                    Real64 HCDeltaT = this->DesignMaxOutletTemp - DataLoopNode::Node(this->HeatCoilInletNodeNum).Temp;
                    Real64 MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
                    this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad);
                    HeatCoilLoad = MaxHeatCoilLoad;
                }
            }

            // If blow thru fan is used, the fan must be simulated after coil sets OnOffFanPartLoadFraction
            if (this->m_FanExists && this->m_FanPlace == FanPlace::BlowThru && DataHVACGlobals::OnOffFanPartLoadFraction < 1.0) {
                if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[this->m_FanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
                } else {
                    Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->m_FanIndex, FanSpeedRatio);
                }
                if (this->m_CoolCoilExists) {
                    this->calcUnitaryCoolingSystem(
                        AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
                }
                if (this->m_HeatCoilExists) {
                    this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad);
                    if (DataLoopNode::Node(this->HeatCoilOutletNodeNum).Temp > this->DesignMaxOutletTemp && !this->m_SimASHRAEModel) {
                        Real64 MDotAir = DataLoopNode::Node(this->HeatCoilInletNodeNum).MassFlowRate;
                        Real64 CpAirIn = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->HeatCoilInletNodeNum).HumRat,
                                                                        DataLoopNode::Node(this->HeatCoilInletNodeNum).Temp);
                        Real64 CpAirOut = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->HeatCoilOutletNodeNum).HumRat,
                                                                         DataLoopNode::Node(this->HeatCoilOutletNodeNum).Temp);
                        Real64 CpAir = (CpAirIn + CpAirOut) / 2;
                        Real64 HCDeltaT = this->DesignMaxOutletTemp - DataLoopNode::Node(this->HeatCoilInletNodeNum).Temp;
                        Real64 MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
                        this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad);
                    }
                }
            }

        } else {

            if (this->m_HeatCoilExists) {
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad);
                if (DataLoopNode::Node(this->HeatCoilOutletNodeNum).Temp > this->DesignMaxOutletTemp && !this->m_SimASHRAEModel) {
                    Real64 MDotAir = DataLoopNode::Node(this->HeatCoilInletNodeNum).MassFlowRate;
                    Real64 CpAirIn = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->HeatCoilInletNodeNum).HumRat,
                                                                    DataLoopNode::Node(this->HeatCoilInletNodeNum).Temp);
                    Real64 CpAirOut = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->HeatCoilOutletNodeNum).HumRat,
                                                                     DataLoopNode::Node(this->HeatCoilOutletNodeNum).Temp);
                    Real64 CpAir = (CpAirIn + CpAirOut) / 2;
                    Real64 HCDeltaT = this->DesignMaxOutletTemp - DataLoopNode::Node(this->HeatCoilInletNodeNum).Temp;
                    Real64 MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
                    this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad);
                }
            }
            if (this->m_CoolCoilExists) {
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }

            // If blow thru fan is used, the fan must be simulated after coil sets OnOffFanPartLoadFraction
            if (this->m_FanExists && this->m_FanPlace == FanPlace::BlowThru && DataHVACGlobals::OnOffFanPartLoadFraction < 1.0) {
                if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[this->m_FanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
                } else {
                    Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->m_FanIndex, FanSpeedRatio);
                }
                if (this->m_HeatCoilExists) {
                    this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad);
                    if (DataLoopNode::Node(this->HeatCoilOutletNodeNum).Temp > this->DesignMaxOutletTemp && !this->m_SimASHRAEModel) {
                        Real64 MDotAir = DataLoopNode::Node(this->HeatCoilInletNodeNum).MassFlowRate;
                        Real64 CpAirIn = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->HeatCoilInletNodeNum).HumRat,
                                                                        DataLoopNode::Node(this->HeatCoilInletNodeNum).Temp);
                        Real64 CpAirOut = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->HeatCoilOutletNodeNum).HumRat,
                                                                         DataLoopNode::Node(this->HeatCoilOutletNodeNum).Temp);
                        Real64 CpAir = (CpAirIn + CpAirOut) / 2;
                        Real64 HCDeltaT = this->DesignMaxOutletTemp - DataLoopNode::Node(this->HeatCoilInletNodeNum).Temp;
                        Real64 MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
                        this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad);
                    }
                }
                if (this->m_CoolCoilExists) {
                    this->calcUnitaryCoolingSystem(
                        AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
                }
            }
        }

        if (this->m_FanExists && this->m_FanPlace == FanPlace::DrawThru) {
            if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->m_FanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->m_FanIndex, FanSpeedRatio);
            }
        }

        Real64 SuppPLR = this->m_SuppHeatPartLoadFrac;
        if (this->m_SuppCoilExists) {
            this->calcUnitarySuppHeatingSystem(FirstHVACIteration, SuppPLR, SuppCoilLoad);
            if ((DataLoopNode::Node(this->m_SuppCoilAirOutletNode).Temp > this->DesignMaxOutletTemp) && SuppPLR > 0.0 && !this->m_SimASHRAEModel) {
                Real64 MDotAir = DataLoopNode::Node(this->m_SuppCoilAirInletNode).MassFlowRate;
                Real64 CpAirIn = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->m_SuppCoilAirInletNode).HumRat,
                                                                DataLoopNode::Node(this->m_SuppCoilAirInletNode).Temp);
                Real64 CpAirOut = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->m_SuppCoilAirOutletNode).HumRat,
                                                                 DataLoopNode::Node(this->m_SuppCoilAirOutletNode).Temp);
                Real64 CpAir = (CpAirIn + CpAirOut) / 2;
                Real64 HCDeltaT = max(0.0, this->DesignMaxOutletTemp - DataLoopNode::Node(this->m_SuppCoilAirInletNode).Temp);
                Real64 MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
                this->calcUnitarySuppHeatingSystem(FirstHVACIteration, SuppPLR, MaxHeatCoilLoad);
                SuppCoilLoad = MaxHeatCoilLoad;
            }
        }

        // If there is a supply side air terminal mixer, calculate its output
        if (this->ATMixerExists) {
            if (this->ATMixerType == DataHVACGlobals::ATMixer_SupplySide) {
                SingleDuct::SimATMixer(this->m_ATMixerName, FirstHVACIteration, this->m_ATMixerIndex);
            }
        }

        calculateCapacity(SensOutput, LatOutput);
    }

    void UnitarySys::calculateCapacity(Real64 &SensOutput, Real64 &LatOutput)
    {

        // Check delta T (outlet to space), IF positive use space HumRat ELSE outlet humrat to calculate
        // sensible capacity as MdotDeltaH at constant humidity ratio
        Real64 MinHumRatio = DataLoopNode::Node(this->NodeNumOfControlledZone).HumRat;
        int OutletNode = this->AirOutNode;
        Real64 AirMassFlow = DataLoopNode::Node(OutletNode).MassFlowRate;
        Real64 ZoneTemp = DataLoopNode::Node(this->NodeNumOfControlledZone).Temp;
        Real64 ZoneHumRat = DataLoopNode::Node(this->NodeNumOfControlledZone).HumRat;
        if (DataLoopNode::Node(OutletNode).Temp < ZoneTemp) MinHumRatio = DataLoopNode::Node(OutletNode).HumRat;

        // calculate sensible load met
        if (this->ATMixerExists) {
            if (this->ATMixerType == DataHVACGlobals::ATMixer_SupplySide) {
                // Air terminal supply side mixer
                int ATMixOutNode = this->ATMixerOutNode;
                SensOutput =
                    DataLoopNode::Node(ATMixOutNode).MassFlowRate * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(ATMixOutNode).Temp, MinHumRatio) -
                                                                     Psychrometrics::PsyHFnTdbW(ZoneTemp, MinHumRatio));
                if (this->m_Humidistat) {
                    //   Calculate latent load met (at constant temperature)
                    LatOutput = DataLoopNode::Node(ATMixOutNode).MassFlowRate *
                                    (Psychrometrics::PsyHFnTdbW(ZoneTemp, DataLoopNode::Node(ATMixOutNode).HumRat) -
                                     Psychrometrics::PsyHFnTdbW(ZoneTemp, ZoneHumRat)) -
                                this->m_LatLoadLoss;
                } else {
                    LatOutput = 0.0;
                }
            } else {
                // Air terminal inlet side mixer
                SensOutput = AirMassFlow * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, MinHumRatio) -
                                            Psychrometrics::PsyHFnTdbW(ZoneTemp, MinHumRatio));
                if (this->m_Humidistat) {
                    //   Calculate latent load met (at constant temperature)
                    LatOutput = AirMassFlow * (Psychrometrics::PsyHFnTdbW(ZoneTemp, DataLoopNode::Node(OutletNode).HumRat) -
                                               Psychrometrics::PsyHFnTdbW(ZoneTemp, ZoneHumRat)) -
                                this->m_LatLoadLoss;
                } else {
                    LatOutput = 0.0;
                }
            }
        } else {
            // Calculate sensible load met (at constant humidity ratio)
            SensOutput = AirMassFlow * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, MinHumRatio) -
                                        Psychrometrics::PsyHFnTdbW(ZoneTemp, MinHumRatio)) -
                         this->m_SenLoadLoss;

            if (this->m_Humidistat) {

                //   Calculate latent load met (at constant temperature)
                LatOutput = AirMassFlow * (Psychrometrics::PsyHFnTdbW(ZoneTemp, DataLoopNode::Node(OutletNode).HumRat) -
                                           Psychrometrics::PsyHFnTdbW(ZoneTemp, ZoneHumRat)) -
                            this->m_LatLoadLoss;
            } else {
                LatOutput = 0.0;
            }
        }
        this->m_SensibleLoadMet = SensOutput;
        this->m_LatentLoadMet = LatOutput;
    }

    void UnitarySys::calcUnitaryCoolingSystem(int const AirLoopNum,          // index to air loop
                                              bool const FirstHVACIteration, // True when first HVAC iteration
                                              Real64 const PartLoadRatio,    // coil operating part-load ratio
                                              int const CompOn,              // compressor control (0=off, 1=on)
                                              Real64 const OnOffAirFlowRatio,
                                              Real64 const CoilCoolHeatRat, // ratio of cooling to heating PLR for cycling fan RH control
                                              bool const HXUnitOn           // Flag to control HX for HXAssisted Cooling Coil
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages unitary cooling system component simulation.

        // Locals
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 OutsideDryBulbTemp; // outdoor temperature (C)
        Real64 mdot;               // water side flow rate (kg/s)
        Real64 QActual;            // actual coil output (W)
        Real64 OutdoorPressure;    // Outdoor barometric pressure at condenser (Pa)
        bool errFlag;              // returned flag from called routine
        bool HeatingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil
                            // is wrapped by UnitarySystem.
        bool CoolingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil
                            // is wrapped by UnitarySystem.

        // Simulate the coil component
        std::string CompName = this->m_CoolingCoilName;
        int CompIndex = this->m_CoolingCoilIndex;
        Real64 CoilPLR = 1.0;
        if (this->m_CondenserNodeNum != 0) {
            OutdoorPressure = DataLoopNode::Node(this->m_CondenserNodeNum).Press;
            // IF node is not connected to anything, pressure = default, use weather data
            if (OutdoorPressure == DataLoopNode::DefaultNodeValues.Press) {
                OutsideDryBulbTemp = DataEnvironment::OutDryBulbTemp;
                //      OutdoorHumRat   = OutHumRat
                //      OutdoorPressure = OutBaroPress
                //      OutdoorWetBulb  = OutWetBulbTemp
            } else {
                OutsideDryBulbTemp = DataLoopNode::Node(this->m_CondenserNodeNum).Temp;
                //      OutdoorHumRat   = DataLoopNode::Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%HumRat
                //      OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb,OutdoorHumRat,OutdoorPressure,RoutineName)
            }
        } else {
            OutsideDryBulbTemp = DataEnvironment::OutDryBulbTemp;
            //    OutdoorHumRat   = OutHumRat
            //    OutdoorPressure = OutBaroPress
            //    OutdoorWetBulb  = OutWetBulbTemp
        }

        {
            auto const SELECT_CASE_var(this->m_CoolingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingSingleSpeed) { // Coil:Cooling:DX:SingleSpeed

                DXCoils::SimDXCoil(
                    blankString, CompOn, FirstHVACIteration, CompIndex, this->m_FanOpMode, PartLoadRatio, OnOffAirFlowRatio, CoilCoolHeatRat);
                this->m_CoolCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if ((SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingHXAssisted) ||
                       (SELECT_CASE_var == DataHVACGlobals::CoilWater_CoolingHXAssisted)) {

                if (this->m_CoolingCoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted) {
                    Real64 mdot =
                        min(DataLoopNode::Node(this->CoolCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->MaxCoolCoilFluidFlow * PartLoadRatio);
                    DataLoopNode::Node(this->CoolCoilFluidInletNode).MassFlowRate = mdot;
                }
                HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(blankString,
                                                                    FirstHVACIteration,
                                                                    CompOn,
                                                                    PartLoadRatio,
                                                                    CompIndex,
                                                                    this->m_FanOpMode,
                                                                    HXUnitOn,
                                                                    OnOffAirFlowRatio,
                                                                    economizerFlag);
                if (this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted)
                    this->m_CoolCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) { // Coil:Cooling:DX:TwoSpeed
                // formerly (v3 and beyond)COIL:DX:MULTISPEED:COOLINGEMPIRICAL

                DXCoils::SimDXCoilMultiSpeed(blankString, this->m_CoolingSpeedRatio, this->m_CoolingCycRatio, CompIndex);
                if (this->m_CoolingSpeedRatio > 0.0) {
                    this->m_CoolCompPartLoadRatio = this->m_CoolingSpeedRatio * double(CompOn);
                } else {
                    this->m_CoolCompPartLoadRatio = this->m_CoolingCycRatio * double(CompOn);
                }

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) { // Coil:Cooling:DX:Multispeed

                if (OutsideDryBulbTemp > this->m_MinOATCompressorCooling) {
                    DXCoils::SimDXCoilMultiSpeed(CompName,
                                                 this->m_CoolingSpeedRatio,
                                                 this->m_CoolingCycRatio,
                                                 CompIndex,
                                                 this->m_CoolingSpeedNum,
                                                 this->m_FanOpMode,
                                                 CompOn,
                                                 this->m_SingleMode);
                    if (this->m_CoolingSpeedNum > 1) {
                        if (this->m_SingleMode == 0) {
                            this->m_CoolCompPartLoadRatio = double(CompOn);
                        } else {
                            this->m_CoolCompPartLoadRatio = this->m_CoolingCycRatio * double(CompOn);
                        }
                    } else {
                        this->m_CoolCompPartLoadRatio = this->m_CoolingCycRatio * double(CompOn);
                    }
                } else {
                    DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, 0.0, CompIndex, this->m_CoolingSpeedNum, this->m_FanOpMode, CompOn);
                    this->m_CoolCompPartLoadRatio = 0.0;
                }

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                // formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL

                DXCoils::SimDXCoilMultiMode(
                    CompName, CompOn, FirstHVACIteration, PartLoadRatio, this->m_DehumidificationMode, CompIndex, this->m_FanOpMode);
                this->m_CoolCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) {

                HeatingActive = false; // set to arbitrary value on entry to function
                CoolingActive = false; // set to arbitrary value on entry to function

                UserDefinedComponents::SimCoilUserDefined(CompName, CompIndex, AirLoopNum, HeatingActive, CoolingActive);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWater) || (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterDetailed)) {

                if (this->CoolCoilWaterFlowRatio == 0.0) {
                    mdot = this->MaxCoolCoilFluidFlow * PartLoadRatio;
                } else {
                    mdot = this->CoolCoilWaterFlowRatio * this->MaxCoolCoilFluidFlow;
                }
                DataLoopNode::Node(this->CoolCoilFluidInletNode).MassFlowRate = mdot;
                WaterCoils::SimulateWaterCoilComponents(
                    CompName, FirstHVACIteration, this->m_CoolingCoilIndex, QActual, this->m_FanOpMode, PartLoadRatio);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {
                if (this->m_CoolingSpeedNum > 1) {
                    CoilPLR = 1.0;
                } else {
                    CoilPLR = PartLoadRatio;
                }
                VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                          CompIndex,
                                                          this->m_FanOpMode,
                                                          this->m_MaxONOFFCyclesperHour,
                                                          this->m_HPTimeConstant,
                                                          this->m_FanDelayTime,
                                                          CompOn,
                                                          CoilPLR,
                                                          this->m_CoolingSpeedNum,
                                                          this->m_CoolingSpeedRatio,
                                                          this->m_CoolingCoilSensDemand,
                                                          this->m_CoolingCoilLatentDemand,
                                                          OnOffAirFlowRatio);
                if (this->m_CoolingSpeedNum > 1) {
                    this->m_CoolCompPartLoadRatio = 1.0;
                } else {
                    this->m_CoolCompPartLoadRatio = PartLoadRatio * double(CompOn);
                }

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {

                if (PartLoadRatio > 0.0 && this->m_WSHPRuntimeFrac > 0.0 && this->m_FanOpMode == DataHVACGlobals::CycFanCycCoil) {
                    DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / this->m_WSHPRuntimeFrac;
                }

                WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                this->m_CoolingCoilIndex,
                                                                this->m_CoolingCoilSensDemand,
                                                                this->m_CoolingCoilLatentDemand,
                                                                this->m_FanOpMode,
                                                                this->m_WSHPRuntimeFrac,
                                                                this->m_MaxONOFFCyclesperHour,
                                                                this->m_HPTimeConstant,
                                                                this->m_FanDelayTime,
                                                                CompOn,
                                                                PartLoadRatio,
                                                                FirstHVACIteration);

                this->m_CoolCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHP) {

                this->heatPumpRunFrac(PartLoadRatio, errFlag, this->m_WSHPRuntimeFrac);

                if (PartLoadRatio > 0.0 && this->m_WSHPRuntimeFrac > 0.0 && this->m_FanOpMode == DataHVACGlobals::CycFanCycCoil) {
                    DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / this->m_WSHPRuntimeFrac;
                }

                WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                    this->m_CoolingCoilIndex,
                                                    this->MaxCoolAirMassFlow,
                                                    this->m_FanOpMode,
                                                    FirstHVACIteration,
                                                    this->m_WSHPRuntimeFrac,
                                                    this->m_MaxONOFFCyclesperHour,
                                                    this->m_HPTimeConstant,
                                                    this->m_FanDelayTime,
                                                    this->m_InitHeatPump,
                                                    this->m_CoolingCoilSensDemand,
                                                    this->m_CoolingCoilLatentDemand,
                                                    CompOn,
                                                    PartLoadRatio);

                this->m_CoolCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {

                PackagedThermalStorageCoil::SimTESCoil(CompName, this->m_CoolingCoilIndex, this->m_FanOpMode, this->m_TESOpMode, PartLoadRatio);
            }
        }

        this->m_CoolingPartLoadFrac = PartLoadRatio;
    }

    void UnitarySys::calcUnitaryHeatingSystem(int const AirLoopNum,           // index to air loop
                                              bool const FirstHVACIteration,  // True when first HVAC iteration
                                              Real64 const PartLoadRatio,     // coil operating part-load ratio
                                              int const CompOn,               // compressor control (0=off, 1=on)
                                              Real64 const OnOffAirFlowRatio, // ratio of on to off flow rate
                                              Real64 HeatCoilLoad             // adjusted heating coil load if outlet temp exceeds max (W)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages unitary heating system component simulation.

        // Locals
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 OutsideDryBulbTemp; // outdoor temperature (C)
        Real64 mdot;               // water side flow rate (kg/s)
        Real64 QActual;            // actual output of coil (W)
        Real64 OutdoorPressure;    // Outdoor barometric pressure at condenser (Pa)
        bool errFlag;              // returned flag from called routine
        bool HeatingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil
                            // is wrapped by UnitarySystem.
        bool CoolingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil
                            // is wrapped by UnitarySystem.

        std::string CompName = this->m_HeatingCoilName;
        Real64 dummy = 0.0;
        Real64 HeatPLR = 1.0;
        if (this->m_CondenserNodeNum != 0) {
            OutdoorPressure = DataLoopNode::Node(this->m_CondenserNodeNum).Press;
            // IF node is not connected to anything, pressure = default, use weather data
            if (OutdoorPressure == DataLoopNode::DefaultNodeValues.Press) {
                OutsideDryBulbTemp = DataEnvironment::OutDryBulbTemp;
                //      OutdoorHumRat   = OutHumRat
                //      OutdoorPressure = OutBaroPress
                //      OutdoorWetBulb  = OutWetBulbTemp
            } else {
                OutsideDryBulbTemp = DataLoopNode::Node(this->m_CondenserNodeNum).Temp;
                //      OutdoorHumRat   = DataLoopNode::Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%HumRat
                //      OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb,OutdoorHumRat,OutdoorPressure,RoutineName)
            }
        } else {
            OutsideDryBulbTemp = DataEnvironment::OutDryBulbTemp;
            //    OutdoorHumRat   = OutHumRat
            //    OutdoorPressure = OutBaroPress
            //    OutdoorWetBulb  = OutWetBulbTemp
        }

        {
            auto const SELECT_CASE_var(this->m_HeatingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_HeatingEmpirical) { // COIL:HEATING:DX:SINGLESPEED

                DXCoils::SimDXCoil(
                    CompName, CompOn, FirstHVACIteration, this->m_HeatingCoilIndex, this->m_FanOpMode, PartLoadRatio, OnOffAirFlowRatio);
                this->m_HeatCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) {

                HeatingActive = false; // set to arbitrary value on entry to function
                CoolingActive = true;  // set to arbitrary value on entry to function

                UserDefinedComponents::SimCoilUserDefined(CompName, this->m_HeatingCoilIndex, AirLoopNum, HeatingActive, CoolingActive);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric)) {
                HeatCoilLoad = PartLoadRatio * m_DesignHeatingCapacity;
                HeatingCoils::SimulateHeatingCoilComponents(
                    CompName, FirstHVACIteration, HeatCoilLoad, this->m_HeatingCoilIndex, _, false, this->m_FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater) {
                HeatingCoils::SimulateHeatingCoilComponents(
                    CompName, FirstHVACIteration, HeatCoilLoad, this->m_HeatingCoilIndex, _, false, this->m_FanOpMode, PartLoadRatio);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) {

                if (OutsideDryBulbTemp > this->m_MinOATCompressorHeating) {
                    DXCoils::SimDXCoilMultiSpeed(CompName,
                                                 this->m_HeatingSpeedRatio,
                                                 this->m_HeatingCycRatio,
                                                 this->m_HeatingCoilIndex,
                                                 this->m_HeatingSpeedNum,
                                                 this->m_FanOpMode,
                                                 CompOn,
                                                 this->m_SingleMode);
                    this->m_HeatCompPartLoadRatio = PartLoadRatio * double(CompOn);
                } else {
                    DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, 0.0, this->m_HeatingCoilIndex, this->m_HeatingSpeedNum, this->m_FanOpMode, CompOn);
                    this->m_HeatCompPartLoadRatio = 0.0;
                }

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {
                HeatingCoils::SimulateHeatingCoilComponents(
                    CompName, FirstHVACIteration, _, 0, _, _, this->m_FanOpMode, PartLoadRatio, this->m_HeatingSpeedNum, this->m_HeatingSpeedRatio);
                this->m_HeatingCycRatio = PartLoadRatio;
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {
                if (this->HeatCoilWaterFlowRatio == 0.0) {
                    mdot = this->MaxHeatCoilFluidFlow * PartLoadRatio;
                } else {
                    mdot = this->HeatCoilWaterFlowRatio * this->MaxHeatCoilFluidFlow;
                }
                DataLoopNode::Node(this->HeatCoilFluidInletNode).MassFlowRate = mdot;
                WaterCoils::SimulateWaterCoilComponents(
                    CompName, FirstHVACIteration, this->m_HeatingCoilIndex, QActual, this->m_FanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {
                // this same CALL is made in the steam coil calc routine
                mdot = min(DataLoopNode::Node(this->HeatCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->MaxHeatCoilFluidFlow * PartLoadRatio);
                DataLoopNode::Node(this->HeatCoilFluidInletNode).MassFlowRate = mdot;
                SteamCoils::SimulateSteamCoilComponents(CompName,
                                                        FirstHVACIteration,
                                                        this->m_HeatingCoilIndex,
                                                        this->m_DesignHeatingCapacity * PartLoadRatio,
                                                        _,
                                                        this->m_FanOpMode,
                                                        PartLoadRatio);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit)) {

                if (this->m_HeatingSpeedNum > 1) {
                    HeatPLR = 1.0;
                } else {
                    HeatPLR = PartLoadRatio;
                }
                VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                          this->m_HeatingCoilIndex,
                                                          this->m_FanOpMode,
                                                          this->m_MaxONOFFCyclesperHour,
                                                          this->m_HPTimeConstant,
                                                          this->m_FanDelayTime,
                                                          CompOn,
                                                          HeatPLR,
                                                          this->m_HeatingSpeedNum,
                                                          this->m_HeatingSpeedRatio,
                                                          this->m_HeatingCoilSensDemand,
                                                          dummy,
                                                          OnOffAirFlowRatio);
                if (this->m_HeatingSpeedNum > 1) {
                    this->m_HeatCompPartLoadRatio = 1.0;
                } else {
                    this->m_HeatCompPartLoadRatio = PartLoadRatio * double(CompOn);
                }
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) {

                if (PartLoadRatio > 0.0 && this->m_WSHPRuntimeFrac > 0.0 && this->m_FanOpMode == DataHVACGlobals::CycFanCycCoil) {
                    DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / this->m_WSHPRuntimeFrac;
                }

                WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                this->m_HeatingCoilIndex,
                                                                this->m_HeatingCoilSensDemand,
                                                                dummy,
                                                                this->m_FanOpMode,
                                                                this->m_WSHPRuntimeFrac,
                                                                this->m_MaxONOFFCyclesperHour,
                                                                this->m_HPTimeConstant,
                                                                this->m_FanDelayTime,
                                                                CompOn,
                                                                PartLoadRatio,
                                                                FirstHVACIteration);
                this->m_HeatCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHP) {

                this->heatPumpRunFrac(PartLoadRatio, errFlag, this->m_WSHPRuntimeFrac);

                if (PartLoadRatio > 0.0 && this->m_WSHPRuntimeFrac > 0.0 && this->m_FanOpMode == DataHVACGlobals::CycFanCycCoil) {
                    DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / this->m_WSHPRuntimeFrac;
                }

                WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                    this->m_HeatingCoilIndex,
                                                    this->MaxHeatAirMassFlow,
                                                    this->m_FanOpMode,
                                                    FirstHVACIteration,
                                                    this->m_WSHPRuntimeFrac,
                                                    this->m_MaxONOFFCyclesperHour,
                                                    this->m_HPTimeConstant,
                                                    this->m_FanDelayTime,
                                                    this->m_InitHeatPump,
                                                    this->m_HeatingCoilSensDemand,
                                                    dummy,
                                                    CompOn,
                                                    PartLoadRatio);
                this->m_HeatCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else {
                ShowFatalError("CalcUnitaryHeatingSystem: Invalid Unitary System coil type = " +
                               DataHVACGlobals::cAllCoilTypes(this->m_HeatingCoilType_Num));
            }
        }

        this->m_HeatingPartLoadFrac = PartLoadRatio;
    }

    void UnitarySys::calcUnitarySuppHeatingSystem(bool const FirstHVACIteration, // True when first HVAC iteration
                                                  Real64 const PartLoadRatio,    // coil operating part-load ratio
                                                  Real64 const SuppCoilLoad      // adjusted supp coil load when outlet temp exceeds max (W)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages supplemental heater simulation.

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);   // Maximum number of iterations for solver
        Real64 const Acc(1.e-3); // Accuracy of solver result

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // std::string CompName;    // Name of Unitary System object
        Real64 SuppHeatCoilLoad; // load passed to supplemental heating coil (W)
        Real64 QActual;          // actual coil output (W)
        Real64 mdot;             // water coil water mass flow rate (kg/s)
        std::vector<Real64> Par; // Parameter array passed to solver
        int SolFla;              // Flag of solver, num iterations if >0, else error index
        Real64 PartLoadFrac;     // temporary PLR variable

        Par.resize(5);
        // work is needed to figure out how to adjust other coil types if outlet temp exceeds maximum
        // this works for gas and electric heating coils
        std::string CompName = this->m_SuppHeatCoilName;
        if (DataEnvironment::OutDryBulbTemp <= this->m_MaxOATSuppHeat || (MoistureLoad < 0.0 && this->m_CoolingPartLoadFrac > 0.0)) {
            SuppHeatCoilLoad = SuppCoilLoad;
            //} else {
            //    SuppHeatCoilLoad = this->m_DesignSuppHeatingCapacity * PartLoadRatio;
            //}
        } else {
            SuppHeatCoilLoad = 0.0;
        }

        {
            auto const SELECT_CASE_var(this->m_SuppHeatCoilType_Num);

            if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) || (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric)) {
                {
                    auto const SELECT_CASE_var1(this->m_ControlType);
                    if (SELECT_CASE_var1 == ControlType::Setpoint) {
                        HeatingCoils::SimulateHeatingCoilComponents(
                            CompName, FirstHVACIteration, _, this->m_SuppHeatCoilIndex, _, true, this->m_FanOpMode, PartLoadRatio);
                    } else {
                        HeatingCoils::SimulateHeatingCoilComponents(
                            CompName, FirstHVACIteration, SuppHeatCoilLoad, this->m_SuppHeatCoilIndex, _, true, this->m_FanOpMode, PartLoadRatio);
                    }
                }
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater) {
                HeatingCoils::SimulateHeatingCoilComponents(
                    CompName, FirstHVACIteration, SuppHeatCoilLoad, this->m_SuppHeatCoilIndex, _, true, this->m_FanOpMode, PartLoadRatio);

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {
                // if (present(SuppCoilLoad)) {
                if (SuppHeatCoilLoad > 0.0) {
                    // see if HW coil has enough capacity to meet the load
                    mdot = min(DataLoopNode::Node(this->m_SuppCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->m_MaxSuppCoilFluidFlow);
                    DataLoopNode::Node(this->m_SuppCoilFluidInletNode).MassFlowRate = mdot;
                    //     simulate water coil to find operating capacity
                    WaterCoils::SimulateWaterCoilComponents(
                        this->m_SuppHeatCoilName, FirstHVACIteration, this->m_SuppHeatCoilIndex, QActual, this->m_FanOpMode, PartLoadRatio);
                    if (QActual > SuppHeatCoilLoad) {
                        Par[1] = double(this->m_UnitarySysNum);
                        if (FirstHVACIteration) {
                            Par[2] = 1.0;
                        } else {
                            Par[2] = 0.0;
                        }
                        Par[3] = SuppHeatCoilLoad;
                        Par[4] = 1.0; // SuppHeatingCoilFlag
                        Par[5] = 1.0; // Load based control
                        General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, &this->hotWaterHeatingCoilResidual, 0.0, 1.0, Par);
                        this->m_SuppHeatPartLoadFrac = PartLoadFrac;
                    } else {
                        this->m_SuppHeatPartLoadFrac = 1.0;
                    }
                }
                //} else {
                //    mdot = min(DataLoopNode::Node(this->m_SuppCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->m_MaxSuppCoilFluidFlow *
                //    PartLoadRatio); DataLoopNode::Node(this->m_SuppCoilFluidInletNode).MassFlowRate = mdot;

                //    WaterCoils::SimulateWaterCoilComponents(
                //        CompName, FirstHVACIteration, this->m_SuppHeatCoilIndex, QActual, this->m_FanOpMode, PartLoadRatio);
                //}
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {
                mdot = min(DataLoopNode::Node(this->m_SuppCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->m_MaxSuppCoilFluidFlow * PartLoadRatio);
                DataLoopNode::Node(this->m_SuppCoilFluidInletNode).MassFlowRate = mdot;
                SteamCoils::SimulateSteamCoilComponents(
                    CompName, FirstHVACIteration, this->m_SuppHeatCoilIndex, SuppHeatCoilLoad, _, this->m_FanOpMode, PartLoadRatio);

            } else {
            }
        }

        //  UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac = PartLoadRatio
    }

    void UnitarySys::controlCoolingSystemToSP(int const AirLoopNum,          // index to air loop
                                              bool const FirstHVACIteration, // First HVAC iteration flag
                                              bool &HXUnitOn,                // flag to enable heat exchanger heat recovery
                                              int &CompOn                    // compressor on/off control
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013
        //       MODIFIED       Nov. 2016, R. Zhang, LBNL. Applied the coil supply air temperature sensor offset fault model

        // PURPOSE OF THIS SUBROUTINE:
        //  Simulate the coil object at the required PLR.

        // METHODOLOGY EMPLOYED:
        //  Calculate operating PLR and adjust speed when using multispeed coils.
        //  Meet moisture load if required to do so.

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);         // Maximum number of iterations for solver
        Real64 const Acc(1.e-3);       // Accuracy of solver result
        Real64 const HumRatAcc(1.e-6); // Accuracy of solver result

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FullOutput; // Sensible capacity (outlet - inlet) when the compressor is on
        Real64 ReqOutput;  // Sensible capacity (outlet - inlet) required to meet load or setpoint temperature
        // for variable speed or 2 speed compressors
        Real64 OutletTempDXCoil;       // Actual outlet temperature of the DX cooling coil
        Real64 OutletHumRatLS;         // Actual outlet humrat of the variable speed DX cooling coil at low speed
        Real64 OutletHumRatHS;         // Actual outlet humrat of the variable speed DX cooling coil at high speed
        Real64 OutletHumRatDXCoil;     // Actual outlet humidity ratio of the DX cooling coil
        std::vector<Real64> Par(14);   // Parameter array passed to solver
        Real64 TempMinPLR;             // Used to find latent PLR when max iterations exceeded
        Real64 TempMaxPLR;             // Used to find latent PLR when max iterations exceeded
        Real64 TempOutletTempDXCoil;   // Used to find latent PLR when max iterations exceeded
        Real64 TempOutletHumRatDXCoil; // Used to find latent PLR when max iterations exceeded
        Real64 FullLoadHumRatOut;      // DX coil outlet air humidity ratio with comprssor full on
        Real64 OutletTemp;
        bool HeatingActive;     // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.
        bool CoolingActive;     // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.
        Real64 OutdoorDryBulb;  // local variable for OutDryBulbTemp
        Real64 mdot;            // water coil water flow rate [kg/s]
        Real64 maxPartLoadFrac; // calculated maximum water side PLR for RegulaFalsi call (when plant limits flow max PLR != 1)

        // Set local variables
        // Retrieve the load on the controlled zone
        int OutletNode = this->CoolCoilOutletNodeNum;
        int InletNode = this->CoolCoilInletNodeNum;
        Real64 DesOutTemp = this->m_DesiredOutletTemp;
        Real64 DesOutHumRat = this->m_DesiredOutletHumRat;
        int CoilType_Num = this->m_CoolingCoilType_Num;
        Real64 LoopDXCoilMaxRTFSave = 0.0;
        if (DataAirflowNetwork::SimulateAirflowNetwork > DataAirflowNetwork::AirflowNetworkControlMultizone) {
            LoopDXCoilMaxRTFSave = DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF;
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF = 0.0;
        }

        std::string CompName = this->m_CoolingCoilName;
        int FanOpMode = this->m_FanOpMode;
        Real64 SpeedRatio = 0.0;
        int SpeedNum = 0;
        Real64 CycRatio = 0.0;
        Real64 PartLoadFrac = 0.0;
        int DehumidMode = 0;
        bool SensibleLoad = false;
        bool LatentLoad = false;
        Real64 m_WSHPRuntimeFrac = 0.0;
        Real64 dummy = 0.0;
        Real64 SensLoad = 0.0;
        int SolFla = 0;
        int SolFlaLat = 0;
        Real64 NoLoadTempOut = 0.0;
        Real64 NoLoadHumRatOut = 0.0;
        Real64 OnOffAirFlowRatio = 0.0; // Autodesk:Init Patch to prevent use uninitialized in calls to SimVariableSpeedCoils

        if (this->m_CondenserNodeNum != 0) {
            OutdoorDryBulb = DataLoopNode::Node(this->m_CondenserNodeNum).Temp;
        } else {
            OutdoorDryBulb = DataEnvironment::OutDryBulbTemp;
        }

        // Check the dehumidification control type. IF it's multimode, turn off the HX to find the sensible PLR. Then check to
        // see if the humidity load is met without the use of the HX. Always run the HX for the other modes.
        if (this->m_DehumidControlType_Num != DehumCtrlType::Multimode) {
            HXUnitOn = true;
        } else {
            HXUnitOn = false;
        }

        // IF there is a fault of coil SAT Sensor (zrp_Nov2016)
        if (this->m_FaultyCoilSATFlag) {
            // calculate the sensor offset using fault information
            int FaultIndex = this->m_FaultyCoilSATIndex;
            this->m_FaultyCoilSATOffset = FaultsManager::FaultsCoilSATSensor(FaultIndex).CalFaultOffsetAct();
            // update the DesOutTemp
            DesOutTemp -= this->m_FaultyCoilSATOffset;
        }

        // IF DXCoolingSystem is scheduled on and there is flow
        if ((ScheduleManager::GetCurrentScheduleValue(this->m_SysAvailSchedPtr) > 0.0) &&
            ScheduleManager::GetCurrentScheduleValue(this->m_CoolingCoilAvailSchPtr) > 0.0 &&
            (DataLoopNode::Node(InletNode).MassFlowRate > MinAirMassFlow)) {

            // Determine if there is a sensible load on this system
            if (DataLoopNode::Node(InletNode).Temp - DesOutTemp > DataHVACGlobals::TempControlTol) SensibleLoad = true;
            // if a heat pump and other coil is on, disable this coil
            if (this->m_HeatPump && this->m_HeatingPartLoadFrac > 0.0) SensibleLoad = false;

            // Determine if there is a latent load on this system - for future use to serve latent-only loads
            if (DataLoopNode::Node(InletNode).HumRat > DesOutHumRat) LatentLoad = true;

            // disable latent dehumidification if there is no sensible load and latent only is not allowed
            if (this->m_RunOnLatentOnlyWithSensible && !SensibleLoad) LatentLoad = false;

            // disable compressor if OAT is below minimum outdoor temperature
            if (OutdoorDryBulb < this->m_MinOATCompressorCooling) {
                SensibleLoad = false;
                LatentLoad = false;
            }

            // IF DXCoolingSystem runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
            // Multimode coil will switch to enhanced dehumidification IF available and needed, but it
            // still runs to meet the sensible load. Multimode applies to Multimode or HXAssistedCooling coils.
            if ((SensibleLoad && this->m_RunOnSensibleLoad) || (LatentLoad && this->m_RunOnLatentLoad)) {
                // calculate sensible PLR, don't care IF latent is true here but need to gaurd for
                // when LatentLoad=TRUE and SensibleLoad=FALSE
                ReqOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                            (Psychrometrics::PsyHFnTdbW(DesOutTemp, DataLoopNode::Node(OutletNode).HumRat) -
                             Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(OutletNode).HumRat));

                PartLoadFrac = 0.0;
                CompOn = 0;

                if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL
                    this->m_CompPartLoadRatio = PartLoadFrac;

                    DXCoils::SimDXCoil(CompName, On, FirstHVACIteration, this->m_CoolingCoilIndex, FanOpMode, PartLoadFrac);

                } else if ((CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) ||
                           (CoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted)) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

                    if (this->CoolCoilFluidInletNode > 0) DataLoopNode::Node(this->CoolCoilFluidInletNode).MassFlowRate = 0.0;

                    HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(
                        CompName, FirstHVACIteration, On, PartLoadFrac, this->m_CoolingCoilIndex, FanOpMode, HXUnitOn, _, economizerFlag);
                    if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) this->m_CompPartLoadRatio = PartLoadFrac;
                } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                    DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, PartLoadFrac, this->m_CoolingCoilIndex);

                } else if (CoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                    this->simMultiSpeedCoils(
                        AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, CoolingCoil, this->m_SpeedNum);

                } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                           (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {

                    this->m_CoolingCoilSensDemand = ReqOutput;
                    VariableSpeedCoils::SimVariableSpeedCoils("",
                                                              this->m_CoolingCoilIndex,
                                                              FanOpMode,
                                                              this->m_MaxONOFFCyclesperHour,
                                                              this->m_HPTimeConstant,
                                                              this->m_FanDelayTime,
                                                              CompOn,
                                                              CycRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              SensLoad,
                                                              dummy,
                                                              OnOffAirFlowRatio);

                } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {

                    DXCoils::SimDXCoilMultiMode(CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, this->m_CoolingCoilIndex, FanOpMode);
                    this->m_CompPartLoadRatio = PartLoadFrac;
                } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWater) ||
                           (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed)) { // COIL:COOLING:WATER

                    WaterCoils::SimulateWaterCoilComponents(
                        CompName, FirstHVACIteration, this->m_CoolingCoilIndex, _, this->m_FanOpMode, PartLoadFrac);

                } else if (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {

                    WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                    this->m_CoolingCoilIndex,
                                                                    ReqOutput,
                                                                    dummy,
                                                                    FanOpMode,
                                                                    m_WSHPRuntimeFrac,
                                                                    this->m_MaxONOFFCyclesperHour,
                                                                    this->m_HPTimeConstant,
                                                                    this->m_FanDelayTime,
                                                                    0,
                                                                    PartLoadFrac,
                                                                    FirstHVACIteration);
                    this->m_CoolingCoilSensDemand = 0.0;

                } else if (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP) {

                    WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                        this->m_CoolingCoilIndex,
                                                        this->MaxCoolAirMassFlow,
                                                        FanOpMode,
                                                        FirstHVACIteration,
                                                        m_WSHPRuntimeFrac,
                                                        this->m_MaxONOFFCyclesperHour,
                                                        this->m_HPTimeConstant,
                                                        this->m_FanDelayTime,
                                                        this->m_InitHeatPump,
                                                        ReqOutput,
                                                        dummy,
                                                        0,
                                                        PartLoadFrac);

                } else if (CoilType_Num == DataHVACGlobals::Coil_UserDefined) {

                    HeatingActive = false; // set to arbitrary value on entry to function
                    CoolingActive = true;  // set to arbitrary value on entry to function
                    UserDefinedComponents::SimCoilUserDefined(CompName, this->m_CoolingCoilIndex, AirLoopNum, HeatingActive, CoolingActive);
                    if (CoolingActive) PartLoadFrac = 1.0;

                } else if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {

                    PackagedThermalStorageCoil::SimTESCoil(CompName, this->m_CoolingCoilIndex, FanOpMode, this->m_TESOpMode, PartLoadFrac);

                } else {
                }

                //      NoOutput = DataLoopNode::Node(InletNode)%MassFlowRate *  &
                //                   (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
                //                    - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))
                NoLoadTempOut = DataLoopNode::Node(OutletNode).Temp;
                NoLoadHumRatOut = DataLoopNode::Node(OutletNode).HumRat;

                //     Changed logic to use temperature instead of load. The Psyc calcs can cause slight errors.
                //     For example it's possible that (NoOutput-ReqOutput) > Acc while (Node(OutletNode)%Temp-DesOutTemp) is not
                //     This can (and did) lead to RegulaFalsi errors

                //      IF ((NoOutput-ReqOutput) .LT. Acc) THEN
                //     IF outlet temp at no load is lower than DesOutTemp (set point), do not operate the coil
                //      and if coolReheat, check hum rat as well
                if (((NoLoadTempOut - DesOutTemp) < Acc) && ((NoLoadHumRatOut - DesOutHumRat) < HumRatAcc)) {
                    PartLoadFrac = 0.0;
                } else if (SensibleLoad) { // need to turn on compressor to see if load is met
                    PartLoadFrac = 1.0;
                    CompOn = 1;
                    m_WSHPRuntimeFrac = 1.0;

                    if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

                        DXCoils::SimDXCoil(CompName, On, FirstHVACIteration, this->m_CoolingCoilIndex, FanOpMode, PartLoadFrac);
                        this->m_CompPartLoadRatio = PartLoadFrac;

                    } else if ((CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) ||
                               (CoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted)) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

                        if (this->CoolCoilFluidInletNode > 0)
                            DataLoopNode::Node(this->CoolCoilFluidInletNode).MassFlowRate = max(0.0, this->MaxCoolCoilFluidFlow);
                        HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(
                            CompName, FirstHVACIteration, On, PartLoadFrac, this->m_CoolingCoilIndex, FanOpMode, HXUnitOn, _, economizerFlag);

                        if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) this->m_CompPartLoadRatio = PartLoadFrac;

                    } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                        CycRatio = 1.0;
                        for (SpeedNum = 1; SpeedNum <= this->m_NumOfSpeedCooling; ++SpeedNum) {
                            SpeedRatio = double(SpeedNum) - 1.0;
                            DXCoils::SimDXCoilMultiSpeed(CompName, SpeedRatio, CycRatio, this->m_CoolingCoilIndex);
                            OutletTemp = DXCoils::DXCoilOutletTemp(this->m_CoolingCoilIndex);
                            if (OutletTemp < DesOutTemp && SensibleLoad) break; // this isn't going to work IF dehumidIFying
                        }

                    } else if (CoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                        CycRatio = 1.0;
                        SpeedRatio = 0.0;
                        for (SpeedNum = 1; SpeedNum <= this->m_NumOfSpeedCooling; ++SpeedNum) {
                            if (SpeedNum > 1) CycRatio = 0.0;
                            if (SpeedNum > 1) SpeedRatio = 1.0;
                            this->m_CoolingSpeedNum = SpeedNum;
                            this->simMultiSpeedCoils(
                                AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, CoolingCoil, SpeedNum);
                            OutletTemp = DataLoopNode::Node(OutletNode).Temp;
                            if (OutletTemp < DesOutTemp && SensibleLoad) break;
                        }

                    } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                               (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {

                        CycRatio = 1.0;
                        SpeedRatio = 1.0;
                        SensLoad = -1.0; // turns on coil
                        this->m_CoolingSpeedRatio = SpeedRatio;
                        this->m_CoolingPartLoadFrac = PartLoadFrac;
                        for (SpeedNum = 1; SpeedNum <= this->m_NumOfSpeedCooling; ++SpeedNum) {
                            this->m_CoolingSpeedNum = SpeedNum;
                            VariableSpeedCoils::SimVariableSpeedCoils("",
                                                                      this->m_CoolingCoilIndex,
                                                                      FanOpMode,
                                                                      this->m_MaxONOFFCyclesperHour,
                                                                      this->m_HPTimeConstant,
                                                                      this->m_FanDelayTime,
                                                                      CompOn,
                                                                      CycRatio,
                                                                      SpeedNum,
                                                                      SpeedRatio,
                                                                      SensLoad,
                                                                      dummy,
                                                                      OnOffAirFlowRatio);
                            OutletTemp = DataLoopNode::Node(OutletNode).Temp;
                            if (OutletTemp < DesOutTemp && SensibleLoad) break;
                        }

                    } else if (CoilType_Num ==
                               DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) { // Coil:Cooling:DX:TwoStageWithHumidityControlMode

                        DXCoils::SimDXCoilMultiMode(CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, this->m_CoolingCoilIndex, FanOpMode);
                        this->m_CompPartLoadRatio = PartLoadFrac;

                    } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWater) ||
                               (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed)) { // COIL:COOLING:WATER

                        mdot = this->MaxCoolCoilFluidFlow;
                        PlantUtilities::SetComponentFlowRate(mdot,
                                                             this->CoolCoilFluidInletNode,
                                                             this->CoolCoilFluidOutletNodeNum,
                                                             this->CoolCoilLoopNum,
                                                             this->CoolCoilLoopSide,
                                                             this->CoolCoilBranchNum,
                                                             this->CoolCoilCompNum);

                        WaterCoils::SimulateWaterCoilComponents(
                            CompName, FirstHVACIteration, this->m_CoolingCoilIndex, _, this->m_FanOpMode, PartLoadFrac);

                    } else if (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {

                        WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                        this->m_CoolingCoilIndex,
                                                                        ReqOutput,
                                                                        dummy,
                                                                        FanOpMode,
                                                                        m_WSHPRuntimeFrac,
                                                                        this->m_MaxONOFFCyclesperHour,
                                                                        this->m_HPTimeConstant,
                                                                        this->m_FanDelayTime,
                                                                        1,
                                                                        PartLoadFrac,
                                                                        FirstHVACIteration);
                        this->m_CoolingCoilSensDemand = ReqOutput;

                    } else if (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP) {

                        WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                            this->m_CoolingCoilIndex,
                                                            this->MaxCoolAirMassFlow,
                                                            FanOpMode,
                                                            FirstHVACIteration,
                                                            m_WSHPRuntimeFrac,
                                                            this->m_MaxONOFFCyclesperHour,
                                                            this->m_HPTimeConstant,
                                                            this->m_FanDelayTime,
                                                            this->m_InitHeatPump,
                                                            ReqOutput,
                                                            dummy,
                                                            0,
                                                            PartLoadFrac);

                    } else if (CoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                        HeatingActive = false; // set to arbitrary value on entry to function
                        CoolingActive = false; // set to arbitrary value on entry to function

                        UserDefinedComponents::SimCoilUserDefined(CompName, this->m_CoolingCoilIndex, AirLoopNum, HeatingActive, CoolingActive);
                        if (CoolingActive) PartLoadFrac = 1.0;

                    } else if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {

                        // TES coil simulated above with PLR=0. Operating mode is known here, no need to simulate again to determine operating mode.
                        if (this->m_TESOpMode == PackagedThermalStorageCoil::OffMode ||
                            this->m_TESOpMode == PackagedThermalStorageCoil::ChargeOnlyMode) { // cannot cool
                            PartLoadFrac = 0.0;
                        } else {
                            // Get full load result
                            PackagedThermalStorageCoil::SimTESCoil(CompName, this->m_CoolingCoilIndex, FanOpMode, this->m_TESOpMode, PartLoadFrac);
                        }

                    } else {
                    }

                    FullOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                                 (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat) -
                                  Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(OutletNode).HumRat));

                    FullLoadHumRatOut = DataLoopNode::Node(OutletNode).HumRat;

                    //        IF ((FullOutput - ReqOutput) .GT. Acc) THEN ! old method
                    //        IF ((Node(OutletNode)%Temp-DesOutTemp) .GT. Acc) THEN ! new method gets caught when temps are very close
                    if (DataLoopNode::Node(OutletNode).Temp > DesOutTemp - Acc) {
                        PartLoadFrac = 1.0;
                        if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling &&
                            (this->m_TESOpMode == PackagedThermalStorageCoil::OffMode ||
                             this->m_TESOpMode == PackagedThermalStorageCoil::ChargeOnlyMode)) {
                            PartLoadFrac = 0.0;
                        }
                    } else if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling &&
                               (this->m_TESOpMode == PackagedThermalStorageCoil::OffMode ||
                                this->m_TESOpMode == PackagedThermalStorageCoil::ChargeOnlyMode)) {
                        PartLoadFrac = 0.0;
                    } else {

                        Par[9] = double(AirLoopNum);
                        Par[10] = 0.0;
                        if (FirstHVACIteration) Par[10] = 1.0;

                        if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {

                            Par[1] = double(this->m_CoolingCoilIndex);
                            Par[2] = DesOutTemp;
                            Par[5] = double(FanOpMode);
                            General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->DOE2DXCoilResidual, 0.0, 1.0, Par);
                            this->m_CompPartLoadRatio = PartLoadFrac;

                        } else if ((CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) ||
                                   (CoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted)) {

                            Par[1] = double(this->m_CoolingCoilIndex);
                            Par[2] = DesOutTemp;
                            // FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
                            if (FirstHVACIteration) {
                                Par[3] = 1.0;
                            } else {
                                Par[3] = 0.0;
                            }
                            if (HXUnitOn) {
                                Par[4] = 1.0;
                            } else {
                                Par[4] = 0.0;
                            }
                            Par[5] = double(FanOpMode);
                            Par[6] = double(this->m_UnitarySysNum);
                            General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->HXAssistedCoolCoilTempResidual, 0.0, 1.0, Par);
                            if (SolFla == -1) {

                                //                 RegulaFalsi may not find sensible PLR when the latent degradation model is used.
                                //                 IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                                TempMaxPLR = -0.1;
                                TempOutletTempDXCoil = DataLoopNode::Node(InletNode).Temp;
                                while ((TempOutletTempDXCoil - DesOutTemp) > 0.0 && TempMaxPLR <= 1.0) {
                                    //                   find upper limit of PLR
                                    TempMaxPLR += 0.1;
                                    HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(CompName,
                                                                                        FirstHVACIteration,
                                                                                        On,
                                                                                        TempMaxPLR,
                                                                                        this->m_CoolingCoilIndex,
                                                                                        FanOpMode,
                                                                                        HXUnitOn,
                                                                                        _,
                                                                                        economizerFlag);
                                    TempOutletTempDXCoil = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletTemp(this->m_CoolingCoilIndex);
                                }
                                TempMinPLR = TempMaxPLR;
                                while ((TempOutletTempDXCoil - DesOutTemp) < 0.0 && TempMinPLR >= 0.0) {
                                    // pull upper limit of PLR DOwn to last valid limit (i.e. outlet temp still exceeds DesOutTemp)
                                    TempMaxPLR = TempMinPLR;
                                    // find minimum limit of PLR
                                    TempMinPLR -= 0.01;
                                    HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(CompName,
                                                                                        FirstHVACIteration,
                                                                                        On,
                                                                                        TempMinPLR,
                                                                                        this->m_CoolingCoilIndex,
                                                                                        FanOpMode,
                                                                                        HXUnitOn,
                                                                                        _,
                                                                                        economizerFlag);
                                    TempOutletTempDXCoil = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletTemp(this->m_CoolingCoilIndex);
                                }
                                // Relax boundary slightly to assure a solution can be found using RegulaFalsi (i.e. one boundary may
                                // be very near the desired result)
                                TempMinPLR = max(0.0, (TempMinPLR - 0.01));
                                TempMaxPLR = min(1.0, (TempMaxPLR + 0.01));
                                //                 tighter boundary of solution has been found, CALL RegulaFalsi a second time
                                General::SolveRoot(
                                    Acc, MaxIte, SolFla, PartLoadFrac, this->HXAssistedCoolCoilTempResidual, TempMinPLR, TempMaxPLR, Par);
                                if (SolFla == -1) {
                                    if (!DataGlobals::WarmupFlag) {
                                        if (this->warnIndex.m_HXAssistedSensPLRIter < 1) {
                                            ++this->warnIndex.m_HXAssistedSensPLRIter;
                                            ShowWarningError(
                                                this->UnitType +
                                                " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + this->Name);
                                            ShowContinueError("Estimated part-load ratio   = " +
                                                              General::RoundSigDigits((ReqOutput / FullOutput), 3));
                                            ShowContinueError("Calculated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                            ShowContinueErrorTimeStamp(
                                                "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            this->UnitType + " \"" + this->Name +
                                                "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR "
                                                "statistics follow.",
                                            this->warnIndex.m_HXAssistedSensPLRIterIndex,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                } else if (SolFla == -2) {
                                    PartLoadFrac = ReqOutput / FullOutput;
                                    if (!DataGlobals::WarmupFlag) {
                                        if (this->warnIndex.m_HXAssistedSensPLRFail < 1) {
                                            ++this->warnIndex.m_HXAssistedSensPLRFail;
                                            ShowWarningError(this->UnitType +
                                                             " - DX unit sensible part-load ratio calculation unexpectedly failed: part-load ratio "
                                                             "limits exceeded, for unit = " +
                                                             this->Name);
                                            ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                            ShowContinueErrorTimeStamp(
                                                "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            this->UnitType + " \"" + this->Name +
                                                "\" - DX unit sensible part-load ratio calculation unexpectedly failed error continues. Sensible PLR "
                                                "statistics follow.",
                                            this->warnIndex.m_HXAssistedSensPLRFailIndex,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                }
                            } else if (SolFla == -2) {
                                PartLoadFrac = ReqOutput / FullOutput;
                                if (!DataGlobals::WarmupFlag) {
                                    if (this->warnIndex.m_HXAssistedSensPLRFail2 < 1) {
                                        ++this->warnIndex.m_HXAssistedSensPLRFail2;
                                        ShowWarningError(
                                            this->UnitType +
                                            " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " +
                                            this->Name);
                                        ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                        ShowContinueErrorTimeStamp(
                                            "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                    }
                                    ShowRecurringWarningErrorAtEnd(this->UnitType + " \"" + this->Name +
                                                                       "\" - DX unit sensible part-load ratio calculation failed error continues. "
                                                                       "Sensible PLR statistics follow.",
                                                                   this->warnIndex.m_HXAssistedSensPLRFailIndex2,
                                                                   PartLoadFrac,
                                                                   PartLoadFrac);
                                }
                            }
                            if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) this->m_CompPartLoadRatio = PartLoadFrac;

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                            Par[1] = double(this->m_CoolingCoilIndex);
                            Par[2] = DesOutTemp;
                            // Par(3) is only needed for variable speed coils (see DXCoilVarSpeedResidual and DXCoilCyclingResidual)
                            Par[3] = double(this->m_UnitarySysNum);
                            this->m_CoolingSpeedRatio = SpeedRatio;
                            if (SpeedRatio == 1.0) {
                                General::SolveRoot(Acc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedResidual, 0.0, 1.0, Par);
                                PartLoadFrac = SpeedRatio;
                            } else {
                                General::SolveRoot(Acc, MaxIte, SolFla, CycRatio, this->DXCoilCyclingResidual, 0.0, 1.0, Par);
                                PartLoadFrac = CycRatio;
                            }

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                            Par[1] = double(this->m_CoolingCoilIndex);
                            Par[2] = DesOutTemp;
                            Par[3] = double(this->m_UnitarySysNum);
                            // Par[4] = CycRatio or SpeedRatio
                            Par[5] = this->m_CoolingSpeedNum;
                            Par[6] = 1.0; // UnitarySystem(UnitarySysNum)%FanOpMode
                            Par[7] = 1.0; // CompOp
                            Par[8] = ReqOutput;

                            if (this->m_CoolingSpeedNum > 1.0) {
                                Par[4] = CycRatio;
                                General::SolveRoot(Acc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedResidual, 0.0, 1.0, Par);
                                PartLoadFrac = SpeedRatio;
                            } else {
                                SpeedRatio = 0.0;
                                this->m_CoolingSpeedRatio = SpeedRatio;
                                Par[4] = SpeedRatio;
                                General::SolveRoot(Acc, MaxIte, SolFla, CycRatio, this->DXCoilCyclingResidual, 0.0, 1.0, Par);
                                PartLoadFrac = CycRatio;
                            }

                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {

                            Par[1] = double(this->m_CoolingCoilIndex);
                            Par[2] = DesOutTemp;
                            Par[3] = double(this->m_UnitarySysNum);
                            // Par[4] = CycRatio or SpeedRatio
                            Par[5] = this->m_CoolingSpeedNum;
                            Par[6] = double(m_FanOpMode);
                            Par[7] = 1.0; // CompOp
                            Par[8] = ReqOutput;

                            if (this->m_CoolingSpeedNum > 1.0) {
                                Par[4] = CycRatio;
                                General::SolveRoot(Acc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedResidual, 0.0, 1.0, Par);
                                this->m_CoolingCycRatio = CycRatio;
                                this->m_CoolingSpeedRatio = SpeedRatio;
                                this->m_CoolingPartLoadFrac = SpeedRatio;
                                this->calcPassiveSystem(AirLoopNum, FirstHVACIteration);
                                PartLoadFrac = SpeedRatio;
                            } else {
                                this->m_CoolingSpeedRatio = SpeedRatio;
                                Par[4] = SpeedRatio;
                                General::SolveRoot(Acc, MaxIte, SolFla, CycRatio, this->DXCoilCyclingResidual, 0.0, 1.0, Par);
                                this->m_CoolingCycRatio = CycRatio;
                                this->m_CoolingPartLoadFrac = CycRatio;
                                this->calcPassiveSystem(AirLoopNum, FirstHVACIteration);
                                PartLoadFrac = CycRatio;
                            }

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {

                            Par[1] = double(this->m_CoolingCoilIndex);
                            Par[2] = DesOutTemp;
                            // dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                            Par[3] = double(DehumidMode);
                            Par[4] = double(FanOpMode);
                            General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, &this->multiModeDXCoilResidual, 0.0, 1.0, Par);
                            this->m_CompPartLoadRatio = PartLoadFrac;

                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWater) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed)) {

                            Par[1] = double(this->m_UnitarySysNum);
                            if (FirstHVACIteration) {
                                Par[2] = 1.0;
                            } else {
                                Par[2] = 0.0;
                            }
                            Par[3] = DesOutTemp;

                            // calculate max waterside PLR from mdot request above in case plant chokes water flow
                            maxPartLoadFrac =
                                min(1.0,
                                    ((mdot / this->MaxCoolCoilFluidFlow) +
                                     0.001)); // plant can limit flow and RegulaFalsi could hit max iteration limit (leave a little slop, 0.001)
                            General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->coolWaterTempResidual, 0.0, maxPartLoadFrac, Par);

                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP)) {
                            Par[1] = double(this->m_UnitarySysNum);
                            if (FirstHVACIteration) {
                                Par[2] = 1.0;
                            } else {
                                Par[2] = 0.0;
                            }
                            Par[3] = DesOutTemp;
                            Par[4] = ReqOutput;
                            this->m_CoolingCoilSensDemand = ReqOutput;
                            General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->coolWatertoAirHPTempResidual, 0.0, 1.0, Par);

                        } else if (CoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                            // do nothing, user defined coil cannot be controlled

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {

                            Par[1] = double(this->m_UnitarySysNum);
                            Par[2] = DesOutTemp;
                            Par[3] = 0.0; // DesOutHumRat; set to 0 if temp controlled
                            General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->TESIceStorageCoilOutletResidual, 0.0, 1.0, Par);

                        } else {
                            ShowMessage(" For :" + this->UnitType + "=\"" + this->Name + "\"");
                            ShowFatalError("ControlCoolingSystemToSP: Invalid cooling coil type = " + DataHVACGlobals::cAllCoilTypes(CoilType_Num));
                        }
                    }
                }

                //     IF system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
                //     ELSE use operating humidity ratio to test against humidity setpoint
                if (PartLoadFrac == 0.0) {
                    OutletHumRatDXCoil = NoLoadHumRatOut;
                } else {
                    OutletHumRatDXCoil = DataLoopNode::Node(OutletNode).HumRat;
                }

                // IF humidity setpoint is not satisfied and humidity control type is MultiMode,
                // then enable heat exchanger and run to meet sensible load

                if ((OutletHumRatDXCoil > (DesOutHumRat + HumRatAcc)) && (PartLoadFrac < 1.0) &&
                    (this->m_DehumidControlType_Num == DehumCtrlType::Multimode)) {

                    if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) { // CoilSystem:Cooling:DX:HeatExchangerAssisted
                        // Determine required part load when heat exchanger is ON
                        HXUnitOn = true;
                        PartLoadFrac = 1.0;
                        HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(
                            CompName, FirstHVACIteration, On, PartLoadFrac, this->m_CoolingCoilIndex, FanOpMode, HXUnitOn, _, economizerFlag);

                        OutletTempDXCoil = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletTemp(this->m_CoolingCoilIndex);

                        //               FullOutput will be different than the FullOutput determined above during sensible PLR calculations
                        FullOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                                     (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(OutletNode).HumRat) -
                                      Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(OutletNode).HumRat));

                        //   Check to see if the system can meet the load with the compressor off
                        //   If NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
                        if ((NoLoadTempOut - DesOutTemp) < Acc) {
                            PartLoadFrac = 0.0;
                            //          OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above.
                            //          if this temp is greater than or very near the desired outlet temp, then run the compressor at PartLoadFrac
                            //          = 1.
                            //            ELSEIF ((OutletTempDXCoil > DesOutTemp) .OR. ABS(OutletTempDXCoil - DesOutTemp) .LE. (Acc*2.0d0)) THEN
                        } else if (OutletTempDXCoil > DesOutTemp - (Acc * 2.0)) {
                            PartLoadFrac = 1.0;
                        } else {
                            Par[1] = double(this->m_CoolingCoilIndex);
                            Par[2] = DesOutTemp;
                            // FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1.0 and FALSE = 0.0
                            if (FirstHVACIteration) {
                                Par[3] = 1.0;
                            } else {
                                Par[3] = 0.0;
                            }
                            if (HXUnitOn) {
                                Par[4] = 1.0;
                            } else {
                                Par[4] = 0.0;
                            }
                            Par[5] = double(FanOpMode);
                            General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->HXAssistedCoolCoilTempResidual, 0.0, 1.0, Par);
                        }
                        this->m_CompPartLoadRatio = PartLoadFrac;

                    } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {

                        // Get full load result
                        PartLoadFrac = 1.0;
                        DehumidMode = 1;
                        this->m_DehumidificationMode = DehumidMode;
                        DXCoils::SimDXCoilMultiMode(CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, this->m_CoolingCoilIndex, FanOpMode);
                        FullOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                                     (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(InletNode).HumRat) -
                                      Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat));

                        // Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
                        // Check that this is the case; IF not set PartLoadFrac = 0.0 (off) and return
                        // Calculate the part load fraction
                        if (FullOutput >= 0) {
                            PartLoadFrac = 0.0;
                        } else {
                            OutletTempDXCoil = DXCoils::DXCoilOutletTemp(this->m_CoolingCoilIndex);
                            OutletHumRatDXCoil = DXCoils::DXCoilOutletHumRat(this->m_CoolingCoilIndex);
                            // If sensible load and setpoint cannot be met, set PLR = 1. if no sensible load and
                            // latent load exists and setpoint cannot be met, set PLR = 1.
                            // why is our logic different? Did we figure something out that reduced the logic?
                            //                  IF ((SensibleLoad .and. LatentLoad .AND. .NOT. UnitarySystem(UnitarySysNum)%RunOnLatentLoad .AND. &
                            //                       OutletHumRatDXCoil >= DesOutHumRat)) THEN
                            if ((OutletTempDXCoil > (DesOutTemp - (Acc * 2.0)) && SensibleLoad && this->m_RunOnSensibleLoad) ||
                                (OutletHumRatDXCoil > (DesOutHumRat - (HumRatAcc * 2.0)) && !SensibleLoad && LatentLoad && this->m_RunOnLatentLoad)) {
                                PartLoadFrac = 1.0;
                                //                  ELSEIF ((SensibleLoad .and. LatentLoad .AND. .NOT. UnitarySystem(UnitarySysNum)%RunOnLatentLoad
                                //                  .AND. &
                                //                       OutletHumRatDXCoil < DesOutHumRat)) THEN
                            } else if (!SensibleLoad && (OutletHumRatDXCoil < DesOutHumRat && LatentLoad && this->m_RunOnLatentLoad)) {
                                PartLoadFrac = ReqOutput / FullOutput;
                                Par[1] = double(this->m_CoolingCoilIndex);
                                Par[2] = DesOutHumRat;
                                // dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                                Par[3] = double(DehumidMode);
                                Par[4] = double(FanOpMode);
                                General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->multiModeDXCoilHumRatResidual, 0.0, 1.0, Par);
                            } else { // must be a sensible load so find PLR
                                PartLoadFrac = ReqOutput / FullOutput;
                                Par[1] = double(this->m_CoolingCoilIndex);
                                Par[2] = DesOutTemp;
                                // Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                                Par[3] = double(DehumidMode);
                                Par[4] = double(FanOpMode);
                                General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->multiModeDXCoilResidual, 0.0, 1.0, Par);
                            }
                        }
                        this->m_CompPartLoadRatio = PartLoadFrac;

                    } else {
                    }
                } // END IF humidity ratio setpoint not met - Multimode humidity control

                // IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
                // then overcool to meet moisture load

                if ((OutletHumRatDXCoil > DesOutHumRat) && (PartLoadFrac < 1.0) && LatentLoad &&
                    (this->m_DehumidControlType_Num == DehumCtrlType::CoolReheat)) {

                    //           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
                    //           do not run the compressor
                    if ((NoLoadHumRatOut - DesOutHumRat) < HumRatAcc) {
                        // PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
                        //           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
                        //           run the compressor at PartLoadFrac = 1.
                        //        ELSEIF ((DesOutHumRat-FullLoadHumRatOut) .LT. HumRatAcc) THEN
                    } else if (FullLoadHumRatOut > (DesOutHumRat - HumRatAcc)) {
                        PartLoadFrac = 1.0;
                        //           ELSE find the PLR to meet the load

                    } else {

                        if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {

                            Par[1] = double(this->m_CoolingCoilIndex);
                            Par[2] = DesOutHumRat;
                            Par[5] = double(FanOpMode);
                            General::SolveRoot(HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, this->DOE2DXCoilHumRatResidual, 0.0, 1.0, Par);
                            this->m_CompPartLoadRatio = PartLoadFrac;

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) {

                            //               IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
                            //               do not run the compressor
                            if ((NoLoadHumRatOut - DesOutHumRat) < HumRatAcc * 2.0) {
                                // PartLoadFrac = PartLoadFrac; // keep part-load fraction from sensible calculation // Self-assignment commented out
                                //                If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the
                                //                DesOutHumRat, run the compressor at PartLoadFrac = 1.
                            } else if ((DesOutHumRat - FullLoadHumRatOut) < HumRatAcc * 2.0) {
                                PartLoadFrac = 1.0;
                                //               ELSE find the PLR to meet the load
                            } else {
                                Par[1] = double(this->m_CoolingCoilIndex);
                                Par[2] = DesOutHumRat;
                                // FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
                                if (FirstHVACIteration) {
                                    Par[3] = 1.0;
                                } else {
                                    Par[3] = 0.0;
                                }
                                if (HXUnitOn) {
                                    Par[4] = 1.0;
                                } else {
                                    Par[4] = 0.0;
                                }
                                Par[5] = double(FanOpMode);
                                General::SolveRoot(HumRatAcc, MaxIte, SolFla, PartLoadFrac, this->HXAssistedCoolCoilHRResidual, 0.0, 1.0, Par);
                                if (SolFla == -1) {

                                    //                   RegulaFalsi may not find latent PLR when the latent degradation model is used.
                                    //                   IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                                    TempMaxPLR = -0.1;
                                    TempOutletHumRatDXCoil = OutletHumRatDXCoil;
                                    while ((OutletHumRatDXCoil - TempOutletHumRatDXCoil) >= 0.0 && TempMaxPLR <= 1.0) {
                                        //                     find upper limit of LatentPLR
                                        TempMaxPLR += 0.1;
                                        HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(CompName,
                                                                                            FirstHVACIteration,
                                                                                            On,
                                                                                            TempMaxPLR,
                                                                                            this->m_CoolingCoilIndex,
                                                                                            FanOpMode,
                                                                                            HXUnitOn,
                                                                                            _,
                                                                                            economizerFlag);
                                        OutletHumRatDXCoil = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletHumRat(this->m_CoolingCoilIndex);
                                    }
                                    TempMinPLR = TempMaxPLR;
                                    while ((OutletHumRatDXCoil - TempOutletHumRatDXCoil) <= 0.0 && TempMinPLR >= 0.0) {
                                        //                     pull upper limit of LatentPLR DOwn to last valid limit (i.e. latent output still
                                        //                     exceeds SystemMoisuterLoad)
                                        TempMaxPLR = TempMinPLR;
                                        //                     find minimum limit of Latent PLR
                                        TempMinPLR -= 0.01;
                                        HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(CompName,
                                                                                            FirstHVACIteration,
                                                                                            On,
                                                                                            TempMaxPLR,
                                                                                            this->m_CoolingCoilIndex,
                                                                                            FanOpMode,
                                                                                            HXUnitOn,
                                                                                            _,
                                                                                            economizerFlag);
                                        OutletHumRatDXCoil = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletHumRat(this->m_CoolingCoilIndex);
                                    }
                                    //                   tighter boundary of solution has been found, CALL RegulaFalsi a second time
                                    General::SolveRoot(
                                        HumRatAcc, MaxIte, SolFla, PartLoadFrac, this->HXAssistedCoolCoilHRResidual, TempMinPLR, TempMaxPLR, Par);
                                    if (SolFla == -1) {
                                        if (!DataGlobals::WarmupFlag) {
                                            if (this->warnIndex.m_HXAssistedCRLatPLRIter < 1) {
                                                ++this->warnIndex.m_HXAssistedCRLatPLRIter;
                                                ShowWarningError(
                                                    this->UnitType +
                                                    " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " +
                                                    this->Name);
                                                ShowContinueError("Estimated latent part-load ratio  = " +
                                                                  General::RoundSigDigits((ReqOutput / FullOutput), 3));
                                                ShowContinueError("Calculated latent part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                                ShowContinueErrorTimeStamp("The calculated latent part-load ratio will be used and the simulation "
                                                                           "continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                this->UnitType + " \"" + this->Name +
                                                    "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR "
                                                    "statistics follow.",
                                                this->warnIndex.m_HXAssistedCRLatPLRIterIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }

                                    } else if (SolFla == -2) {

                                        PartLoadFrac = ReqOutput / FullOutput;
                                        if (!DataGlobals::WarmupFlag) {
                                            if (this->warnIndex.m_HXAssistedCRLatPLRFail < 1) {
                                                ++this->warnIndex.m_HXAssistedCRLatPLRFail;
                                                ShowWarningError(this->UnitType +
                                                                 " - DX unit latent part-load ratio calculation failed unexpectedly: part-load ratio "
                                                                 "limits exceeded, for unit = " +
                                                                 this->Name);
                                                ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                                ShowContinueErrorTimeStamp(
                                                    "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                this->UnitType + " \"" + this->Name +
                                                    "\" - DX unit latent part-load ratio calculation failed unexpectedly error continues. Latent PLR "
                                                    "statistics follow.",
                                                this->warnIndex.m_HXAssistedCRLatPLRFailIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    }
                                } else if (SolFla == -2) {
                                    PartLoadFrac = ReqOutput / FullOutput;
                                    if (!DataGlobals::WarmupFlag) {
                                        if (this->warnIndex.m_HXAssistedCRLatPLRFail2 < 1) {
                                            ++this->warnIndex.m_HXAssistedCRLatPLRFail2;
                                            ShowWarningError(
                                                this->UnitType +
                                                " - DX unit latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " +
                                                this->Name);
                                            ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                            ShowContinueErrorTimeStamp(
                                                "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            this->UnitType + " \"" + this->Name +
                                                "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics "
                                                "follow.",
                                            this->warnIndex.m_HXAssistedCRLatPLRFailIndex2,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                }
                            }
                            this->m_CompPartLoadRatio = PartLoadFrac;

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                            //               Simulate MultiSpeed DX coil at sensible result
                            DXCoils::SimDXCoilMultiSpeed(CompName, SpeedRatio, CycRatio, this->m_CoolingCoilIndex);

                            OutletHumRatDXCoil = DXCoils::DXCoilOutletHumRat(this->m_CoolingCoilIndex);
                            // IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
                            // then overcool to meet moisture load

                            if (OutletHumRatDXCoil > DesOutHumRat) {

                                CycRatio = 0.0;
                                SpeedRatio = 0.0;

                                DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, 1.0, this->m_CoolingCoilIndex);
                                OutletHumRatLS = DXCoils::DXCoilOutletHumRat(this->m_CoolingCoilIndex);
                                if (OutletHumRatLS > DesOutHumRat) {
                                    CycRatio = 1.0;
                                    DXCoils::SimDXCoilMultiSpeed(CompName, 1.0, 1.0, this->m_CoolingCoilIndex);
                                    OutletHumRatHS = DXCoils::DXCoilOutletHumRat(this->m_CoolingCoilIndex);
                                    if (OutletHumRatHS < DesOutHumRat) {
                                        Par[1] = double(this->m_CoolingCoilIndex);
                                        Par[2] = DesOutHumRat;
                                        General::SolveRoot(HumRatAcc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedHumRatResidual, 0.0, 1.0, Par);
                                    } else {
                                        SpeedRatio = 1.0;
                                    }
                                } else {
                                    SpeedRatio = 0.0;
                                    Par[1] = double(this->m_CoolingCoilIndex);
                                    Par[2] = DesOutHumRat;
                                    General::SolveRoot(HumRatAcc, MaxIte, SolFla, CycRatio, this->DXCoilCyclingHumRatResidual, 0.0, 1.0, Par);
                                }
                            }

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                            DXCoils::SimDXCoilMultiSpeed(CompName, SpeedRatio, CycRatio, this->m_CoolingCoilIndex);
                            OutletHumRatDXCoil = DXCoils::DXCoilOutletHumRat(this->m_CoolingCoilIndex);

                            // IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
                            // then overcool to meet moisture load

                            if (OutletHumRatDXCoil > DesOutHumRat) {

                                CycRatio = 0.0;
                                SpeedRatio = 0.0;

                                DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, 1.0, this->m_CoolingCoilIndex);
                                OutletHumRatLS = DXCoils::DXCoilOutletHumRat(this->m_CoolingCoilIndex);
                                if (OutletHumRatLS > DesOutHumRat) {
                                    CycRatio = 1.0;
                                    DXCoils::SimDXCoilMultiSpeed(CompName, 1.0, 1.0, this->m_CoolingCoilIndex);
                                    OutletHumRatHS = DXCoils::DXCoilOutletHumRat(this->m_CoolingCoilIndex);
                                    if (OutletHumRatHS < DesOutHumRat) {
                                        Par[1] = double(this->m_CoolingCoilIndex);
                                        Par[2] = DesOutHumRat;
                                        Par[3] = ReqOutput;
                                        General::SolveRoot(HumRatAcc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedHumRatResidual, 0.0, 1.0, Par);
                                    } else {
                                        SpeedRatio = 1.0;
                                    }
                                } else {
                                    SpeedRatio = 0.0;
                                    Par[1] = double(this->m_CoolingCoilIndex);
                                    Par[2] = DesOutHumRat;
                                    Par[3] = ReqOutput;
                                    General::SolveRoot(HumRatAcc, MaxIte, SolFla, CycRatio, this->DXCoilCyclingHumRatResidual, 0.0, 1.0, Par);
                                }
                            }
                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {
                            VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                                      this->m_CoolingCoilIndex,
                                                                      this->m_FanOpMode,
                                                                      this->m_MaxONOFFCyclesperHour,
                                                                      this->m_HPTimeConstant,
                                                                      this->m_FanDelayTime,
                                                                      1,
                                                                      CycRatio,
                                                                      SpeedNum,
                                                                      SpeedRatio,
                                                                      ReqOutput,
                                                                      dummy,
                                                                      OnOffAirFlowRatio);
                            OutletHumRatLS = DataLoopNode::Node(this->CoolCoilOutletNodeNum).HumRat;

                            if (OutletHumRatLS > DesOutHumRat) {
                                CycRatio = 1.0;

                                VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                                          this->m_CoolingCoilIndex,
                                                                          this->m_FanOpMode,
                                                                          this->m_MaxONOFFCyclesperHour,
                                                                          this->m_HPTimeConstant,
                                                                          this->m_FanDelayTime,
                                                                          1,
                                                                          1.0,
                                                                          SpeedNum,
                                                                          1.0,
                                                                          ReqOutput,
                                                                          dummy,
                                                                          OnOffAirFlowRatio);

                                OutletHumRatHS = DataLoopNode::Node(this->CoolCoilOutletNodeNum).HumRat;

                                if (OutletHumRatHS < DesOutHumRat) {
                                    Par[1] = double(this->m_CoolingCoilIndex);
                                    Par[2] = DesOutHumRat;
                                    Par[3] = double(this->m_UnitarySysNum);
                                    if (SpeedNum == 1) {
                                        General::SolveRoot(HumRatAcc, MaxIte, SolFla, CycRatio, this->DXCoilCyclingHumRatResidual, 0.0, 1.0, Par);
                                    } else {
                                        General::SolveRoot(HumRatAcc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedHumRatResidual, 0.0, 1.0, Par);
                                    }
                                } else {
                                    if (SpeedNum == 1) {
                                        CycRatio = 1.0;
                                    } else {
                                        SpeedRatio = 1.0;
                                    }
                                }
                            } else {
                                SpeedRatio = 0.0;
                                Par[1] = double(this->m_CoolingCoilIndex);
                                Par[2] = DesOutHumRat;
                                Par[3] = double(this->m_UnitarySysNum);
                                General::SolveRoot(HumRatAcc, MaxIte, SolFla, CycRatio, this->DXCoilVarSpeedHumRatResidual, 0.0, 1.0, Par);
                            }
                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {

                            Par[1] = double(this->m_CoolingCoilIndex);
                            Par[2] = DesOutHumRat;
                            // dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                            Par[3] = double(DehumidMode);
                            Par[4] = double(FanOpMode);
                            General::SolveRoot(Acc, MaxIte, SolFlaLat, PartLoadFrac, this->multiModeDXCoilHumRatResidual, 0.0, 1.0, Par);
                            this->m_CompPartLoadRatio = PartLoadFrac;

                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWater) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed)) { // COIL:COOLING:WATER

                            Par[1] = double(this->m_UnitarySysNum);
                            if (FirstHVACIteration) {
                                Par[2] = 1.0;
                            } else {
                                Par[2] = 0.0;
                            }
                            Par[3] = DesOutHumRat;

                            General::SolveRoot(HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, this->coolWaterHumRatResidual, 0.0, 1.0, Par);

                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP)) {

                            Par[1] = double(this->m_UnitarySysNum);
                            if (FirstHVACIteration) {
                                Par[2] = 1.0;
                            } else {
                                Par[2] = 0.0;
                            }
                            Par[3] = DesOutHumRat;
                            Par[4] = ReqOutput;

                            General::SolveRoot(HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, this->coolWatertoAirHPHumRatResidual, 0.0, 1.0, Par);

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {

                            if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling &&
                                (this->m_TESOpMode != PackagedThermalStorageCoil::OffMode &&
                                 this->m_TESOpMode != PackagedThermalStorageCoil::ChargeOnlyMode)) {
                                Par[1] = double(this->m_UnitarySysNum);
                                Par[2] = 0.0; // DesOutTemp; set to 0 if humrat controlled
                                Par[3] = DesOutHumRat;
                                General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->TESIceStorageCoilOutletResidual, 0.0, 1.0, Par);
                            }

                        } else {
                        }
                    }
                }
            }
        }

        if (SolFla == -1) {
            if (!DataGlobals::WarmupFlag) {
                if (this->warnIndex.m_SensPLRIter < 1) {
                    ++this->warnIndex.m_SensPLRIter;
                    ShowWarningError(this->UnitType + " - Iteration limit exceeded calculating part-load ratio for unit = " + this->Name);
                    ShowContinueError("Estimated part-load ratio  = " + General::RoundSigDigits((ReqOutput / FullOutput), 3));
                    ShowContinueError("Calculated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->UnitType + " \"" + this->Name +
                            "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.",
                        this->warnIndex.m_SensPLRIterIndex,
                        PartLoadFrac,
                        PartLoadFrac);
                }
            }
        } else if (SolFla == -2) {
            PartLoadFrac = ReqOutput / FullOutput;
            if (!DataGlobals::WarmupFlag) {
                if (this->warnIndex.m_SensPLRFail < 1) {
                    ++this->warnIndex.m_SensPLRFail;
                    ShowWarningError(this->UnitType +
                                     " - sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + this->Name);
                    ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->UnitType + " \"" + this->Name +
                            "\" - sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.",
                        this->warnIndex.m_SensPLRFailIndex,
                        PartLoadFrac,
                        PartLoadFrac);
                }
            }
        }

        if (SolFlaLat == -1 && SolFla != -1) {
            if (!DataGlobals::WarmupFlag) {
                if (this->warnIndex.m_LatPLRIter < 1) {
                    ++this->warnIndex.m_LatPLRIter;
                    ShowWarningError(this->UnitType + " - Iteration limit exceeded calculating latent part-load ratio for unit = " + this->Name);
                    ShowContinueError("Estimated part-load ratio   = " + General::RoundSigDigits((ReqOutput / FullOutput), 3));
                    ShowContinueError("Calculated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(
                    this->UnitType + " \"" + this->Name +
                        "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR statistics follow.",
                    this->warnIndex.m_LatPLRIterIndex,
                    PartLoadFrac,
                    PartLoadFrac);
            }
        } else if (SolFlaLat == -2 && SolFla != -2) {
            //               RegulaFalsi returns PLR = minPLR when a solution cannot be found, recalculate PartLoadFrac.
            if (NoLoadHumRatOut - FullLoadHumRatOut != 0.0) {
                PartLoadFrac = (NoLoadHumRatOut - DesOutHumRat) / (NoLoadHumRatOut - FullLoadHumRatOut);
            } else {
                PartLoadFrac = 1.0;
            }
            if (!DataGlobals::WarmupFlag) {
                if (this->warnIndex.m_LatPLRFail < 1) {
                    ++this->warnIndex.m_LatPLRFail;
                    ShowWarningError(this->UnitType +
                                     " - latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + this->Name);
                    ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(this->UnitType + " \"" + this->Name +
                                                   "\" - latent part-load ratio calculation failed error continues. Latent PLR statistics follow.",
                                               this->warnIndex.m_LatPLRFailIndex,
                                               PartLoadFrac,
                                               PartLoadFrac);
            }
        }
        // Set the final results

        if (PartLoadFrac > 1.0) {
            PartLoadFrac = 1.0;
        } else if (PartLoadFrac < 0.0) {
            PartLoadFrac = 0.0;
        }

        this->m_CoolingPartLoadFrac = PartLoadFrac;
        this->m_CoolingSpeedRatio = SpeedRatio;
        this->m_CoolingCycRatio = CycRatio;
        this->m_DehumidificationMode = DehumidMode;

        if (DataAirflowNetwork::SimulateAirflowNetwork > DataAirflowNetwork::AirflowNetworkControlMultizone) {
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF =
                max(DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF, LoopDXCoilMaxRTFSave);
        }

        if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
            this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {
            mdot = PartLoadFrac * this->MaxCoolCoilFluidFlow;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->CoolCoilFluidInletNode,
                                                 this->CoolCoilFluidOutletNodeNum,
                                                 this->CoolCoilLoopNum,
                                                 this->CoolCoilLoopSide,
                                                 this->CoolCoilBranchNum,
                                                 this->CoolCoilCompNum);
        }
    }

    void UnitarySys::controlHeatingSystemToSP(int const AirLoopNum,          // index to air loop
                                              bool const FirstHVACIteration, // First HVAC iteration flag
                                              int &CompOn,                   // compressor on/off control
                                              Real64 &HeatCoilLoad           // load met by heating coil
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        //  Simulate the coil object at the required PLR.

        // METHODOLOGY EMPLOYED:
        //  Calculate operating PLR and adjust speed when using multispeed coils.

        // Locals
        bool const SuppHeatingCoilFlag(false);

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);    // Maximum number of iterations for solver
        Real64 const Acc(1.0e-3); // Accuracy of solver result

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FullOutput; // Sensible capacity (outlet - inlet) when the compressor is on
        Real64 ReqOutput;  // Sensible capacity (outlet - inlet) required to meet load or set point temperature

        std::vector<Real64> Par(11);    // Parameter array passed to solver
        Real64 m_WSHPRuntimeFrac = 0.0; // Run time fraction of water to air hp
        Real64 OutdoorDryBulb = 0.0;    // local variable for OutDryBulbTemp
        Real64 OutdoorHumRat = 0.0;     // local variable for OutHumRat
        Real64 OutdoorPressure = 0.0;   // local variable for OutBaroPress
        Real64 OutdoorWetBulb = 0.0;    // local variable for OutWetBulbTemp
        bool HeatingActive = false;     // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.
        bool CoolingActive = false;     // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.
        Real64 mdot = 0.0;              // water coil water flow rate [kg/s]
        Real64 maxPartLoadFrac = 0.0;   // calculated maximum water side PLR for RegulaFalsi call (when plant limits flow max PLR != 1)

        // Set local variables
        // Retrieve the load on the controlled zone
        int InletNode = this->HeatCoilInletNodeNum;
        int OutletNode = this->HeatCoilOutletNodeNum;
        std::string CompName = this->m_HeatingCoilName;
        int CompIndex = this->m_HeatingCoilIndex;
        int FanOpMode = this->m_FanOpMode;
        Real64 DesOutTemp = this->m_DesiredOutletTemp;

        Real64 LoopHeatingCoilMaxRTFSave = 0.0;
        Real64 LoopDXCoilMaxRTFSave = 0.0;
        if (DataAirflowNetwork::SimulateAirflowNetwork > DataAirflowNetwork::AirflowNetworkControlMultizone) {
            LoopHeatingCoilMaxRTFSave = DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF;
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF = 0.0;
            LoopDXCoilMaxRTFSave = DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF;
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF = 0.0;
        }

        Real64 PartLoadFrac = 0.0;
        Real64 SpeedRatio = 0.0;
        Real64 CycRatio = 0.0;
        int SpeedNum = 0;
        Real64 dummy = 0.0;
        int SolFla = 0;
        bool SensibleLoad = false;
        Real64 SensLoad = 0.0;
        bool LatentLoad = false;
        Real64 OutletTemp = 0.0;

        if (this->m_CondenserNodeNum != 0) {
            OutdoorDryBulb = DataLoopNode::Node(this->m_CondenserNodeNum).Temp;
            if (this->m_CondenserType == DataHVACGlobals::WaterCooled) {
                OutdoorHumRat = DataEnvironment::OutHumRat;
                OutdoorPressure = DataEnvironment::OutBaroPress;
                OutdoorWetBulb = DataEnvironment::OutWetBulbTemp;
            } else {
                OutdoorPressure = DataLoopNode::Node(this->m_CondenserNodeNum).Press;
                // IF node is not connected to anything, pressure = default, use weather data
                if (OutdoorPressure == DataLoopNode::DefaultNodeValues.Press) {
                    OutdoorDryBulb = DataEnvironment::OutDryBulbTemp;
                    OutdoorHumRat = DataEnvironment::OutHumRat;
                    OutdoorPressure = DataEnvironment::OutBaroPress;
                    OutdoorWetBulb = DataEnvironment::OutWetBulbTemp;
                } else {
                    OutdoorHumRat = DataLoopNode::Node(this->m_CondenserNodeNum).HumRat;
                    //     this should use Node%WetBulbTemp or a PSYC function, not OAWB
                    OutdoorWetBulb = DataLoopNode::Node(this->m_CondenserNodeNum).OutAirWetBulb;
                }
            }
        } else {
            OutdoorDryBulb = DataEnvironment::OutDryBulbTemp;
            OutdoorHumRat = DataEnvironment::OutHumRat;
            OutdoorPressure = DataEnvironment::OutBaroPress;
            OutdoorWetBulb = DataEnvironment::OutWetBulbTemp;
        }

        // IF there is a fault of coil SAT Sensor (zrp_Nov2016)
        if (this->m_FaultyCoilSATFlag) {
            // calculate the sensor offset using fault information
            int FaultIndex = this->m_FaultyCoilSATIndex;
            this->m_FaultyCoilSATOffset = FaultsManager::FaultsCoilSATSensor(FaultIndex).CalFaultOffsetAct();
            // update the DesOutTemp
            DesOutTemp -= this->m_FaultyCoilSATOffset;
        }

        // IF DXHeatingSystem is scheduled on and there is flow
        if (ScheduleManager::GetCurrentScheduleValue(this->m_SysAvailSchedPtr) > 0.0 &&
            ScheduleManager::GetCurrentScheduleValue(this->m_HeatingCoilAvailSchPtr) > 0.0 &&
            DataLoopNode::Node(InletNode).MassFlowRate > MinAirMassFlow) {

            // Determine if there is a sensible load on this system
            if (DesOutTemp - DataLoopNode::Node(InletNode).Temp > DataHVACGlobals::TempControlTol) SensibleLoad = true;
            // if a heat pump and other coil is on, disable this coil
            if (this->m_HeatPump && this->m_CoolingPartLoadFrac > 0.0) SensibleLoad = false;

            // disable compressor if OAT is below minimum outdoor temperature
            if (OutdoorDryBulb < this->m_MinOATCompressorHeating) {
                SensibleLoad = false;
            }

            // IF DXHeatingSystem runs with a heating load then set PartLoadFrac on Heating System
            if (SensibleLoad) {

                ReqOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                            (Psychrometrics::PsyHFnTdbW(DesOutTemp, DataLoopNode::Node(InletNode).HumRat) -
                             Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat));
                ReqOutput = max(0.0, ReqOutput);

                // Get no load result
                PartLoadFrac = 0.0;
                m_WSHPRuntimeFrac = 0.0;
                CompOn = 0;

                {
                    auto const SELECT_CASE_var(this->m_HeatingCoilType_Num);

                    if (SELECT_CASE_var == DataHVACGlobals::CoilDX_HeatingEmpirical) {

                        DXCoils::SimDXCoil(CompName, On, FirstHVACIteration, CompIndex, FanOpMode, PartLoadFrac);
                        this->m_CompPartLoadRatio = PartLoadFrac;

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) { // do nothing, user defined coil cannot be controlled

                        UserDefinedComponents::SimCoilUserDefined(CompName, CompIndex, AirLoopNum, HeatingActive, CoolingActive);
                        if (HeatingActive) PartLoadFrac = 1.0;

                    } else if ((SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {

                        this->simMultiSpeedCoils(
                            AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, HeatingCoil, this->m_SpeedNum);

                    } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit)) {

                        this->m_HeatingCoilSensDemand = ReqOutput;
                        VariableSpeedCoils::SimVariableSpeedCoils("",
                                                                  this->m_HeatingCoilIndex,
                                                                  FanOpMode,
                                                                  this->m_MaxONOFFCyclesperHour,
                                                                  this->m_HPTimeConstant,
                                                                  this->m_FanDelayTime,
                                                                  CompOn,
                                                                  CycRatio,
                                                                  SpeedNum,
                                                                  SpeedRatio,
                                                                  SensLoad,
                                                                  dummy);

                    } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater)) {

                        HeatingCoils::SimulateHeatingCoilComponents(
                            CompName, FirstHVACIteration, DataLoopNode::SensedLoadFlagValue, CompIndex, _, _, FanOpMode);

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                        WaterCoils::SimulateWaterCoilComponents(
                            CompName, FirstHVACIteration, this->m_HeatingCoilIndex, _, this->m_FanOpMode, PartLoadFrac);

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                        SteamCoils::SimulateSteamCoilComponents(CompName,
                                                                FirstHVACIteration,
                                                                this->m_HeatingCoilIndex,
                                                                1.0,
                                                                _,
                                                                this->m_FanOpMode,
                                                                PartLoadFrac); // QCoilReq, simulate any load > 0 to get max capacity

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) {

                        if (FirstHVACIteration) this->m_CompPartLoadRatio = 1;
                        WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                        CompIndex,
                                                                        ReqOutput,
                                                                        dummy,
                                                                        FanOpMode,
                                                                        this->m_CompPartLoadRatio,
                                                                        this->m_MaxONOFFCyclesperHour,
                                                                        this->m_HPTimeConstant,
                                                                        this->m_FanDelayTime,
                                                                        0,
                                                                        PartLoadFrac,
                                                                        FirstHVACIteration);
                        this->m_CompPartLoadRatio = PartLoadFrac;
                        this->m_HeatingCoilSensDemand = 0.0;

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHP) {

                        WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                            CompIndex,
                                                            this->MaxHeatAirMassFlow,
                                                            FanOpMode,
                                                            FirstHVACIteration,
                                                            m_WSHPRuntimeFrac,
                                                            this->m_MaxONOFFCyclesperHour,
                                                            this->m_HPTimeConstant,
                                                            this->m_FanDelayTime,
                                                            this->m_InitHeatPump,
                                                            ReqOutput,
                                                            dummy,
                                                            0,
                                                            PartLoadFrac);
                        this->m_CompPartLoadRatio = PartLoadFrac;

                    } else {
                    }
                }

                //     IF outlet temp at no load is within ACC of set point, do not run the coil
                if (std::abs(DataLoopNode::Node(OutletNode).Temp - DesOutTemp) < Acc ||
                    this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                    // do nothing, coil is at the set point.
                } else if ((DataLoopNode::Node(OutletNode).Temp - DesOutTemp) > Acc) { // IF outlet temp is above set point turn off coil
                    PartLoadFrac = 0.0;
                } else { // ELSE get full load result

                    // Get full load result
                    PartLoadFrac = 1.0;
                    m_WSHPRuntimeFrac = 1.0;
                    CompOn = 1;

                    {
                        auto const SELECT_CASE_var(this->m_HeatingCoilType_Num);

                        if (SELECT_CASE_var == DataHVACGlobals::CoilDX_HeatingEmpirical) { // Coil:Heating:DX:SingleSpeed

                            DXCoils::SimDXCoil(CompName, On, FirstHVACIteration, this->m_HeatingCoilIndex, FanOpMode, PartLoadFrac);
                            this->m_CompPartLoadRatio = PartLoadFrac;

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) {

                            //  should never get here, coil cannot be controlled and has already been simulated

                        } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) {

                            CycRatio = 1.0;
                            SpeedRatio = 0.0;
                            for (SpeedNum = 1; SpeedNum <= this->m_NumOfSpeedHeating; ++SpeedNum) {
                                if (SpeedNum > 1) CycRatio = 0.0;
                                if (SpeedNum > 1) SpeedRatio = 1.0;
                                this->m_HeatingSpeedNum = SpeedNum;
                                this->simMultiSpeedCoils(
                                    AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, HeatingCoil, SpeedNum);
                                OutletTemp = DataLoopNode::Node(OutletNode).Temp;
                                if (OutletTemp > DesOutTemp && SensibleLoad) break;
                            }

                        } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) ||
                                   (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {

                            CycRatio = 1.0;
                            SpeedRatio = 1.0;
                            SensLoad = 1.0; // turns on coil
                            this->m_HeatingSpeedRatio = SpeedRatio;
                            this->m_HeatingPartLoadFrac = PartLoadFrac;
                            for (SpeedNum = 1; SpeedNum <= this->m_NumOfSpeedHeating; ++SpeedNum) {
                                this->m_HeatingSpeedNum = SpeedNum;
                                this->simMultiSpeedCoils(
                                    AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, HeatingCoil, SpeedNum);
                                OutletTemp = DataLoopNode::Node(OutletNode).Temp;
                                SpeedRatio = double(SpeedNum) - 1.0;
                                if (OutletTemp > DesOutTemp && SensibleLoad) break;
                            }

                        } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) ||
                                   (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit)) {

                            CycRatio = 1.0;
                            SpeedRatio = 1.0;
                            SensLoad = 1.0; // turns on coil
                            this->m_HeatingSpeedRatio = SpeedRatio;
                            this->m_HeatingPartLoadFrac = PartLoadFrac;
                            for (SpeedNum = 1; SpeedNum <= this->m_NumOfSpeedHeating; ++SpeedNum) {
                                this->m_HeatingSpeedNum = SpeedNum;
                                VariableSpeedCoils::SimVariableSpeedCoils("",
                                                                          this->m_HeatingCoilIndex,
                                                                          FanOpMode,
                                                                          this->m_MaxONOFFCyclesperHour,
                                                                          this->m_HPTimeConstant,
                                                                          this->m_FanDelayTime,
                                                                          CompOn,
                                                                          CycRatio,
                                                                          SpeedNum,
                                                                          SpeedRatio,
                                                                          SensLoad,
                                                                          dummy);
                                OutletTemp = DataLoopNode::Node(OutletNode).Temp;
                                if (OutletTemp > DesOutTemp && SensibleLoad) break;
                            }

                        } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) ||
                                   (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric)) {

                            HeatingCoils::SimulateHeatingCoilComponents(
                                CompName, FirstHVACIteration, this->m_DesignHeatingCapacity, CompIndex, _, _, FanOpMode);

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater) {

                            HeatingCoils::SimulateHeatingCoilComponents(CompName, FirstHVACIteration, ReqOutput, CompIndex, _, _, FanOpMode);

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                            mdot = this->MaxHeatCoilFluidFlow;
                            PlantUtilities::SetComponentFlowRate(mdot,
                                                                 this->HeatCoilFluidInletNode,
                                                                 this->HeatCoilFluidOutletNodeNum,
                                                                 this->HeatCoilLoopNum,
                                                                 this->HeatCoilLoopSide,
                                                                 this->HeatCoilBranchNum,
                                                                 this->HeatCoilCompNum);

                            WaterCoils::SimulateWaterCoilComponents(
                                CompName, FirstHVACIteration, this->m_HeatingCoilIndex, _, this->m_FanOpMode, PartLoadFrac);

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                            mdot = this->MaxHeatCoilFluidFlow;
                            PlantUtilities::SetComponentFlowRate(mdot,
                                                                 this->HeatCoilFluidInletNode,
                                                                 this->HeatCoilFluidOutletNodeNum,
                                                                 this->HeatCoilLoopNum,
                                                                 this->HeatCoilLoopSide,
                                                                 this->HeatCoilBranchNum,
                                                                 this->HeatCoilCompNum);

                            SteamCoils::SimulateSteamCoilComponents(CompName,
                                                                    FirstHVACIteration,
                                                                    this->m_HeatingCoilIndex,
                                                                    1.0,
                                                                    _,
                                                                    this->m_FanOpMode,
                                                                    PartLoadFrac); // QCoilReq, simulate any load > 0 to get max capacity

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) {

                            WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                            CompIndex,
                                                                            ReqOutput,
                                                                            dummy,
                                                                            FanOpMode,
                                                                            m_WSHPRuntimeFrac,
                                                                            this->m_MaxONOFFCyclesperHour,
                                                                            this->m_HPTimeConstant,
                                                                            this->m_FanDelayTime,
                                                                            1,
                                                                            PartLoadFrac,
                                                                            FirstHVACIteration);
                            this->m_HeatingCoilSensDemand = ReqOutput;
                            this->m_CompPartLoadRatio = PartLoadFrac;

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHP) {
                            WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                                CompIndex,
                                                                this->MaxHeatAirMassFlow,
                                                                FanOpMode,
                                                                FirstHVACIteration,
                                                                m_WSHPRuntimeFrac,
                                                                this->m_MaxONOFFCyclesperHour,
                                                                this->m_HPTimeConstant,
                                                                this->m_FanDelayTime,
                                                                this->m_InitHeatPump,
                                                                ReqOutput,
                                                                dummy,
                                                                0,
                                                                PartLoadFrac);
                            this->m_CompPartLoadRatio = PartLoadFrac;

                        } else {
                        }
                    }

                    FullOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                                 (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(InletNode).HumRat) -
                                  Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat));

                    //       If the outlet temp is within ACC of set point,
                    if (std::abs(DataLoopNode::Node(OutletNode).Temp - DesOutTemp) < Acc ||
                        this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                        // do nothing, coil is at set point
                    } else if (DataLoopNode::Node(OutletNode).Temp < (DesOutTemp - Acc)) { // IF outlet temp is below set point coil must be on
                        PartLoadFrac = 1.0;
                    } else { // ELSE find the PLR to meet the set point

                        {
                            auto const SELECT_CASE_var(this->m_HeatingCoilType_Num);

                            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_HeatingEmpirical) { // Coil:Heating:DX:SingleSpeed

                                Par[1] = double(CompIndex);
                                Par[2] = DesOutTemp;
                                Par[3] = 1.0;               // OnOffAirFlowFrac assume = 1.0 for continuous fan dx system
                                Par[5] = double(FanOpMode); // this does nothing since set point based control requires constant fan
                                General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->DXHeatingCoilResidual, 0.0, 1.0, Par);
                                this->m_CompPartLoadRatio = PartLoadFrac;

                            } else if ((SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {

                                Par[1] = double(this->m_HeatingCoilIndex);
                                Par[2] = DesOutTemp;
                                Par[3] = double(this->m_UnitarySysNum);
                                // Par(4) = CycRatio or SpeedRatio
                                Par[5] = this->m_HeatingSpeedNum;
                                Par[6] = double(FanOpMode);
                                Par[7] = 1.0; // UnitarySystem(UnitarySysNum)%CompOp
                                Par[8] = ReqOutput;
                                if (this->m_HeatingSpeedNum > 1.0) {
                                    Par[4] = CycRatio;
                                    General::SolveRoot(Acc, MaxIte, SolFla, SpeedRatio, this->heatingCoilVarSpeedResidual, 0.0, 1.0, Par);
                                    this->m_HeatingCycRatio = CycRatio;
                                    this->m_HeatingSpeedRatio = SpeedRatio;
                                    this->m_HeatingPartLoadFrac = SpeedRatio;
                                    this->calcPassiveSystem(AirLoopNum, FirstHVACIteration);
                                    PartLoadFrac = SpeedRatio;
                                } else {
                                    SpeedRatio = 0.0;
                                    this->m_HeatingSpeedRatio = SpeedRatio;
                                    Par[4] = SpeedRatio;
                                    General::SolveRoot(Acc, MaxIte, SolFla, CycRatio, this->heatingCoilVarSpeedCycResidual, 0.0, 1.0, Par);
                                    this->m_HeatingCycRatio = CycRatio;
                                    this->m_HeatingPartLoadFrac = CycRatio;
                                    this->calcPassiveSystem(AirLoopNum, FirstHVACIteration);
                                    PartLoadFrac = CycRatio;
                                }

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) {

                                HeatingCoils::SimulateHeatingCoilComponents(
                                    this->m_HeatingCoilName, FirstHVACIteration, ReqOutput, CompIndex, _, true, FanOpMode, PartLoadFrac);
                                PartLoadFrac = ReqOutput / FullOutput;
                                HeatCoilLoad = ReqOutput;

                            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater)) {

                                Par[1] = double(this->m_UnitarySysNum);
                                if (FirstHVACIteration) {
                                    Par[2] = 1.0;
                                } else {
                                    Par[2] = 0.0;
                                }
                                Par[3] = DesOutTemp;
                                if (SuppHeatingCoilFlag) {
                                    Par[4] = 1.0;
                                } else {
                                    Par[4] = 0.0;
                                }
                                Par[5] = double(FanOpMode);
                                General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->gasElecHeatingCoilResidual, 0.0, 1.0, Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                                Par[1] = double(this->m_UnitarySysNum);
                                if (FirstHVACIteration) {
                                    Par[2] = 1.0;
                                } else {
                                    Par[2] = 0.0;
                                }
                                Par[3] = DesOutTemp;
                                if (SuppHeatingCoilFlag) {
                                    Par[4] = 1.0;
                                } else {
                                    Par[4] = 0.0;
                                }
                                Par[5] = 0.0;

                                // calculate max waterside PLR from mdot request above in case plant chokes water flow
                                maxPartLoadFrac =
                                    min(1.0,
                                        ((mdot / this->MaxHeatCoilFluidFlow) +
                                         0.001)); // plant can limit flow and RegulaFalsi could hit max iteration limit (leave a little slop, 0.001)
                                General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->hotWaterHeatingCoilResidual, 0.0, maxPartLoadFrac, Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                                Par[1] = double(this->m_UnitarySysNum);
                                if (FirstHVACIteration) {
                                    Par[2] = 1.0;
                                } else {
                                    Par[2] = 0.0;
                                }
                                Par[3] = DesOutTemp;
                                if (SuppHeatingCoilFlag) {
                                    Par[4] = 1.0;
                                } else {
                                    Par[4] = 0.0;
                                }

                                // calculate max waterside PLR from mdot request above in case plant chokes water flow
                                maxPartLoadFrac =
                                    min(1.0,
                                        ((mdot / this->MaxHeatCoilFluidFlow) +
                                         0.001)); // plant can limit flow and RegulaFalsi could hit max iteration limit (leave a little slop, 0.001)
                                General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->steamHeatingCoilResidual, 0.0, maxPartLoadFrac, Par);

                            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHP)) {

                                Par[1] = double(this->m_UnitarySysNum);
                                if (FirstHVACIteration) {
                                    Par[2] = 1.0;
                                } else {
                                    Par[2] = 0.0;
                                }
                                Par[3] = DesOutTemp;
                                Par[4] = ReqOutput;
                                this->m_HeatingCoilSensDemand = ReqOutput;

                                General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->heatWatertoAirHPTempResidual, 0.0, 1.0, Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) {

                                // should never get here, user defined coil cannot be controlled and has already been simulated

                            } else {
                                ShowMessage(" For :" + this->UnitType + "=\"" + this->Name + "\"");
                                ShowFatalError("ControlHeatingSystemToSP: Invalid heating coil type = " +
                                               DataHVACGlobals::cAllCoilTypes(this->m_HeatingCoilType_Num));
                            }
                        }
                    }
                }
            }
        }

        if (PartLoadFrac > 1.0) {
            PartLoadFrac = 1.0;
        } else if (PartLoadFrac < 0.0) {
            PartLoadFrac = 0.0;
        }

        if (SolFla < 0) {
            if (SolFla == -1) {
                if (!DataGlobals::WarmupFlag) {
                    if (this->warnIndex.m_HeatCoilSensPLRIter < 1) {
                        ++this->warnIndex.m_HeatCoilSensPLRIter;
                        ShowWarningError(this->UnitType +
                                         " - Iteration limit exceeded calculating sensible part-load ratio for unit = " + this->Name);
                        ShowContinueError("Estimated part-load ratio  = " + General::RoundSigDigits((ReqOutput / FullOutput), 3));
                        ShowContinueError("Calculated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                        ShowContinueErrorTimeStamp("The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(
                            this->UnitType + " \"" + this->Name +
                                "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.",
                            this->warnIndex.m_HeatCoilSensPLRIterIndex,
                            PartLoadFrac,
                            PartLoadFrac);
                    }
                }
            } else if (SolFla == -2) {
                PartLoadFrac = ReqOutput / FullOutput;
                if (!DataGlobals::WarmupFlag) {
                    if (this->warnIndex.m_HeatCoilSensPLRFail < 1) {
                        ++this->warnIndex.m_HeatCoilSensPLRFail;
                        ShowWarningError(this->UnitType +
                                         " - sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + this->Name);
                        ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                        ShowContinueErrorTimeStamp("The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(
                            this->UnitType + " \"" + this->Name +
                                "\" - sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.",
                            this->warnIndex.m_HeatCoilSensPLRFailIndex,
                            PartLoadFrac,
                            PartLoadFrac);
                    }
                }
            }
        }

        // Set the final results
        this->m_HeatingPartLoadFrac = PartLoadFrac;
        this->m_HeatingSpeedRatio = SpeedRatio;
        this->m_HeatingCycRatio = CycRatio;

        if (DataAirflowNetwork::SimulateAirflowNetwork > DataAirflowNetwork::AirflowNetworkControlMultizone) {
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF =
                max(DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF, LoopHeatingCoilMaxRTFSave);
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF =
                max(DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF, LoopDXCoilMaxRTFSave);
        }

        if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater || this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
            mdot = PartLoadFrac * this->MaxHeatCoilFluidFlow;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->HeatCoilFluidInletNode,
                                                 this->HeatCoilFluidOutletNodeNum,
                                                 this->HeatCoilLoopNum,
                                                 this->HeatCoilLoopSide,
                                                 this->HeatCoilBranchNum,
                                                 this->HeatCoilCompNum);
        }
    }

    void UnitarySys::controlSuppHeatSystemToSP(int const AirLoopNum,         // index to air loop
                                               bool const FirstHVACIteration // First HVAC iteration flag
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013
        //       MODIFIED       Nov. 2016, R. Zhang, LBNL. Applied the coil supply air temperature sensor offset fault model
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        //  This subroutine updates the System outlet nodes.

        // METHODOLOGY EMPLOYED:
        //  Data is moved from the System data structure to the System outlet nodes.

        // Locals
        const bool SuppHeatingCoilFlag(true);

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const MaxIte(500);    // Maximum number of iterations for solver
        Real64 const Acc(1.0e-3); // Accuracy of solver result
        int const SolveMaxIter(50);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 FullOutput = 0.0;      // Sensible capacity (outlet - inlet) when the compressor is on
        Real64 ReqOutput = 0.0;       // Sensible capacity (outlet - inlet) required to meet load or set point temperature
        Real64 QCoilActual = 0.0;     // Heating coil operating capacity [W]
        std::vector<Real64> Par(8);   // Parameter array passed to solver
        Real64 NoLoadTempOut = 0.0;   // save outlet temp when coil is off (C)
        bool HeatingActive = false;   // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.
        bool CoolingActive = false;   // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.
        Real64 mdot = 0.0;            // water coil water flow rate [kg/s]
        Real64 maxPartLoadFrac = 0.0; // calculated maximum water side PLR for RegulaFalsi call (when plant limits flow max PLR != 1)
        Real64 PartLoadFrac = 0.0;
        int SolFla = 0.0;
        bool SensibleLoad = false;

        // Set local variables
        int OutletNode = this->m_SuppCoilAirOutletNode;
        int InletNode = this->m_SuppCoilAirInletNode;
        Real64 DesOutTemp = this->m_DesiredOutletTemp;
        std::string CompName = this->m_SuppHeatCoilName;
        int CompIndex = this->m_SuppHeatCoilIndex;
        int FanOpMode = this->m_FanOpMode;

        Real64 LoopHeatingCoilMaxRTFSave = 0.0;
        Real64 LoopDXCoilMaxRTFSave = 0.0;
        if (DataAirflowNetwork::SimulateAirflowNetwork > DataAirflowNetwork::AirflowNetworkControlMultizone) {
            LoopHeatingCoilMaxRTFSave = DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF;
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF = 0.0;
            LoopDXCoilMaxRTFSave = DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF;
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF = 0.0;
        }

        // IF there is a fault of coil SAT Sensor (zrp_Nov2016)
        if (this->m_FaultyCoilSATFlag) {
            // calculate the sensor offset using fault information
            int FaultIndex = this->m_FaultyCoilSATIndex;
            this->m_FaultyCoilSATOffset = FaultsManager::FaultsCoilSATSensor(FaultIndex).CalFaultOffsetAct();
            // update the DesOutTemp
            DesOutTemp -= this->m_FaultyCoilSATOffset;
        }

        if ((ScheduleManager::GetCurrentScheduleValue(this->m_SysAvailSchedPtr) > 0.0) &&
            (DataLoopNode::Node(InletNode).MassFlowRate > MinAirMassFlow)) {

            // Determine if there is a sensible load on this system
            if ((DataLoopNode::Node(InletNode).Temp < DesOutTemp) &&
                (std::abs(DataLoopNode::Node(InletNode).Temp - DesOutTemp) > DataHVACGlobals::TempControlTol))
                SensibleLoad = true;

            if (SensibleLoad) {

                ReqOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                            (Psychrometrics::PsyHFnTdbW(DesOutTemp, DataLoopNode::Node(InletNode).HumRat) -
                             Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat));

                // Get no load result
                PartLoadFrac = 0.0;

                {
                    auto const SELECT_CASE_var(this->m_SuppHeatCoilType_Num);

                    if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) ||
                        (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric) ||
                        (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater)) {
                        HeatingCoils::SimulateHeatingCoilComponents(CompName,
                                                                    FirstHVACIteration,
                                                                    DataLoopNode::SensedLoadFlagValue,
                                                                    CompIndex,
                                                                    QCoilActual,
                                                                    SuppHeatingCoilFlag,
                                                                    FanOpMode,
                                                                    PartLoadFrac); // QCoilReq= 0.0d0,  &
                        if (!(SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater)) {
                            PartLoadFrac = QCoilActual / this->m_DesignSuppHeatingCapacity;
                        } else {
                            if (QCoilActual > DataHVACGlobals::SmallLoad) {
                                PartLoadFrac = 1.0;
                            } else {
                                PartLoadFrac = 0.0;
                            }
                        }
                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                        WaterCoils::SimulateWaterCoilComponents(
                            CompName, FirstHVACIteration, this->m_SuppHeatCoilIndex, _, this->m_FanOpMode, PartLoadFrac);

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                        SteamCoils::SimulateSteamCoilComponents(CompName,
                                                                FirstHVACIteration,
                                                                this->m_SuppHeatCoilIndex,
                                                                1.0,
                                                                _,
                                                                this->m_FanOpMode,
                                                                PartLoadFrac); // QCoilReq, simulate any load > 0 to get max capacity

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) { // do nothing, user defined coil cannot be controlled
                        UserDefinedComponents::SimCoilUserDefined(CompName, CompIndex, AirLoopNum, HeatingActive, CoolingActive);
                        if (HeatingActive) PartLoadFrac = 1.0;

                    } else {
                    }
                }

                NoLoadTempOut = DataLoopNode::Node(OutletNode).Temp;
                //      NoOutput = DataLoopNode::Node(InletNode)%MassFlowRate *  &
                //                       (PsyHFnTdbW(NoLoadTempOut,Node(OutletNode)%HumRat)  &
                //                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

                //     If OutletTemp is within ACC of set point, either coil operated or is not needed
                if (std::abs(DataLoopNode::Node(OutletNode).Temp - DesOutTemp) < Acc ||
                    this->m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                    // do nothing, coil is at set point (i.e., gas/elec/steam/user coil will try to hit set point)
                } else if (PartLoadFrac > 0.0) {
                    // do nothing, coil tried to hit set point (i.e., gas/elec/steam/user coil tried to hit set point but missed
                } else if (NoLoadTempOut > (DesOutTemp - Acc)) {
                    PartLoadFrac = 0.0; // outlet temp > set point, coil is not needed
                } else {                // outlet temp too low, turn on coil

                    // Get full load result
                    PartLoadFrac = 1.0;

                    {
                        auto const SELECT_CASE_var(this->m_SuppHeatCoilType_Num);

                        if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) ||
                            (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric)) {

                            HeatingCoils::SimulateHeatingCoilComponents(CompName,
                                                                        FirstHVACIteration,
                                                                        DataLoopNode::SensedLoadFlagValue,
                                                                        CompIndex,
                                                                        QCoilActual,
                                                                        SuppHeatingCoilFlag,
                                                                        FanOpMode,
                                                                        PartLoadFrac);
                            PartLoadFrac = QCoilActual / this->m_DesignSuppHeatingCapacity;

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater) {

                            HeatingCoils::SimulateHeatingCoilComponents(
                                CompName, FirstHVACIteration, DataLoopNode::SensedLoadFlagValue, CompIndex, _, SuppHeatingCoilFlag, FanOpMode);

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                            mdot = this->m_MaxSuppCoilFluidFlow;
                            PlantUtilities::SetComponentFlowRate(mdot,
                                                                 this->m_SuppCoilFluidInletNode,
                                                                 this->m_SuppCoilFluidOutletNodeNum,
                                                                 this->m_SuppCoilLoopNum,
                                                                 this->m_SuppCoilLoopSide,
                                                                 this->m_SuppCoilBranchNum,
                                                                 this->m_SuppCoilCompNum);

                            WaterCoils::SimulateWaterCoilComponents(
                                CompName, FirstHVACIteration, this->m_SuppHeatCoilIndex, _, this->m_FanOpMode, PartLoadFrac);

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                            mdot = this->m_MaxSuppCoilFluidFlow;
                            PlantUtilities::SetComponentFlowRate(mdot,
                                                                 this->m_SuppCoilFluidInletNode,
                                                                 this->m_SuppCoilFluidOutletNodeNum,
                                                                 this->m_SuppCoilLoopNum,
                                                                 this->m_SuppCoilLoopSide,
                                                                 this->m_SuppCoilBranchNum,
                                                                 this->m_SuppCoilCompNum);

                            SteamCoils::SimulateSteamCoilComponents(CompName,
                                                                    FirstHVACIteration,
                                                                    this->m_SuppHeatCoilIndex,
                                                                    1.0,
                                                                    _,
                                                                    this->m_FanOpMode,
                                                                    PartLoadFrac); // QCoilReq, simulate any load > 0 to get max capacity

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) {

                            //  should never get here, coil has already been simulated

                        } else {
                        }
                    }

                    FullOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                                 (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(InletNode).HumRat) -
                                  Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat));

                    //         If the FullOutput outlet temp is less than (insufficient heating) or very near set point,
                    //         run the coil at PartLoadFrac = 1.
                    if (DataLoopNode::Node(OutletNode).Temp < (DesOutTemp + Acc)) {
                        PartLoadFrac = 1.0;
                    } else {

                        {
                            auto const SELECT_CASE_var(this->m_SuppHeatCoilType_Num);

                            if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) ||
                                (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric) ||
                                (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater)) {

                                Par[1] = double(this->m_UnitarySysNum);
                                if (FirstHVACIteration) {
                                    Par[2] = 1.0;
                                } else {
                                    Par[2] = 0.0;
                                }
                                Par[3] = DesOutTemp;
                                if (SuppHeatingCoilFlag) {
                                    Par[4] = 1.0;
                                } else {
                                    Par[4] = 0.0;
                                }
                                Par[5] = double(FanOpMode);
                                General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->gasElecHeatingCoilResidual, 0.0, 1.0, Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                                Par[1] = double(this->m_UnitarySysNum);
                                if (FirstHVACIteration) {
                                    Par[2] = 1.0;
                                } else {
                                    Par[2] = 0.0;
                                }
                                Par[3] = DesOutTemp;
                                if (SuppHeatingCoilFlag) {
                                    Par[4] = 1.0;
                                } else {
                                    Par[4] = 0.0;
                                }
                                Par[5] = 0.0;

                                // calculate max waterside PLR from mdot request above in case plant chokes water flow
                                maxPartLoadFrac =
                                    min(1.0,
                                        ((mdot / this->m_MaxSuppCoilFluidFlow) +
                                         0.001)); // plant can limit flow and RegulaFalsi could hit max iteration limit (leave a little slop, 0.001)
                                General::SolveRoot(
                                    Acc, SolveMaxIter, SolFla, PartLoadFrac, this->hotWaterHeatingCoilResidual, 0.0, maxPartLoadFrac, Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                                Par[1] = double(this->m_UnitarySysNum);
                                if (FirstHVACIteration) {
                                    Par[2] = 1.0;
                                } else {
                                    Par[2] = 0.0;
                                }
                                Par[3] = DesOutTemp;
                                if (SuppHeatingCoilFlag) {
                                    Par[4] = 1.0;
                                } else {
                                    Par[4] = 0.0;
                                }

                                // calculate max waterside PLR from mdot request above in case plant chokes water flow
                                maxPartLoadFrac =
                                    min(1.0,
                                        ((mdot / this->m_MaxSuppCoilFluidFlow) +
                                         0.001)); // plant can limit flow and RegulaFalsi could hit max iteration limit (leave a little slop, 0.001)
                                General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->steamHeatingCoilResidual, 0.0, maxPartLoadFrac, Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) {

                                //  do nothing, coil has already been simulated

                            } else {
                            }
                        }

                    } // IF ((FullOutput - ReqOutput) < Acc) THEN
                }     // IF ((NoOutput-ReqOutput) > Acc) THEN
            }         // IF (SensibleLoad ) THEN
        }             // IF((GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%m_SysAvailSchedPtr) > 0.0d0) .AND. &

        if (PartLoadFrac > 1.0) {
            PartLoadFrac = 1.0;
        } else if (PartLoadFrac < 0.0) {
            PartLoadFrac = 0.0;
        }

        if (SolFla == -1) {
            if (!DataGlobals::WarmupFlag) {
                if (this->warnIndex.m_SuppHeatCoilSensPLRIter < 1) {
                    ++this->warnIndex.m_SuppHeatCoilSensPLRIter;
                    ShowWarningError(this->UnitType + " - Iteration limit exceeded calculating sensible part-load ratio for unit = " + this->Name);
                    ShowContinueError("Estimated part-load ratio  = " + General::RoundSigDigits((ReqOutput / FullOutput), 3));
                    ShowContinueError("Calculated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->UnitType + " \"" + this->Name +
                            "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.",
                        this->warnIndex.m_SuppHeatCoilSensPLRIterIndex,
                        PartLoadFrac,
                        PartLoadFrac);
                }
            } // IF(.NOT. WarmupFlag)THEN
        } else if (SolFla == -2) {
            PartLoadFrac = ReqOutput / FullOutput;
            if (!DataGlobals::WarmupFlag) {
                if (this->warnIndex.m_SuppHeatCoilSensPLRFail < 1) {
                    ++this->warnIndex.m_SuppHeatCoilSensPLRFail;
                    ShowWarningError(this->UnitType +
                                     " - sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + this->Name);
                    ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->UnitType + " \"" + this->Name +
                            "\" - sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.",
                        this->warnIndex.m_SuppHeatCoilSensPLRFailIndex,
                        PartLoadFrac,
                        PartLoadFrac);
                }
            } // IF(.NOT. WarmupFlag)THEN
        }     // IF (SolFla == -1) THEN

        this->m_SuppHeatPartLoadFrac = PartLoadFrac;

        // LoopHeatingCoilMaxRTF used for AirflowNetwork gets set in child components (gas and fuel)
        if (DataAirflowNetwork::SimulateAirflowNetwork > DataAirflowNetwork::AirflowNetworkControlMultizone) {
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF =
                max(DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopHeatingCoilMaxRTF, LoopHeatingCoilMaxRTFSave);
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF =
                max(DataAirLoop::AirLoopAFNInfo(AirLoopNum).AFNLoopDXCoilRTF, LoopDXCoilMaxRTFSave);
        }

        if (this->m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater ||
            this->m_SuppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
            mdot = PartLoadFrac * this->m_MaxSuppCoilFluidFlow;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->m_SuppCoilFluidInletNode,
                                                 this->m_SuppCoilFluidOutletNodeNum,
                                                 this->m_SuppCoilLoopNum,
                                                 this->m_SuppCoilLoopSide,
                                                 this->m_SuppCoilBranchNum,
                                                 this->m_SuppCoilCompNum);
        }
    }

    void UnitarySys::simMultiSpeedCoils(int const AirLoopNum,          // Index to air loop
                                        bool const FirstHVACIteration, // True when first HVAC iteration
                                        int &CompOn,                   // compresor on/off control
                                        bool const SensibleLoad,
                                        bool const LatentLoad,
                                        Real64 const PartLoadFrac,
                                        int const CoilType,
                                        int const SpeedNumber)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma, FSEC
        //       DATE WRITTEN   March 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages multispeed and variable speed cooling coil simulation.

        // Locals
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string CompName = ""; // Name of Unitary System object
        Real64 SensLoad = 0.0;
        Real64 LatLoad = 0.0;
        int CoilTypeNum = 0;
        int SpeedNum = 0;
        int CoilOutletNodeNum = 0;
        int CompIndex = 0;
        Real64 SpeedRatio = 0.0;
        Real64 CycRatio = 0.0;

        Real64 dummy = 0.0;

        // if (present(SpeedNumber)) {
        SpeedNum = SpeedNumber;
        //} else {
        //    SpeedNum = 1;
        //}

        if (CoilType == CoolingCoil) {

            CompName = this->m_CoolingCoilName;
            CompIndex = this->m_CoolingCoilIndex;
            CoilTypeNum = this->m_CoolingCoilType_Num;
            CoilOutletNodeNum = this->CoolCoilOutletNodeNum;
            if (SensibleLoad) {
                SensLoad = -1.0;
                CoolingLoad = true;
                HeatingLoad = false;
            }
            if (LatentLoad) LatLoad = -1.0;

        } else {

            CompName = this->m_HeatingCoilName;
            CompIndex = this->m_HeatingCoilIndex;
            CoilTypeNum = this->m_HeatingCoilType_Num;
            CoilOutletNodeNum = this->HeatCoilOutletNodeNum;

            if (SensibleLoad) {
                SensLoad = 1.0;
                CoolingLoad = false;
                HeatingLoad = true;
            } else {
                SensLoad = 0.0;
                HeatingLoad = false;
            }
            LatLoad = 0.0;
            this->m_FanOpMode = 1; // why is this here?
        }

        Real64 OnOffAirFlowRatio = 1.0;
        this->setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadFrac); // 1.0d0 = PartLoadRatio

        this->calcPassiveSystem(AirLoopNum, FirstHVACIteration);

        if ((CoilTypeNum == DataHVACGlobals::CoilDX_MultiSpeedCooling) || (CoilTypeNum == DataHVACGlobals::CoilDX_MultiSpeedHeating)) {

            if (CoilType == DataHVACGlobals::Cooling) {
                if (this->m_CoolingSpeedNum <= 1.0) {
                    SpeedRatio = 0.0;
                    CycRatio = PartLoadFrac;
                } else {
                    if (this->m_SingleMode == 0) {
                        SpeedRatio = PartLoadFrac;
                        CycRatio = 0.0;
                    } else {
                        SpeedRatio = 1.0;
                        CycRatio = PartLoadFrac;
                    }
                }
            } else {
                if (this->m_HeatingSpeedNum <= 1.0) {
                    SpeedRatio = 0.0;
                    CycRatio = PartLoadFrac;
                } else {
                    if (this->m_SingleMode == 0) {
                        SpeedRatio = PartLoadFrac;
                        CycRatio = 0.0;
                    } else {
                        SpeedRatio = 1.0;
                        CycRatio = PartLoadFrac;
                    }
                }
            }
            DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, PartLoadFrac, CompIndex, SpeedNum, this->m_FanOpMode, 1, this->m_SingleMode);

        } else if (CoilTypeNum == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {

            VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                      CompIndex,
                                                      this->m_FanOpMode,
                                                      this->m_MaxONOFFCyclesperHour,
                                                      this->m_HPTimeConstant,
                                                      this->m_FanDelayTime,
                                                      CompOn,
                                                      PartLoadFrac,
                                                      SpeedNum,
                                                      this->m_CoolingSpeedRatio,
                                                      SensLoad,
                                                      dummy,
                                                      OnOffAirFlowRatio);

        } else if (CoilTypeNum == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {

            VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                      CompIndex,
                                                      this->m_FanOpMode,
                                                      this->m_MaxONOFFCyclesperHour,
                                                      this->m_HPTimeConstant,
                                                      this->m_FanDelayTime,
                                                      CompOn,
                                                      PartLoadFrac,
                                                      SpeedNum,
                                                      this->m_HeatingSpeedRatio,
                                                      SensLoad,
                                                      dummy,
                                                      OnOffAirFlowRatio);

        } else if (CoilTypeNum == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) {

            VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                      CompIndex,
                                                      this->m_FanOpMode,
                                                      this->m_MaxONOFFCyclesperHour,
                                                      this->m_HPTimeConstant,
                                                      this->m_FanDelayTime,
                                                      CompOn,
                                                      PartLoadFrac,
                                                      SpeedNum,
                                                      this->m_CoolingSpeedRatio,
                                                      SensLoad,
                                                      dummy,
                                                      OnOffAirFlowRatio);

        } else if (CoilTypeNum == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) {

            VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                      CompIndex,
                                                      this->m_FanOpMode,
                                                      this->m_MaxONOFFCyclesperHour,
                                                      this->m_HPTimeConstant,
                                                      this->m_FanDelayTime,
                                                      CompOn,
                                                      PartLoadFrac,
                                                      SpeedNum,
                                                      this->m_HeatingSpeedRatio,
                                                      SensLoad,
                                                      dummy,
                                                      OnOffAirFlowRatio);

        } else if ((CoilTypeNum == DataHVACGlobals::Coil_HeatingElectric_MultiStage) ||
                   (CoilTypeNum == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {

            HeatingCoils::SimulateHeatingCoilComponents(
                CompName, FirstHVACIteration, _, CompIndex, _, _, this->m_FanOpMode, PartLoadFrac, SpeedNum, this->m_HeatingSpeedRatio);
        } else {
        }
    }

    void UnitarySys::calcPassiveSystem(int const AirLoopNum,         // index to air loop
                                       bool const FirstHVACIteration // True when first HVAC iteration
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the set point based output of the unitary system.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 PartLoadRatio = 0.0; // coil operating part-load ratio
        int CompOn = 0;             // compressor control (0=off, 1=on)
        bool HXUnitOn = false;

        Real64 OnOffAirFlowRatio = 1.0;
        Real64 CoilCoolHeatRat = 1.0;
        Real64 HeatCoilLoad = 0.0;
        // CALL the series of components that simulate a Unitary System
        if (this->m_FanExists && this->m_FanPlace == FanPlace::BlowThru) {
            if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->m_FanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->m_FanIndex, FanSpeedRatio);
            }
        }

        if (this->m_CoolingCoilUpstream) {

            if (this->m_CoolCoilExists) {
                PartLoadRatio = this->m_CoolingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                HXUnitOn = false;
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }
            if (this->m_HeatCoilExists) {
                PartLoadRatio = this->m_HeatingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, HeatCoilLoad);
            }

        } else {

            if (this->m_HeatCoilExists) {
                PartLoadRatio = this->m_HeatingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, HeatCoilLoad);
            }
            if (this->m_CoolCoilExists) {
                PartLoadRatio = this->m_CoolingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                HXUnitOn = false;
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }
        }

        if (this->m_FanExists && this->m_FanPlace == FanPlace::DrawThru) {
            if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->m_FanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->m_FanIndex, FanSpeedRatio);
            }
        }

        // CALL reheat coils next
        if (this->m_SuppCoilExists) {
            SuppHeatingCoilFlag = true;
            this->calcUnitarySuppSystemToSP(FirstHVACIteration);
            SuppHeatingCoilFlag = false;
        }
    }

    void UnitarySys::reportUnitarySystem(int const AirLoopNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   July 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the report variable for the coils.

        Real64 ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        Real64 QTotUnitOut = 0.0;
        Real64 QSensUnitOut = 0.0;
        this->m_PartLoadFrac = 0.0;
        this->m_CompPartLoadRatio = 0.0;
        this->m_CycRatio = 0.0;
        this->m_SpeedRatio = 0.0;
        this->FanPartLoadRatio = 0.0;
        this->m_TotalAuxElecPower = 0.0;
        this->m_HeatingAuxElecConsumption = 0.0;
        this->m_CoolingAuxElecConsumption = 0.0;
        this->m_ElecPower = 0.0;
        this->m_ElecPowerConsumption = 0.0;

        int OutletNode = this->AirOutNode;

        Real64 AirMassFlow = DataLoopNode::Node(OutletNode).MassFlowRate;
        auto const SELECT_CASE_var(this->m_ControlType);
        // Noticed that these are calculated differently.
        // That doesn't make sense except that NodeNumOfControlledZone = 0 for set point control because the control zone name is not required.
        if (SELECT_CASE_var == ControlType::Setpoint) {
            if (OutletNode > 0) {
                int InletNode = this->AirInNode;
                Real64 MinHumRatio = DataLoopNode::Node(InletNode).HumRat;
                QSensUnitOut = AirMassFlow * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, MinHumRatio) -
                                              Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, MinHumRatio)) -
                               this->m_SenLoadLoss;
                QTotUnitOut = AirMassFlow * (DataLoopNode::Node(OutletNode).Enthalpy - DataLoopNode::Node(InletNode).Enthalpy);
            }
        } else {
            if (OutletNode > 0 && this->NodeNumOfControlledZone > 0) {
                Real64 MinHumRatio = DataLoopNode::Node(this->NodeNumOfControlledZone).HumRat;
                QSensUnitOut = AirMassFlow * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, MinHumRatio) -
                                              Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(this->NodeNumOfControlledZone).Temp, MinHumRatio)) -
                               this->m_SenLoadLoss;
                QTotUnitOut = AirMassFlow * (DataLoopNode::Node(OutletNode).Enthalpy - DataLoopNode::Node(this->NodeNumOfControlledZone).Enthalpy);
            }
        }

        // set the system part-load ratio report variable
        this->m_PartLoadFrac = max(this->m_CoolingPartLoadFrac, this->m_HeatingPartLoadFrac);
        // set the compressor part-load ratio report variable
        this->m_CompPartLoadRatio = max(this->m_CoolCompPartLoadRatio, this->m_HeatCompPartLoadRatio);

        if (HeatingLoad) {
            if (QTotUnitOut > 0.0) { // heating
                this->m_TotCoolEnergyRate = 0.0;
                this->m_SensCoolEnergyRate = 0.0;
                this->m_LatCoolEnergyRate = 0.0;
                this->m_TotHeatEnergyRate = QTotUnitOut;
                this->m_SensHeatEnergyRate = std::abs(max(0.0, QSensUnitOut));
                this->m_LatHeatEnergyRate = std::abs(max(0.0, (QTotUnitOut - QSensUnitOut)));
            } else {
                this->m_TotCoolEnergyRate = std::abs(QTotUnitOut);
                this->m_SensCoolEnergyRate = std::abs(min(0.0, QSensUnitOut));
                this->m_LatCoolEnergyRate = std::abs(min(0.0, (QTotUnitOut - QSensUnitOut)));
                this->m_TotHeatEnergyRate = 0.0;
                this->m_SensHeatEnergyRate = 0.0;
                this->m_LatHeatEnergyRate = 0.0;
            }
        } else {
            if (QTotUnitOut <= 0.0) { // cooling
                this->m_TotCoolEnergyRate = std::abs(min(0.0, QTotUnitOut));
                this->m_SensCoolEnergyRate = std::abs(min(0.0, QSensUnitOut));
                this->m_LatCoolEnergyRate = std::abs(min(0.0, (QTotUnitOut - QSensUnitOut)));
                this->m_TotHeatEnergyRate = 0.0;
                this->m_SensHeatEnergyRate = 0.0;
                this->m_LatHeatEnergyRate = 0.0;
            } else {
                this->m_TotCoolEnergyRate = 0.0;
                this->m_SensCoolEnergyRate = 0.0;
                this->m_LatCoolEnergyRate = 0.0;
                this->m_TotHeatEnergyRate = QTotUnitOut;
                this->m_SensHeatEnergyRate = std::abs(max(0.0, QSensUnitOut));
                this->m_LatHeatEnergyRate = std::abs(max(0.0, (QTotUnitOut - QSensUnitOut)));
            }
        }

        if (this->m_FanExists && OutletNode > 0) {
            if (CompOnMassFlow > 0.0) {
                this->FanPartLoadRatio = DataLoopNode::Node(OutletNode).MassFlowRate / CompOnMassFlow;
            } else {
                this->FanPartLoadRatio = 0.0;
            }
            if (AirLoopNum > 0) {
                if (this->m_FanOpMode == DataHVACGlobals::CycFanCycCoil) {
                    DataAirLoop::AirLoopFlow(AirLoopNum).FanPLR = this->FanPartLoadRatio;
                } else {
                    DataAirLoop::AirLoopFlow(AirLoopNum).FanPLR = 0.0;
                }
            }
        }

        Real64 locFanElecPower = 0.0;
        if (this->m_FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            locFanElecPower = HVACFan::fanObjs[this->m_FanIndex]->fanPower();
        } else {
            locFanElecPower = Fans::GetFanPower(this->m_FanIndex);
        }

        {
            auto const SELECT_CASE_var(this->m_CoolingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                // need to make sure these are 0 for non-variable speed coils (or not report these variables)
                this->m_CycRatio = max(this->m_CoolingCycRatio, this->m_HeatingCycRatio);
                this->m_SpeedRatio = max(this->m_CoolingSpeedRatio, this->m_HeatingSpeedRatio);
                this->m_SpeedNum = max(this->m_CoolingSpeedNum, this->m_HeatingSpeedNum);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                this->m_CycRatio = max(this->m_CoolingCycRatio, this->m_HeatingCycRatio);
                this->m_SpeedRatio = max(this->m_CoolingSpeedRatio, this->m_HeatingSpeedRatio);
                this->m_SpeedNum = max(this->m_CoolingSpeedNum, this->m_HeatingSpeedNum);

                Real64 CompPartLoadFrac = this->m_CompPartLoadRatio;
                if (CoolingLoad) {

                    this->m_TotalAuxElecPower = this->m_AncillaryOnPower * CompPartLoadFrac + this->m_AncillaryOffPower * (1.0 - CompPartLoadFrac);
                    this->m_CoolingAuxElecConsumption = this->m_AncillaryOnPower * CompPartLoadFrac * ReportingConstant;
                }
                if (this->m_LastMode == CoolingMode) {
                    this->m_CoolingAuxElecConsumption += this->m_AncillaryOffPower * (1.0 - CompPartLoadFrac) * ReportingConstant;
                }
                this->m_ElecPower = locFanElecPower + DataHVACGlobals::DXElecCoolingPower + DataHVACGlobals::DXElecHeatingPower +
                                    DataHVACGlobals::ElecHeatingCoilPower + this->m_TotalAuxElecPower;
                this->m_ElecPowerConsumption = this->m_ElecPower * ReportingConstant;

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWater || SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterDetailed) {

                if (this->m_MultiSpeedCoolingCoil) {
                    this->m_CycRatio = max(this->m_CoolingCycRatio, this->m_HeatingCycRatio);
                    this->m_SpeedRatio = max(this->m_CoolingSpeedRatio, this->m_HeatingSpeedRatio);
                    this->m_SpeedNum = max(this->m_CoolingSpeedNum, this->m_HeatingSpeedNum);
                }
                this->m_ElecPower = locFanElecPower;
                this->m_ElecPowerConsumption = this->m_ElecPower * ReportingConstant;

            } else {
            }
        }

        {
            auto const SELECT_CASE_var(this->m_HeatingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                this->m_CycRatio = max(this->m_CoolingCycRatio, this->m_HeatingCycRatio);
                this->m_SpeedRatio = max(this->m_CoolingSpeedRatio, this->m_HeatingSpeedRatio);
                this->m_SpeedNum = max(this->m_CoolingSpeedNum, this->m_HeatingSpeedNum);

                Real64 CompPartLoadFrac = this->m_CompPartLoadRatio;
                if (HeatingLoad) {

                    this->m_TotalAuxElecPower = this->m_AncillaryOnPower * CompPartLoadFrac + this->m_AncillaryOffPower * (1.0 - CompPartLoadFrac);
                    this->m_HeatingAuxElecConsumption = this->m_AncillaryOnPower * CompPartLoadFrac * ReportingConstant;
                }
                if (this->m_LastMode == HeatingMode) {
                    this->m_HeatingAuxElecConsumption += this->m_AncillaryOffPower * (1.0 - CompPartLoadFrac) * ReportingConstant;
                }
                this->m_ElecPower =
                    locFanElecPower + DataHVACGlobals::DXElecCoolingPower + DataHVACGlobals::DXElecHeatingPower + this->m_TotalAuxElecPower;
                this->m_ElecPowerConsumption = this->m_ElecPower * ReportingConstant;

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage)) {
                this->m_CycRatio = max(this->m_CoolingCycRatio, this->m_HeatingCycRatio);
                this->m_SpeedRatio = max(this->m_CoolingSpeedRatio, this->m_HeatingSpeedRatio);

                this->m_ElecPower =
                    locFanElecPower + DataHVACGlobals::DXElecCoolingPower + DataHVACGlobals::ElecHeatingCoilPower + this->m_TotalAuxElecPower;
                this->m_ElecPowerConsumption = this->m_ElecPower * ReportingConstant;

            } else {
            }
        }

        if (DataAirflowNetwork::SimulateAirflowNetwork == DataAirflowNetwork::AirflowNetworkControlMultiADS ||
            DataAirflowNetwork::SimulateAirflowNetwork == DataAirflowNetwork::AirflowNetworkControlSimpleADS) {
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).LoopSystemOnMassFlowrate = CompOnMassFlow;
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).LoopSystemOffMassFlowrate = CompOffMassFlow;
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).LoopFanOperationMode = this->m_FanOpMode;
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).LoopOnOffFanPartLoadRatio = this->FanPartLoadRatio;
            DataAirLoop::AirLoopAFNInfo(AirLoopNum).LoopCompCycRatio = this->m_CycRatio;
        }

        if (this->m_FirstPass) {

            if (!DataGlobals::SysSizingCalc) {

                if (DataSizing::CurOASysNum > 0) {
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).AirFlow = false;
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).CoolingAirFlow = false;
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).HeatingAirFlow = false;
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).Capacity = false;
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).CoolingCapacity = false;
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).HeatingCapacity = false;
                    this->m_FirstPass = false;
                } else if (DataSizing::CurSysNum > 0) {
                    DataAirLoop::AirLoopControlInfo(DataSizing::CurSysNum).UnitarySysSimulating = false;
                    DataSizing::resetHVACSizingGlobals(DataSizing::CurZoneEqNum, DataSizing::CurSysNum, this->m_FirstPass);
                } else if (DataSizing::CurZoneEqNum > 0) {
                    DataSizing::resetHVACSizingGlobals(DataSizing::CurZoneEqNum, DataSizing::CurSysNum, this->m_FirstPass);
                } else {
                    this->m_FirstPass = false;
                }
            }
        }

        // reset to 1 in case blow through fan configuration (fan resets to 1, but for blow thru fans coil sets back down < 1)
        DataHVACGlobals::OnOffFanPartLoadFraction = 1.0;
        DataSizing::ZoneEqUnitarySys = false;
    }

    void UnitarySys::unitarySystemHeatRecovery()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Chandan Sharma
        //       DATE WRITTEN:    May 2013

        // PURPOSE OF THIS SUBROUTINE:
        //  Calculate the heat recovered from UnitarySystem

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const routineName("UnitarySystemHeatRecovery");

        Real64 ReportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

        int HeatRecInNode = this->m_HeatRecoveryInletNodeNum;
        int HeatRecOutNode = this->m_HeatRecoveryOutletNodeNum;

        Real64 HeatRecInletTemp = DataLoopNode::Node(HeatRecInNode).Temp;
        Real64 HeatRecOutletTemp = 0.0;

        // Set heat recovery mass flow rates
        Real64 HeatRecMassFlowRate = DataLoopNode::Node(HeatRecInNode).MassFlowRate;

        Real64 QHeatRec = DataHVACGlobals::MSHPWasteHeat;

        if (HeatRecMassFlowRate > 0.0) {

            Real64 CpHeatRec = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(this->m_HRLoopNum).FluidName, HeatRecInletTemp, DataPlant::PlantLoop(this->m_HRLoopNum).FluidIndex, routineName);

            HeatRecOutletTemp = QHeatRec / (HeatRecMassFlowRate * CpHeatRec) + HeatRecInletTemp;
            // coil model should be handling max outlet water temp (via limit to heat transfer) since heat rejection needs to be accounted for by the
            // coil
            if (HeatRecOutletTemp > this->m_MaxHROutletWaterTemp) {
                HeatRecOutletTemp = max(HeatRecInletTemp, this->m_MaxHROutletWaterTemp);
                QHeatRec = HeatRecMassFlowRate * CpHeatRec * (HeatRecOutletTemp - HeatRecInletTemp);
            }
        } else {
            HeatRecOutletTemp = HeatRecInletTemp;
            QHeatRec = 0.0;
        }

        PlantUtilities::SafeCopyPlantNode(HeatRecInNode, HeatRecOutNode);

        DataLoopNode::Node(HeatRecOutNode).Temp = HeatRecOutletTemp;

        this->m_HeatRecoveryRate = QHeatRec;
        this->m_HeatRecoveryEnergy = this->m_HeatRecoveryRate * ReportingConstant;
        this->m_HeatRecoveryInletTemp = HeatRecInletTemp;
        this->m_HeatRecoveryOutletTemp = HeatRecOutletTemp;
        this->m_HeatRecoveryMassFlowRate = HeatRecMassFlowRate;
    }

    void UnitarySys::heatPumpRunFrac(Real64 const PLR,   // part load ratio
                                     bool &errFlag,      // part load factor out of range flag
                                     Real64 &RuntimeFrac // the required run time fraction to meet part load
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Kenneth Tang
        //       DATE WRITTEN   Apr 2004

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
        // Ernest OrlanDO Lawrence Berkeley National Laboratory.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 PartLoadFactor; // Part load factor
        Real64 Nmax;           // Maximum cycling rate [cycles/hr]
        Real64 tau;            // Heat pump time constant [s]
        Real64 pr;             // On-cycle power use fraction [~]
        Real64 error;          // Calculation error
        Real64 PLF1;           // ith term of part load factor
        Real64 PLF2;           // (i+1)th term of part load factor
        Real64 A;              // Variable for simplIFy equation
        int NumIteration;      // Iteration Counter

        Nmax = this->m_MaxONOFFCyclesperHour;
        tau = this->m_HPTimeConstant;
        pr = this->m_OnCyclePowerFraction;

        // Initialize
        errFlag = false;
        error = 1;
        NumIteration = 0;

        // Initial guess for part load fraction
        PLF1 = 1;

        // Calculate PLF using successive substitution until convergence
        // is achieved
        while (true) {
            ++NumIteration;

            if (PLR == 1) {
                // Set part load fraction, PLF1=1.0 IF PLR=1.0 and EXIT loop
                PLF1 = 1;
                break;
            }

            if (NumIteration > 100) {
                // EXIT loop IF interation exceed 100
                errFlag = true;
                PLF1 = 1;
                break;
            }

            if (error < 0.00001) {
                // EXIT loop IF convergence is achieved
                break;

            } else {
                // Calculate PLF
                A = 4.0 * tau * (Nmax / 3600.0) * (1 - PLR / PLF1);
                if (A < 1.5e-3) {
                    // A safety check to prevent PLF2 = 1 - A * (1 - Exp(-1 / A))
                    // from "float underflow error". Occurs when PLR is very close to 1.0,
                    // small A value, thus Exp(-1/A) = 0
                    PLF2 = 1.0 - A;
                } else {
                    PLF2 = 1.0 - A * (1.0 - std::exp(-1.0 / A));
                }
                error = std::abs((PLF2 - PLF1) / PLF1);
                PLF1 = PLF2;
            }
        }

        // Adjust PLF for the off cycle power consumption IF
        // on-cycle power use is specified by the user
        if (pr > 0.0) {
            PartLoadFactor = PLR / ((PLR / PLF1) + (1.0 - PLR / PLF1) * pr);
        } else {
            PartLoadFactor = PLF1;
        }

        if (PartLoadFactor <= 0.0) {
            PartLoadFactor = 0;
            RuntimeFrac = 0;
            errFlag = true;
        } else {
            RuntimeFrac = PLR / PartLoadFactor;
        }

        if (RuntimeFrac > 1.0) {
            RuntimeFrac = 1.0;
        }
    }

    Real64 UnitarySys::hotWaterHeatingCoilResidual(Real64 const PartLoadFrac,     // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                   std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Chandan Sharma, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // hot water Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimulateWaterCoilComponents to get outlet temperature at the given part load ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // Residual to be minimized to zero

        // Argument array dimensioning
        // Par(2) = desired air outlet temperature [C]

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirTemp; // Outlet air temperature [C]

        int UnitarySysNum = int(Par[1]);
        bool FirstHVACIteration = (Par[2] > 0.0);
        bool SuppHeatingCoilFlag = (Par[4] > 0.0);
        bool LoadBased = (Par[5] > 0.0);
        Real64 QActual = 0.0;
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        if (!SuppHeatingCoilFlag) {
            Real64 mdot =
                min(DataLoopNode::Node(thisSys.HeatCoilFluidOutletNodeNum).MassFlowRateMaxAvail, thisSys.MaxHeatCoilFluidFlow * PartLoadFrac);
            DataLoopNode::Node(thisSys.HeatCoilFluidInletNode).MassFlowRate = mdot;
            WaterCoils::SimulateWaterCoilComponents(
                thisSys.m_HeatingCoilName, FirstHVACIteration, thisSys.m_HeatingCoilIndex, QActual, thisSys.m_FanOpMode, PartLoadFrac);
            OutletAirTemp = DataLoopNode::Node(thisSys.HeatCoilOutletNodeNum).Temp;
        } else {
            Real64 mdot =
                min(DataLoopNode::Node(thisSys.m_SuppCoilFluidOutletNodeNum).MassFlowRateMaxAvail, thisSys.m_MaxSuppCoilFluidFlow * PartLoadFrac);
            DataLoopNode::Node(thisSys.m_SuppCoilFluidInletNode).MassFlowRate = mdot;
            WaterCoils::SimulateWaterCoilComponents(
                thisSys.m_SuppHeatCoilName, FirstHVACIteration, thisSys.m_SuppHeatCoilIndex, QActual, thisSys.m_FanOpMode, PartLoadFrac);
            OutletAirTemp = DataLoopNode::Node(thisSys.m_SuppCoilAirOutletNode).Temp;
        }
        if (LoadBased) {
            Residuum = Par[3] - QActual;
        } else {
            Residuum = Par[3] - OutletAirTemp;
        }

        return Residuum;
    }

    Real64 UnitarySys::DOE2DXCoilResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                          std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   November 2003

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcDOe2DXCoil to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // par(2) = desired air outlet temperature [C]
        // par(5) = supply air fan operating mode (DataHVACGlobals::ContFanCycCoil)

        int CoilIndex = int(Par[1]);
        int FanOpMode = int(Par[5]);
        DXCoils::CalcDoe2DXCoil(CoilIndex, On, true, PartLoadRatio, FanOpMode);
        Real64 OutletAirTemp = DXCoils::DXCoilOutletTemp(CoilIndex);
        Residuum = Par[2] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::DOE2DXCoilHumRatResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   January 2008

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet humrat - actual outlet humrat)
        // DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcDOe2DXCoil to get outlet humidity ratio at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // par(2) = desired air outlet humidity ratio [kg/kg]
        // par(5) = supply air fan operating mode (DataHVACGlobals::ContFanCycCoil)

        int CoilIndex = int(Par[1]);
        int FanOpMode = int(Par[5]);
        DXCoils::CalcDoe2DXCoil(CoilIndex, On, true, PartLoadRatio, FanOpMode);
        Real64 OutletAirHumRat = DXCoils::DXCoilOutletHumRat(CoilIndex);
        Residuum = Par[2] - OutletAirHumRat;

        return Residuum;
    }

    Real64 UnitarySys::calcUnitarySystemLoadResidual(Real64 const PartLoadRatio,    // DX cooling coil part load ratio
                                                     std::vector<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the unitary system

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // Return value
        Real64 Residuum; // Result (force to 0)

        // Argument array dimensioning
        //   Parameter description example:
        //       Par(1)  = REAL(UnitarySysNum,r64) ! Index to Unitary System
        //       Par(2)  = 0.0                  ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = REAL(OpMode,r64)     ! Fan control, IF 1.0 then cycling fan, if 0.0 then continuous fan
        //       Par(4)  = REAL(CompOp,r64)     ! Compressor control, IF 1.0 then compressor ON, if 0.0 then compressor OFF
        //       Par(5)  = SensLoad or MoistureLoad   ! Sensible or Latent load to be met by unitary system
        //       Par(6)  = HeatingLoad or CoolingLoad ! Type of load FLAG, 0.0 IF heating load, 1.0 IF cooling or moisture load
        //       Par(7)  = 1.0                  ! Output calculation FLAG, 0.0 for latent capacity, 1.0 for sensible capacity
        //       Par(8)  = OnOffAirFlowRatio    ! Ratio of compressor ON air mass flow to AVERAGE air mass flow over time step
        //       Par(9)  = HXUnitOn             ! flag to enable HX, 1=ON and 2=OFF
        //       Par(10) = HeatingCoilPLR       ! used to calculate latent degradation for cycling fan RH control

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 HeatPLR;    // heating coil part load ratio
        Real64 CoolPLR;    // cooling coil part load ratio
        Real64 SensOutput; // sensible output of system
        Real64 LatOutput;  // latent output of system
        Real64 HeatCoilLoad = 0.0;
        Real64 SupHeaterLoad = 0.0;

        // Convert parameters to usable variables
        int UnitarySysNum = int(Par[1]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        bool FirstHVACIteration = (Par[2] > 0.0);
        // int FanOpMode = int(Par[3]);
        int CompOp = int(Par[4]);
        Real64 LoadToBeMet = Par[5];
        Real64 OnOffAirFlowRatio = Par[8];

        if (Par[6] == 1.0) {
            CoolPLR = PartLoadRatio;
            HeatPLR = 0.0;
        } else {
            CoolPLR = 0.0;
            HeatPLR = PartLoadRatio;
        }
        bool SensibleLoad = (Par[7] > 0.0);
        bool HXUnitOn = (Par[9] > 0.0);
        int AirLoopNum = int(Par[11]);

        thisSys.setSpeedVariables(SensibleLoad, PartLoadRatio);

        thisSys.calcUnitarySystemToLoad(AirLoopNum,
                                        FirstHVACIteration,
                                        CoolPLR,
                                        HeatPLR,
                                        OnOffAirFlowRatio,
                                        SensOutput,
                                        LatOutput,
                                        HXUnitOn,
                                        HeatCoilLoad,
                                        SupHeaterLoad,
                                        CompOp);

        // Calculate residual based on output calculation flag
        if (SensibleLoad) {
            if (std::abs(LoadToBeMet) == 0.0) {
                Residuum = (SensOutput - LoadToBeMet) / 100.0;
            } else {
                Residuum = (SensOutput - LoadToBeMet) / LoadToBeMet;
            }
        } else {
            if (std::abs(LoadToBeMet) == 0.0) {
                Residuum = (LatOutput - LoadToBeMet) / 100.0;
            } else {
                Residuum = (LatOutput - LoadToBeMet) / LoadToBeMet;
            }
        }

        return Residuum;
    }

    Real64 UnitarySys::HXAssistedCoolCoilTempResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                      std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   November 2003

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function (desired outlet temp - actual outlet temp)
        //  DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        //  Calls CalcHXAssistedCoolingCoil to get outlet temperature at the given part load ratio
        //  and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // par(2) = desired air outlet temperature [C]
        // par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
        // par(4) = HX control (On/Off)
        // par(5) = supply air fan operating mode (DataHVACGlobals::ContFanCycCoil)

        int CoilIndex = int(Par[1]);
        // FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
        bool FirstHVACIteration = (Par[3] > 0.0);
        bool HXUnitOn = (Par[4] == 1.0);
        int FanOpMode = int(Par[5]);
        int UnitarySysNum = int(Par[6]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        if (thisSys.CoolCoilFluidInletNode > 0) {
            DataLoopNode::Node(thisSys.CoolCoilFluidInletNode).MassFlowRate = thisSys.MaxCoolCoilFluidFlow * PartLoadRatio;
        }
        HVACHXAssistedCoolingCoil::CalcHXAssistedCoolingCoil(CoilIndex, FirstHVACIteration, On, PartLoadRatio, HXUnitOn, FanOpMode);
        Real64 OutletAirTemp = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletTemp(CoilIndex);
        Residuum = Par[2] - OutletAirTemp;
        return Residuum;
    }

    Real64 UnitarySys::HXAssistedCoolCoilHRResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                    std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   January 2008

        // PURPOSE OF THIS FUNCTION:
        //  Calculates residual function (desired outlet humrat - actual outlet humrat)
        //  DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        //  Calls CalcHXAssistedCoolingCoil to get outlet humidity ratio at the given part load ratio
        //  and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // par(2) = desired air outlet humidity ratio [kg/kg]
        // par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
        // par(4) = HX control (On/Off)
        // par(5) = supply air fan operating mode (DataHVACGlobals::ContFanCycCoil)

        int CoilIndex = int(Par[1]);
        // FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
        bool FirstHVACIteration = (Par[3] > 0.0);
        bool HXUnitOn = (Par[4] == 1.0);
        int FanOpMode = int(Par[5]);
        HVACHXAssistedCoolingCoil::CalcHXAssistedCoolingCoil(
            CoilIndex, FirstHVACIteration, On, PartLoadRatio, HXUnitOn, FanOpMode, _, economizerFlag);
        Real64 OutletAirHumRat = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletHumRat(CoilIndex);
        Residuum = Par[2] - OutletAirHumRat;
        return Residuum;
    }

    Real64 UnitarySys::DXCoilVarSpeedResidual(Real64 const SpeedRatio,       // compressor speed ratio (1.0 is max, 0.0 is min)
                                              std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   September 2002

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp).
        // DX Coil output depends on the compressor speed which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcMultiSpeedDXCoil to get outlet temperature at the given compressor speed
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirTemp(0.0); // outlet air temperature [C]
        Real64 CycRatio;
        int SpeedNum;
        int FanOpMode;
        int CompOp;
        Real64 ReqOutput;
        Real64 dummy;
        Real64 RuntimeFrac;
        Real64 OnOffAirFlowRatio;
        Real64 SensLoad;

        int CoilIndex = int(Par[1]);
        int UnitarySysNum = int(Par[3]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        {
            auto const SELECT_CASE_var(thisSys.m_CoolingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                DXCoils::CalcMultiSpeedDXCoil(CoilIndex, SpeedRatio, 1.0);
                OutletAirTemp = DXCoils::DXCoilOutletTemp(CoilIndex);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                OnOffAirFlowRatio = 1.0;

                thisSys.setAverageAirFlow(SpeedRatio, OnOffAirFlowRatio);
                DXCoils::CalcMultiSpeedDXCoilCooling(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompOp, 0);
                OutletAirTemp = DXCoils::DXCoilOutletTemp(CoilIndex);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                ReqOutput = Par[8];
                dummy = 0.0;
                SensLoad = -1.0;
                RuntimeFrac = 1.0;
                OnOffAirFlowRatio = 1.0;

                VariableSpeedCoils::SimVariableSpeedCoils("",
                                                          CoilIndex,
                                                          FanOpMode,
                                                          thisSys.m_MaxONOFFCyclesperHour,
                                                          thisSys.m_HPTimeConstant,
                                                          thisSys.m_FanDelayTime,
                                                          CompOp,
                                                          CycRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          SensLoad,
                                                          dummy,
                                                          OnOffAirFlowRatio);

                OutletAirTemp = DataLoopNode::Node(thisSys.CoolCoilOutletNodeNum).Temp;

            } else {
                assert(false);
            }
        }

        Residuum = Par[2] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::heatingCoilVarSpeedResidual(Real64 const SpeedRatio,       // compressor speed ratio (1.0 is max, 0.0 is min)
                                                   std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   September 2002

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp).
        // DX Coil output depends on the compressor speed which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls calc routines of  multi Speed or variable Coil to get outlet temperature at the given compressor speed
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirTemp(0.0); // outlet air temperature [C]
        Real64 CycRatio;
        int SpeedNum;
        int FanOpMode;
        int CompOp;
        Real64 ReqOutput;
        Real64 OnOffAirFlowRatio;
        Real64 SensLoad;
        Real64 LatLoad;

        int CoilIndex = int(Par[1]);
        int UnitarySysNum = int(Par[3]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        {
            auto const SELECT_CASE_var(thisSys.m_HeatingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                OnOffAirFlowRatio = 1.0;

                thisSys.setAverageAirFlow(SpeedRatio, OnOffAirFlowRatio);

                DXCoils::CalcMultiSpeedDXCoilHeating(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0);

                OutletAirTemp = DXCoils::DXCoilOutletTemp(CoilIndex);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit)) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                ReqOutput = Par[8];
                OnOffAirFlowRatio = 1.0;
                SensLoad = 1.0;
                LatLoad = -1.0;

                // can't call only the calc routine with these coil types since Init sets air flow rate based on speed num and cycling ratio
                VariableSpeedCoils::SimVariableSpeedCoils("",
                                                          CoilIndex,
                                                          FanOpMode,
                                                          thisSys.m_MaxONOFFCyclesperHour,
                                                          thisSys.m_HPTimeConstant,
                                                          thisSys.m_FanDelayTime,
                                                          CompOp,
                                                          CycRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          SensLoad,
                                                          LatLoad,
                                                          OnOffAirFlowRatio);

                OutletAirTemp = DataLoopNode::Node(thisSys.HeatCoilOutletNodeNum).Temp;

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);

                HeatingCoils::CalcMultiStageElectricHeatingCoil(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode);

                OutletAirTemp = DataLoopNode::Node(thisSys.HeatCoilOutletNodeNum).Temp;

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);

                HeatingCoils::CalcMultiStageElectricHeatingCoil(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode);

                OutletAirTemp = DataLoopNode::Node(thisSys.HeatCoilOutletNodeNum).Temp;

            } else {
                assert(false);
            }
        }

        Residuum = Par[2] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::DXCoilVarSpeedHumRatResidual(Real64 const SpeedRatio,       // compressor speed ratio (1.0 is max, 0.0 is min)
                                                    std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   January 2008

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet humrat - actual outlet humrat).
        // DX Coil output depends on the compressor speed which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls calc routine sof multi speed or variable speed coils to get outlet humidity ratio at the given compressor speed
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirHumRat(0.0); // outlet air humidity ratio
        Real64 CycRatio;
        int SpeedNum;
        int FanOpMode;
        int CompOp;
        Real64 ReqOutput;
        Real64 SensLoad;
        Real64 LatLoad;
        Real64 RuntimeFrac;
        Real64 OnOffAirFlowRatio;

        int CoilIndex = int(Par[1]);
        int UnitarySysNum = int(Par[3]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        {
            auto const SELECT_CASE_var(thisSys.m_CoolingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                DXCoils::CalcMultiSpeedDXCoil(CoilIndex, SpeedRatio, 1.0);
                OutletAirHumRat = DXCoils::DXCoilOutletHumRat(CoilIndex);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                OnOffAirFlowRatio = 1.0;

                thisSys.setAverageAirFlow(SpeedRatio, OnOffAirFlowRatio);
                DXCoils::CalcMultiSpeedDXCoilCooling(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompOp, 0);
                OutletAirHumRat = DXCoils::DXCoilOutletHumRat(CoilIndex);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                ReqOutput = Par[8];
                SensLoad = -1.0;
                LatLoad = 0.0;
                RuntimeFrac = 1.0;
                OnOffAirFlowRatio = 1.0;

                VariableSpeedCoils::SimVariableSpeedCoils("",
                                                          CoilIndex,
                                                          FanOpMode,
                                                          thisSys.m_MaxONOFFCyclesperHour,
                                                          thisSys.m_HPTimeConstant,
                                                          thisSys.m_FanDelayTime,
                                                          CompOp,
                                                          CycRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          SensLoad,
                                                          LatLoad,
                                                          OnOffAirFlowRatio);

                OutletAirHumRat = DataLoopNode::Node(thisSys.CoolCoilOutletNodeNum).HumRat;

            } else {
                assert(false);
            }
        }

        Residuum = Par[2] - OutletAirHumRat;

        return Residuum;
    }

    Real64 UnitarySys::DXCoilCyclingResidual(Real64 const CycRatio,         // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                             std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   September 2002

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // DX Coil output depends on the cycling ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls multi or variable speed coil to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirTemp(0.0); // outlet air temperature [C]
        Real64 SpeedRatio;
        int SpeedNum;
        int FanOpMode;
        int CompOp;
        Real64 ReqOutput;
        Real64 dummy;
        Real64 SensLoad;
        Real64 OnOffAirFlowRatio;

        //            Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
        //            Par(2) = DesOutTemp
        //            Par(3) = UnitarySysNum
        //            Par(4) = SpeedRatio
        //            Par(5) = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
        //            Par(6) = UnitarySystem(UnitarySysNum)%FanOpMode
        //            Par(7) = 1.0d0 ! CompOp

        int CoilIndex = int(Par[1]);
        int UnitarySysNum = int(Par[3]);
        int AirloopNum = int(Par[9]);
        bool FirstHVACIteration = (Par[10] > 0.0);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];
        {
            auto const SELECT_CASE_var(thisSys.m_CoolingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                if (thisSys.m_FanPlace == FanPlace::BlowThru) { // must simulate fan if blow through since OnOffFanPartLoadFrac affects fan heat
                    thisSys.m_CoolingCycRatio = CycRatio;
                    thisSys.m_CoolingPartLoadFrac = CycRatio;
                    thisSys.calcPassiveSystem(AirloopNum, FirstHVACIteration);
                } else {
                    DXCoils::CalcMultiSpeedDXCoil(CoilIndex, 0.0, CycRatio);
                }

                OutletAirTemp = DXCoils::DXCoilOutletTemp(CoilIndex);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                SpeedRatio = int(Par[4]);
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                OnOffAirFlowRatio = 1.0;

                thisSys.setAverageAirFlow(CycRatio, OnOffAirFlowRatio);
                if (thisSys.m_FanPlace == FanPlace::BlowThru) { // must simulate fan if blow through since OnOffFanPartLoadFrac affects fan heat
                    thisSys.m_CoolingCycRatio = CycRatio;
                    thisSys.m_CoolingPartLoadFrac = CycRatio;
                    thisSys.calcPassiveSystem(AirloopNum, FirstHVACIteration);
                } else {
                    DXCoils::CalcMultiSpeedDXCoilCooling(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompOp, 0);
                }
                OutletAirTemp = DXCoils::DXCoilOutletTemp(CoilIndex);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {

                SpeedRatio = Par[4]; // Autodesk:Init Added line to elim use uninitialized
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                if (CycRatio == 0.0) CompOp = 0;
                ReqOutput = Par[8];
                dummy = 0.0;
                OnOffAirFlowRatio = 1.0;

                SensLoad = -1.0;
                VariableSpeedCoils::SimVariableSpeedCoils("",
                                                          CoilIndex,
                                                          FanOpMode,
                                                          thisSys.m_MaxONOFFCyclesperHour,
                                                          thisSys.m_HPTimeConstant,
                                                          thisSys.m_FanDelayTime,
                                                          CompOp,
                                                          CycRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          SensLoad,
                                                          dummy,
                                                          OnOffAirFlowRatio);

                OutletAirTemp = DataLoopNode::Node(thisSys.CoolCoilOutletNodeNum).Temp;

            } else {
                assert(false);
            }
        }

        Residuum = Par[2] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::DXCoilCyclingHumRatResidual(Real64 const CycRatio,         // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                   std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   September 2002

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // DX Coil output depends on the cycling ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcMultiSpeedDXCoil to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirHumRat(0.0); // outlet air humidity ratio [kg/kg]
        Real64 SpeedRatio;
        int SpeedNum;
        int FanOpMode;
        int CompOp;
        Real64 ReqOutput;
        Real64 SensLoad;
        Real64 LatLoad;
        Real64 OnOffAirFlowRatio;

        int CoilIndex = int(Par[1]);
        int UnitarySysNum = int(Par[3]);
        auto &thisSys = unitarySys[UnitarySysNum];

        {
            auto const SELECT_CASE_var(thisSys.m_CoolingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                DXCoils::CalcMultiSpeedDXCoil(CoilIndex, 0.0, CycRatio);

                OutletAirHumRat = DXCoils::DXCoilOutletHumRat(CoilIndex);
            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                SpeedRatio = int(Par[4]);
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                OnOffAirFlowRatio = 1.0;

                thisSys.setAverageAirFlow(CycRatio, OnOffAirFlowRatio);
                DXCoils::CalcMultiSpeedDXCoilCooling(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, CompOp, 0);
                OutletAirHumRat = DXCoils::DXCoilOutletHumRat(CoilIndex);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {

                SpeedRatio = int(Par[4]); // Autodesk:Init Added line to elim use uninitialized
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                ReqOutput = Par[8];
                SensLoad = -1.0;
                LatLoad = 0.0;
                OnOffAirFlowRatio = 1.0;

                VariableSpeedCoils::SimVariableSpeedCoils("",
                                                          CoilIndex,
                                                          FanOpMode,
                                                          thisSys.m_MaxONOFFCyclesperHour,
                                                          thisSys.m_HPTimeConstant,
                                                          thisSys.m_FanDelayTime,
                                                          CompOp,
                                                          CycRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          SensLoad,
                                                          LatLoad,
                                                          OnOffAirFlowRatio);

                OutletAirHumRat = DataLoopNode::Node(thisSys.CoolCoilOutletNodeNum).HumRat;

            } else {
                assert(false);
            }
        }

        Residuum = Par[2] - OutletAirHumRat;

        return Residuum;
    }

    Real64 UnitarySys::heatingCoilVarSpeedCycResidual(Real64 const CycRatio,         // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                      std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   September 2002

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // DX Coil output depends on the cycling ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls multi or variable speed coil to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirTemp(0.0); // outlet air temperature [C]
        Real64 SpeedRatio;
        int SpeedNum;
        int FanOpMode;
        int CompOp;
        Real64 ReqOutput;
        Real64 SensLoad;
        Real64 LatLoad;
        Real64 OnOffAirFlowRatio;

        //            Par(1) = REAL(UnitarySystem(UnitarySysNum)%CoolingCoilIndex,r64)
        //            Par(2) = DesOutTemp
        //            Par(3) = UnitarySysNum
        //            Par(4) = SpeedRatio
        //            Par(5) = UnitarySystem(UnitarySysNum)%CoolingSpeedNum
        //            Par(6) = UnitarySystem(UnitarySysNum)%FanOpMode
        //            Par(7) = 1.0d0 ! CompOp

        int CoilIndex = int(Par[1]);
        int UnitarySysNum = int(Par[3]);
        auto &thisSys = unitarySys[UnitarySysNum];

        {
            auto const SELECT_CASE_var(thisSys.m_HeatingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) {

                SpeedRatio = int(Par[4]);
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                OnOffAirFlowRatio = 1.0;

                thisSys.setAverageAirFlow(CycRatio, OnOffAirFlowRatio);
                DXCoils::CalcMultiSpeedDXCoilHeating(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode, 0);
                OutletAirTemp = DXCoils::DXCoilOutletTemp(CoilIndex);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit)) {

                SpeedRatio = int(Par[4]); // Autodesk:Init Added line to elim use uninitialized
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                if (CycRatio == 0.0) CompOp = 0;
                ReqOutput = Par[8];
                SensLoad = -1.0;
                LatLoad = 0.0;
                OnOffAirFlowRatio = 1.0;

                VariableSpeedCoils::SimVariableSpeedCoils("",
                                                          CoilIndex,
                                                          FanOpMode,
                                                          thisSys.m_MaxONOFFCyclesperHour,
                                                          thisSys.m_HPTimeConstant,
                                                          thisSys.m_FanDelayTime,
                                                          CompOp,
                                                          CycRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          SensLoad,
                                                          LatLoad,
                                                          OnOffAirFlowRatio);

                OutletAirTemp = DataLoopNode::Node(thisSys.HeatCoilOutletNodeNum).Temp;

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) {

                SpeedRatio = int(Par[4]);
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);

                HeatingCoils::CalcMultiStageElectricHeatingCoil(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode);

                OutletAirTemp = DataLoopNode::Node(thisSys.HeatCoilOutletNodeNum).Temp;

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage) {

                SpeedRatio = int(Par[4]);
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);

                HeatingCoils::CalcMultiStageGasHeatingCoil(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode);

                OutletAirTemp = DataLoopNode::Node(thisSys.HeatCoilOutletNodeNum).Temp;

            } else {
                assert(false);
            }
        }

        Residuum = Par[2] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::TESIceStorageCoilOutletResidual(Real64 PartLoadRatio,          // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                       std::vector<Real64> const &Par // data array
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR        Richard Raustad, FSEC
        //       DATE WRITTEN   August 2015

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimWatertoAirHP or SimWatertoAirHPSimple to get outlet humidity ratio at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // Par( 1 ) = double( UnitarySysNum );
        // Par( 2 ) = DesOutTemp;
        // Par( 3 ) = 0.0; // DesOutHumRat; set to 0 if temp controlled

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirTemp;   // outlet air temperature [C]
        Real64 OutletAirHumRat; // outlet air humidity ratio [kg/kg]

        int UnitarySysNum = int(Par[1]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        Real64 DesiredOutletTemp = Par[2];
        Real64 DesiredOutletHumRat = Par[3];

        PackagedThermalStorageCoil::SimTESCoil(
            thisSys.m_CoolingCoilName, thisSys.m_CoolingCoilIndex, thisSys.m_FanOpMode, thisSys.m_TESOpMode, PartLoadRatio);

        if (DesiredOutletHumRat > 0.0) {
            OutletAirHumRat = DataLoopNode::Node(thisSys.CoolCoilOutletNodeNum).HumRat;
            Residuum = OutletAirHumRat - DesiredOutletHumRat;
        } else {
            OutletAirTemp = DataLoopNode::Node(thisSys.CoolCoilOutletNodeNum).Temp;
            Residuum = OutletAirTemp - DesiredOutletTemp;
        }

        return Residuum;
    }

    Real64 UnitarySys::multiModeDXCoilResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                               std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         M. J. Witte, GARD Analytics, Inc.
        //       DATE WRITTEN   February 2005
        //                      (based on DOE2DXCoilResidual by Richard Raustad, FSEC)

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimDXCoilMultiMode to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // par(2) = desired air outlet temperature [C]
        // par(3) = dehumidification mode (0=normal, 1=enhanced)
        // par(4) = supply air fan operating mode (DataHVACGlobals::ContFanCycCoil)

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int CoilIndex;        // index of this coil
        Real64 OutletAirTemp; // outlet air temperature [C]
        int DehumidMode;      // dehumidification mode (par3)
        int FanOpMode;        // supply air fan operating mode

        CoilIndex = int(Par[1]);
        DehumidMode = int(Par[3]);
        FanOpMode = int(Par[4]);
        DXCoils::SimDXCoilMultiMode("", On, false, PartLoadRatio, DehumidMode, CoilIndex, FanOpMode);
        OutletAirTemp = DXCoils::DXCoilOutletTemp(CoilIndex);
        Residuum = Par[2] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::multiModeDXCoilHumRatResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                     std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   January 2008

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet humrat - actual outlet humrat)
        // DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimDXCoilMultiMode to get outlet humidity ratio at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // par(2) = desired air outlet humidity ratio [kg/kg]
        // par(3) = dehumidification mode (0=normal, 1=enhanced)
        // par(4) = supply air fan operating mode (DataHVACGlobals::ContFanCycCoil)

        int CoilIndex = int(Par[1]);
        int DehumidMode = int(Par[3]);
        int FanOpMode = int(Par[4]);
        DXCoils::SimDXCoilMultiMode("", On, false, PartLoadRatio, DehumidMode, CoilIndex, FanOpMode);
        Real64 OutletAirHumRat = DXCoils::DXCoilOutletHumRat(CoilIndex);
        Residuum = Par[2] - OutletAirHumRat;

        return Residuum;
    }

    Real64 UnitarySys::coolWaterHumRatResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                               std::vector<Real64> const &Par // par(1) = CoolWater coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Chandan Sharma, FSEC
        //       DATE WRITTEN   January 2013

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet humrat - actual outlet humrat)
        // Cool water coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimulateWaterCoilComponents to get outlet temp at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // par(2) = desired air outlet humidity ratio [kg/kg]
        // par(5) = supply air fan operating mode (DataHVACGlobals::ContFanCycCoil)

        int UnitarySysNum = int(Par[1]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];
        bool FirstHVACIteration = (Par[2] > 0.0);

        Real64 mdot = min(DataLoopNode::Node(thisSys.CoolCoilFluidOutletNodeNum).MassFlowRateMaxAvail, thisSys.MaxCoolCoilFluidFlow * PartLoadRatio);
        DataLoopNode::Node(thisSys.CoolCoilFluidInletNode).MassFlowRate = mdot;
        WaterCoils::SimulateWaterCoilComponents(thisSys.m_CoolingCoilName, FirstHVACIteration, thisSys.m_CoolingCoilIndex, _, _, PartLoadRatio);

        Real64 OutletAirHumRat = DataLoopNode::Node(thisSys.CoolCoilOutletNodeNum).HumRat;
        Residuum = Par[3] - OutletAirHumRat;

        return Residuum;
    }

    Real64 UnitarySys::coolWaterTempResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                             std::vector<Real64> const &Par // par(1) = CoolWater coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Chandan Sharma, FSEC
        //       DATE WRITTEN   January 2013

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // Cool water coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimulateWaterCoilComponents to get outlet temp at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // par(2) = desired air outlet humidity ratio [kg/kg]
        // par(5) = supply air fan operating mode (DataHVACGlobals::ContFanCycCoil)

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirTemp; // outlet air humidity ratio [kg/kg]

        int UnitarySysNum = int(Par[1]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];
        bool FirstHVACIteration = (Par[2] > 0.0);

        Real64 mdot = min(DataLoopNode::Node(thisSys.CoolCoilFluidOutletNodeNum).MassFlowRateMaxAvail, thisSys.MaxCoolCoilFluidFlow * PartLoadRatio);
        DataLoopNode::Node(thisSys.CoolCoilFluidInletNode).MassFlowRate = mdot;
        WaterCoils::SimulateWaterCoilComponents(thisSys.m_CoolingCoilName, FirstHVACIteration, thisSys.m_CoolingCoilIndex, _, _, PartLoadRatio);

        OutletAirTemp = DataLoopNode::Node(thisSys.CoolCoilOutletNodeNum).Temp;
        Residuum = Par[3] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::gasElecHeatingCoilResidual(Real64 const PartLoadFrac,     // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                  std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Chandan Sharma, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // hot water Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimulateHeatingCoilComponents to get outlet temperature at the given part load ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // Residual to be minimized to zero

        // Argument array dimensioning
        // Par(2) = desired air outlet temperature [C]

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirTemp; // Outlet air temperature [C]

        int UnitarySysNum = int(Par[1]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        bool FirstHVACIteration = (Par[2] > 0.0);
        bool SuppHeatingCoilFlag = (Par[4] > 0.0);
        bool FanOpMode = Par[5]; // RR this was a 4
        // heating coils using set point control pass DataLoopNode::SensedLoadFlagValue as QCoilReq to indicate temperature control
        if (!SuppHeatingCoilFlag) {
            HeatingCoils::SimulateHeatingCoilComponents(thisSys.m_HeatingCoilName,
                                                        FirstHVACIteration,
                                                        DataLoopNode::SensedLoadFlagValue,
                                                        thisSys.m_HeatingCoilIndex,
                                                        _,
                                                        _,
                                                        FanOpMode,
                                                        PartLoadFrac);
            OutletAirTemp = DataLoopNode::Node(thisSys.HeatCoilOutletNodeNum).Temp;
        } else {
            HeatingCoils::SimulateHeatingCoilComponents(thisSys.m_SuppHeatCoilName,
                                                        FirstHVACIteration,
                                                        DataLoopNode::SensedLoadFlagValue,
                                                        thisSys.m_SuppHeatCoilIndex,
                                                        _,
                                                        true,
                                                        FanOpMode,
                                                        PartLoadFrac);
            OutletAirTemp = DataLoopNode::Node(thisSys.m_SuppCoilAirOutletNode).Temp;
        }
        Residuum = Par[3] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::steamHeatingCoilResidual(Real64 const PartLoadFrac,     // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Chandan Sharma, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // hot Steam Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimulateSteamCoilComponents to get outlet temperature at the given part load ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // Residual to be minimized to zero

        // Argument array dimensioning
        // Par(2) = desired air outlet temperature [C]

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirTemp; // Outlet air temperature [C]
        Real64 mdot;

        int UnitarySysNum = int(Par[1]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        bool FirstHVACIteration = (Par[2] > 0.0);
        bool SuppHeatingCoilFlag = (Par[4] > 0.0);

        if (!SuppHeatingCoilFlag) {
            mdot = min(DataLoopNode::Node(thisSys.HeatCoilFluidOutletNodeNum).MassFlowRateMaxAvail, thisSys.MaxHeatCoilFluidFlow * PartLoadFrac);
            DataLoopNode::Node(thisSys.HeatCoilFluidInletNode).MassFlowRate = mdot;
            SteamCoils::SimulateSteamCoilComponents(
                thisSys.m_HeatingCoilName, FirstHVACIteration, thisSys.m_HeatingCoilIndex, 1.0, _, thisSys.m_FanOpMode, PartLoadFrac);
        } else {
            mdot = min(DataLoopNode::Node(thisSys.m_SuppCoilFluidOutletNodeNum).MassFlowRateMaxAvail, thisSys.m_MaxSuppCoilFluidFlow * PartLoadFrac);
            DataLoopNode::Node(thisSys.m_SuppCoilFluidInletNode).MassFlowRate = mdot;
            SteamCoils::SimulateSteamCoilComponents(
                thisSys.m_SuppHeatCoilName, FirstHVACIteration, thisSys.m_SuppHeatCoilIndex, 1.0, _, thisSys.m_FanOpMode, PartLoadFrac);
        }
        OutletAirTemp = DataLoopNode::Node(thisSys.HeatCoilOutletNodeNum).Temp; // RR this should be supp coil
        Residuum = Par[3] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::heatWatertoAirHPTempResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                    std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Chandan Sharma, FSEC
        //       DATE WRITTEN   January 2013

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // Heat water coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimWatertoAirHP or SimWatertoAirHPSimple to get outlet humidity ratio at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // par(2) = desired air outlet humidity ratio [kg/kg]
        // par(5) = supply air fan operating mode (DataHVACGlobals::ContFanCycCoil)

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 OutletAirTemp; // outlet air humidity ratio [kg/kg]
        bool errFlag;
        Real64 RuntimeFrac;
        Real64 dummy;

        int UnitarySysNum = int(Par[1]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        bool FirstHVACIteration = (Par[2] > 0.0);
        Real64 ReqOutput = Par[4]; // RR this was a 1

        thisSys.heatPumpRunFrac(PartLoadRatio, errFlag, RuntimeFrac);

        if (RuntimeFrac > 0.0 && thisSys.m_FanOpMode == DataHVACGlobals::CycFanCycCoil) {
            DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / RuntimeFrac;
        } else {
            DataHVACGlobals::OnOffFanPartLoadFraction = 1.0;
        }

        thisSys.m_CompPartLoadRatio = PartLoadRatio;
        thisSys.m_WSHPRuntimeFrac = RuntimeFrac;

        dummy = 0.0;
        if (thisSys.m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) {
            WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                            thisSys.m_HeatingCoilIndex,
                                                            ReqOutput,
                                                            dummy,
                                                            thisSys.m_FanOpMode,
                                                            RuntimeFrac,
                                                            thisSys.m_MaxONOFFCyclesperHour,
                                                            thisSys.m_HPTimeConstant,
                                                            thisSys.m_FanDelayTime,
                                                            1,
                                                            PartLoadRatio,
                                                            FirstHVACIteration);
        } else {
            WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                thisSys.m_HeatingCoilIndex,
                                                thisSys.MaxHeatAirMassFlow,
                                                thisSys.m_FanOpMode,
                                                FirstHVACIteration,
                                                RuntimeFrac,
                                                thisSys.m_MaxONOFFCyclesperHour,
                                                thisSys.m_HPTimeConstant,
                                                thisSys.m_FanDelayTime,
                                                thisSys.m_InitHeatPump,
                                                ReqOutput,
                                                dummy,
                                                0,
                                                PartLoadRatio);
        }

        OutletAirTemp = DataLoopNode::Node(thisSys.HeatCoilOutletNodeNum).Temp;
        Residuum = Par[3] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::coolWatertoAirHPHumRatResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                      std::vector<Real64> const &Par // par(1) = CoolWatertoAirHP coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Chandan Sharma, FSEC
        //       DATE WRITTEN   January 2013

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet humrat - actual outlet humrat)
        // Cool water coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimWatertoAirHP or SimWatertoAirHPSimple to get outlet humidity ratio at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // par(2) = desired air outlet humidity ratio [kg/kg]
        // par(5) = supply air fan operating mode (DataHVACGlobals::ContFanCycCoil)

        int UnitarySysNum = int(Par[1]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        bool FirstHVACIteration = (Par[2] > 0.0);
        Real64 ReqOutput = Par[4];
        bool errFlag = false;
        Real64 RuntimeFrac = 0.0; // heat pump runtime fraction

        thisSys.heatPumpRunFrac(PartLoadRatio, errFlag, RuntimeFrac);

        thisSys.m_CompPartLoadRatio = PartLoadRatio;
        thisSys.m_WSHPRuntimeFrac = RuntimeFrac;

        Real64 dummy = 0.0;
        if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {
            WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                            thisSys.m_CoolingCoilIndex,
                                                            ReqOutput,
                                                            dummy,
                                                            thisSys.m_FanOpMode,
                                                            RuntimeFrac,
                                                            thisSys.m_MaxONOFFCyclesperHour,
                                                            thisSys.m_HPTimeConstant,
                                                            thisSys.m_FanDelayTime,
                                                            0,
                                                            PartLoadRatio,
                                                            FirstHVACIteration);
        } else {
            WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                thisSys.m_CoolingCoilIndex,
                                                thisSys.MaxCoolAirMassFlow,
                                                thisSys.m_FanOpMode,
                                                FirstHVACIteration,
                                                RuntimeFrac,
                                                thisSys.m_MaxONOFFCyclesperHour,
                                                thisSys.m_HPTimeConstant,
                                                thisSys.m_FanDelayTime,
                                                thisSys.m_InitHeatPump,
                                                ReqOutput,
                                                dummy,
                                                0,
                                                PartLoadRatio);
        }

        Real64 OutletAirHumRat = DataLoopNode::Node(thisSys.CoolCoilOutletNodeNum).HumRat;
        Residuum = Par[3] - OutletAirHumRat;

        return Residuum;
    }

    Real64 UnitarySys::coolWatertoAirHPTempResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                    std::vector<Real64> const &Par // par(1) = CoolWatertoAirHP coil number
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR        Chandan Sharma, FSEC
        //       DATE WRITTEN   January 2013

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // Cool water coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls SimWatertoAirHP or SimWatertoAirHPSimple to get outlet humidity ratio at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning
        // par(2) = desired air outlet humidity ratio [kg/kg]
        // par(5) = supply air fan operating mode (DataHVACGlobals::ContFanCycCoil)

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        bool errFlag = false;
        Real64 RuntimeFrac = 0.0;

        int UnitarySysNum = int(Par[1]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        bool FirstHVACIteration = (Par[2] > 0.0);
        Real64 ReqOutput = Par[4];

        thisSys.heatPumpRunFrac(PartLoadRatio, errFlag, RuntimeFrac);

        if (RuntimeFrac > 0.0 && thisSys.m_FanOpMode == DataHVACGlobals::CycFanCycCoil) {
            DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / RuntimeFrac;
        } else {
            DataHVACGlobals::OnOffFanPartLoadFraction = 1;
        }

        thisSys.m_CompPartLoadRatio = PartLoadRatio;
        thisSys.m_WSHPRuntimeFrac = RuntimeFrac;

        Real64 dummy = 0.0;
        if (thisSys.m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {
            WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                            thisSys.m_CoolingCoilIndex,
                                                            ReqOutput,
                                                            dummy,
                                                            thisSys.m_FanOpMode,
                                                            RuntimeFrac,
                                                            thisSys.m_MaxONOFFCyclesperHour,
                                                            thisSys.m_HPTimeConstant,
                                                            thisSys.m_FanDelayTime,
                                                            1,
                                                            PartLoadRatio,
                                                            FirstHVACIteration);
        } else {
            WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                thisSys.m_CoolingCoilIndex,
                                                thisSys.MaxCoolAirMassFlow,
                                                thisSys.m_FanOpMode,
                                                FirstHVACIteration,
                                                RuntimeFrac,
                                                thisSys.m_MaxONOFFCyclesperHour,
                                                thisSys.m_HPTimeConstant,
                                                thisSys.m_FanDelayTime,
                                                thisSys.m_InitHeatPump,
                                                ReqOutput,
                                                dummy,
                                                0,
                                                PartLoadRatio);
        }

        Real64 OutletAirTemp = DataLoopNode::Node(thisSys.CoolCoilOutletNodeNum).Temp;
        Residuum = Par[3] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::DXHeatingCoilResidual(Real64 const PartLoadFrac,     // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                             std::vector<Real64> const &Par // par(1) = DX coil number
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2006

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (desired outlet temp - actual outlet temp)
        // DX Coil output depends on the part load ratio which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcDOe2DXCoil to get outlet temperature at the given cycling ratio
        // and calculates the residual as defined above

        // Return value
        Real64 Residuum; // Residual to be minimized to zero

        int CoilIndex = int(Par[1]);
        Real64 OnOffAirFlowFrac = Par[3];

        DXCoils::CalcDXHeatingCoil(CoilIndex, PartLoadFrac, DataHVACGlobals::ContFanCycCoil, OnOffAirFlowFrac);

        Real64 OutletAirTemp = DXCoils::DXCoilOutletTemp(CoilIndex);
        Residuum = Par[2] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::calcUnitarySystemWaterFlowResidual(Real64 const PartLoadRatio,    // coil part load ratio
                                                          std::vector<Real64> const &Par // Function parameters
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   January 2017

        // PURPOSE OF THIS SUBROUTINE:
        // To calculate the part-load ratio for the UnitarySystem coil with varying part load ratio

        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to CALL this Function to converge on a solution

        // Return value
        Real64 Residuum; // Result (forces solution to be within tolerance)
        Real64 HeatCoilLoad = 0.0;
        Real64 SupHeaterLoad = 0.0;

        // Argument array dimensioning
        //   Parameter description example:
        //       Par(1)  = double(UnitarySysNum)    ! Index to unitary system
        //       Par(2)  = 0.0                      ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //       Par(3)  = double(ControlledZoneNum) ! zone index
        //       Par(4)  = QZnReq                   ! zone load [W]
        //       Par(5)  = double(AirControlNode)   ! UnitarySystem air inlet node number
        //       Par(6)  = OnOffAirFlowRatio        ! ratio of coil on air flow rate to coil off air flow rate
        //       Par(7)  = double(AirLoopNum)       ! index to air loop
        //       Par(8)  = double(WaterControlNode) ! CW or HW control node number
        //       Par(9)  = lowWaterMdot             ! water flow rate at low speed fan that meets outlet air set point temperature
        //       Par(10) = highWaterMdot            ! water flow rate at high speed fan that meets outlet air set point temperature
        //       Par(11) = lowSpeedRatio            ! ratio of low speed fan flow rate to high speed fan flow rate
        //       Par(12) = airMdot                  ! air flow rate used for function calculations
        //       Par(13) = SATempTarget             ! SA temperature target [C], 0 if target is load [W]
        //       Par(14) = systemMaxAirFlowRate     ! UnitarySystem maximum air flow rate [kg/s]
        //       Par(15) = LoadType                 ! 1.0 for CoolingLoad otherwise don't care
        //       Par(16) = iteration method         ! 1 = iteration on coil capacity, 2 = iterate on air flow rate at constant coil capacity

        // Convert parameters to usable variables
        int UnitarySysNum = int(Par[1]);
        UnitarySys &thisSys = unitarySys[UnitarySysNum];

        bool FirstHVACIteration = (Par[2] > 0.0);
        // int ControlledZoneNum = int(Par[3]);
        Real64 QZnReq = Par[4];
        int AirControlNode = int(Par[5]);
        Real64 OnOffAirFlowRat = Par[6];
        int AirLoopNum = int(Par[7]);
        int WaterControlNode = int(Par[8]);
        // Real64 lowWaterMdot = Par[9];
        Real64 highWaterMdot = Par[10];
        Real64 lowSpeedRatio = Par[11];
        Real64 airMdot = Par[12];
        Real64 SATempTarget = 0.0;
        bool LoadIsTarget = false;
        if (Par[13] == 0.0) {
            LoadIsTarget = true;
        } else {
            SATempTarget = Par[13];
        }
        Real64 systemMaxAirFlowRate = Par[14];
        bool coolingLoad = (Par[15] > 0.0);
        bool iterateOnAirOnly = (Par[16] > 1.0);
        bool HXUnitOn = true;

        if (iterateOnAirOnly) {

            // set air flow rate bounded by low speed and high speed air flow rates
            DataLoopNode::Node(AirControlNode).MassFlowRate = airMdot * (lowSpeedRatio + (PartLoadRatio * (1.0 - lowSpeedRatio)));
            // FanPartLoadRatio is used to pass info over to function SetAverageAirFlow since air and coil PLR are disassociated in the model
            // FanPartLoadRatio is a report variable that is updated (overwritten) in ReportUnitarySystem
            thisSys.FanPartLoadRatio = PartLoadRatio;
            //			if( WaterControlNode > 0 ) Node( WaterControlNode ).MassFlowRate = highWaterMdot;

        } else {

            DataLoopNode::Node(AirControlNode).MassFlowRate = airMdot;
            thisSys.FanPartLoadRatio =
                max(0.0, ((airMdot - (systemMaxAirFlowRate * lowSpeedRatio)) / ((1.0 - lowSpeedRatio) * systemMaxAirFlowRate)));

            if (WaterControlNode > 0) {
                Real64 waterMdot = highWaterMdot * PartLoadRatio;
                DataLoopNode::Node(WaterControlNode).MassFlowRate = waterMdot;
            }
        }

        Real64 coolingPLR = 0.0;
        Real64 heatingPLR = 0.0;

        if (WaterControlNode > 0 && WaterControlNode == thisSys.CoolCoilFluidInletNode) {
            // cooling load using water cooling coil
            coolingPLR = PartLoadRatio;
            thisSys.m_CoolingPartLoadFrac = PartLoadRatio;
            thisSys.CoolCoilWaterFlowRatio = DataLoopNode::Node(WaterControlNode).MassFlowRate / thisSys.MaxCoolCoilFluidFlow;
        } else if (WaterControlNode > 0 && WaterControlNode == thisSys.HeatCoilFluidInletNode) {
            // heating load using water heating coil
            heatingPLR = PartLoadRatio;
            thisSys.m_HeatingPartLoadFrac = PartLoadRatio;
            thisSys.HeatCoilWaterFlowRatio = DataLoopNode::Node(WaterControlNode).MassFlowRate / thisSys.MaxHeatCoilFluidFlow;
        } else if (coolingLoad) { // non-water coil with cooling load
            coolingPLR = PartLoadRatio;
            thisSys.m_CoolingPartLoadFrac = coolingPLR;
        } else { // must be non-water coil with heating load
            heatingPLR = PartLoadRatio;
            thisSys.m_HeatingPartLoadFrac = heatingPLR;
        }

        Real64 SensOutput = 0.0;
        Real64 LatOutput = 0.0;
        thisSys.calcUnitarySystemToLoad(AirLoopNum,
                                        FirstHVACIteration,
                                        coolingPLR,
                                        heatingPLR,
                                        OnOffAirFlowRat,
                                        SensOutput,
                                        LatOutput,
                                        HXUnitOn,
                                        HeatCoilLoad,
                                        SupHeaterLoad,
                                        1.0);

        if (LoadIsTarget) {
            // Calculate residual based on output magnitude
            if (std::abs(QZnReq) <= 100.0) {
                Residuum = (SensOutput - QZnReq) / 100.0;
            } else {
                Residuum = (SensOutput - QZnReq) / QZnReq;
            }
        } else {
            // Calculate residual based on outlet temperature
            Residuum = (DataLoopNode::Node(thisSys.AirOutNode).Temp - SATempTarget) * 10.0;
        }

        return Residuum;
    }

    void UnitarySys::setSpeedVariables(bool const SensibleLoad,   // True when meeting a sensible load (not a moisture load)
                                       Real64 const PartLoadRatio // operating PLR
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines operating PLR and calculates the load based system output.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool errFlag = false;           // error flag returned from subroutine
        Real64 RuntimeFrac = 0.0;       // heat pump runtime fraction
        Real64 OnOffAirFlowRatio = 0.0; // compressor on to average flow rate

        if (HeatingLoad && SensibleLoad) {
            this->m_CoolingSpeedRatio = 0.0;
            this->m_CoolingCycRatio = 0.0;
            if (this->m_MultiSpeedHeatingCoil || this->m_VarSpeedHeatingCoil) {
                if (this->m_HeatingSpeedNum <= 1) {
                    this->m_HeatingSpeedRatio = 0.0;
                    this->m_HeatingCycRatio = PartLoadRatio;
                    if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                        DataHVACGlobals::MSHPMassFlowRateLow = CompOnMassFlow; // #5737
                    } else {
                        DataHVACGlobals::MSHPMassFlowRateLow = CompOnMassFlow * PartLoadRatio; // #5518
                    }
                } else {
                    if (this->m_SingleMode == 0) {
                        this->m_HeatingSpeedRatio = PartLoadRatio;
                        this->m_HeatingCycRatio = 1.0;
                    } else {
                        this->m_HeatingSpeedRatio = 1.0;
                        this->m_HeatingCycRatio = PartLoadRatio;
                    }
                }
            } else if (this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                       this->m_HeatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP) {
                this->heatPumpRunFrac(PartLoadRatio, errFlag, RuntimeFrac);
                if (RuntimeFrac > 0.0 && this->m_FanOpMode == DataHVACGlobals::CycFanCycCoil) { // was DataHVACGlobals::ContFanCycCoil
                    DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / RuntimeFrac;
                } else {
                    DataHVACGlobals::OnOffFanPartLoadFraction = 1;
                }
                this->m_CompPartLoadRatio = PartLoadRatio;
                this->m_WSHPRuntimeFrac = RuntimeFrac;
                this->m_HeatingSpeedNum = 0;
            }
        } else {
            this->m_HeatingSpeedRatio = 0.0;
            this->m_HeatingCycRatio = 0.0;
            if (this->m_MultiSpeedCoolingCoil || this->m_VarSpeedCoolingCoil) {
                if (this->m_CoolingSpeedNum <= 1) {
                    this->m_CoolingSpeedRatio = 0.0;
                    this->m_CoolingCycRatio = PartLoadRatio;
                    if (this->m_FanOpMode == DataHVACGlobals::ContFanCycCoil) {
                        DataHVACGlobals::MSHPMassFlowRateLow = CompOnMassFlow; // #5737
                    } else {
                        DataHVACGlobals::MSHPMassFlowRateLow = CompOnMassFlow * PartLoadRatio; // #5518
                    }
                } else {
                    if (this->m_SingleMode == 0) {
                        this->m_CoolingSpeedRatio = PartLoadRatio;
                        this->m_CoolingCycRatio = 1.0;
                    } else {
                        this->m_CoolingSpeedRatio = 1.0;
                        this->m_CoolingCycRatio = PartLoadRatio;
                    }
                }
            } else if (this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple ||
                       this->m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP) {
                this->heatPumpRunFrac(PartLoadRatio, errFlag, RuntimeFrac);
                if (RuntimeFrac > 0.0 &&
                    this->m_FanOpMode ==
                        DataHVACGlobals::CycFanCycCoil) { // was DataHVACGlobals::ContFanCycCoil, maybe file an issue or see if it fixes some
                    DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / RuntimeFrac;
                } else {
                    DataHVACGlobals::OnOffFanPartLoadFraction = 1.0;
                }
                this->m_CompPartLoadRatio = PartLoadRatio;
                this->m_WSHPRuntimeFrac = RuntimeFrac;
                this->m_CoolingSpeedNum = 0;
            } else if (this->m_CoolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                if (this->m_CoolingSpeedNum == 1) {
                    this->m_CoolingSpeedRatio = 0.0;
                    this->m_CoolingCycRatio = PartLoadRatio;
                } else {
                    this->m_CoolingSpeedRatio = PartLoadRatio;
                    this->m_CoolingCycRatio = 1.0;
                }
            } else {
                this->m_CoolingSpeedNum = 0;
            }
        }
        OnOffAirFlowRatio = 1.0;
        this->setAverageAirFlow(PartLoadRatio, OnOffAirFlowRatio);
    }

    void UnitarySys::checkUnitarySysCoilInOASysExists(std::string const &UnitarySysName, int const ZoneOAUnitNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   April 2013

        // PURPOSE OF THIS SUBROUTINE:
        // After making sure get input is done, checks if the Coil System DX coil is in the
        // OA System.  IF exists then the DX cooling coil is 100% DOAS DX coil.
        // METHODOLOGY EMPLOYED:
        // Based on CheckDXCoolingCoilInOASysExists by Bereket Nigusse in HVACDXSystem

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CheckUnitarySysCoilInOASysExists: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        if (UnitarySystems::getInputOnceFlag) {
            getUnitarySystemInput(UnitarySysName, false, ZoneOAUnitNum);
            UnitarySystems::getInputOnceFlag = false;
        }

        bool UnitarySysFound = false;
        if (numUnitarySystems > 0) {
            for (int UnitarySysNum = 0; UnitarySysNum < numUnitarySystems; ++UnitarySysNum) {
                if (UtilityRoutines::SameString(UnitarySysName, unitarySys[UnitarySysNum].Name)) {
                    if (unitarySys[UnitarySysNum].m_ThisSysInputShouldBeGotten) getUnitarySystemInput(UnitarySysName, false, ZoneOAUnitNum);
                    if (unitarySys[UnitarySysNum].m_ISHundredPercentDOASDXCoil) {
                        if (!(unitarySys[UnitarySysNum].m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed ||
                              unitarySys[UnitarySysNum].m_CoolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {
                            DXCoils::SetDXCoilTypeData(unitarySys[UnitarySysNum].m_CoolingCoilName);
                        }
                    }
                    UnitarySysFound = true;
                    break;
                }
            }
            if (!UnitarySysFound) {
                ShowSevereError(RoutineName + "System not found = UnitarySystem \"" + UnitarySysName + "\"");
            }
        } else {
            ShowSevereError(RoutineName + "System not found = UnitarySystem \"" + UnitarySysName + "\"");
        }
    }

    void UnitarySys::getUnitarySysHeatCoolCoil(std::string const &UnitarySysName, // Name of Unitary System object
                                               bool &CoolingCoil,                 // Cooling coil exists
                                               bool &HeatingCoil,                 // Heating coil exists
                                               int const ZoneOAUnitNum            // index to zone OA unit
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   April 2013

        // PURPOSE OF THIS FUNCTION:
        // Determined weather Unitary system has heating or cooling coils

        if (UnitarySystems::getInputOnceFlag) {
            getUnitarySystemInput(UnitarySysName, false, ZoneOAUnitNum);
            UnitarySystems::getInputOnceFlag = false;
        }

        for (int UnitarySysNum = 0; UnitarySysNum < numUnitarySystems; ++UnitarySysNum) {
            if (UtilityRoutines::SameString(UnitarySysName, unitarySys[UnitarySysNum].Name)) {
                if (unitarySys[UnitarySysNum].m_ThisSysInputShouldBeGotten) getUnitarySystemInput(UnitarySysName, false, ZoneOAUnitNum);
                if (unitarySys[UnitarySysNum].m_CoolCoilExists) {
                    CoolingCoil = true;
                }
                if (unitarySys[UnitarySysNum].m_HeatCoilExists || unitarySys[UnitarySysNum].m_SuppCoilExists) {
                    HeatingCoil = true;
                }
                break;
            }
        }
    }

} // namespace UnitarySystems
} // namespace EnergyPlus
