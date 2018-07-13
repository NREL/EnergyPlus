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
#include <PackagedThermalStorageCoil.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportCoilSelection.hh>
#include <ReportSizingManager.hh>
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
        bool initUnitarySystemsOneTimeFlag(true); // one time flag
        int numUnitarySystemsSized(0);            // counter used to delete UnitarySystemNumericFields array after last system is sized
        bool initUnitarySystemsErrFlag(false);
        bool initUnitarySystemsErrorsFound(false);
        bool initLoadBasedControlOneTimeFlag(true);
        bool initLoadBasedControlAirLoopPass(true);
        int airLoopPassCounter(0);
        bool initLoadBasedControlFlowFracFlagReady(true);
        Real64 initLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax(0.0);
        Real64 initUnitarySystemsQActual(0.0);
        bool getUnitarySystemDoOnlyOnceFlag(true); // for things that should only be done once in GetUnitarySystemInputData
    }                                              // namespace

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
        : designSpecMSHPType_Num(0), noLoadAirFlowRateRatio(0.0), numOfSpeedHeating(0), numOfSpeedCooling(0), singleModeFlag(false)
    {
    }

    UnitarySys::UnitarySys() // constructor
        : unitarySystemType_Num(0), myGetInputSuccessfulFlag(false), thisSysInputShouldBeGotten(true), sysAvailSchedPtr(0),
          controlType(controlTypeEnum::controlTypeNone), controlZoneNum(0), dehumidControlType_Num(dehumCtrlTypeEnum::dehumidControl_None),
          humidistat(false), airInNode(0), airOutNode(0), validASHRAECoolCoil(false), validASHRAEHeatCoil(false), simASHRAEModel(false), fanIndex(0),
          fanPlace(fanPlaceEnum::notYetSet), fanOpModeSchedPtr(0), fanExists(false), fanType_Num(0), requestAutoSize(false),
          actualFanVolFlowRate(0.0), designFanVolFlowRate(0.0), designMassFlowRate(0.0), fanAvailSchedPtr(0),
          fanOpMode(fanOpModeEnum::fanOpModeNotYetSet), ATMixerIndex(0), ATMixerType(0), ATMixerPriNode(0), ATMixerSecNode(0), ATMixerOutNode(0),
          ATMixerExists(false), nodeNumOfControlledZone(0), airLoopEquipment(false), zoneInletNode(0), zoneSequenceCoolingNum(0),
          zoneSequenceHeatingNum(0), heatCoilExists(false), heatingSizingRatio(0.0), heatingCoilType_Num(0), DXHeatingCoil(false),
          heatingCoilIndex(0), heatingCoilAvailSchPtr(0), designHeatingCapacity(0.0), maxHeatAirVolFlow(0.0), numOfSpeedHeating(0),
          heatCoilFluidInletNode(0), maxHeatCoilFluidFlow(0.0), multiSpeedHeatingCoil(false), varSpeedHeatingCoil(false), systemHeatControlNodeNum(0),
          heatCoilInletNodeNum(0), heatCoilOutletNodeNum(0), coolCoilExists(false), coolingCoilType_Num(0), numOfSpeedCooling(0),
          coolingCoilAvailSchPtr(0), designCoolingCapacity(0.0), maxCoolAirVolFlow(0.0), condenserNodeNum(0), condenserType(0), coolingCoilIndex(0),
          heatPump(false), actualDXCoilIndexForHXAssisted(0), maxCoolCoilFluidFlow(0.0), coolCoilFluidInletNode(0), multiSpeedCoolingCoil(false),
          varSpeedCoolingCoil(false), systemCoolControlNodeNum(0), coolCoilInletNodeNum(0), coolCoilOutletNodeNum(0), waterCyclingMode(0),
          ISHundredPercentDOASDXCoil(false), designMinOutletTemp(0.0), runOnSensibleLoad(false), runOnLatentLoad(false),
          runOnLatentOnlyWithSensible(false), dehumidificationMode(0), suppHeatCoilType_Num(0), suppCoilExists(0), designSuppHeatingCapacity(0.0),
          suppCoilAirInletNode(0), suppCoilAirOutletNode(0), suppCoilFluidInletNode(0), maxSuppCoilFluidFlow(0.0), suppHeatCoilIndex(0),
          suppHeatControlNodeNum(0), coolingSAFMethod(0), heatingSAFMethod(0), noCoolHeatSAFMethod(0), maxNoCoolHeatAirVolFlow(0.0),
          airFlowControl(useCompFlow::flowNotYetSet), coolingCoilUpstream(false), designMaxOutletTemp(0.0), maxOATSuppHeat(0.0),
          minOATCompressorCooling(0.0), minOATCompressorHeating(0.0), maxONOFFCyclesperHour(0.0), HPTimeConstant(0.0), onCyclePowerFraction(0.0),
          fanDelayTime(0.0), ancillaryOnPower(0.0), ancillaryOffPower(0.0), designHRWaterVolumeFlow(0.0), maxHROutletWaterTemp(0.0),
          idleVolumeAirRate(0.0), heatRecActive(false), heatRecoveryInletNodeNum(0), heatRecoveryOutletNodeNum(0), noLoadAirFlowRateRatio(0.0),
          idleMassFlowRate(0.0), idleSpeedRatio(0.0), singleMode(0), multiOrVarSpeedHeatCoil(false), multiOrVarSpeedCoolCoil(false),
          partLoadFrac(0.0), coolingPartLoadFrac(0.0), heatingPartLoadFrac(0.0), suppHeatPartLoadFrac(0.0), heatCompPartLoadRatio(0.0),
          coolCompPartLoadRatio(0.0), speedRatio(0.0), cycRatio(0.0), myEnvrnFlag(true), myPlantScanFlag(true), mySuppCoilPlantScanFlag(true),
          mySetPointCheckFlag(true), mySizingCheckFlag(true), initHeatPump(false), HRLoopNum(0), HRLoopSideNum(0), HRBranchNum(0), HRCompNum(0),
          coolCoilLoopNum(0), coolCoilLoopSide(0), coolCoilBranchNum(0), coolCoilCompNum(0), coolCoilFluidOutletNodeNum(0), heatCoilLoopNum(0),
          heatCoilLoopSide(0), heatCoilBranchNum(0), heatCoilCompNum(0), heatCoilFluidOutletNodeNum(0), suppCoilLoopNum(0), suppCoilLoopSide(0),
          suppCoilBranchNum(0), suppCoilCompNum(0), suppCoilFluidOutletNodeNum(0), maxCoolAirMassFlow(0.0), maxHeatAirMassFlow(0.0),
          maxNoCoolHeatAirMassFlow(0.0), WSHPRuntimeFrac(0.0), compPartLoadRatio(0.0), coolingCoilSensDemand(0.0), coolingCoilLatentDemand(0.0),
          heatingCoilSensDemand(0.0), senLoadLoss(0.0), latLoadLoss(0.0), designHeatRecMassFlowRate(0.0), heatRecoveryMassFlowRate(0.0),
          heatRecoveryRate(0.0), heatRecoveryEnergy(0.0), heatRecoveryInletTemp(0.0), heatRecoveryOutletTemp(0.0), iterationCounter(0),
          desiredOutletTemp(0.0), desiredOutletHumRat(0.0), frostControlStatus(0), coolingCycRatio(0.0), coolingSpeedRatio(0.0), coolingSpeedNum(0),
          heatingCycRatio(0.0), heatingSpeedRatio(0.0), heatingSpeedNum(0), speedNum(0), dehumidInducedHeatingDemandRate(0.0),
          coolCoilWaterFlowRatio(0.0), heatCoilWaterFlowRatio(0.0), fanPartLoadRatio(0.0), totalAuxElecPower(0.0), heatingAuxElecConsumption(0.0),
          coolingAuxElecConsumption(0.0), elecPower(0.0), elecPowerConsumption(0.0), lastMode(0), firstPass(true), totCoolEnergyRate(0.0),
          sensCoolEnergyRate(0.0), latCoolEnergyRate(0.0), totHeatEnergyRate(0.0), sensHeatEnergyRate(0.0), latHeatEnergyRate(0.0),
          designFanVolFlowRateEMSOverrideOn(false), maxHeatAirVolFlowEMSOverrideOn(false), maxCoolAirVolFlowEMSOverrideOn(false),
          maxNoCoolHeatAirVolFlowEMSOverrideOn(false), designFanVolFlowRateEMSOverrideValue(0.0), maxHeatAirVolFlowEMSOverrideValue(0.0),
          maxCoolAirVolFlowEMSOverrideValue(0.0), maxNoCoolHeatAirVolFlowEMSOverrideValue(0.0), EMSOverrideSensZoneLoadRequest(false),
          EMSOverrideMoistZoneLoadRequest(false), EMSSensibleZoneLoadValue(0.0), EMSMoistureZoneLoadValue(0.0), stageNum(0), staged(false),
          coolCountAvail(0), coolIndexAvail(0), heatCountAvail(0), heatIndexAvail(0), heatingFanSpeedRatio(0.0), coolingFanSpeedRatio(0.0),
          noHeatCoolSpeedRatio(0.0), myFanFlag(true), myCheckFlag(true), controlZoneMassFlowFrac(0.0), sensibleLoadMet(0.0), latentLoadMet(0.0),
          myStagedFlag(false), sensibleLoadPredicted(0.0), moistureLoadPredicted(0.0), faultyCoilSATFlag(false), faultyCoilSATIndex(0),
          faultyCoilSATOffset(0.0), TESOpMode(0), HXAssistedSensPLRIter(0), HXAssistedSensPLRIterIndex(0), HXAssistedSensPLRFail(0),
          HXAssistedSensPLRFailIndex(0), HXAssistedSensPLRFail2(0), HXAssistedSensPLRFailIndex2(0), HXAssistedLatPLRIter(0),
          HXAssistedLatPLRIterIndex(0), HXAssistedLatPLRFail(0), HXAssistedLatPLRFailIndex(0), HXAssistedCRLatPLRIter(0),
          HXAssistedCRLatPLRIterIndex(0), HXAssistedCRLatPLRFail(0), HXAssistedCRLatPLRFailIndex(0), HXAssistedCRLatPLRFail2(0),
          HXAssistedCRLatPLRFailIndex2(0), SensPLRIter(0), SensPLRIterIndex(0), SensPLRFail(0), SensPLRFailIndex(0), LatPLRIter(0),
          LatPLRIterIndex(0), LatPLRFail(0), LatPLRFailIndex(0), HeatCoilSensPLRIter(0), HeatCoilSensPLRIterIndex(0), HeatCoilSensPLRFail(0),
          HeatCoilSensPLRFailIndex(0), SuppHeatCoilSensPLRIter(0), SuppHeatCoilSensPLRIterIndex(0), SuppHeatCoilSensPLRFail(0),
          SuppHeatCoilSensPLRFailIndex(0), DXCoilSensPLRIter(0), DXCoilSensPLRIterIndex(0), DXCoilSensPLRFail(0), DXCoilSensPLRFailIndex(0),
          MSpdSensPLRIter(0), MSpdSensPLRIterIndex(0), MSpdCycSensPLRIter(0), MSpdCycSensPLRIterIndex(0), MSpdLatPLRIter(0), MSpdLatPLRIterIndex(0),
          MSpdCycLatPLRIter(0), MSpdCycLatPLRIterIndex(0), LatMaxIterIndex(0), LatRegulaFalsIFailedIndex(0), lowSpeedCoolFanRatio(0.0),
          lowSpeedHeatFanRatio(0.0)
    {
    }

    void UnitarySys::simulate(std::string const &unitarySystemName,
                              bool const FirstHVACIteration,
                              int const &AirLoopNum,
                              int &CompIndex,
                              bool &HeatActive,
                              bool &CoolActive,
                              int const OAUnitNum,
                              Real64 const OAUCoilOutTemp,
                              bool const ZoneEquipment)
    {
        int CompOn = 0;
        int unitarySysNum(-1);

        // Obtains and Allocates unitary system related parameters from input file
        if (this->thisSysInputShouldBeGotten) {
            // Get the unitary system input
            getUnitarySystemInput(unitarySystemName);
            if (this->myGetInputSuccessfulFlag) this->thisSysInputShouldBeGotten = false;
        }

        // Find the correct unitary system Number
        if (CompIndex == 0) {
            // unitarySysNum = UtilityRoutines::FindItemInList(unitarySystemName, UnitarySystems::UnitarySys);
            unitarySysNum = getUnitarySystemIndex(unitarySystemName);
            if (unitarySysNum == -1) { // zero based index
                ShowFatalError("SimUnitarySystem: Unitary System not found=" + unitarySystemName);
            }
            CompIndex = unitarySysNum;
        } else {
            unitarySysNum = CompIndex;
            // zero based index
            if (unitarySysNum > (numUnitarySystems - 1) || unitarySysNum < 0) {
                ShowFatalError("SimUnitarySystem:  Invalid CompIndex passed=" + General::TrimSigDigits(unitarySysNum) + ", Number of Unit Systems=" +
                               General::TrimSigDigits(numUnitarySystems) + ", Unitary System name=" + unitarySystemName);
            }
            // if (CheckEquipName(unitarySysNum)) {
            //    if (UnitarySystemName != this->Name) {
            //        ShowFatalError("SimUnitarySystem: Invalid CompIndex passed=" + TrimSigDigits(UnitarySysNum) + ", Unitary System name=" +
            //                       UnitarySystemName + ", stored Unit Name for that index=" + this->Name);
            //    }
            //    CheckEquipName(UnitarySysNum) = false;
            //}
        }

        // if (!this->myGetInputSuccessfulFlag) {
        //    // Need to do inputs again for this and any others
        //    UnitarySystems::getInputFlag = true;
        //    this->getUnitarySystemInput(this->unitarySystemName);
        //    UnitarySystems::getInputFlag = false;
        //}

        // if (present(HeatActive)) HeatActive = false;
        // if (present(CoolActive)) CoolActive = false;
        HeatActive = false;
        CoolActive = false;

        fanSpeedRatio = 1.0;
        // if (present(ZoneEquipment)) {
        if (ZoneEquipment) {
            this->initUnitarySystems(0, FirstHVACIteration, OAUnitNum, OAUCoilOutTemp);
        } else {
            this->initUnitarySystems(AirLoopNum, FirstHVACIteration, OAUnitNum, OAUCoilOutTemp);
        }

        // MassFlowRateMaxAvail issues are impeding non-VAV air loop equipment by limiting air flow
        // temporarily open up flow limits while simulating, and then set this same value at the INLET after this parent has simulated
        Real64 tempMassFlowRateMaxAvail = DataLoopNode::Node(this->airInNode).MassFlowRateMaxAvail;
        DataLoopNode::Node(this->airInNode).MassFlowRateMaxAvail = this->designMassFlowRate;

        bool HXUnitOn = false;
        {
            auto const SELECT_CASE_var(this->controlType);
            if (SELECT_CASE_var == controlTypeEnum::controlTypeSetpoint) {
                // if (present(ZoneEquipment)) {
                if (ZoneEquipment) {
                    this->controlUnitarySystemtoSP(0, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                } else {
                    this->controlUnitarySystemtoSP(AirLoopNum, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                }
            } else if (SELECT_CASE_var == controlTypeEnum::controlTypeLoad || SELECT_CASE_var == controlTypeEnum::controlTypeCCMASHRAE) {
                // if (present(ZoneEquipment)) {
                if (ZoneEquipment) {
                    // ControlUnitarySystemtoLoad(UnitarySysNum, 0, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                } else {
                    // ControlUnitarySystemtoLoad(UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                }
            }
        }

        // Report the current output
        // if (present(ZoneEquipment)) {
        if (ZoneEquipment) {
            this->reportUnitarySystem(0);
        } else {
            this->reportUnitarySystem(AirLoopNum);
        }

        // if (present(CoolActive)) {
        CoolActive = false;
        if (this->coolingPartLoadFrac * double(CompOn) > 0.0) CoolActive = true;
        //}
        // if (present(HeatActive)) {
        HeatActive = false;
        if (this->heatingPartLoadFrac * double(CompOn) > 0.0 || this->suppHeatPartLoadFrac * double(CompOn) > 0.0) HeatActive = true;
        //}

        // set econo lockout flag
        // If the sysem is not an equipment of Outdoor air unit
        //  IF (AirLoopNum /=-1 .AND. ALLOCATED(AirLoopControlInfo) .AND. UnitarySystem(UnitarySysNum)%AirLoopEquipment) THEN
        if (AirLoopNum > 0 && allocated(DataAirLoop::AirLoopControlInfo) && this->airLoopEquipment) {

            if ((this->heatCompPartLoadRatio > 0.0 || this->speedRatio > 0.0 || this->cycRatio > 0.0) &&
                DataAirLoop::AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor) {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithCompressor = true;
            } else {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithCompressor = false;
            }

            // if (present(HeatActive)) {
            if ((HeatActive) && (DataAirLoop::AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor ||
                                 DataAirLoop::AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithHeating)) {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithHeating = true;
            } else {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithHeating = false;
            }
            //} else {
            //    DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithHeating = false;
            //}
        }

        // Calculate heat recovery
        if (this->heatRecActive) {
            this->unitarySystemHeatRecovery();
        }

        // Coils should have been sized by now. Set this flag to false in case other equipment is downstream of Unitary System.
        // can't do this since there are other checks that need this flag (e.g., HVACManager, SetHeatToReturnAirFlag())
        //  AirLoopControlInfo(AirLoopNum)%UnitarySys = .FALSE.

        DataLoopNode::Node(this->airInNode).MassFlowRateMaxAvail = tempMassFlowRateMaxAvail;
    }

    DesignSpecMSHP *DesignSpecMSHP::factory(int object_type_of_num, std::string const objectName)
    {

        if (getMSHPInputOnceFlag) {
            DesignSpecMSHP::getDesignSpecMSHP();
            getMSHPInputOnceFlag = false;
        }
        for (auto &dSpec : designSpecMSHP) {
            if (UtilityRoutines::SameString(dSpec.name, objectName) && dSpec.designSpecMSHPType_Num == object_type_of_num) {
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
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                DesignSpecMSHP thisDesignSpec;

                thisDesignSpec.name = thisObjectName;
                thisDesignSpec.numOfSpeedHeating = fields.at("number_of_speeds_for_heating"); // required field
                thisDesignSpec.numOfSpeedCooling = fields.at("number_of_speeds_for_cooling"); // required field
                int maxSpeeds = max(thisDesignSpec.numOfSpeedHeating, thisDesignSpec.numOfSpeedCooling);
                thisDesignSpec.designSpecMSHPType_Num = 1; // add global int value for factory

                std::string loc_singleModeOp("No");
                if (fields.find("single_mode_operation") != fields.end()) { // not required field
                    loc_singleModeOp = UtilityRoutines::MakeUPPERCase(fields.at("single_mode_operation"));
                }
                // set single mode flag
                if (UtilityRoutines::SameString(loc_singleModeOp, "Yes")) {
                    thisDesignSpec.singleModeFlag = true;
                } else if (UtilityRoutines::SameString(loc_singleModeOp, "No")) {
                    thisDesignSpec.singleModeFlag = false;
                } else {
                }

                Real64 loc_noLoadAirFlowRateRatio(1.0);
                if (fields.find("no_load_supply_air_flow_rate_ratio") != fields.end()) { // not required field
                    loc_noLoadAirFlowRateRatio = fields.at("no_load_supply_air_flow_rate_ratio");
                }
                thisDesignSpec.noLoadAirFlowRateRatio = loc_noLoadAirFlowRateRatio;

                thisDesignSpec.heatingVolFlowRatio.resize(maxSpeeds);
                thisDesignSpec.coolingVolFlowRatio.resize(maxSpeeds);

                auto speedFlowRatios = fields.find("flow_ratios");
                if (speedFlowRatios != fields.end()) {
                    auto flowRatioArray = speedFlowRatios.value();
                    int numSpeedInputs = flowRatioArray.size();
                    if (numSpeedInputs == maxSpeeds) {
                        int speedNum = -1;
                        for (auto flowRatio : flowRatioArray) {
                            speedNum += 1;
                            auto coolingSpeedRatioObject = flowRatio.at("cooling_speed_supply_air_flow_ratio");
                            if (coolingSpeedRatioObject == "Autosize") {
                                thisDesignSpec.coolingVolFlowRatio[speedNum] = -99999;
                            } else {
                                thisDesignSpec.coolingVolFlowRatio[speedNum] = coolingSpeedRatioObject;
                            }
                            auto heatingSpeedRatioObject = flowRatio.at("heating_speed_supply_air_flow_ratio");
                            if (heatingSpeedRatioObject == "Autosize") {
                                thisDesignSpec.heatingVolFlowRatio[speedNum] = -99999;
                            } else {
                                thisDesignSpec.heatingVolFlowRatio[speedNum] = heatingSpeedRatioObject;
                            }
                        }
                    } else if (numSpeedInputs < maxSpeeds) {
                        ShowSevereError(cCurrentModuleObject + ": Error getting inputs for system named: " + thisObjectName);
                        ShowContinueError("Number of speed inputs (" + General::TrimSigDigits(Real64(numSpeedInputs), 0) +
                                          " is less than number of speeds (" + General::TrimSigDigits(Real64(maxSpeeds), 0) + ").");
                        errorsFound = true;
                    } else {
                        ShowSevereError(cCurrentModuleObject + ": Error getting inputs for system named: " + thisObjectName);
                        ShowContinueError("Number of speed inputs (" + General::TrimSigDigits(Real64(numSpeedInputs), 0) +
                                          " is greater than number of speeds (" + General::TrimSigDigits(Real64(maxSpeeds), 0) + ").");
                        errorsFound = true;
                    }
                }
                designSpecMSHP.push_back(thisDesignSpec);
            }
        }
    } // namespace UnitarySystems

    UnitarySys *UnitarySys::factory(int object_type_of_num, std::string const objectName)
    {
        if (UnitarySystems::getInputOnceFlag) {
            UnitarySys::getUnitarySystemInput(objectName);
            UnitarySystems::getInputOnceFlag = false;
        }
        for (auto &sys : unitarySys) {
            if (UtilityRoutines::SameString(sys.name, objectName) && sys.unitarySystemType_Num == object_type_of_num) {
                UnitarySys *thisSys = &sys;
                if (thisSys->myGetInputSuccessfulFlag) thisSys->thisSysInputShouldBeGotten = false;
                return &sys;
            }
        }
        ShowFatalError("UnitarySystem factory: Error getting inputs for system named: " + objectName);
        return nullptr;
    }

    int getDesignSpecMSHPIndex(       // lookup vector index for fan object name in object array EnergyPlus::HVACFan::fanObjs
        std::string const &objectName // IDF name in input
    )
    {
        int index = -1;
        bool found = false;
        for (std::size_t loop = 0; loop < designSpecMSHP.size(); ++loop) {
            DesignSpecMSHP *thisDesignSpecMSHPObjec = &designSpecMSHP[loop];
            if (UtilityRoutines::SameString(objectName, thisDesignSpecMSHPObjec->name)) {
                index = loop;
                found = true;
                break;
            }
        }
        if (!found) {
            ShowSevereError("getDesignSpecMSHPIndex: did not find UnitarySystemPerformance:Multispeed name =" + objectName + ". Check inputs");
        }
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
            if (UtilityRoutines::SameString(objectName, thisUnitarySysObjec->name)) {
                index = loop;
                found = true;
                break;
            }
        }
        return index;
    }

    void UnitarySys::initUnitarySystems(int const &AirLoopNum,
                                        bool const &FirstHVACIteration,
                                        Optional_int_const OAUnitNum,
                                        Optional<Real64 const> OAUCoilOutTemp)
    {
        static std::string const routineName("InitUnitarySystems");

        if (myOneTimeFlag) {
            // initialize or allocate something once
            myOneTimeFlag = false;
        }

        if (!DataGlobals::SysSizingCalc && this->mySizingCheckFlag) {
            if (AirLoopNum > 0) {
                if (this->fanExists && (this->coolCoilExists && (this->heatCoilExists || this->suppCoilExists)))
                    DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySys = true;
                DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating = true;
            }
            this->sizeUnitarySystem(FirstHVACIteration, AirLoopNum);
            this->mySizingCheckFlag = false;
            if (AirLoopNum > 0) {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).FanOpMode = this->fanOpMode;
                DataAirLoop::AirLoopControlInfo(AirLoopNum).CycFanSchedPtr = this->fanOpModeSchedPtr;
            } else if (AirLoopNum < 0) {
                if (this->controlType == controlTypeEnum::controlTypeCCMASHRAE) {
                    ShowSevereError(this->unitType + ": " + this->name);
                    ShowContinueError("  Invalid application of Control Type = SingleZoneVAV in outdoor air system.");
                    ShowFatalError("InitUnitarySystems: Program terminated for previous conditions.");
                }
            }
        }

        if (AirLoopNum == -1) { // This DX system is component of ZoneHVAC:OutdoorAirUnit
            int OutdoorAirUnitNum = OAUnitNum;
            int OAUCoilOutletTemp = OAUCoilOutTemp;
        }

        // Scan hot water and steam heating coil plant components for one time initializations
        if (this->myPlantScanFlag && allocated(DataPlant::PlantLoop)) {
            if (this->heatRecActive) {
                initUnitarySystemsErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(this->name,
                                                        DataPlant::TypeOf_UnitarySystemRecovery,
                                                        this->HRLoopNum,
                                                        this->HRLoopSideNum,
                                                        this->HRBranchNum,
                                                        this->HRCompNum,
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
            if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed ||
                this->coolingCoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted) {
                if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater) {
                    TypeOfCoilWaterCooling = DataPlant::TypeOf_CoilWaterCooling;
                    CoolingCoilType = "Coil:Cooling:Water";
                    CoolingCoilName = this->coolingCoilName;
                } else if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {
                    TypeOfCoilWaterCooling = DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
                    CoolingCoilType = "Coil:Cooling:Water:DetailedGeometry";
                    CoolingCoilName = this->coolingCoilName;
                } else {
                    TypeOfCoilWaterCooling = HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(
                        DataHVACGlobals::cAllCoilTypes(this->coolingCoilType_Num), this->coolingCoilName, initUnitarySystemsErrFlag, true);
                    if (TypeOfCoilWaterCooling == DataHVACGlobals::Coil_CoolingWater) {
                        TypeOfCoilWaterCooling = DataPlant::TypeOf_CoilWaterCooling;
                        CoolingCoilType = "Coil:Cooling:Water";
                    } else if (TypeOfCoilWaterCooling == DataHVACGlobals::Coil_CoolingWaterDetailed) {
                        TypeOfCoilWaterCooling = DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
                        CoolingCoilType = "Coil:Cooling:Water:DetailedGeometry";
                    }
                    CoolingCoilName = HVACHXAssistedCoolingCoil::GetHXDXCoilName(
                        DataHVACGlobals::cAllCoilTypes(this->coolingCoilType_Num), this->coolingCoilName, initUnitarySystemsErrFlag);
                }
                initUnitarySystemsErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(CoolingCoilName,
                                                        TypeOfCoilWaterCooling,
                                                        this->coolCoilLoopNum,
                                                        this->coolCoilLoopSide,
                                                        this->coolCoilBranchNum,
                                                        this->coolCoilCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        initUnitarySystemsErrFlag);
                if (initUnitarySystemsErrFlag) {
                    ShowFatalError("InitUnitarySystem: Program terminated for previous conditions.");
                }
                this->maxCoolCoilFluidFlow = WaterCoils::GetCoilMaxWaterFlowRate(CoolingCoilType, CoolingCoilName, initUnitarySystemsErrorsFound);

                if (this->maxCoolCoilFluidFlow > 0.0) {
                    Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->coolCoilLoopNum).FluidName,
                                                                   DataGlobals::CWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->coolCoilLoopNum).FluidIndex,
                                                                   routineName);
                    this->maxCoolCoilFluidFlow *= rho;
                }
                // fill outlet node for coil
                this->coolCoilFluidOutletNodeNum = DataPlant::PlantLoop(this->coolCoilLoopNum)
                                                       .LoopSide(this->coolCoilLoopSide)
                                                       .Branch(this->coolCoilBranchNum)
                                                       .Comp(this->coolCoilCompNum)
                                                       .NodeNumOut;
            }
            int TypeOfCoilWaterHeating = 0;
            std::string HeatingCoilType = "";
            if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater || this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                    TypeOfCoilWaterHeating = DataPlant::TypeOf_CoilWaterSimpleHeating;
                    HeatingCoilType = "Coil:Heating:Water";
                    WaterCoils::SetCoilDesFlow(DataHVACGlobals::cAllCoilTypes(this->heatingCoilType_Num),
                                               this->heatingCoilName,
                                               this->maxHeatAirVolFlow,
                                               initUnitarySystemsErrorsFound);
                } else {
                    TypeOfCoilWaterHeating = DataPlant::TypeOf_CoilSteamAirHeating;
                    HeatingCoilType = "Coil:Heating:Steam";
                }
                initUnitarySystemsErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(this->heatingCoilName,
                                                        TypeOfCoilWaterHeating,
                                                        this->heatCoilLoopNum,
                                                        this->heatCoilLoopSide,
                                                        this->heatCoilBranchNum,
                                                        this->heatCoilCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        initUnitarySystemsErrFlag);
                if (initUnitarySystemsErrFlag) {
                    ShowFatalError("InitUnitarySystem: Program terminated for previous conditions.");
                }
                if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                    this->maxHeatCoilFluidFlow =
                        WaterCoils::GetCoilMaxWaterFlowRate(HeatingCoilType, this->heatingCoilName, initUnitarySystemsErrorsFound);

                    if (this->maxHeatCoilFluidFlow > 0.0) {
                        Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->heatCoilLoopNum).FluidName,
                                                                       DataGlobals::CWInitConvTemp,
                                                                       DataPlant::PlantLoop(this->heatCoilLoopNum).FluidIndex,
                                                                       routineName);
                        this->maxHeatCoilFluidFlow =
                            WaterCoils::GetCoilMaxWaterFlowRate(HeatingCoilType, this->heatingCoilName, initUnitarySystemsErrorsFound) * rho;
                    }
                } else {
                    this->maxHeatCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(this->heatingCoilIndex, initUnitarySystemsErrorsFound);
                    if (this->maxHeatCoilFluidFlow > 0.0) {
                        int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                        Real64 TempSteamIn = 100.0;
                        Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                        this->maxHeatCoilFluidFlow *= SteamDensity;
                    }
                }
                // fill outlet node for coil
                this->heatCoilFluidOutletNodeNum = DataPlant::PlantLoop(this->heatCoilLoopNum)
                                                       .LoopSide(this->heatCoilLoopSide)
                                                       .Branch(this->heatCoilBranchNum)
                                                       .Comp(this->heatCoilCompNum)
                                                       .NodeNumOut;
            }

            this->myPlantScanFlag = false;

        } else if (this->myPlantScanFlag && !DataGlobals::AnyPlantInModel) {
            this->myPlantScanFlag = false;
        }

        // Scan Supplemental hot water and steam heating coil plant components for one time initializations
        if (this->mySuppCoilPlantScanFlag && allocated(DataPlant::PlantLoop)) {
            if (this->suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                initUnitarySystemsErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(this->suppHeatCoilName,
                                                        DataPlant::TypeOf_CoilWaterSimpleHeating,
                                                        this->suppCoilLoopNum,
                                                        this->suppCoilLoopSide,
                                                        this->suppCoilBranchNum,
                                                        this->suppCoilCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        initUnitarySystemsErrFlag);
                WaterCoils::SetCoilDesFlow(DataHVACGlobals::cAllCoilTypes(this->suppHeatCoilType_Num),
                                           this->suppHeatCoilName,
                                           this->maxHeatAirVolFlow,
                                           initUnitarySystemsErrorsFound);

                if (initUnitarySystemsErrFlag) {
                    ShowFatalError("InitUnitarySystems: Program terminated for previous conditions.");
                }
                this->maxSuppCoilFluidFlow =
                    WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->suppHeatCoilName, initUnitarySystemsErrorsFound);

                if (this->maxSuppCoilFluidFlow > 0.0) {
                    Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->suppCoilLoopNum).FluidName,
                                                                   DataGlobals::CWInitConvTemp,
                                                                   DataPlant::PlantLoop(this->suppCoilLoopNum).FluidIndex,
                                                                   routineName);
                    this->maxSuppCoilFluidFlow =
                        WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->suppHeatCoilName, initUnitarySystemsErrorsFound) * rho;
                }
                // fill outlet node for coil
                this->suppCoilFluidOutletNodeNum = DataPlant::PlantLoop(this->suppCoilLoopNum)
                                                       .LoopSide(this->suppCoilLoopSide)
                                                       .Branch(this->suppCoilBranchNum)
                                                       .Comp(this->suppCoilCompNum)
                                                       .NodeNumOut;

            } else if (this->suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                initUnitarySystemsErrFlag = false;
                PlantUtilities::ScanPlantLoopsForObject(this->suppHeatCoilName,
                                                        DataPlant::TypeOf_CoilSteamAirHeating,
                                                        this->suppCoilLoopNum,
                                                        this->suppCoilLoopSide,
                                                        this->suppCoilBranchNum,
                                                        this->suppCoilCompNum,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        _,
                                                        initUnitarySystemsErrFlag);
                if (initUnitarySystemsErrFlag) {
                    ShowFatalError("InitUnitarySystems: Program terminated for previous conditions.");
                }
                this->maxSuppCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(this->suppHeatCoilIndex, initUnitarySystemsErrorsFound);
                if (this->maxSuppCoilFluidFlow > 0.0) {
                    int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                    Real64 TempSteamIn = 100.0;
                    Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                    this->maxSuppCoilFluidFlow *= SteamDensity;
                }

                // fill outlet node for coil
                this->suppCoilFluidOutletNodeNum = DataPlant::PlantLoop(this->suppCoilLoopNum)
                                                       .LoopSide(this->suppCoilLoopSide)
                                                       .Branch(this->suppCoilBranchNum)
                                                       .Comp(this->suppCoilCompNum)
                                                       .NodeNumOut;
            }

            this->mySuppCoilPlantScanFlag = false;

        } else if (this->mySuppCoilPlantScanFlag && !DataGlobals::AnyPlantInModel) {
            this->mySuppCoilPlantScanFlag = false;
        }

        // do the Begin Environment initializations
        if (DataGlobals::BeginEnvrnFlag && this->myEnvrnFlag) {
            this->designMassFlowRate = this->designFanVolFlowRate * DataEnvironment::StdRhoAir;
            this->maxCoolAirMassFlow = this->maxCoolAirVolFlow * DataEnvironment::StdRhoAir;
            this->maxHeatAirMassFlow = this->maxHeatAirVolFlow * DataEnvironment::StdRhoAir;
            this->maxNoCoolHeatAirMassFlow = this->maxNoCoolHeatAirVolFlow * DataEnvironment::StdRhoAir;
            this->WSHPRuntimeFrac = 0.0;
            this->compPartLoadRatio = 0.0;
            this->coolingCoilSensDemand = 0.0;
            this->coolingCoilLatentDemand = 0.0;
            this->heatingCoilSensDemand = 0.0;
            this->senLoadLoss = 0.0;
            if (this->humidistat) {
                this->latLoadLoss = 0.0;
            }

            if ((this->heatRecActive) && (!this->myPlantScanFlag)) {

                Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HRLoopNum).FluidName,
                                                               DataGlobals::HWInitConvTemp,
                                                               DataPlant::PlantLoop(this->HRLoopNum).FluidIndex,
                                                               routineName);

                this->designHeatRecMassFlowRate = this->designHRWaterVolumeFlow * rho;

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->designHeatRecMassFlowRate,
                                                   this->heatRecoveryInletNodeNum,
                                                   this->heatRecoveryOutletNodeNum,
                                                   this->HRLoopNum,
                                                   this->HRLoopSideNum,
                                                   this->HRBranchNum,
                                                   this->HRCompNum);
            }
            //   set fluid-side hardware limits
            if (this->coolCoilFluidInletNode > 0) {

                if (this->maxCoolCoilFluidFlow == DataSizing::AutoSize) {
                    // If water coil max water flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                    std::string CoolingCoilType = "";
                    if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater) {
                        CoolingCoilType = "Coil:Cooling:Water";
                    } else {
                        CoolingCoilType = "Coil:Cooling:Water:DetailedGeometry";
                    }
                    WaterCoils::SimulateWaterCoilComponents(this->coolingCoilName, FirstHVACIteration, this->coolingCoilIndex);
                    Real64 CoilMaxVolFlowRate =
                        WaterCoils::GetCoilMaxWaterFlowRate(CoolingCoilType, this->coolingCoilName, initUnitarySystemsErrorsFound);
                    if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                        Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->coolCoilLoopNum).FluidName,
                                                                       DataGlobals::CWInitConvTemp,
                                                                       DataPlant::PlantLoop(this->coolCoilLoopNum).FluidIndex,
                                                                       routineName);
                        this->maxCoolCoilFluidFlow = CoilMaxVolFlowRate * rho;
                    }
                }

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->maxCoolCoilFluidFlow,
                                                   this->coolCoilFluidInletNode,
                                                   this->coolCoilFluidOutletNodeNum,
                                                   this->coolCoilLoopNum,
                                                   this->coolCoilLoopSide,
                                                   this->coolCoilBranchNum,
                                                   this->coolCoilCompNum);
            }
            if (this->heatCoilFluidInletNode > 0) {

                if (this->maxHeatCoilFluidFlow == DataSizing::AutoSize) {
                    // IF water coil max water flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                    if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                        WaterCoils::SimulateWaterCoilComponents(this->heatingCoilName, FirstHVACIteration, this->heatingCoilIndex);
                        Real64 CoilMaxVolFlowRate =
                            WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->heatingCoilName, initUnitarySystemsErrorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->heatCoilLoopNum).FluidName,
                                                                           DataGlobals::CWInitConvTemp,
                                                                           DataPlant::PlantLoop(this->heatCoilLoopNum).FluidIndex,
                                                                           routineName);
                            this->maxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                    }
                    // If steam coil max steam flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                    if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                        SteamCoils::SimulateSteamCoilComponents(this->heatingCoilName,
                                                                FirstHVACIteration,
                                                                this->heatingCoilIndex,
                                                                1.0,
                                                                initUnitarySystemsQActual); // QCoilReq, simulate any load > 0 to get max capacity
                        Real64 CoilMaxVolFlowRate = SteamCoils::GetCoilMaxSteamFlowRate(this->heatingCoilIndex, initUnitarySystemsErrorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            Real64 TempSteamIn = 100.0;
                            Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                            this->maxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                    }
                }

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->maxHeatCoilFluidFlow,
                                                   this->heatCoilFluidInletNode,
                                                   this->heatCoilFluidOutletNodeNum,
                                                   this->heatCoilLoopNum,
                                                   this->heatCoilLoopSide,
                                                   this->heatCoilBranchNum,
                                                   this->heatCoilCompNum);
            }
            if (this->suppCoilFluidInletNode > 0) {
                if (this->maxSuppCoilFluidFlow == DataSizing::AutoSize) {
                    if (this->suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                        // If water coil max water flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                        WaterCoils::SimulateWaterCoilComponents(this->suppHeatCoilName, FirstHVACIteration, this->suppHeatCoilIndex);
                        Real64 CoilMaxVolFlowRate =
                            WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->suppHeatCoilName, initUnitarySystemsErrorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->suppCoilLoopNum).FluidName,
                                                                           DataGlobals::CWInitConvTemp,
                                                                           DataPlant::PlantLoop(this->suppCoilLoopNum).FluidIndex,
                                                                           routineName);
                            this->maxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                    }
                    if (this->suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                        SteamCoils::SimulateSteamCoilComponents(this->suppHeatCoilName,
                                                                FirstHVACIteration,
                                                                this->suppHeatCoilIndex,
                                                                1.0,
                                                                initUnitarySystemsQActual); // QCoilReq, simulate any load > 0 to get max capacity
                        Real64 CoilMaxVolFlowRate = SteamCoils::GetCoilMaxSteamFlowRate(this->suppHeatCoilIndex, initUnitarySystemsErrorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            Real64 TempSteamIn = 100.0;
                            Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                            this->maxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                    }
                    PlantUtilities::InitComponentNodes(0.0,
                                                       this->maxSuppCoilFluidFlow,
                                                       this->suppCoilFluidInletNode,
                                                       this->suppCoilFluidOutletNodeNum,
                                                       this->suppCoilLoopNum,
                                                       this->suppCoilLoopSide,
                                                       this->suppCoilBranchNum,
                                                       this->suppCoilCompNum);
                }
            }
            this->myEnvrnFlag = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            this->myEnvrnFlag = true;
        }

        // Init maximum available Heat Recovery flow rate
        if ((this->heatRecActive) && (!this->myPlantScanFlag)) {
            Real64 mdotHR = 0.0;
            if (ScheduleManager::GetCurrentScheduleValue(this->sysAvailSchedPtr) > 0.0) {
                if (FirstHVACIteration) {
                    mdotHR = this->designHeatRecMassFlowRate;
                } else {
                    if (this->heatRecoveryMassFlowRate > 0.0) {
                        mdotHR = this->heatRecoveryMassFlowRate;
                    } else {
                        mdotHR = this->designHeatRecMassFlowRate;
                    }
                }
            } else {
                mdotHR = 0.0;
            }

            mdotHR = min(DataLoopNode::Node(this->heatRecoveryOutletNodeNum).MassFlowRateMaxAvail, mdotHR);
            DataLoopNode::Node(this->heatRecoveryInletNodeNum).MassFlowRate = mdotHR;
        }

        // get operating capacity of water and steam coil
        if (FirstHVACIteration || this->dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
            if (FirstHVACIteration) {
                this->iterationCounter = 0;
                // this->iterationMode[] = 0;
            }
            if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {

                //     set water-side mass flow rates
                Real64 mdot = this->maxCoolCoilFluidFlow;
                PlantUtilities::SetComponentFlowRate(mdot,
                                                     this->coolCoilFluidInletNode,
                                                     this->coolCoilFluidOutletNodeNum,
                                                     this->coolCoilLoopNum,
                                                     this->coolCoilLoopSide,
                                                     this->coolCoilBranchNum,
                                                     this->coolCoilCompNum);
                //     simulate water coil to find operating capacity
                WaterCoils::SimulateWaterCoilComponents(this->coolingCoilName, FirstHVACIteration, this->coolingCoilIndex, initUnitarySystemsQActual);
                this->designCoolingCapacity = initUnitarySystemsQActual;

            } // from IF(UnitarySystem(UnitarySysNum)%CoolingCoilType_Num == Coil_CoolingWater .OR. Coil_CoolingWaterDetailed
            if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {

                //     set water-side mass flow rates
                Real64 mdot = this->maxHeatCoilFluidFlow;
                PlantUtilities::SetComponentFlowRate(mdot,
                                                     this->heatCoilFluidInletNode,
                                                     this->heatCoilFluidOutletNodeNum,
                                                     this->heatCoilLoopNum,
                                                     this->heatCoilLoopSide,
                                                     this->heatCoilBranchNum,
                                                     this->heatCoilCompNum);
                //     simulate water coil to find operating capacity
                WaterCoils::SimulateWaterCoilComponents(this->heatingCoilName, FirstHVACIteration, this->heatingCoilIndex, initUnitarySystemsQActual);
                this->designHeatingCapacity = initUnitarySystemsQActual;

            } // from IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingWater) THEN

            if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {

                //     set water-side mass flow rates
                Real64 mdot = this->maxHeatCoilFluidFlow;
                PlantUtilities::SetComponentFlowRate(mdot,
                                                     this->heatCoilFluidInletNode,
                                                     this->heatCoilFluidOutletNodeNum,
                                                     this->heatCoilLoopNum,
                                                     this->heatCoilLoopSide,
                                                     this->heatCoilBranchNum,
                                                     this->heatCoilCompNum);
                //     simulate steam coil to find operating capacity
                SteamCoils::SimulateSteamCoilComponents(
                    this->heatingCoilName,
                    FirstHVACIteration,
                    this->heatingCoilIndex,
                    1.0,
                    initUnitarySystemsQActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
                this->designHeatingCapacity = SteamCoils::GetCoilCapacity(
                    DataHVACGlobals::cAllCoilTypes(this->heatingCoilType_Num), this->heatingCoilName, initUnitarySystemsErrorsFound);

            } // from IF(UnitarySystem(UnitarySysNum)%HeatingCoilType_Num == Coil_HeatingSteam) THEN
            if (this->suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {

                //     set steam-side mass flow rates
                Real64 mdot = this->maxSuppCoilFluidFlow;
                PlantUtilities::SetComponentFlowRate(mdot,
                                                     this->suppCoilFluidInletNode,
                                                     this->suppCoilFluidOutletNodeNum,
                                                     this->suppCoilLoopNum,
                                                     this->suppCoilLoopSide,
                                                     this->suppCoilBranchNum,
                                                     this->suppCoilCompNum);
                //     simulate water coil to find operating capacity
                if (mdot > 0.0) { // not sure why this is here and not used for other coil types, wouldn't capacity be 0 if water flow = 0? Maybe a
                                  // speed issue where coil doesn't need to be simulation if mdot=0.
                    WaterCoils::SimulateWaterCoilComponents(
                        this->suppHeatCoilName, FirstHVACIteration, this->suppHeatCoilIndex, initUnitarySystemsQActual);
                    this->designSuppHeatingCapacity = initUnitarySystemsQActual;
                } else {
                    this->designSuppHeatingCapacity = 0.0;
                }

            } // from IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingWater) THEN

            if (this->suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {

                //     set air-side and steam-side mass flow rates
                Real64 mdot = this->maxSuppCoilFluidFlow;
                PlantUtilities::SetComponentFlowRate(mdot,
                                                     this->suppCoilFluidInletNode,
                                                     this->suppCoilFluidOutletNodeNum,
                                                     this->suppCoilLoopNum,
                                                     this->suppCoilLoopSide,
                                                     this->suppCoilBranchNum,
                                                     this->suppCoilCompNum);
                //     simulate steam coil to find operating capacity
                SteamCoils::SimulateSteamCoilComponents(
                    this->suppHeatCoilName,
                    FirstHVACIteration,
                    this->suppHeatCoilIndex,
                    1.0,
                    initUnitarySystemsQActual); // QCoilReq, simulate any load > 0 to get max capacity of steam coil
                this->designSuppHeatingCapacity =
                    SteamCoils::GetCoilCapacity("Coil:Heating:Steam", this->suppHeatCoilName, initUnitarySystemsErrorsFound);

            } // from IF(UnitarySystem(UnitarySysNum)%SuppHeatCoilType_Num == Coil_HeatingSteam) THEN
        }     // from IF( FirstHVACIteration ) THEN

        this->iterationCounter += 1;

        if (this->mySetPointCheckFlag) {
            if (!DataGlobals::SysSizingCalc && DataHVACGlobals::DoSetPointTest) {

                if (this->coolCoilExists) {
                    int ControlNode = this->systemCoolControlNodeNum;
                    if (ControlNode > 0) {
                        this->checkNodeSetPoint(AirLoopNum, ControlNode, UnitarySystems::CoolingCoil, OAUCoilOutTemp);
                    }
                }

                if (this->heatCoilExists) {
                    int ControlNode = this->systemHeatControlNodeNum;
                    if (ControlNode > 0) {
                        this->checkNodeSetPoint(AirLoopNum, ControlNode, UnitarySystems::HeatingCoil, OAUCoilOutTemp);
                    }
                }

                if (this->suppCoilExists) {
                    int ControlNode = this->suppHeatControlNodeNum;
                    if (ControlNode > 0) {
                        this->checkNodeSetPoint(AirLoopNum, ControlNode, UnitarySystems::SuppHeatCoil, OAUCoilOutTemp);
                    }
                }

                this->mySetPointCheckFlag = false;
            }
        }

        this->coolingPartLoadFrac = 0.0;
        this->heatingPartLoadFrac = 0.0;
        this->suppHeatPartLoadFrac = 0.0;
        this->coolingCycRatio = 0.0;
        this->coolingSpeedRatio = 0.0;
        this->coolingSpeedNum = 0;
        this->heatingCycRatio = 0.0;
        this->heatingSpeedRatio = 0.0;
        this->heatingSpeedNum = 0;
        this->heatingCoilSensDemand = 0.0;
        this->coolingCoilSensDemand = 0.0;
        this->coolingCoilLatentDemand = 0.0;
        this->dehumidInducedHeatingDemandRate = 0.0;
        this->coolCoilWaterFlowRatio = 0.0;
        this->heatCoilWaterFlowRatio = 0.0;

        // water/steam coil initialization
        if (this->coolCoilFluidInletNode > 0) {
            Real64 mdot = 0.0;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->coolCoilFluidInletNode,
                                                 this->coolCoilFluidOutletNodeNum,
                                                 this->coolCoilLoopNum,
                                                 this->coolCoilLoopSide,
                                                 this->coolCoilBranchNum,
                                                 this->coolCoilCompNum);
        }
        if (this->heatCoilFluidInletNode > 0) {
            Real64 mdot = 0.0;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->heatCoilFluidInletNode,
                                                 this->heatCoilFluidOutletNodeNum,
                                                 this->heatCoilLoopNum,
                                                 this->heatCoilLoopSide,
                                                 this->heatCoilBranchNum,
                                                 this->heatCoilCompNum);
        }
        if (this->suppCoilFluidInletNode > 0) {
            Real64 mdot = 0.0;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->suppCoilFluidInletNode,
                                                 this->suppCoilFluidOutletNodeNum,
                                                 this->suppCoilLoopNum,
                                                 this->suppCoilLoopSide,
                                                 this->suppCoilBranchNum,
                                                 this->suppCoilCompNum);
        }

        this->initHeatPump = true;
        m_massFlow1 = 0.0;
        m_massFlow2 = 0.0;
        m_runTimeFraction1 = 0.0;
        m_runTimeFraction2 = 0.0;
    }

    void UnitarySys::checkNodeSetPoint(int const AirLoopNum,                 // number of the current air loop being simulated
                                       int const ControlNode,                // Node to test for set point
                                       int const CoilType,                   // True if cooling coil, then test for HumRatMax set point
                                       Optional<Real64 const> OAUCoilOutTemp // the coil inlet temperature of OutdoorAirUnit
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
            if (this->ISHundredPercentDOASDXCoil) {
                this->frostControlSetPointLimit(
                    this->desiredOutletTemp, DataLoopNode::Node(ControlNode).HumRatMax, DataEnvironment::OutBaroPress, this->designMinOutletTemp, 1);
            }
        } else if (AirLoopNum != -1) { // Not an Outdoor air unit

            bool SetPointErrorFlag = false;
            if (DataLoopNode::Node(ControlNode).TempSetPoint == DataLoopNode::SensedNodeFlagValue &&
                this->controlType == controlTypeEnum::controlTypeSetpoint) {
                if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                    ShowSevereError(this->unitType + ": Missing temperature setpoint for unitary system = " + this->name);
                    ShowContinueError("  use a Setpoint Manager to establish a setpoint at the coil control node.");
                    SetPointErrorFlag = true;
                } else {
                    EMSManager::CheckIfNodeSetPointManagedByEMS(ControlNode, EMSManager::iTemperatureSetPoint, SetPointErrorFlag);
                    if (SetPointErrorFlag) {
                        ShowSevereError(this->unitType + ": Missing temperature setpoint for unitary system = " + this->name);
                        ShowContinueError("  use a Setpoint Manager to establish a setpoint at the coil control node.");
                        ShowContinueError("  or use an EMS actuator to establish a temperature setpoint at the coil control node.");
                    }
                }
            }
            if ((this->dehumidControlType_Num != dehumCtrlTypeEnum::dehumidControl_None) &&
                (DataLoopNode::Node(ControlNode).HumRatMax == DataLoopNode::SensedNodeFlagValue) &&
                this->controlType == controlTypeEnum::controlTypeSetpoint && CoilType == CoolingCoil) {
                if (!DataGlobals::AnyEnergyManagementSystemInModel) {
                    ShowSevereError(this->unitType + ": Missing humidity ratio setpoint (HUMRATMAX) for unitary system = " + this->name);
                    ShowContinueError("  use a Setpoint Manager to establish a setpoint at the coil control node.");
                    SetPointErrorFlag = true;
                } else {
                    EMSManager::CheckIfNodeSetPointManagedByEMS(ControlNode, EMSManager::iHumidityRatioMaxSetPoint, SetPointErrorFlag);
                    if (SetPointErrorFlag) {
                        ShowSevereError(this->unitType + ": Missing maximum humidity ratio setpoint (HUMRATMAX) for unitary system = " + this->name);
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

        Real64 AirMassFlow = DataLoopNode::Node(this->coolCoilInletNodeNum).MassFlowRate;
        if (ControlMode == RunOnSensible && AirMassFlow > MinAirMassFlow && TempSetPoint < DataLoopNode::Node(this->coolCoilInletNodeNum).Temp) {
            if (TempSetPoint < TfrostControl) {
                TempSetPoint = TfrostControl;
                this->frostControlStatus = 1;
            }
        } else if (ControlMode == RunOnLatent && AirMassFlow > MinAirMassFlow &&
                   HumRatSetPoint < DataLoopNode::Node(this->coolCoilInletNodeNum).HumRat) {
            Real64 HumRatioSat = Psychrometrics::PsyWFnTdpPb(TfrostControl, BaroPress, routineName);
            if (HumRatioSat > HumRatSetPoint) {
                HumRatSetPoint = HumRatioSat;
                this->frostControlStatus = 2;
            }
        } else {
            this->frostControlStatus = 0;
        }
    }

    void UnitarySys::getUnitarySystemInput(std::string const &objectName)
    {

        bool errorsFound(false);

        UnitarySys::getUnitarySystemInputData(objectName, errorsFound);

        if (errorsFound) {
            ShowFatalError("getUnitarySystemInputData: did not find UnitarySystem. Check inputs");
        }
    }

    void UnitarySys::sizeUnitarySystem(bool const FirstHVACIteration,
                                       int const AirLoopNum // does this need to be optional?
    )
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

        // Using/Aliasing
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
        std::string FanName;       // fan name
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

        bool anyEMSRan;
        EMSManager::ManageEMS(DataGlobals::emsCallFromUnitarySystemSizing, anyEMSRan); // calling point

        std::string SizingString("");
        std::string CompName = this->name;
        std::string CompType = this->unitType;
        int CoolingSAFlowMethod = this->coolingSAFMethod;
        int HeatingSAFlowMethod = this->heatingSAFMethod;
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
        int SupFanNum = 0;

        if (DataSizing::CurSysNum > 0 && DataSizing::CurOASysNum == 0 && this->fanExists) {
            if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).supFanVecIndex = this->fanIndex;
                DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).supFanModelTypeEnum = DataAirSystems::objectVectorOOFanSystemModel;
            } else {
                DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).SupFanNum = this->fanIndex;
                DataAirSystems::PrimaryAirSystem(DataSizing::CurSysNum).supFanModelTypeEnum = DataAirSystems::structArrayLegacyFanModels;
            }
        }

        // STEP 1: find the DataSizing::AutoSized cooling air flow rate and capacity
        if (this->coolCoilExists) {
            if (!this->heatCoilExists) DataSizing::ZoneCoolingOnlyFan = true;
            FieldNum = 3; // N3 , \field Cooling Supply Air Flow Rate
            PrintFlag = false;
            SizingMethod = DataHVACGlobals::CoolingAirflowSizing;
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [m3/s]";
            TempSize = this->maxCoolAirVolFlow;
            SaveCurDuctType = DataSizing::CurDuctType;
            DataSizing::CurDuctType = DataHVACGlobals::Cooling;
            if ((CoolingSAFlowMethod == SupplyAirFlowRate) || (CoolingSAFlowMethod == None)) {
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysCoolingFlow = TempSize;
            } else if (CoolingSAFlowMethod == FlowPerFloorArea) {
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysCoolingFlow = TempSize;
                this->maxCoolAirVolFlow = DataSizing::AutoSize;
            } else if (CoolingSAFlowMethod == FractionOfAutoSizedCoolingValue) {
                TempSize = DataSizing::AutoSize;
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysCoolingFlow = TempSize * this->maxCoolAirVolFlow;
                this->maxCoolAirVolFlow = DataSizing::AutoSize;
            } else if (CoolingSAFlowMethod == FlowPerCoolingCapacity) {
                if (this->designCoolingCapacity == DataSizing::AutoSize) {
                    TempSize = DataSizing::AutoSize;
                    ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                    SizingMethod = DataHVACGlobals::CoolingCapacitySizing;
                    DataSizing::DataFlowUsedForSizing = TempSize;
                    TempSize = DataSizing::AutoSize;
                    if (this->coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed ||
                        this->coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling ||
                        this->coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed ||
                        this->coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                        DataSizing::DataTotCapCurveIndex = DXCoils::GetDXCoilCapFTCurveIndex(this->coolingCoilIndex, ErrFound);
                        DataSizing::DataIsDXCoil = true;
                    }
                    ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                    CoolCapAtPeak = TempSize;
                    SysCoolingFlow = TempSize * this->maxCoolAirVolFlow;
                    DataSizing::DataTotCapCurveIndex = 0;
                    EqSizing.CoolingCapacity = true;
                    EqSizing.DesCoolingLoad = CoolCapAtPeak;
                } else {
                    SysCoolingFlow = this->designCoolingCapacity * this->maxCoolAirVolFlow;
                    CoolCapAtPeak = this->designCoolingCapacity;
                    DataSizing::DXCoolCap = CoolCapAtPeak;
                }
                this->maxCoolAirVolFlow = DataSizing::AutoSize;
            } else {
                // should never happen
                ShowSevereError(RoutineName + ": " + CompType + " = " + CompName);
                ShowContinueError("Illegal entry for Cooling Supply Air Flow Rate Method.");
            }

            DataSizing::CurDuctType = SaveCurDuctType;
            EqSizing.CoolingAirFlow = true;
            EqSizing.CoolingAirVolFlow = SysCoolingFlow;

            // Cooling airflow should be known at this point. Now find DataSizing::AutoSized design cooling capacity.
            if (CoolingSAFlowMethod != FlowPerCoolingCapacity && this->designCoolingCapacity < 0.0) {
                SizingMethod = DataHVACGlobals::CoolingCapacitySizing;
                DataSizing::DataFlowUsedForSizing = EqSizing.CoolingAirVolFlow;
                TempSize = DataSizing::AutoSize;
                if (this->coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed ||
                    this->coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling ||
                    this->coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed ||
                    this->coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                    DataSizing::DataTotCapCurveIndex = DXCoils::GetDXCoilCapFTCurveIndex(this->coolingCoilIndex, ErrFound);
                    DataSizing::DataIsDXCoil = true;
                }
                if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    DataSizing::DataTotCapCurveIndex = VariableSpeedCoils::GetVSCoilCapFTCurveIndex(this->coolingCoilIndex, ErrFound);
                    DataSizing::DataIsDXCoil = true;
                }
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                CoolCapAtPeak = TempSize;
                DataSizing::DXCoolCap = CoolCapAtPeak;
                EqSizing.CoolingCapacity = true;
                EqSizing.DesCoolingLoad = CoolCapAtPeak;
            } else {
                CoolCapAtPeak = this->designCoolingCapacity;
            }
            DataSizing::DataIsDXCoil = false;
            DataSizing::DataTotCapCurveIndex = 0;
            DataSizing::DataFlowUsedForSizing = 0.0;
        }

        // STEP 2: find the DataSizing::AutoSized heating air flow rate and capacity
        if (this->heatCoilExists) {
            if (!this->coolCoilExists) DataSizing::ZoneHeatingOnlyFan = true;
            FieldNum = 7; // N7 , \field Heating Supply Air Flow Rate
            PrintFlag = false;
            SizingMethod = DataHVACGlobals::HeatingAirflowSizing;
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [m3/s]";
            TempSize = this->maxHeatAirVolFlow;
            SaveCurDuctType = DataSizing::CurDuctType;
            DataSizing::CurDuctType = DataHVACGlobals::Heating;
            if ((HeatingSAFlowMethod == SupplyAirFlowRate) || (HeatingSAFlowMethod == None)) {
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysHeatingFlow = TempSize;
            } else if (HeatingSAFlowMethod == FlowPerFloorArea) {
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysHeatingFlow = TempSize;
                this->maxHeatAirVolFlow = DataSizing::AutoSize;
            } else if (HeatingSAFlowMethod == FractionOfAutoSizedHeatingValue) {
                TempSize = DataSizing::AutoSize;
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SysHeatingFlow = TempSize * this->maxHeatAirVolFlow;
                this->maxHeatAirVolFlow = DataSizing::AutoSize;
            } else if (HeatingSAFlowMethod == FlowPerHeatingCapacity) {
                TempSize = DataSizing::AutoSize;
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                SizingMethod = DataHVACGlobals::HeatingCapacitySizing;
                DataSizing::DataFlowUsedForSizing = TempSize;
                TempSize = DataSizing::AutoSize;
                DataSizing::DataFracOfAutosizedCoolingCapacity = 1.0;
                DataSizing::DataHeatSizeRatio = this->heatingSizingRatio;
                if (this->heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                    this->heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                    DataSizing::DataTotCapCurveIndex = DXCoils::GetDXCoilCapFTCurveIndex(this->heatingCoilIndex, ErrFound);
                    DataSizing::DataIsDXCoil = true;
                }
                if (DataSizing::CurSysNum > 0)
                    DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating =
                        false; // set to false to allow calculation of actual heating capacity
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                if (DataSizing::CurSysNum > 0) DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating = true;
                HeatCapAtPeak = TempSize;
                SysHeatingFlow = TempSize * this->maxHeatAirVolFlow;
                this->maxHeatAirVolFlow = DataSizing::AutoSize;
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
            if (HeatingSAFlowMethod != FlowPerHeatingCapacity && this->designHeatingCapacity == DataSizing::AutoSize) {
                SizingMethod = DataHVACGlobals::HeatingCapacitySizing;
                DataSizing::DataFlowUsedForSizing = EqSizing.HeatingAirVolFlow;
                TempSize = DataSizing::AutoSize;
                DataSizing::DataHeatSizeRatio = this->heatingSizingRatio;
                if (this->heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical ||
                    this->heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                    DataSizing::DataTotCapCurveIndex = DXCoils::GetDXCoilCapFTCurveIndex(this->heatingCoilIndex, ErrFound);
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
                HeatCapAtPeak = this->designHeatingCapacity;
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
        if (this->heatPump) { // if a heat pump, use maximum values and set main air flow and capacity variables
            EqSizing.AirFlow = true;
            EqSizing.AirVolFlow = max(EqSizing.CoolingAirVolFlow, EqSizing.HeatingAirVolFlow);
            //			EqSizing.CoolingAirVolFlow = EqSizing.AirVolFlow;
            //			EqSizing.HeatingAirVolFlow = EqSizing.AirVolFlow;
            EqSizing.Capacity = true;
            EqSizing.DesCoolingLoad = max(EqSizing.DesCoolingLoad, EqSizing.DesHeatingLoad);
            EqSizing.DesHeatingLoad = EqSizing.DesCoolingLoad;
            DataSizing::DXCoolCap = EqSizing.DesCoolingLoad;
        } else if (!this->coolCoilExists && DataSizing::CurZoneEqNum > 0) {
            DataSizing::DXCoolCap = EqSizing.DesHeatingLoad;
        }

        // STEP 5: report system parameters (e.g., air flow rates, capacities, etc.)
        if (this->fanExists) {

            SizingMethod = DataHVACGlobals::SystemAirflowSizing;
            EqSizing.SystemAirFlow = true;
            EqSizing.AirVolFlow = max(EqSizing.CoolingAirVolFlow, EqSizing.HeatingAirVolFlow);
            if (this->designFanVolFlowRate <= 0.0) { // attempt to catch any missed logic in GetUnitarySystem
                this->designFanVolFlowRate = DataSizing::AutoSize;
            }
            PrintFlag = true;
            DataSizing::DataEMSOverrideON = this->designFanVolFlowRateEMSOverrideOn;
            DataSizing::DataEMSOverride = this->designFanVolFlowRateEMSOverrideValue;
            TempSize = this->designFanVolFlowRate;
            SizingString = "Supply Air Flow Rate [m3/s]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->designFanVolFlowRate = TempSize;
            DataSizing::DataEMSOverrideON = false;
            EqSizing.SystemAirFlow = false;
        }

        // not sure what to do if UnitarySystem has only 1 coil type and flow needs to occur when present coil is off
        // how does constant fan operating mode pertain here?
        if (this->heatCoilExists && !this->coolCoilExists) {
            if (this->maxCoolAirVolFlow == DataSizing::AutoSize) this->maxCoolAirVolFlow = EqSizing.HeatingAirVolFlow;
        } else if (this->coolCoilExists && !this->heatCoilExists) {
            if (this->maxHeatAirVolFlow == DataSizing::AutoSize) this->maxHeatAirVolFlow = EqSizing.CoolingAirVolFlow;
        }

        if (this->heatCoilExists) {

            SizingMethod = DataHVACGlobals::HeatingAirflowSizing;
            if (this->maxHeatAirVolFlow <= 0.0) { // attempt to catch any missed logic in GetUnitarySystem
                this->maxHeatAirVolFlow = DataSizing::AutoSize;
            }
            FieldNum = 7; // N7 , \field Heating Supply Air Flow Rate
            PrintFlag = true;
            DataSizing::DataEMSOverrideON = this->maxHeatAirVolFlowEMSOverrideOn;
            DataSizing::DataEMSOverride = this->maxHeatAirVolFlowEMSOverrideValue;
            TempSize = this->maxHeatAirVolFlow;
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [m3/s]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->maxHeatAirVolFlow = TempSize;
            DataSizing::DataEMSOverrideON = false;
            DataSizing::DataConstantUsedForSizing = 0.0;
        }

        if (this->coolCoilExists) {

            SizingMethod = DataHVACGlobals::CoolingAirflowSizing;
            if (this->maxCoolAirVolFlow <= 0.0) { // attempt to catch any missed logic in GetUnitarySystem
                this->maxCoolAirVolFlow = DataSizing::AutoSize;
            }
            FieldNum = 3; // N3 , \field Cooling Supply Air Flow Rate
            PrintFlag = true;
            DataSizing::DataEMSOverrideON = this->maxCoolAirVolFlowEMSOverrideOn;
            DataSizing::DataEMSOverride = this->maxCoolAirVolFlowEMSOverrideValue;
            TempSize = this->maxCoolAirVolFlow;
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [m3/s]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->maxCoolAirVolFlow = TempSize;
            DataSizing::DataEMSOverrideON = false;
            DataSizing::DataConstantUsedForSizing = 0.0;
        }

        if (this->coolCoilExists || this->heatCoilExists || this->suppCoilExists) {

            SizingMethod = DataHVACGlobals::SystemAirflowSizing;

            if (this->noCoolHeatSAFMethod <= SupplyAirFlowRate && this->controlType == controlTypeEnum::controlTypeCCMASHRAE) {
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                if (this->maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                    DataSizing::DataConstantUsedForSizing = max(this->maxCoolAirVolFlow, this->maxHeatAirVolFlow);
                    if (this->coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed ||
                        this->heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                        minNoLoadFlow = 0.6667;
                    } else {
                        if (this->noLoadAirFlowRateRatio < 1.0) {
                            minNoLoadFlow = this->noLoadAirFlowRateRatio;
                        } else {
                            minNoLoadFlow = 0.5;
                        }
                    }
                    if (this->maxCoolAirVolFlow >= this->maxHeatAirVolFlow) {
                        DataSizing::DataFractionUsedForSizing = min(minNoLoadFlow, (this->maxHeatAirVolFlow / this->maxCoolAirVolFlow) - 0.01);
                    } else {
                        DataSizing::DataFractionUsedForSizing = min(minNoLoadFlow, (this->maxCoolAirVolFlow / this->maxHeatAirVolFlow) - 0.01);
                    }
                } else {
                    DataSizing::DataConstantUsedForSizing = this->maxNoCoolHeatAirVolFlow;
                    DataSizing::DataFractionUsedForSizing = 1.0;
                }
            } else if (this->noCoolHeatSAFMethod == FractionOfAutoSizedCoolingValue) {
                this->maxNoCoolHeatAirVolFlow *= EqSizing.CoolingAirVolFlow;
                DataSizing::DataConstantUsedForSizing = this->maxNoCoolHeatAirVolFlow;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->maxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
            } else if (this->noCoolHeatSAFMethod == FractionOfAutoSizedHeatingValue) {
                this->maxNoCoolHeatAirVolFlow *= EqSizing.HeatingAirVolFlow;
                DataSizing::DataConstantUsedForSizing = this->maxNoCoolHeatAirVolFlow;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->maxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
            } else if (this->noCoolHeatSAFMethod == FlowPerCoolingCapacity) {
                this->maxNoCoolHeatAirVolFlow *= EqSizing.DesCoolingLoad;
                DataSizing::DataConstantUsedForSizing = this->maxNoCoolHeatAirVolFlow;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->maxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
            } else if (this->noCoolHeatSAFMethod == FlowPerHeatingCapacity) {
                this->maxNoCoolHeatAirVolFlow *= EqSizing.DesHeatingLoad;
                DataSizing::DataConstantUsedForSizing = this->maxNoCoolHeatAirVolFlow;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->maxNoCoolHeatAirVolFlow = DataSizing::AutoSize;
            } else {
                DataSizing::DataFractionUsedForSizing = this->noLoadAirFlowRateRatio;
            }

            FieldNum = 11; // N11 , \field No Load Supply Air Flow Rate
            PrintFlag = true;
            DataSizing::DataEMSOverrideON = this->maxNoCoolHeatAirVolFlowEMSOverrideOn;
            DataSizing::DataEMSOverride = this->maxNoCoolHeatAirVolFlowEMSOverrideValue;
            TempSize = this->maxNoCoolHeatAirVolFlow;
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [m3/s]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->maxNoCoolHeatAirVolFlow = TempSize;
            DataSizing::DataEMSOverrideON = false;
            DataSizing::DataConstantUsedForSizing = 0.0;
            DataSizing::DataFractionUsedForSizing = 0.0;
        }

        if (this->maxCoolAirVolFlow > 0.0) {
            this->lowSpeedCoolFanRatio = this->maxNoCoolHeatAirVolFlow / this->maxCoolAirVolFlow;
        }
        if (this->maxHeatAirVolFlow > 0.0) {
            this->lowSpeedHeatFanRatio = this->maxNoCoolHeatAirVolFlow / this->maxHeatAirVolFlow;
        }

        // initialize multi-speed coils
        if ((this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) ||
            (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed)) {

            if (this->numOfSpeedCooling > 0) {
                if (this->coolVolumeFlowRate.size() == 0) this->coolVolumeFlowRate.resize(this->numOfSpeedCooling);
                if (this->coolMassFlowRate.size() == 0) this->coolMassFlowRate.resize(this->numOfSpeedCooling);
                if (this->MSCoolingSpeedRatio.size() == 0) this->MSCoolingSpeedRatio.resize(this->numOfSpeedCooling);
            }

            MSHPIndex = this->designSpecMSHPIndex;
            if (MSHPIndex > -1) {
                designSpecMSHP[MSHPIndex].coolingVolFlowRatio.resize(designSpecMSHP[MSHPIndex].numOfSpeedCooling);
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedCooling - 1; Iter >= 0; --Iter) {
                    if (designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter] == DataSizing::AutoSize) {
                        designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter] = double(Iter + 1) / double(designSpecMSHP[MSHPIndex].numOfSpeedCooling);
                    }
                }
            }

            VariableSpeedCoils::SimVariableSpeedCoils(blankString,
                                                      this->coolingCoilIndex,
                                                      0,
                                                      this->maxONOFFCyclesperHour,
                                                      this->HPTimeConstant,
                                                      this->fanDelayTime,
                                                      0,
                                                      0.0,
                                                      1,
                                                      0.0,
                                                      0.0,
                                                      0.0,
                                                      0.0); // conduct the sizing operation in the VS WSHP
            this->numOfSpeedCooling = VariableSpeedCoils::VarSpeedCoil(this->coolingCoilIndex).NumOfSpeeds;
            DataSizing::DXCoolCap = VariableSpeedCoils::GetCoilCapacityVariableSpeed(
                DataHVACGlobals::cAllCoilTypes(this->coolingCoilType_Num), this->coolingCoilName, ErrFound);
            EqSizing.DesCoolingLoad = DataSizing::DXCoolCap;

            for (Iter = 1; Iter <= this->numOfSpeedCooling; ++Iter) {
                this->coolVolumeFlowRate[Iter] = VariableSpeedCoils::VarSpeedCoil(this->coolingCoilIndex).MSRatedAirVolFlowRate(Iter);
                this->coolMassFlowRate[Iter] = this->coolVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                // it seems the ratio should reference the actual flow rates, not the fan flow ???
                if (this->designFanVolFlowRate > 0.0 && this->fanExists) {
                    this->MSCoolingSpeedRatio[Iter] = this->coolVolumeFlowRate[Iter] / this->designFanVolFlowRate;
                } else {
                    this->MSCoolingSpeedRatio[Iter] = this->coolVolumeFlowRate[Iter] / this->coolVolumeFlowRate[this->numOfSpeedCooling];
                }
            }

            this->idleVolumeAirRate = this->coolVolumeFlowRate[0];
            this->idleMassFlowRate = this->coolMassFlowRate[0];
            this->idleSpeedRatio = this->MSCoolingSpeedRatio[0];

        } else if (this->coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

            if (this->numOfSpeedCooling > 0) {
                if (this->coolVolumeFlowRate.size() == 0) this->coolVolumeFlowRate.resize(this->numOfSpeedCooling);
                if (this->coolMassFlowRate.size() == 0) this->coolMassFlowRate.resize(this->numOfSpeedCooling);
                if (this->MSCoolingSpeedRatio.size() == 0) this->MSCoolingSpeedRatio.resize(this->numOfSpeedCooling);
            }

            // set the multi-speed high flow rate variable in case a non-zero air flow rate resides on the coil inlet during sizing (e.g., upstream
            // system ran prior to this one)
            DataHVACGlobals::MSHPMassFlowRateHigh =
                EqSizing.CoolingAirVolFlow *
                DataEnvironment::StdRhoAir; // doesn't matter what this value is since only coil size is needed and CompOn = 0 here
            DXCoils::SimDXCoilMultiSpeed(blankString, 1.0, 1.0, this->coolingCoilIndex, 0, 0, 0);
            DataSizing::DXCoolCap = DXCoils::GetCoilCapacityByIndexType(this->coolingCoilIndex, this->coolingCoilType_Num, ErrFound);
            EqSizing.DesCoolingLoad = DataSizing::DXCoolCap;
            MSHPIndex = this->designSpecMSHPIndex;

            if (MSHPIndex > -1) {
                // use reverse order since we divide by CoolVolumeFlowRate(max)
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedCooling - 1; Iter >= 0; --Iter) {
                    if (designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter] == DataSizing::AutoSize)
                        designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter] = double(Iter + 1) / double(designSpecMSHP[MSHPIndex].numOfSpeedCooling);
                    this->coolVolumeFlowRate[Iter] = this->maxCoolAirVolFlow * designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter];
                    this->coolMassFlowRate[Iter] = this->coolVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                    this->MSCoolingSpeedRatio[Iter] =
                        this->coolVolumeFlowRate[Iter] / this->coolVolumeFlowRate[designSpecMSHP[MSHPIndex].numOfSpeedCooling - 1];
                }
                this->idleVolumeAirRate = this->coolVolumeFlowRate[0];
                this->idleMassFlowRate = this->coolMassFlowRate[0];
                this->idleSpeedRatio = this->MSCoolingSpeedRatio[0];
            }
        }

        if (this->heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
            this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
            this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {

            if (this->numOfSpeedHeating > 0) {
                if (this->heatVolumeFlowRate.size() == 0) this->heatVolumeFlowRate.resize(this->numOfSpeedHeating);
                if (this->heatMassFlowRate.size() == 0) this->heatMassFlowRate.resize(this->numOfSpeedHeating);
                if (this->MSHeatingSpeedRatio.size() == 0) this->MSHeatingSpeedRatio.resize(this->numOfSpeedHeating);
            }

            MSHPIndex = this->designSpecMSHPIndex;

            if (MSHPIndex > -1) {
                // use reverse order since we divide by HeatVolumeFlowRate(max)
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedHeating - 1; Iter >= 0; --Iter) {
                    if (designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter] == DataSizing::AutoSize) {
                        if (this->controlType == controlTypeEnum::controlTypeSetpoint &&
                            (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                             this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {
                            designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter] = 1.0;
                        } else {
                            designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter] =
                                double(Iter + 1) / double(designSpecMSHP[MSHPIndex].numOfSpeedHeating);
                        }
                    } else {
                        if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                            this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {
                            if (designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter] < 1.0 && this->controlType == controlTypeSetpoint) {
                                ShowWarningError(RoutineName + ": " + CompType + " = " + CompName);
                                ShowContinueError("Design specification object = " + designSpecMSHP[MSHPIndex].name);
                                ShowContinueError("When control type = SetPointBased the outlet air temperature must change with coil capacity, if "
                                                  "air flow also changes outlet air temperature will be relatively constant.");
                                ShowContinueError("Speed " + General::TrimSigDigits(Iter) +
                                                  " Supply Air Flow Ratio During Heating Operation will be set = 1.0 and the simulation continues");
                                designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter] = 1.0;
                            }
                        }
                    }
                    this->heatVolumeFlowRate[Iter] = this->maxHeatAirVolFlow * designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter];
                    this->heatMassFlowRate[Iter] = this->heatVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                    this->MSHeatingSpeedRatio[Iter] =
                        this->heatVolumeFlowRate[Iter] / this->heatVolumeFlowRate[designSpecMSHP[MSHPIndex].numOfSpeedHeating - 1];
                }
                // these coil types do not have an idle speed air flow rate
                this->idleVolumeAirRate = 0.0;
                this->idleMassFlowRate = 0.0;
                this->idleSpeedRatio = 0.0;
            }
        } else if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                   this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {

            MSHPIndex = this->designSpecMSHPIndex;
            if (MSHPIndex > -1) {
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedHeating - 1; Iter >= 0; --Iter) {
                    if (designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter] == DataSizing::AutoSize) {
                        designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter] = double(Iter + 1) / double(designSpecMSHP[MSHPIndex].numOfSpeedHeating);
                    }
                }
            }

            VariableSpeedCoils::SimVariableSpeedCoils(blankString,
                                                      this->heatingCoilIndex,
                                                      0,
                                                      this->maxONOFFCyclesperHour,
                                                      this->HPTimeConstant,
                                                      this->fanDelayTime,
                                                      0,
                                                      0.0,
                                                      1,
                                                      0.0,
                                                      0.0,
                                                      0.0,
                                                      0.0); // conduct the sizing operation in the VS WSHP

            this->numOfSpeedHeating = VariableSpeedCoils::VarSpeedCoil(this->heatingCoilIndex).NumOfSpeeds;

            if (this->numOfSpeedHeating > 0) {
                if (this->heatVolumeFlowRate.size() == 0) this->heatVolumeFlowRate.resize(this->numOfSpeedHeating);
                if (this->heatMassFlowRate.size() == 0) this->heatMassFlowRate.resize(this->numOfSpeedHeating);
                if (this->MSHeatingSpeedRatio.size() == 0) this->MSHeatingSpeedRatio.resize(this->numOfSpeedHeating);
            }

            for (Iter = 1; Iter <= this->numOfSpeedHeating; ++Iter) {
                this->heatVolumeFlowRate[Iter] = VariableSpeedCoils::VarSpeedCoil(this->heatingCoilIndex).MSRatedAirVolFlowRate(Iter);
                this->heatMassFlowRate[Iter] = this->heatVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                if (this->designFanVolFlowRate > 0.0 && this->fanExists) {
                    this->MSHeatingSpeedRatio[Iter] = this->heatVolumeFlowRate[Iter] / this->designFanVolFlowRate;
                } else {
                    this->MSHeatingSpeedRatio[Iter] = this->heatVolumeFlowRate[Iter] / this->heatVolumeFlowRate[this->numOfSpeedHeating];
                }
            }

            if (this->coolCoilExists) {
                if (this->coolVolumeFlowRate.size() > 0) {
                    this->idleVolumeAirRate = min(this->idleVolumeAirRate, this->heatVolumeFlowRate[0]);
                    this->idleMassFlowRate = min(this->idleMassFlowRate, this->heatMassFlowRate[0]);
                    this->idleSpeedRatio = min(this->idleSpeedRatio, this->MSHeatingSpeedRatio[0]);
                } else {
                    this->idleVolumeAirRate = this->heatVolumeFlowRate[0];
                    this->idleMassFlowRate = this->heatMassFlowRate[0];
                    this->idleSpeedRatio = this->MSHeatingSpeedRatio[0];
                }
            } else {
                this->idleVolumeAirRate = this->heatVolumeFlowRate[0];
                this->idleMassFlowRate = this->heatMassFlowRate[0];
                this->idleSpeedRatio = this->MSHeatingSpeedRatio[0];
            }
        }
        if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
            this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {

            if (this->numOfSpeedCooling > 0) {
                if (this->coolVolumeFlowRate.size() == 0) this->coolVolumeFlowRate.resize(this->numOfSpeedCooling);
                if (this->coolMassFlowRate.size() == 0) this->coolMassFlowRate.resize(this->numOfSpeedCooling);
                if (this->MSCoolingSpeedRatio.size() == 0) this->MSCoolingSpeedRatio.resize(this->numOfSpeedCooling);
            }
            MSHPIndex = this->designSpecMSHPIndex;

            if (MSHPIndex > -1) {
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedCooling - 1; Iter >= 0; --Iter) {
                    if (designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter] == DataSizing::AutoSize)
                        designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter] = double(Iter + 1) / double(designSpecMSHP[MSHPIndex].numOfSpeedCooling);
                    this->coolVolumeFlowRate[Iter] = this->maxCoolAirVolFlow * designSpecMSHP[MSHPIndex].coolingVolFlowRatio[Iter];
                    this->coolMassFlowRate[Iter] = this->coolVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                    this->MSCoolingSpeedRatio[Iter] =
                        this->coolVolumeFlowRate[Iter] / this->coolVolumeFlowRate[designSpecMSHP[MSHPIndex].numOfSpeedCooling - 1];
                }
                this->idleVolumeAirRate = this->coolVolumeFlowRate[0];
                this->idleMassFlowRate = this->coolMassFlowRate[0];
                this->idleSpeedRatio = this->MSCoolingSpeedRatio[0];
            }
        }
        if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {

            if (this->numOfSpeedHeating > 0) {
                if (this->heatVolumeFlowRate.size() == 0) this->heatVolumeFlowRate.resize(this->numOfSpeedHeating);
                if (this->heatMassFlowRate.size() == 0) this->heatMassFlowRate.resize(this->numOfSpeedHeating);
                if (this->MSHeatingSpeedRatio.size() == 0) this->MSHeatingSpeedRatio.resize(this->numOfSpeedHeating);
            }

            MSHPIndex = this->designSpecMSHPIndex;
            if (MSHPIndex > 0) {
                for (Iter = designSpecMSHP[MSHPIndex].numOfSpeedHeating - 1; Iter >= 0; --Iter) {
                    if (designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter] == DataSizing::AutoSize) {
                        designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter] = double(Iter + 1) / double(designSpecMSHP[MSHPIndex].numOfSpeedHeating);
                    }
                    this->heatVolumeFlowRate[Iter] = this->maxHeatAirVolFlow * designSpecMSHP[MSHPIndex].heatingVolFlowRatio[Iter];
                    this->heatMassFlowRate[Iter] = this->heatVolumeFlowRate[Iter] * DataEnvironment::StdRhoAir;
                    this->MSHeatingSpeedRatio[Iter] =
                        this->heatVolumeFlowRate[Iter] / this->heatVolumeFlowRate[designSpecMSHP[MSHPIndex].numOfSpeedHeating - 1];
                }
                this->idleVolumeAirRate = this->heatVolumeFlowRate[0];
                this->idleMassFlowRate = this->heatMassFlowRate[0];
                this->idleSpeedRatio = this->MSHeatingSpeedRatio[0];
            }
        }

        // Not sure if this may be needed for special cases
        if (this->coolCoilExists && this->maxCoolAirVolFlow < 0.0) {
            if (!DataSizing::SysSizingRunDone) {
                BranchNum = BranchInputManager::GetAirBranchIndex("AirloopHVAC:UnitarySystem", this->name);
                FanType = "";
                FanName = "";
                BranchFanFlow = 0.0;
                if (BranchNum > 0.0) BranchInputManager::GetBranchFanTypeName(BranchNum, FanType, FanName, ErrFound);
                if (!ErrFound && BranchNum > 0) {
                    if (this->fanType_Num = DataHVACGlobals::FanType_SystemModelObject) {
                        BranchFanFlow = HVACFan::fanObjs[this->fanIndex]->designAirVolFlowRate;
                    } else {
                        BranchFanFlow = Fans::GetFanDesignVolumeFlowRate(FanType, FanName, ErrFound);
                    }
                }
                if (BranchFanFlow > 0.0) {
                    this->maxCoolAirVolFlow = BranchFanFlow;
                } else {
                    SystemFlow = 0.0;
                    if (AirLoopNum > 0.0) SystemFlow = DataAirSystems::PrimaryAirSystem(AirLoopNum).DesignVolFlowRate;
                    if (SystemFlow > 0.0) {
                        this->maxCoolAirVolFlow = SystemFlow;
                    } else {
                        // what do I do?
                    }
                }
            }
        }

        // Change the Volume Flow Rates to Mass Flow Rates
        this->designMassFlowRate = this->designFanVolFlowRate * DataEnvironment::StdRhoAir;
        this->maxCoolAirMassFlow = this->maxCoolAirVolFlow * DataEnvironment::StdRhoAir;
        this->maxHeatAirMassFlow = this->maxHeatAirVolFlow * DataEnvironment::StdRhoAir;
        this->maxNoCoolHeatAirMassFlow = this->maxNoCoolHeatAirVolFlow * DataEnvironment::StdRhoAir;

        // why is this here?
        this->senLoadLoss = 0.0;
        if (this->humidistat) {
            this->latLoadLoss = 0.0;
        }

        if (this->coolCoilExists) {

            SizingMethod = DataHVACGlobals::CoolingCapacitySizing;
            // water coils must report their size to parent objects (or split out sizing routines for water coils so they can be call from here)
            if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {
                WaterCoils::SimulateWaterCoilComponents(
                    this->coolingCoilName, FirstHVACIteration, this->coolingCoilIndex, QActual, this->fanOpMode, 1.0);
                DataSizing::DataConstantUsedForSizing = WaterCoils::GetWaterCoilCapacity(
                    UtilityRoutines::MakeUPPERCase(DataHVACGlobals::cAllCoilTypes(this->coolingCoilType_Num)), this->coolingCoilName, ErrFound);
                EqSizing.DesCoolingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->designCoolingCapacity = DataSizing::AutoSize;
            } else if (this->coolingCoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted) {
                HXCoilName = HVACHXAssistedCoolingCoil::GetHXDXCoilName(
                    DataHVACGlobals::cAllCoilTypes(this->coolingCoilType_Num), this->coolingCoilName, ErrFound);
                ActualCoolCoilType = HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(
                    DataHVACGlobals::cAllCoilTypes(this->coolingCoilType_Num), this->coolingCoilName, ErrFound, true);
                HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(blankString, true, On, 1.0, this->coolingCoilIndex, 1, false, 1.0, false);
                DataSizing::DataConstantUsedForSizing = WaterCoils::GetWaterCoilCapacity(
                    UtilityRoutines::MakeUPPERCase(DataHVACGlobals::cAllCoilTypes(ActualCoolCoilType)), HXCoilName, ErrFound);
                EqSizing.DesCoolingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->designCoolingCapacity = DataSizing::AutoSize;
            } else if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {
                WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                this->coolingCoilIndex,
                                                                this->coolingCoilSensDemand,
                                                                this->coolingCoilLatentDemand,
                                                                0,
                                                                0.0,
                                                                this->maxONOFFCyclesperHour,
                                                                this->HPTimeConstant,
                                                                this->fanDelayTime,
                                                                0,
                                                                0.0,
                                                                FirstHVACIteration);
                DataSizing::DataConstantUsedForSizing = WaterToAirHeatPumpSimple::GetCoilCapacity(
                    DataHVACGlobals::cAllCoilTypes(this->coolingCoilType_Num), this->coolingCoilName, ErrFound);
                EqSizing.DesCoolingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->designCoolingCapacity = DataSizing::AutoSize;
                if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                    this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP)
                    EqSizing.DesHeatingLoad = DataSizing::DataConstantUsedForSizing;
            } else if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP) {
                WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                    this->coolingCoilIndex,
                                                    this->maxCoolAirMassFlow,
                                                    this->fanOpMode,
                                                    FirstHVACIteration,
                                                    0.0,
                                                    this->maxONOFFCyclesperHour,
                                                    this->HPTimeConstant,
                                                    this->fanDelayTime,
                                                    this->initHeatPump,
                                                    0.0,
                                                    0.0,
                                                    0,
                                                    0.0);
                DataSizing::DataConstantUsedForSizing =
                    WaterToAirHeatPump::GetCoilCapacity(DataHVACGlobals::cAllCoilTypes(this->coolingCoilType_Num), this->coolingCoilName, ErrFound);
                EqSizing.DesCoolingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                    this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple)
                    EqSizing.DesHeatingLoad = DataSizing::DataConstantUsedForSizing;
            } else if (this->coolingCoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {
                PackagedThermalStorageCoil::SimTESCoil(this->coolingCoilName, this->coolingCoilIndex, this->fanOpMode, this->TESOpMode, 0.0);
                PackagedThermalStorageCoil::GetTESCoilCoolingCapacity(
                    this->coolingCoilName, DataSizing::DataConstantUsedForSizing, ErrFound, CompType);
                EqSizing.DesCoolingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
            }

            PrintFlag = true;
            TempSize = this->designCoolingCapacity;
            DataSizing::DataFlowUsedForSizing = this->maxCoolAirVolFlow;
            SizingString = "Nominal Cooling Capacity [W]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->designCoolingCapacity = TempSize;
            DataSizing::DataConstantUsedForSizing = 0.0;
            DataSizing::DataFractionUsedForSizing = 0.0;
            DataSizing::DataFlowUsedForSizing = 0.0;
        }

        if (this->heatCoilExists) {

            SizingMethod = DataHVACGlobals::HeatingCapacitySizing;

            // water coils must report their size to parent objects (or split out sizing routines for water coils so they can be call from here)
            if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                WaterCoils::SimulateWaterCoilComponents(
                    this->heatingCoilName, FirstHVACIteration, this->heatingCoilIndex, QActual, this->fanOpMode, 1.0);
                DataSizing::DataConstantUsedForSizing = WaterCoils::GetWaterCoilCapacity(
                    UtilityRoutines::MakeUPPERCase(DataHVACGlobals::cAllCoilTypes(this->heatingCoilType_Num)), this->heatingCoilName, ErrFound);
                EqSizing.DesHeatingLoad = DataSizing::DataConstantUsedForSizing;
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                this->designHeatingCapacity = DataSizing::AutoSize;
            }

            PrintFlag = true;
            TempSize = this->designHeatingCapacity;
            SizingString = "Nominal Heating Capacity [W]";
            if (DataSizing::CurSysNum > 0)
                DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating =
                    false; // set to false to allow calculation of parent object heating capacity
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) DataSizing::DXCoolCap = TempSize;
            if (DataSizing::CurSysNum > 0) DataAirLoop::AirLoopControlInfo(AirLoopNum).UnitarySysSimulating = true;
            this->designHeatingCapacity = TempSize;
            DataSizing::DataConstantUsedForSizing = 0.0;
            DataSizing::DataFractionUsedForSizing = 0.0;
            DataSizing::DataHeatSizeRatio = 1.0;
        }

        DataSizing::UnitaryHeatCap = this->designHeatingCapacity;

        if ((this->heatCoilExists || this->suppCoilExists) && this->controlType != controlTypeEnum::controlTypeCCMASHRAE) {

            SizingMethod = DataHVACGlobals::MaxHeaterOutletTempSizing;
            PrintFlag = true;
            TempSize = this->designMaxOutletTemp;
            FieldNum = 17; // N17, \field Maximum Supply Air Temperature
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [C]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->designMaxOutletTemp = TempSize;
        }

        if (this->suppCoilExists) {

            SizingMethod = DataHVACGlobals::HeatingCapacitySizing;

            PrintFlag = false;
            TempSize = this->designSuppHeatingCapacity;
            SizingString = "Supplemental Heating Coil Nominal Capacity [W]";
            if (TempSize == DataSizing::AutoSize) {
                IsAutoSize = true;
                ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
                this->designSuppHeatingCapacity = TempSize;
            }

            if (this->humidistat && this->dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_CoolReheat && IsAutoSize) {
                DataSizing::DataConstantUsedForSizing = max(this->designSuppHeatingCapacity, this->designCoolingCapacity);
                DataSizing::DataFractionUsedForSizing = 1.0;
                SizingMethod = DataHVACGlobals::AutoCalculateSizing;
                TempSize = DataSizing::AutoSize;
            }

            PrintFlag = true;
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            this->designSuppHeatingCapacity = TempSize;
            IsAutoSize = false;
            DataSizing::DataConstantUsedForSizing = 0.0;
            DataSizing::DataFractionUsedForSizing = 0.0;

            DataSizing::SuppHeatCap = this->designSuppHeatingCapacity;
        }

        // register plant flow rate. Not sure this has ever been tested.
        if (this->heatRecActive) {
            PlantUtilities::RegisterPlantCompDesignFlow(this->heatRecoveryInletNodeNum, this->designHRWaterVolumeFlow);
        }

        // Set flow rate for unitary system with no fan
        if (DataSizing::CurOASysNum == 0 && DataSizing::CurZoneEqNum == 0 && this->designFanVolFlowRate <= 0.0) {
            SystemFlow = 0;
            if (AirLoopNum > 0) SystemFlow = DataAirSystems::PrimaryAirSystem(AirLoopNum).DesignVolFlowRate;
            if (SystemFlow > 0.0) {
                this->designFanVolFlowRate = SystemFlow;
            } else {
                this->designFanVolFlowRate = max(this->maxCoolAirVolFlow, this->maxHeatAirVolFlow);
            }
            this->designMassFlowRate = this->designFanVolFlowRate * DataEnvironment::StdRhoAir;
        }

        // Moved from InitLoadBasedControl
        // Find the number of zones (zone Inlet Nodes) attached to an air loop from the air loop number
        if (this->airLoopEquipment && this->controlType != controlTypeEnum::controlTypeSetpoint) {
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
                    if (DataAirLoop::AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZoneInSysIndex) == this->controlZoneNum) {
                        initLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax = DataLoopNode::Node(ZoneInletNodeNum).MassFlowRateMax;
                    }
                }
                if (SumOfMassFlowRateMax != 0.0) {
                    if (initLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax >= DataHVACGlobals::SmallAirVolFlow) {
                        this->controlZoneMassFlowFrac = initLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax / SumOfMassFlowRateMax;
                    } else {
                        ShowSevereError(this->unitType + " = " + this->name);
                        ShowContinueError(" The Fraction of Supply Air Flow That Goes Through the Controlling Zone is set to 1.");
                        this->controlZoneMassFlowFrac = 1.0;
                    }
                    ReportSizingManager::ReportSizingOutput(this->unitType,
                                                            this->name,
                                                            "Fraction of Supply Air Flow That Goes Through the Controlling Zone",
                                                            this->controlZoneMassFlowFrac);
                }
            }
        } else {
            this->controlZoneMassFlowFrac = 1.0;
        }

        if (this->controlType == controlTypeEnum::controlTypeCCMASHRAE) {

            SizingDesRunThisSys = false;
            DataSizing::DataZoneUsedForSizing = this->controlZoneNum;
            CheckThisZoneForSizing(DataSizing::DataZoneUsedForSizing, SizingDesRunThisSys);

            SizingMethod = DataHVACGlobals::ASHRAEMinSATCoolingSizing;
            capacityMultiplier = 0.5; // one-half of design zone load
            if (SizingDesRunThisSys) {
                DataSizing::DataCapacityUsedForSizing = DataSizing::FinalZoneSizing(this->controlZoneNum).DesCoolLoad * capacityMultiplier;
            } else {
                DataSizing::DataCapacityUsedForSizing = this->designCoolingCapacity * capacityMultiplier;
            }
            DataSizing::DataCapacityUsedForSizing /= this->controlZoneMassFlowFrac;
            DataSizing::DataFlowUsedForSizing = this->maxNoCoolHeatAirVolFlow;
            PrintFlag = true;
            FieldNum = 2; // Minimum Supply Air Temperature in Cooling Mode
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [C]";
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, this->designMinOutletTemp, PrintFlag, RoutineName);

            SizingMethod = DataHVACGlobals::ASHRAEMaxSATHeatingSizing;
            FieldNum = 17; // Maximum Supply Air Temperature in Heating Mode
            // SizingString = UnitarySystemNumericFields(UnitarySysNum).FieldNames(FieldNum) + " [C]";
            if (SizingDesRunThisSys) {
                DataSizing::DataCapacityUsedForSizing = DataSizing::FinalZoneSizing(this->controlZoneNum).DesHeatLoad * capacityMultiplier;
            } else {
                DataSizing::DataCapacityUsedForSizing = this->designHeatingCapacity * capacityMultiplier;
            }
            DataSizing::DataCapacityUsedForSizing /= this->controlZoneMassFlowFrac;
            DataSizing::DataFlowUsedForSizing = this->maxNoCoolHeatAirVolFlow;
            ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, this->designMaxOutletTemp, PrintFlag, RoutineName);

            DataSizing::DataCapacityUsedForSizing = 0.0; // reset so other routines don't use this inadvertently
            DataSizing::DataFlowUsedForSizing = 0.0;
            DataSizing::DataZoneUsedForSizing = 0;

            // check that MaxNoCoolHeatAirVolFlow is less than both MaxCoolAirVolFlow and MaxHeatAirVolFlow
            if (this->maxNoCoolHeatAirVolFlow >= this->maxCoolAirVolFlow || this->maxNoCoolHeatAirVolFlow >= this->maxHeatAirVolFlow) {
                ShowSevereError(this->unitType + " = " + this->name);
                ShowContinueError(" For SingleZoneVAV control the No Load Supply Air Flow Rate must be less than both the cooling and heating supply "
                                  "air flow rates.");
                this->maxNoCoolHeatAirVolFlow = min(this->maxCoolAirVolFlow, this->maxHeatAirVolFlow) - 0.01;
                ShowContinueError(" The SingleZoneVAV control No Load Supply Air Flow Rate is reset to " +
                                  General::TrimSigDigits(this->maxNoCoolHeatAirVolFlow, 5) + " and the simulation continues.");
            }
        }

        CoolingLoad = TempCoolingLoad;
        HeatingLoad = TempHeatingLoad;
        // if (++NumUnitarySystemsSized == NumUnitarySystem)
        //    UnitarySystemNumericFields.deallocate(); // remove temporary array for field names at end of sizing
    }

    void UnitarySys::getUnitarySystemInputData(std::string const &objectName, bool errorsFound)
    {

        static std::string const getUnitarySystemInput("getUnitarySystemInputData");
        static std::string const unitarySysHeatPumpPerformanceObjectType("UnitarySystemPerformance:Multispeed");

        std::string cCurrentModuleObject = "UnitarySystem";

        auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == inputProcessor->epJSON.end()) {
            ShowSevereError("getUnitarySystemInputData: did not find UnitarySystem object in input file. Check inputs");
            errorsFound = true;
        } else {
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                auto const &thisObjectName = instance.key();
                // only get the current data
                if (!UtilityRoutines::SameString(objectName, thisObjectName)) continue;
                if (getUnitarySystemIndex(objectName) == -1) ++numUnitarySystems;

                auto const &fields = instance.value();
                UnitarySys thisSys;

                thisSys.iterationMode.resize(20);
                std::string loc_controlType = fields.at("control_type");
                std::string loc_controlZoneName("");
                if (fields.find("controlling_zone_or_thermostat_location") != fields.end()) { // not required field
                    loc_controlZoneName = UtilityRoutines::MakeUPPERCase(fields.at("controlling_zone_or_thermostat_location"));
                } else if (loc_controlType == "Load") {
                    ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                    ShowContinueError("Controlling Zone or Thermostat Location cannot be blank when Control Type = Load");
                    errorsFound = true;
                }
                std::string loc_dehumControlType("");
                if (fields.find("dehumidification_control_type") != fields.end()) { // not required field, has default
                    loc_dehumControlType = UtilityRoutines::MakeUPPERCase(fields.at("dehumidification_control_type"));
                } else {
                    loc_dehumControlType = "NONE"; // default value
                }
                std::string loc_sysAvailSched("");
                if (fields.find("availability_schedule_name") != fields.end()) { // not required field
                    loc_sysAvailSched = UtilityRoutines::MakeUPPERCase(fields.at("availability_schedule_name"));
                }
                std::string loc_airInNodeName = UtilityRoutines::MakeUPPERCase(fields.at("air_inlet_node_name"));   // required field
                std::string loc_airOutNodeName = UtilityRoutines::MakeUPPERCase(fields.at("air_outlet_node_name")); // required field

                std::string loc_fanType("");
                if (fields.find("supply_fan_object_type") != fields.end()) { // not required field
                    loc_fanType = UtilityRoutines::MakeUPPERCase(fields.at("supply_fan_object_type"));
                }

                std::string loc_fanName("");
                if (fields.find("supply_fan_name") != fields.end()) { // not required field
                    loc_fanName = UtilityRoutines::MakeUPPERCase(fields.at("supply_fan_name"));
                }

                std::string loc_supFanPlace("");
                if (fields.find("fan_placement") != fields.end()) { // not required field
                    loc_supFanPlace = UtilityRoutines::MakeUPPERCase(fields.at("fan_placement"));
                }

                std::string loc_supFanOpMode("");
                if (fields.find("supply_air_fan_operating_mode_schedule_name") != fields.end()) { // not required field
                    loc_supFanOpMode = UtilityRoutines::MakeUPPERCase(fields.at("supply_air_fan_operating_mode_schedule_name"));
                }

                std::string loc_heatingCoilType("");
                if (fields.find("heating_coil_object_type") != fields.end()) { // not required field
                    loc_heatingCoilType = UtilityRoutines::MakeUPPERCase(fields.at("heating_coil_object_type"));
                }

                std::string loc_heatingCoilName("");
                if (fields.find("heating_coil_name") != fields.end()) { // not required field
                    loc_heatingCoilName = UtilityRoutines::MakeUPPERCase(fields.at("heating_coil_name"));
                }

                Real64 loc_heatingSizingRatio(1.0);
                if (fields.find("dx_heating_coil_sizing_ratio") != fields.end()) { // not required field, has default
                    loc_heatingSizingRatio = fields.at("dx_heating_coil_sizing_ratio");
                }

                std::string loc_coolingCoilType("");
                if (fields.find("cooling_coil_object_type") != fields.end()) { // not required field
                    loc_coolingCoilType = UtilityRoutines::MakeUPPERCase(fields.at("cooling_coil_object_type"));
                }

                std::string loc_coolingCoilName("");
                if (fields.find("cooling_coil_name") != fields.end()) { // not required field
                    loc_coolingCoilName = UtilityRoutines::MakeUPPERCase(fields.at("cooling_coil_name"));
                }

                std::string loc_ISHundredPercentDOASDXCoil("No");
                if (fields.find("use_doas_dx_cooling_coil") != fields.end()) { // not required field, has default
                    loc_ISHundredPercentDOASDXCoil = UtilityRoutines::MakeUPPERCase(fields.at("use_doas_dx_cooling_coil"));
                }

                Real64 loc_designMinOutletTemp(2.0);
                if (fields.find("minimum_supply_air_temperature") != fields.end()) { // not required field, has default
                    loc_designMinOutletTemp = fields.at("minimum_supply_air_temperature");
                }

                std::string loc_latentControlFlag("SensibleOnlyLoadControl");
                if (fields.find("latent_load_control") != fields.end()) { // not required field, has default
                    loc_latentControlFlag = UtilityRoutines::MakeUPPERCase(fields.at("latent_load_control"));
                }

                std::string loc_suppHeatCoilType("");
                if (fields.find("supplemental_heating_coil_object_type") != fields.end()) { // not required field
                    loc_suppHeatCoilType = UtilityRoutines::MakeUPPERCase(fields.at("supplemental_heating_coil_object_type"));
                }

                std::string loc_suppHeatCoilName("");
                if (fields.find("supplemental_heating_coil_name") != fields.end()) { // not required field
                    loc_suppHeatCoilName = UtilityRoutines::MakeUPPERCase(fields.at("supplemental_heating_coil_name"));
                }

                std::string loc_coolingSAFMethod("");
                if (fields.find("cooling_supply_air_flow_rate_method") != fields.end()) { // not required field
                    loc_coolingSAFMethod = UtilityRoutines::MakeUPPERCase(fields.at("cooling_supply_air_flow_rate_method"));
                }

                Real64 loc_coolingSAFMethod_SAFlow(-999.0);
                if (fields.find("cooling_supply_air_flow_rate") != fields.end()) { // not required field, autosizable
                    std::string tempFieldVal = UtilityRoutines::MakeUPPERCase(fields.at("cooling_supply_air_flow_rate"));
                    if (UtilityRoutines::SameString(tempFieldVal, "AutoSize")) {
                        loc_coolingSAFMethod_SAFlow = DataSizing::AutoSize;
                    } else {
                        loc_coolingSAFMethod_SAFlow = fields.at("cooling_supply_air_flow_rate");
                    }
                }

                Real64 loc_coolingSAFMethod_SAFlowPerFloorArea(-999.0);
                if (fields.find("cooling_supply_air_flow_rate_per_floor_area") != fields.end()) { // not required field
                    loc_coolingSAFMethod_SAFlowPerFloorArea = fields.at("cooling_supply_air_flow_rate_per_floor_area");
                }

                Real64 loc_coolingSAFMethod_FracOfAutosizedCoolingSAFlow(-999.0);
                if (fields.find("cooling_fraction_of_autosized_cooling_supply_air_flow") != fields.end()) { // not required field
                    loc_coolingSAFMethod_FracOfAutosizedCoolingSAFlow = fields.at("cooling_fraction_of_autosized_cooling_supply_air_flow");
                }

                Real64 loc_coolingSAFMethod_FlowPerCoolingCapacity(-999.0);
                if (fields.find("cooling_supply_air_flow_rate_per_unit_of_capacity") != fields.end()) { // not required field
                    loc_coolingSAFMethod_FlowPerCoolingCapacity = fields.at("cooling_supply_air_flow_rate_per_unit_of_capacity");
                }

                std::string loc_heatingSAFMethod("");
                if (fields.find("heating_supply_air_flow_rate_method") != fields.end()) { // not required field
                    loc_heatingSAFMethod = UtilityRoutines::MakeUPPERCase(fields.at("heating_supply_air_flow_rate_method"));
                }

                Real64 loc_heatingSAFMethod_SAFlow(-999.0);
                if (fields.find("heating_supply_air_flow_rate") != fields.end()) { // not required field
                    std::string tempFieldVal = UtilityRoutines::MakeUPPERCase(fields.at("cooling_supply_air_flow_rate"));
                    if (UtilityRoutines::SameString(tempFieldVal, "AutoSize")) {
                        loc_coolingSAFMethod_SAFlow = DataSizing::AutoSize;
                    } else {
                        loc_heatingSAFMethod_SAFlow = fields.at("heating_supply_air_flow_rate");
                    }
                }

                Real64 loc_heatingSAFMethod_SAFlowPerFloorArea(-999.0);
                if (fields.find("heating_supply_air_flow_rate_per_floor_area") != fields.end()) { // not required field
                    loc_heatingSAFMethod_SAFlowPerFloorArea = fields.at("heating_supply_air_flow_rate_per_floor_area");
                }

                Real64 loc_heatingSAFMethod_FracOfAutosizedHeatingSAFlow(-999.0);
                if (fields.find("heating_fraction_of_autosized_heating_supply_air_flow") != fields.end()) { // not required field
                    loc_heatingSAFMethod_FracOfAutosizedHeatingSAFlow = fields.at("heating_fraction_of_autosized_heating_supply_air_flow");
                }

                Real64 loc_heatingSAFMethod_FlowPerHeatingCapacity(-999.0);
                if (fields.find("heating_supply_air_flow_rate_per_unit_of_capacity") != fields.end()) { // not required field
                    loc_heatingSAFMethod_FlowPerHeatingCapacity = fields.at("heating_supply_air_flow_rate_per_unit_of_capacity");
                }

                std::string loc_noCoolHeatSAFMethod("");
                if (fields.find("no_load_supply_air_flow_rate_method") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod = UtilityRoutines::MakeUPPERCase(fields.at("no_load_supply_air_flow_rate_method"));
                }

                Real64 loc_noCoolHeatSAFMethod_SAFlow(-999.0);
                if (fields.find("no_load_supply_air_flow_rate") != fields.end()) { // not required field
                    std::string tempFieldVal = UtilityRoutines::MakeUPPERCase(fields.at("cooling_supply_air_flow_rate"));
                    if (UtilityRoutines::SameString(tempFieldVal, "AutoSize")) {
                        loc_coolingSAFMethod_SAFlow = DataSizing::AutoSize;
                    } else {
                        loc_noCoolHeatSAFMethod_SAFlow = fields.at("no_load_supply_air_flow_rate");
                    }
                }

                Real64 loc_noCoolHeatSAFMethod_SAFlowPerFloorArea(-999.0);
                if (fields.find("no_load_supply_air_flow_rate_per_floor_area") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod_SAFlowPerFloorArea = fields.at("no_load_supply_air_flow_rate_per_floor_area");
                }

                Real64 loc_noCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow(-999.0);
                if (fields.find("no_load_fraction_of_autosized_cooling_supply_air_flow") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow = fields.at("no_load_fraction_of_autosized_cooling_supply_air_flow");
                }

                Real64 loc_noCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow(-999.0);
                if (fields.find("no_load_fraction_of_autosized_heating_supply_air_flow") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow = fields.at("no_load_fraction_of_autosized_heating_supply_air_flow");
                }

                Real64 loc_noCoolHeatSAFMethod_FlowPerCoolingCapacity(-999.0);
                if (fields.find("no_load_supply_air_flow_rate_per_unit_of_capacity_during_cooling_operation") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod_FlowPerCoolingCapacity =
                        fields.at("no_load_supply_air_flow_rate_per_unit_of_capacity_during_cooling_operation");
                }

                Real64 loc_noCoolHeatSAFMethod_FlowPerHeatingCapacity(-999.0);
                if (fields.find("heating_supply_air_flow_rate_per_unit_of_capacity_during_heating_operation") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod_FlowPerHeatingCapacity =
                        fields.at("heating_supply_air_flow_rate_per_unit_of_capacity_during_heating_operation");
                }

                Real64 loc_designMaxOutletTemp(80.0);
                if (fields.find("maximum_supply_air_temperature") != fields.end()) { // not required field, has default
                    loc_designMaxOutletTemp = fields.at("maximum_supply_air_temperature");
                }

                Real64 loc_maxOATSuppHeat(21.0);
                if (fields.find("maximum_outdoor_dry_bulb_temperature_for_supplemental_heater_operation") !=
                    fields.end()) { // not required field, has default
                    loc_maxOATSuppHeat = fields.at("maximum_outdoor_dry_bulb_temperature_for_supplemental_heater_operation");
                }

                std::string loc_condenserInletNodeName = "";
                if (fields.find("outdoor_dry_bulb_temperature_sensor_node_name") != fields.end()) { // not required field
                    loc_condenserInletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("outdoor_dry_bulb_temperature_sensor_node_name"));
                }

                Real64 loc_maxONOFFCyclesperHour(2.5);
                if (fields.find("maximum_cycling_rate") != fields.end()) { // not required field, has default
                    loc_maxONOFFCyclesperHour = fields.at("maximum_cycling_rate");
                }

                Real64 loc_HPTimeConstant(60.0);
                if (fields.find("heat_pump_time_constant") != fields.end()) { // not required field, has default
                    loc_HPTimeConstant = fields.at("heat_pump_time_constant");
                }

                Real64 loc_onCyclePowerFraction(0.01);
                if (fields.find("fraction_of_on_cycle_power_use") != fields.end()) { // not required field, has default
                    loc_onCyclePowerFraction = fields.at("fraction_of_on_cycle_power_use");
                }

                Real64 loc_fanDelayTime(60.0);
                if (fields.find("heat_pump_fan_delay_time") != fields.end()) { // not required field, has default
                    loc_fanDelayTime = fields.at("heat_pump_fan_dealy_time");
                }

                Real64 loc_ancillaryOnPower(0.0);
                if (fields.find("ancillary_on_cycle_electric_power") != fields.end()) { // not required field, has default
                    loc_ancillaryOnPower = fields.at("ancillary_on_cycle_electric_power");
                }

                Real64 loc_ancillaryOffPower(0.0);
                if (fields.find("ancillary_off_cycle_electric_power") != fields.end()) { // not required field, has default
                    loc_ancillaryOffPower = fields.at("ancillary_off_cycle_electric_power");
                }

                Real64 loc_designHRWaterVolumeFlow(0.0);
                if (fields.find("design_heat_recovery_water_flow_rate") != fields.end()) { // not required field, has default
                    loc_designHRWaterVolumeFlow = fields.at("design_heat_recovery_water_flow_rate");
                }

                Real64 loc_maxHROutletWaterTemp(80.0);
                if (fields.find("maximum_temperature_for_heat_recovery") != fields.end()) { // not required field, has default
                    loc_maxHROutletWaterTemp = fields.at("maximum_temperature_for_heat_recovery");
                }

                std::string loc_heatRecoveryInletNodeName = "";
                if (fields.find("heat_recovery_water_inlet_node_name") != fields.end()) { // not required field
                    loc_heatRecoveryInletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("heat_recovery_water_inlet_node_name"));
                }

                std::string loc_heatRecoveryOutletNodeName = "";
                if (fields.find("heat_recovery_water_outlet_node_name") != fields.end()) { // not required field
                    loc_heatRecoveryOutletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("heat_recovery_water_outlet_node_name"));
                }

                std::string loc_designSpecMultispeedHPType = "";
                if (fields.find("design_specification_multispeed_object_type") != fields.end()) { // not required field
                    loc_designSpecMultispeedHPType = UtilityRoutines::MakeUPPERCase(fields.at("design_specification_multispeed_object_type"));
                }

                std::string loc_designSpecMultispeedHPName = "";
                if (fields.find("design_specification_multispeed_object_name") != fields.end()) { // not required field
                    loc_designSpecMultispeedHPName = UtilityRoutines::MakeUPPERCase(fields.at("design_specification_multispeed_object_name"));
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

                thisSys.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisSys.unitType = cCurrentModuleObject;
                thisSys.unitarySystemType_Num = SimAirServingZones::UnitarySystemModel;
                thisSys.sysAvailSchedPtr = ScheduleManager::GetScheduleIndex(loc_sysAvailSched);

                if (UtilityRoutines::SameString(loc_controlType, "Load")) {
                    thisSys.controlType = controlTypeEnum::controlTypeLoad;
                } else if (UtilityRoutines::SameString(loc_controlType, "SetPoint")) {
                    thisSys.controlType = controlTypeEnum::controlTypeSetpoint;
                } else if (UtilityRoutines::SameString(loc_controlType, "SingleZoneVAV")) {
                    thisSys.controlType = controlTypeEnum::controlTypeCCMASHRAE;
                    thisSys.validASHRAECoolCoil = true;
                    thisSys.validASHRAEHeatCoil = true;
                } else {
                    ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                    ShowContinueError("Invalid Control Type = " + loc_controlType);
                    errorsFound = true;
                }

                if (loc_controlZoneName != "") thisSys.controlZoneNum = UtilityRoutines::FindItemInList(loc_controlZoneName, DataHeatBalance::Zone);

                if (UtilityRoutines::SameString(loc_dehumControlType, "None")) {
                    thisSys.dehumidControlType_Num = dehumCtrlTypeEnum::dehumidControl_None;
                    thisSys.humidistat = false;
                } else if (UtilityRoutines::SameString(loc_dehumControlType, "CoolReheat")) {
                    thisSys.dehumidControlType_Num = dehumCtrlTypeEnum::dehumidControl_CoolReheat;
                    thisSys.humidistat = true;
                } else if (UtilityRoutines::SameString(loc_dehumControlType, "Multimode")) {
                    thisSys.dehumidControlType_Num = dehumCtrlTypeEnum::dehumidControl_Multimode;
                    thisSys.humidistat = true;
                }

                thisSys.airInNode = NodeInputManager::GetOnlySingleNode(loc_airInNodeName,
                                                                        errorsFound,
                                                                        cCurrentModuleObject,
                                                                        thisObjectName,
                                                                        DataLoopNode::NodeType_Air,
                                                                        DataLoopNode::NodeConnectionType_Inlet,
                                                                        1,
                                                                        DataLoopNode::ObjectIsParent);
                thisSys.airOutNode = NodeInputManager::GetOnlySingleNode(loc_airOutNodeName,
                                                                         errorsFound,
                                                                         cCurrentModuleObject,
                                                                         thisObjectName,
                                                                         DataLoopNode::NodeType_Air,
                                                                         DataLoopNode::NodeConnectionType_Outlet,
                                                                         1,
                                                                         DataLoopNode::ObjectIsParent);

                Real64 TotalFloorAreaOnAirLoop = 0.0;
                int AirLoopNumber = 0;
                bool AirNodeFound = false;
                bool AirLoopFound = false;
                bool OASysFound = false;
                bool ZoneEquipmentFound = false;
                bool ZoneInletNodeFound = false;

                // Get AirTerminal mixer data
                SingleDuct::GetATMixer(thisObjectName,
                                       thisSys.ATMixerName,
                                       thisSys.ATMixerIndex,
                                       thisSys.ATMixerType,
                                       thisSys.ATMixerPriNode,
                                       thisSys.ATMixerSecNode,
                                       thisSys.ATMixerOutNode,
                                       thisSys.airOutNode);
                if (thisSys.ATMixerType == DataHVACGlobals::ATMixer_InletSide || thisSys.ATMixerType == DataHVACGlobals::ATMixer_SupplySide) {
                    thisSys.ATMixerExists = true;
                }

                // check if the UnitarySystem is connected as zone equipment
                if (!thisSys.ATMixerExists && !AirLoopFound && !OASysFound) {
                    for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                        for (int ZoneExhNum = 1; ZoneExhNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes; ++ZoneExhNum) {
                            if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum) != thisSys.airInNode) continue;
                            ZoneEquipmentFound = true;
                            //               Find the controlled zone number for the specified thermostat location
                            thisSys.nodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                            TotalFloorAreaOnAirLoop =
                                DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum).FloorArea;
                            thisSys.airLoopEquipment = false;
                            thisSys.zoneInletNode = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum);
                            if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex > 0) {
                                for (int EquipNum = 1; EquipNum <= DataZoneEquipment::ZoneEquipList(
                                                                       DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                                       .NumOfEquipTypes;
                                     ++EquipNum) {
                                    if ((DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                             .EquipType_Num(EquipNum) != DataZoneEquipment::ZoneUnitarySystem_Num) ||
                                        DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .EquipName(EquipNum) != thisObjectName)
                                        continue;
                                    thisSys.zoneSequenceCoolingNum =
                                        DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                            .CoolingPriority(EquipNum);
                                    thisSys.zoneSequenceHeatingNum =
                                        DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                            .HeatingPriority(EquipNum);
                                }
                            }
                            thisSys.controlZoneNum = ControlledZoneNum;
                            break;
                        }
                        if (ZoneEquipmentFound) {
                            for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                 ++ZoneInletNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.airOutNode) continue;
                                ZoneInletNodeFound = true;
                                break;
                            }
                        }
                    }
                    if (!ZoneInletNodeFound) {
                        for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                            for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                 ++ZoneInletNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.airOutNode) continue;
                                ZoneInletNodeFound = true;
                                ZoneEquipmentFound = true;
                                break;
                            }
                        }
                        if (!ZoneInletNodeFound && ZoneEquipmentFound) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Incorrect or misspelled " + cAlphaFields(iAirOutletNodeNameAlphaNum) + " = " +
                            //                  Alphas(iAirOutletNodeNameAlphaNum));
                            ShowContinueError("Node name does not match any controlled zone inlet node name. Check ZoneHVAC:EquipmentConnections "
                                              "object inputs.");
                            errorsFound = true;
                        }
                    }
                    // Need to move this to the end - just comment out for now
                    // if ( ! ZoneEquipmentFound ) {
                    //	ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
                    //	ShowContinueError( "Incorrect or misspelled " + cAlphaFields( iAirInletNodeNameAlphaNum ) + " = " + Alphas(
                    // iAirInletNodeNameAlphaNum ) ); 	ShowContinueError( "Node name does not match any controlled zone exhaust node name.
                    // Check ZoneHVAC:EquipmentConnections object inputs." ); 	ErrorsFound = true;
                    //}
                }

                // check if the UnitarySystem is connected as zone equipment
                if (thisSys.ATMixerExists && thisSys.ATMixerType == DataHVACGlobals::ATMixer_InletSide) {

                    if (!AirLoopFound && !OASysFound) {
                        for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                            for (int ZoneExhNum = 1; ZoneExhNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes;
                                 ++ZoneExhNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum) != thisSys.ATMixerSecNode) continue;
                                ZoneEquipmentFound = true;
                                //               Find the controlled zone number for the specified thermostat location
                                thisSys.nodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                                // TotalZonesOnAirLoop doesn't appear to be used anywhere
                                //++TotalZonesOnAirLoop;
                                TotalFloorAreaOnAirLoop =
                                    DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum).FloorArea;
                                thisSys.airLoopEquipment = false;
                                thisSys.zoneInletNode = thisSys.airOutNode;
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex > 0) {
                                    for (int EquipNum = 1; EquipNum <= DataZoneEquipment::ZoneEquipList(
                                                                           DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                                           .NumOfEquipTypes;
                                         ++EquipNum) {
                                        if ((DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                 .EquipType_Num(EquipNum) != DataZoneEquipment::ZoneUnitarySystem_Num) ||
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                    .EquipName(EquipNum) != thisObjectName)
                                            continue;
                                        thisSys.zoneSequenceCoolingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .CoolingPriority(EquipNum);
                                        thisSys.zoneSequenceHeatingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .HeatingPriority(EquipNum);
                                    }
                                }
                                thisSys.controlZoneNum = ControlledZoneNum;
                                break;
                            }
                            if (ZoneEquipmentFound) {
                                for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                     ++ZoneInletNum) {
                                    if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.airOutNode) continue;
                                    ZoneInletNodeFound = true;
                                    break;
                                }
                            }
                        }
                        if (!ZoneInletNodeFound) {
                            for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                                for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                     ++ZoneInletNum) {
                                    if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.airOutNode) continue;
                                    ZoneInletNodeFound = true;
                                    ZoneEquipmentFound = true;
                                    break;
                                }
                            }
                            if (!ZoneInletNodeFound && ZoneEquipmentFound) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Incorrect or misspelled " + cAlphaFields(iAirOutletNodeNameAlphaNum) + " = " +
                                //                  Alphas(iAirOutletNodeNameAlphaNum));
                                ShowContinueError("Node name does not match any controlled zone inlet node name. Check ZoneHVAC:EquipmentConnections "
                                                  "object inputs.");
                                errorsFound = true;
                            }
                        }
                        // Need to move this to the end - just comment out for now
                        // if ( !ZoneEquipmentFound ) {
                        //	ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
                        //	ShowContinueError( "Incorrect or misspelled " + cAlphaFields( iAirInletNodeNameAlphaNum ) + " = " + Alphas(
                        // iAirInletNodeNameAlphaNum ) ); 	ShowContinueError( "Node name does not match air terminal mixer secondary air node.
                        // Check AirTerminal:SingleDuct:Mixer object inputs." ); 	ErrorsFound = true;
                        //}
                    }
                }

                if (thisSys.ATMixerExists && thisSys.ATMixerType == DataHVACGlobals::ATMixer_SupplySide) {

                    if (!AirLoopFound && !OASysFound) {
                        for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                            for (int ZoneExhNum = 1; ZoneExhNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes;
                                 ++ZoneExhNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum) != thisSys.airInNode) continue;
                                ZoneEquipmentFound = true;
                                //               Find the controlled zone number for the specified thermostat location
                                thisSys.nodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                                // TotalZonesOnAirLoop doesn't appear to be used anywhere
                                //++TotalZonesOnAirLoop;
                                TotalFloorAreaOnAirLoop =
                                    DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum).FloorArea;
                                thisSys.airLoopEquipment = false;
                                thisSys.zoneInletNode = thisSys.ATMixerOutNode;
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex > 0) {
                                    for (int EquipNum = 1; EquipNum <= DataZoneEquipment::ZoneEquipList(
                                                                           DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                                           .NumOfEquipTypes;
                                         ++EquipNum) {
                                        if ((DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                 .EquipType_Num(EquipNum) != DataZoneEquipment::ZoneUnitarySystem_Num) ||
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                    .EquipName(EquipNum) != thisObjectName)
                                            continue;
                                        thisSys.zoneSequenceCoolingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .CoolingPriority(EquipNum);
                                        thisSys.zoneSequenceHeatingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .HeatingPriority(EquipNum);
                                    }
                                }
                                thisSys.controlZoneNum = ControlledZoneNum;
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
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Incorrect or misspelled " + cAlphaFields(iAirOutletNodeNameAlphaNum) + " = " +
                                //                  Alphas(iAirOutletNodeNameAlphaNum));
                                ShowContinueError("Node name does not match any air terminal mixer secondary air inlet node. Check "
                                                  "AirTerminal:SingleDuct:Mixer object inputs.");
                                errorsFound = true;
                            }
                        }
                        // Need to move this to the end - just comment out for now
                        // if ( !ZoneEquipmentFound ) {
                        //	ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
                        //	ShowContinueError( "Incorrect or misspelled " + cAlphaFields( iAirInletNodeNameAlphaNum ) + " = " + Alphas(
                        // iAirInletNodeNameAlphaNum ) ); 	ShowContinueError( "Node name does not match any controlled zone exhaust node name.
                        // Check ZoneHVAC:EquipmentConnections object inputs." ); 	ErrorsFound = true;
                        //}
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
                                        if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum != thisSys.controlZoneNum) continue;
                                        //             Find the controlled zone number for the specified thermostat location
                                        thisSys.nodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                                        thisSys.controlZoneNum = ControlledZoneNum;
                                        //             Determine if system is on air loop served by the thermostat location specified
                                        for (int zoneInNode = 1; zoneInNode <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                             ++zoneInNode) {
                                            if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode) ==
                                                AirLoopNumber) {
                                                // TotalZonesOnAirLoop doesn't appear to be used anywhere
                                                //++TotalZonesOnAirLoop;
                                                thisSys.zoneInletNode = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(zoneInNode);
                                                TotalFloorAreaOnAirLoop +=
                                                    DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum)
                                                        .FloorArea;
                                            }
                                        }
                                        for (int TstatZoneNum = 1; TstatZoneNum <= DataZoneControls::NumTempControlledZones; ++TstatZoneNum) {
                                            if (DataZoneControls::TempControlledZone(TstatZoneNum).ActualZoneNum != thisSys.controlZoneNum) continue;
                                            AirNodeFound = true;
                                        }
                                        for (int TstatZoneNum = 1; TstatZoneNum <= DataZoneControls::NumComfortControlledZones; ++TstatZoneNum) {
                                            if (DataZoneControls::ComfortControlledZone(TstatZoneNum).ActualZoneNum != thisSys.controlZoneNum)
                                                continue;
                                            AirNodeFound = true;
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

                if (!AirLoopFound && !ZoneEquipmentFound && !OASysFound && !DataHVACGlobals::GetAirPathDataDone) {
                    // Unsucessful attempt
                    break;
                } else {
                    thisSys.myGetInputSuccessfulFlag = true;
                }

                if (AirLoopNumber == 0 && !ZoneEquipmentFound &&
                    (thisSys.controlType == controlTypeEnum::controlTypeLoad || thisSys.controlType == controlTypeEnum::controlTypeCCMASHRAE)) {
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
                    BranchNodeConnections::TestCompSet(cCurrentModuleObject, thisSys.name, loc_airInNodeName, loc_airOutNodeName, "Air Nodes");

                if (loc_fanName != "" && loc_fanType != "") {
                    if (UtilityRoutines::SameString(loc_fanType, "Fan:SystemModel")) {
                        if (!HVACFan::checkIfFanNameIsAFanSystem(loc_fanName)) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {
                            thisSys.fanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                            isNotOK = false;
                            ValidateComponent(loc_fanType, loc_fanName, isNotOK, cCurrentModuleObject);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            } else {                                                                // mine data from fan object
                                HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(loc_fanName)); // call constructor
                                thisSys.fanIndex = HVACFan::getFanObjectVectorIndex(loc_fanName);
                                FanVolFlowRate = HVACFan::fanObjs[thisSys.fanIndex]->designAirVolFlowRate;
                                if (FanVolFlowRate == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                                thisSys.actualFanVolFlowRate = FanVolFlowRate;
                                thisSys.designFanVolFlowRate = FanVolFlowRate;
                                FanInletNode = HVACFan::fanObjs[thisSys.fanIndex]->inletNodeNum;
                                FanOutletNode = HVACFan::fanObjs[thisSys.fanIndex]->outletNodeNum;
                                thisSys.fanAvailSchedPtr = HVACFan::fanObjs[thisSys.fanIndex]->availSchedIndex;
                            }
                        }
                    } else {
                        Fans::GetFanType(loc_fanName, thisSys.fanType_Num, isNotOK, cCurrentModuleObject, loc_fanName);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {
                            isNotOK = false;
                            ValidateComponent(loc_fanType, loc_fanName, isNotOK, cCurrentModuleObject);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            } else { // mine data from fan object
                                // Get the fan index
                                Fans::GetFanIndex(loc_fanName, thisSys.fanIndex, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Design Fan Volume Flow Rate
                                errFlag = false;
                                FanVolFlowRate = Fans::GetFanDesignVolumeFlowRate(loc_fanType, loc_fanName, errFlag);
                                if (FanVolFlowRate == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                                thisSys.actualFanVolFlowRate = FanVolFlowRate;
                                thisSys.designFanVolFlowRate = FanVolFlowRate;
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Fan Inlet Node
                                errFlag = false;
                                FanInletNode = Fans::GetFanInletNode(loc_fanType, loc_fanName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Fan Outlet Node
                                errFlag = false;
                                FanOutletNode = Fans::GetFanOutletNode(loc_fanType, loc_fanName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the fan's availability schedule
                                errFlag = false;
                                thisSys.fanAvailSchedPtr = Fans::GetFanAvailSchPtr(loc_fanType, loc_fanName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                            } // IF (IsNotOK) THEN
                        }
                    }
                    thisSys.fanExists = true;
                    thisSys.fanName = loc_fanName;
                } else {
                    if ((loc_fanName == "" && loc_fanType != "") || (loc_fanName != "" && loc_fanType == "")) {
                        ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                        ShowContinueError("Invalid Fan Type or Name: Fan Name = " + loc_fanName + ", Fan Type = " + loc_fanType);
                        errorsFound = true;
                    }
                }

                // Add fan to component sets array
                if (thisSys.fanExists)
                    BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                         thisObjectName,
                                                         loc_fanType,
                                                         loc_fanName,
                                                         DataLoopNode::NodeID(FanInletNode),
                                                         DataLoopNode::NodeID(FanOutletNode));

                if (UtilityRoutines::SameString(loc_supFanPlace, "BlowThrough")) thisSys.fanPlace = fanPlaceEnum::blowThru;
                if (UtilityRoutines::SameString(loc_supFanPlace, "DrawThrough")) thisSys.fanPlace = fanPlaceEnum::drawThru;
                if (thisSys.fanPlace == fanPlaceEnum::notYetSet && thisSys.fanExists) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iFanPlaceAlphaNum) + " = " + Alphas(iFanPlaceAlphaNum));
                    errorsFound = true;
                }

                thisSys.fanOpModeSchedPtr = ScheduleManager::GetScheduleIndex(loc_supFanOpMode);
                if (loc_supFanOpMode != "" && thisSys.fanOpModeSchedPtr == 0) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iFanSchedAlphaNum) + " = " + Alphas(iFanSchedAlphaNum));
                    errorsFound = true;
                } else if (loc_supFanOpMode == "") {
                    if (thisSys.controlType == controlTypeEnum::controlTypeSetpoint) {
                        // Fan operating mode must be constant fan so that the coil outlet temp is proportional to PLR
                        // Cycling fan always outputs the full load outlet air temp so should not be used with set point based control
                        thisSys.fanOpMode = fanOpModeEnum::contFanCycCoil;
                    } else {
                        thisSys.fanOpMode = fanOpModeEnum::cycFanCycCoil;
                        // why does it have to be OnOff or SystemFan? I forget now.
                        // if (thisSys.FanType_Num != FanType_SimpleOnOff &&
                        //    thisSys.FanType_Num != FanType_SystemModelObject && thisSys.FanExists) {
                        //    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        //    ShowContinueError(cAlphaFields(iFanTypeAlphaNum) + " = " + Alphas(iFanTypeAlphaNum));
                        //    ShowContinueError("Fan type must be Fan:OnOff of Fan:SystemModel when " + cAlphaFields(iFanSchedAlphaNum) + " =
                        //    Blank."); ErrorsFound = true;
                        //}
                    }
                } else if (loc_supFanOpMode != "" && thisSys.fanOpMode > fanOpModeEnum::fanOpModeNotYetSet &&
                           thisSys.controlType == controlTypeEnum::controlTypeSetpoint) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.fanOpModeSchedPtr, ">", 0.0, "<=", 1.0)) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("For " + loc_fanType + " = " + loc_fanName);
                        ShowContinueError("Fan operating mode must be continuous (fan operating mode schedule values > 0).");
                        // ShowContinueError("Error found in " + cAlphaFields(iFanSchedAlphaNum) + " = " + Alphas(iFanSchedAlphaNum));
                        ShowContinueError("...schedule values must be (>0., <=1.)");
                        errorsFound = true;
                    }
                }

                // Check fan's schedule for cycling fan operation IF constant volume fan is used
                if (thisSys.fanOpModeSchedPtr > 0 && thisSys.fanType_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.fanOpModeSchedPtr, ">", 0.0, "<=", 1.0)) {
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
                thisSys.heatingSizingRatio = loc_heatingSizingRatio;
                int HeatingCoilPLFCurveIndex = 0;
                thisSys.heatingCoilName = loc_heatingCoilName;
                thisSys.heatingCoilTypeName = loc_heatingCoilType; //  for coil selection report
                if (loc_heatingCoilType != "") {
                    thisSys.heatCoilExists = true;
                    PrintMessage = false;
                } else {
                    thisSys.validASHRAEHeatCoil = false;
                }

                if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:DX:VariableSpeed")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:DX:MultiSpeed")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::CoilDX_MultiSpeedHeating;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Water")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingWater;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Steam")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingSteam;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:WaterToAirHeatPump:EquationFit")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingWaterToAirHPSimple;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:WaterToAirHeatPump:ParameterEstimation")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingWaterToAirHP;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Electric:MultiStage")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingElectric_MultiStage;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Gas:MultiStage")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingGas_MultiStage;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Fuel") ||
                           UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Electric") ||
                           UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Desuperheater")) {
                    thisSys.heatingCoilType_Num = HeatingCoils::GetHeatingCoilTypeNum(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:UserDefined")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_UserDefined;
                } else if (thisSys.heatCoilExists) {
                    thisSys.heatingCoilType_Num = DXCoils::GetCoilTypeNum(loc_heatingCoilType, loc_heatingCoilName, errFlag, PrintMessage);
                }

                if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {

                    thisSys.DXHeatingCoil = true;
                    errFlag = false;

                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;

                    } else { // mine data from DX heating coil

                        // Get DX heating coil index
                        DXCoils::GetDXCoilIndex(loc_heatingCoilName, thisSys.heatingCoilIndex, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get DX heating coil capacity
                        thisSys.designHeatingCapacity = DXCoils::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.designHeatingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get DX coil air flow rate.
                        thisSys.maxHeatAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Nodes
                        HeatingCoilInletNode = DXCoils::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        HeatingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        DXCoils::SetDXCoolingCoilData(
                            thisSys.heatingCoilIndex, errorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, loc_heatingSizingRatio);
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) {

                    thisSys.DXHeatingCoil = true;
                    errFlag = false;

                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else {

                        thisSys.heatingCoilIndex = VariableSpeedCoils::GetCoilIndexVariableSpeed(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.numOfSpeedHeating = VariableSpeedCoils::GetVSCoilNumOfSpeeds(loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;

                        thisSys.maxHeatAirVolFlow =
                            VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        HeatingCoilInletNode = VariableSpeedCoils::GetCoilInletNodeVariableSpeed(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        HeatingCoilOutletNode = VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                        // Get DX heating coil capacity
                        thisSys.designHeatingCapacity =
                            VariableSpeedCoils::GetCoilCapacityVariableSpeed(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.designHeatingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }
                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                    thisSys.DXHeatingCoil = true;
                    errFlag = false;
                    DXCoils::GetDXCoilIndex(loc_heatingCoilName, thisSys.heatingCoilIndex, errFlag, loc_heatingCoilType);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    thisSys.heatingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_heatingCoilType, loc_heatingCoilName, errFlag);

                    // Get DX coil air flow rate.
                    thisSys.maxHeatAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    HeatingCoilInletNode = DXCoils::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }
                    HeatingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    thisSys.designHeatingCapacity = DXCoils::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {

                    errFlag = false;
                    HeatingCoils::GetCoilIndex(loc_heatingCoilName, thisSys.heatingCoilIndex, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }
                    HeatingCoilInletNode = HeatingCoils::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }
                    HeatingCoilOutletNode = HeatingCoils::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    thisSys.heatingCoilAvailSchPtr = HeatingCoils::GetCoilAvailScheduleIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);

                    thisSys.designHeatingCapacity = HeatingCoils::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);

                    if (thisSys.designHeatingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGasOrOtherFuel ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingDesuperheater) {
                    errFlag = false;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    } else {

                        ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from heating coil

                            // Get heating coil index
                            errFlag = false;
                            HeatingCoils::GetCoilIndex(loc_heatingCoilName, thisSys.heatingCoilIndex, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the design heating capacity
                            thisSys.designHeatingCapacity = HeatingCoils::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                            if (thisSys.designHeatingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.heatingCoilAvailSchPtr =
                                HeatingCoils::GetCoilAvailScheduleIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);

                            // Get the Heating Coil Inlet Node
                            HeatingCoilInletNode = HeatingCoils::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Heating Coil Outlet Node
                            HeatingCoilOutletNode = HeatingCoils::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Heating Coil PLF Curve Index
                            HeatingCoilPLFCurveIndex = HeatingCoils::GetHeatingCoilPLFCurveIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                            // These heating coil types do not have an air flow input field
                            if (thisSys.requestAutoSize) {
                                thisSys.maxHeatAirVolFlow = DataSizing::AutoSize;
                            }
                        } // IF (IsNotOK) THEN
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.heatingCoilAvailSchPtr =
                            WaterCoils::GetWaterCoilAvailScheduleIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilIndex = WaterCoils::GetWaterCoilIndex("COIL:HEATING:WATER", loc_heatingCoilName, errFlag);
                        if (thisSys.heatingCoilIndex == 0) {
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
                        thisSys.heatCoilFluidInletNode = WaterCoils::GetCoilWaterInletNode("Coil:Heating:Water", loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil hot water max volume flow rate
                        thisSys.maxHeatCoilFluidFlow = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", loc_heatingCoilName, errFlag);
                        if (thisSys.maxHeatCoilFluidFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Inlet Node
                        HeatingCoilInletNode = WaterCoils::GetCoilInletNode("Coil:Heating:Water", loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = WaterCoils::GetCoilOutletNode("Coil:Heating:Water", loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.heatingCoilAvailSchPtr =
                            SteamCoils::GetSteamCoilAvailScheduleIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilIndex = SteamCoils::GetSteamCoilIndex("COIL:HEATING:STEAM", loc_heatingCoilName, errFlag);
                        if (thisSys.heatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil steam inlet node number
                        errFlag = false;
                        thisSys.heatCoilFluidInletNode = SteamCoils::GetCoilSteamInletNode("Coil:Heating:Steam", loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil steam max volume flow rate
                        thisSys.maxHeatCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(thisSys.heatingCoilIndex, errFlag);
                        if (thisSys.maxHeatCoilFluidFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                        if (thisSys.maxHeatCoilFluidFlow > 0.0) {
                            int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            Real64 TempSteamIn = 100.0;
                            Real64 SteamDensity =
                                FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getUnitarySystemInput);
                            thisSys.maxHeatCoilFluidFlow *= SteamDensity;
                            errFlag = false;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        HeatingCoilInletNode = SteamCoils::GetCoilAirInletNode(thisSys.heatingCoilIndex, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = SteamCoils::GetCoilAirOutletNode(thisSys.heatingCoilIndex, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        if (thisSys.requestAutoSize) {
                            thisSys.maxHeatAirVolFlow = DataSizing::AutoSize;
                        }
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) {
                    thisSys.DXHeatingCoil = true;
                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.heatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilIndex = WaterToAirHeatPumpSimple::GetCoilIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.heatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.designHeatingCapacity = WaterToAirHeatPumpSimple::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                        errFlag = false;
                        thisSys.maxHeatAirVolFlow = WaterToAirHeatPumpSimple::GetCoilAirFlowRate(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        HeatingCoilInletNode = WaterToAirHeatPumpSimple::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = WaterToAirHeatPumpSimple::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP) {
                    thisSys.DXHeatingCoil = true;
                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.heatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilIndex = WaterToAirHeatPumpSimple::GetCoilIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.heatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.designHeatingCapacity = WaterToAirHeatPumpSimple::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        HeatingCoilInletNode = WaterToAirHeatPumpSimple::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = WaterToAirHeatPumpSimple::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from Heating coil object

                        errFlag = false;
                        thisSys.heatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        UserDefinedComponents::GetUserDefinedCoilIndex(loc_heatingCoilName, thisSys.heatingCoilIndex, errFlag, cCurrentModuleObject);
                        if (thisSys.heatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // **** How to get this info ****
                        //						UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = GetWtoAHPCoilCapacity(
                        // CoolingCoilType,  loc_coolingCoilName,  errFlag ); 						if ( errFlag ) {
                        //							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " +
                        // UnitarySystem(
                        // UnitarySysNum
                        //).Name
                        //); 							ErrorsFound = true;
                        //							errFlag = false;
                        //						}

                        // Get the Cooling Coil Inlet Node
                        errFlag = false;
                        UserDefinedComponents::GetUserDefinedCoilAirInletNode(
                            loc_heatingCoilName, HeatingCoilInletNode, errFlag, cCurrentModuleObject);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Cooling Coil Outlet Node
                        UserDefinedComponents::GetUserDefinedCoilAirOutletNode(
                            loc_heatingCoilName, HeatingCoilOutletNode, errFlag, cCurrentModuleObject);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.heatCoilExists) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilTypeAlphaNum) + " = " + Alphas(iHeatingCoilTypeAlphaNum));
                    errorsFound = true;
                } // IF (thisSys%heatingCoilType_Num == Coil_HeatingGasOrOtherFuel .OR. &, etc.

                if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {
                    thisSys.multiSpeedHeatingCoil = true;
                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                    thisSys.varSpeedHeatingCoil = true;
                }

                // coil outlet node set point has priority, IF not exist, then use system outlet node
                if (SetPointManager::NodeHasSPMCtrlVarType(thisSys.airOutNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.systemHeatControlNodeNum = thisSys.airOutNode;
                if (SetPointManager::NodeHasSPMCtrlVarType(HeatingCoilOutletNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.systemHeatControlNodeNum = HeatingCoilOutletNode;

                thisSys.heatCoilInletNodeNum = HeatingCoilInletNode;
                thisSys.heatCoilOutletNodeNum = HeatingCoilOutletNode;
                thisSys.heatingCoilName = loc_heatingCoilName;

                // Add heating coil to component sets array
                if (thisSys.heatCoilExists) {
                    if (thisSys.heatingCoilType_Num != DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                        BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                             thisObjectName,
                                                             loc_heatingCoilType,
                                                             loc_heatingCoilName,
                                                             DataLoopNode::NodeID(HeatingCoilInletNode),
                                                             DataLoopNode::NodeID(HeatingCoilOutletNode));
                    } else {
                        BranchNodeConnections::SetUpCompSets(
                            cCurrentModuleObject, thisObjectName, loc_heatingCoilType, loc_heatingCoilName, "UNDEFINED", "UNDEFINED");
                    }
                }

                // Get Cooling Coil Information IF available
                if (loc_coolingCoilType != "" && loc_coolingCoilName != "") {
                    thisSys.coolCoilExists = true;

                    //       Find the type of coil. do not print message since this may not be the correct coil type.
                    errFlag = false;
                    if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:VariableSpeed")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:MultiSpeed")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::CoilDX_MultiSpeedCooling;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:Water")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingWater;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:Water:DetailedGeometry")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterDetailed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:TwoStageWithHumidityControlMode")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
                        thisSys.coolingCoilType_Num =
                            HVACHXAssistedCoolingCoil::GetCoilGroupTypeNum(loc_coolingCoilType, loc_coolingCoilName, errFlag, PrintMessage);
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
                        thisSys.coolingCoilType_Num =
                            HVACHXAssistedCoolingCoil::GetCoilGroupTypeNum(loc_coolingCoilType, loc_coolingCoilName, errFlag, PrintMessage);
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:WaterToAirHeatPump:EquationFit")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterToAirHPSimple;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterToAirHP;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:SingleSpeed")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::CoilDX_CoolingSingleSpeed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:TwoSpeed")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::CoilDX_CoolingTwoSpeed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:UserDefined")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_UserDefined;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:SingleSpeed:ThermalStorage")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::CoilDX_PackagedThermalStorageCooling;
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilTypeAlphaNum) + " = " + Alphas(iCoolingCoilTypeAlphaNum));
                    }

                    if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed ||
                        thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from DX cooling coil

                            if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) thisSys.numOfSpeedCooling = 2;

                            // Get DX cooling coil index
                            DXCoils::GetDXCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            thisSys.coolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_coolingCoilType, loc_coolingCoilName, errFlag);

                            // Get DX cooling coil capacity
                            errFlag = false;
                            thisSys.designCoolingCapacity = DXCoils::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.designCoolingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get DX coil air flow rate. Latter fields will overwrite this IF input field is present
                            errFlag = false;
                            thisSys.maxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = DXCoils::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            CoolingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get Outdoor condenser node from DX coil object
                            errFlag = false;
                            thisSys.condenserNodeNum = DXCoils::GetCoilCondenserInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            if (thisSys.fanExists) {
                                errFlag = false;
                                DXCoils::SetDXCoolingCoilData(
                                    thisSys.coolingCoilIndex, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, loc_fanName);
                                DXCoils::SetDXCoolingCoilData(
                                    thisSys.coolingCoilIndex, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, thisSys.fanIndex);
                                DXCoils::SetDXCoolingCoilData(thisSys.coolingCoilIndex,
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
                                                              thisSys.fanType_Num);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }
                            }
                            if (thisSys.heatCoilExists) {
                                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                    thisSys.heatPump = true;
                                }

                                // set fan info for heating coils
                                if (thisSys.fanExists) {
                                    if (thisSys.fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.heatingCoilName,
                                                                                     thisSys.heatingCoilTypeName,
                                                                                     thisSys.fanName,
                                                                                     DataAirSystems::objectVectorOOFanSystemModel,
                                                                                     thisSys.fanIndex);
                                    } else {
                                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.heatingCoilName,
                                                                                     thisSys.heatingCoilTypeName,
                                                                                     thisSys.fanName,
                                                                                     DataAirSystems::structArrayLegacyFanModels,
                                                                                     thisSys.fanIndex);
                                    }
                                }
                            }

                        } // IF (IsNotOK) THEN

                        // Push heating coil PLF curve index to DX coil
                        if (HeatingCoilPLFCurveIndex > 0) {
                            DXCoils::SetDXCoolingCoilData(thisSys.coolingCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from DX cooling coil

                            // Get DX cooling coil index
                            DXCoils::GetDXCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            thisSys.coolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_coolingCoilType, loc_coolingCoilName, errFlag);

                            // Get DX cooling coil capacity
                            errFlag = false;
                            thisSys.designCoolingCapacity = DXCoils::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.designCoolingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                            errFlag = false;
                            thisSys.maxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = DXCoils::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            CoolingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get Outdoor condenser node from DX coil object
                            errFlag = false;
                            thisSys.condenserNodeNum = DXCoils::GetCoilCondenserInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                        } // IF (IsNotOK) THEN

                        // Push heating coil PLF curve index to DX coil
                        if (HeatingCoilPLFCurveIndex > 0) {
                            DXCoils::SetDXCoolingCoilData(thisSys.coolingCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                        }

                        if (thisSys.heatCoilExists) {
                            if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.heatPump = true;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from heat exchanger assisted cooling coil

                            // Get DX heat exchanger assisted cooling coil index
                            errFlag = false;
                            HVACHXAssistedCoolingCoil::GetHXDXCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            std::string ChildCoolingCoilName =
                                HVACHXAssistedCoolingCoil::GetHXDXCoilName(loc_coolingCoilType, loc_coolingCoilName, isNotOK);
                            std::string ChildCoolingCoilType =
                                HVACHXAssistedCoolingCoil::GetHXDXCoilType(loc_coolingCoilType, loc_coolingCoilName, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            if (UtilityRoutines::SameString(ChildCoolingCoilType, "COIL:COOLING:DX:SINGLESPEED")) {

                                errFlag = false;
                                thisSys.coolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(ChildCoolingCoilType, ChildCoolingCoilName, errFlag);
                                if (isNotOK) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                                errFlag = false;
                                thisSys.maxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(ChildCoolingCoilType, ChildCoolingCoilName, errFlag);
                                if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get Outdoor condenser node from heat exchanger assisted DX coil object
                                errFlag = false;
                                thisSys.condenserNodeNum = DXCoils::GetCoilCondenserInletNode(
                                    "COIL:COOLING:DX:SINGLESPEED",
                                    HVACHXAssistedCoolingCoil::GetHXDXCoilName(loc_coolingCoilType, loc_coolingCoilName, errFlag),
                                    errFlag);

                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                            } else if (UtilityRoutines::SameString(ChildCoolingCoilType, "COIL:COOLING:DX:VARIABLESPEED")) {
                                thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                                errFlag = false;
                                thisSys.maxCoolAirVolFlow =
                                    VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed(ChildCoolingCoilType, ChildCoolingCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }
                                thisSys.condenserNodeNum = VariableSpeedCoils::GetVSCoilCondenserInletNode(ChildCoolingCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }
                            }

                            // Get DX cooling coil capacity
                            errFlag = false;
                            thisSys.designCoolingCapacity =
                                HVACHXAssistedCoolingCoil::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.designCoolingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = HVACHXAssistedCoolingCoil::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            CoolingCoilOutletNode = HVACHXAssistedCoolingCoil::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Push heating coil PLF curve index to DX coil
                            if (HeatingCoilPLFCurveIndex > 0) {
                                // get the actual index to the DX cooling coil object
                                int DXCoilIndex =
                                    HVACHXAssistedCoolingCoil::GetActualDXCoilIndex(loc_coolingCoilType, loc_coolingCoilName, errorsFound);
                                thisSys.actualDXCoilIndexForHXAssisted = DXCoilIndex;
                                int ActualCoolCoilType =
                                    HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(loc_coolingCoilType, loc_coolingCoilName, errFlag, true);
                                if (ActualCoolCoilType == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                                    DXCoils::SetDXCoolingCoilData(DXCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                                }
                                // what could we do for VS coil here? odd thing here
                            }

                            if (thisSys.heatCoilExists) {
                                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                    thisSys.heatPump = true;
                                }
                            }

                        } // IF (IsNotOK) THEN
                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from heat exchanger assisted cooling coil

                            errFlag = false;
                            int ActualCoolCoilType =
                                HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(loc_coolingCoilType, loc_coolingCoilName, errFlag, true);
                            std::string HXCoilName = HVACHXAssistedCoolingCoil::GetHXDXCoilName(loc_coolingCoilType, loc_coolingCoilName, errFlag);

                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get DX heat exchanger assisted cooling coil index
                            errFlag = false;
                            HVACHXAssistedCoolingCoil::GetHXDXCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr =
                                WaterCoils::GetWaterCoilAvailScheduleIndex(DataHVACGlobals::cAllCoilTypes(ActualCoolCoilType), HXCoilName, errFlag);
                            thisSys.maxCoolCoilFluidFlow =
                                WaterCoils::GetCoilMaxWaterFlowRate(DataHVACGlobals::cAllCoilTypes(ActualCoolCoilType), HXCoilName, errFlag);
                            // Get the Cooling Coil water Inlet Node number
                            thisSys.coolCoilFluidInletNode =
                                WaterCoils::GetCoilWaterInletNode(DataHVACGlobals::cAllCoilTypes(ActualCoolCoilType), HXCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = HVACHXAssistedCoolingCoil::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            CoolingCoilOutletNode = HVACHXAssistedCoolingCoil::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            thisSys.maxCoolAirVolFlow =
                                HVACHXAssistedCoolingCoil::GetHXCoilAirFlowRate(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            thisSys.condenserNodeNum = 0;

                            // Push heating coil PLF curve index to DX coil
                            if (HeatingCoilPLFCurveIndex > 0) {
                                // get the actual index to the DX cooling coil object
                                int DXCoilIndex =
                                    HVACHXAssistedCoolingCoil::GetActualDXCoilIndex(loc_coolingCoilType, loc_coolingCoilName, errorsFound);
                                thisSys.actualDXCoilIndexForHXAssisted = DXCoilIndex;
                                int ActualCoolCoilType =
                                    HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(loc_coolingCoilType, loc_coolingCoilName, errFlag, true);
                                if (ActualCoolCoilType == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                                    DXCoils::SetDXCoolingCoilData(DXCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                                }
                                // VS coil issue here
                            }

                        } // IF (IsNotOK) THEN
                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed ||
                               thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {
                            errFlag = false;
                            thisSys.coolingCoilIndex =
                                VariableSpeedCoils::GetCoilIndexVariableSpeed(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            CoolingCoilInletNode =
                                VariableSpeedCoils::GetCoilInletNodeVariableSpeed(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            CoolingCoilOutletNode =
                                VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.condenserNodeNum = VariableSpeedCoils::GetVSCoilCondenserInletNode(loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;

                            thisSys.numOfSpeedCooling = VariableSpeedCoils::GetVSCoilNumOfSpeeds(loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            errFlag = false;
                            thisSys.designCoolingCapacity =
                                VariableSpeedCoils::GetCoilCapacityVariableSpeed(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.designCoolingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            thisSys.maxCoolAirVolFlow =
                                VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Set fan info
                            if (thisSys.fanExists) {
                                VariableSpeedCoils::setVarSpeedFanInfo(thisSys.coolingCoilIndex, loc_fanName, thisSys.fanIndex, thisSys.fanType_Num);
                            }
                        }

                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        if (thisSys.heatCoilExists) {
                            if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.heatPump = true;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                        errFlag = false;
                        DXCoils::GetDXCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, errFlag, loc_coolingCoilType);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.coolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_coolingCoilType, loc_coolingCoilName, errFlag);

                        errFlag = false;
                        CoolingCoilInletNode = DXCoils::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        errFlag = false;
                        CoolingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        errFlag = false;
                        thisSys.designCoolingCapacity = DXCoils::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                        if (thisSys.designCoolingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                        errFlag = false;
                        thisSys.maxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                        if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        if (thisSys.heatCoilExists) {
                            if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.heatPump = true;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                               thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {

                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr =
                                WaterCoils::GetWaterCoilAvailScheduleIndex(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.coolingCoilIndex = WaterCoils::GetWaterCoilIndex(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.coolingCoilIndex == 0) {
                                // ShowSevereError(cCurrentModuleObject + " illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " +
                                // HeatingCoilName);
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // call for air flow rate not valid for other water coil types
                            if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater) {
                                thisSys.maxCoolAirVolFlow = WaterCoils::GetWaterCoilDesAirFlow(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                    errFlag = false;
                                }
                            }

                            // Get the Cooling Coil water Inlet Node number
                            thisSys.coolCoilFluidInletNode = WaterCoils::GetCoilWaterInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
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
                                                  loc_coolingCoilType + " = \"" + loc_coolingCoilName + ".\"");
                                ShowContinueError("...water coil controllers are not used with " + thisSys.unitType);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil chilled water max volume flow rate
                            errFlag = false;
                            thisSys.maxCoolCoilFluidFlow = WaterCoils::GetCoilMaxWaterFlowRate(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolCoilFluidFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Inlet Node
                            CoolingCoilInletNode = WaterCoils::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            CoolingCoilOutletNode = WaterCoils::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }
                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.coolingCoilIndex = WaterToAirHeatPumpSimple::GetCoilIndex(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.coolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_coolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.designCoolingCapacity =
                                WaterToAirHeatPumpSimple::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                            errFlag = false;
                            thisSys.maxCoolAirVolFlow =
                                WaterToAirHeatPumpSimple::GetCoilAirFlowRate(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            CoolingCoilInletNode = WaterToAirHeatPumpSimple::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            CoolingCoilOutletNode = WaterToAirHeatPumpSimple::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                        if (thisSys.heatCoilExists) {
                            if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.heatPump = true;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.coolingCoilIndex = WaterToAirHeatPumpSimple::GetCoilIndex(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.coolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_coolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.designCoolingCapacity =
                                WaterToAirHeatPumpSimple::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            CoolingCoilInletNode = WaterToAirHeatPumpSimple::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            CoolingCoilOutletNode = WaterToAirHeatPumpSimple::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                        if (thisSys.heatCoilExists) {
                            if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.heatPump = true;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            UserDefinedComponents::GetUserDefinedCoilIndex(
                                loc_coolingCoilName, thisSys.coolingCoilIndex, errFlag, cCurrentModuleObject);
                            if (thisSys.coolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_coolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // **** How to get this info ****
                            //						UnitarySystem( UnitarySysNum ).DesignCoolingCapacity =
                            // GetWtoAHPCoilCapacity(
                            // CoolingCoilType, loc_coolingCoilName, errFlag ); 						if ( errFlag ) {
                            //							ShowContinueError( "Occurs in " + CurrentModuleObject + " = "
                            //+
                            // UnitarySystem(  UnitarySysNum ).Name ); 							ErrorsFound = true;
                            //							errFlag = false;
                            //						}

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            UserDefinedComponents::GetUserDefinedCoilAirInletNode(
                                loc_coolingCoilName, CoolingCoilInletNode, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            UserDefinedComponents::GetUserDefinedCoilAirOutletNode(
                                loc_coolingCoilName, CoolingCoilOutletNode, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            PackagedThermalStorageCoil::GetTESCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, errFlag, cCurrentModuleObject);
                            if (thisSys.coolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_coolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            PackagedThermalStorageCoil::GetTESCoilCoolingAirFlowRate(
                                loc_coolingCoilName, thisSys.maxCoolAirVolFlow, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            PackagedThermalStorageCoil::GetTESCoilCoolingCapacity(
                                loc_coolingCoilName, thisSys.designCoolingCapacity, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            PackagedThermalStorageCoil::GetTESCoilAirInletNode(
                                loc_coolingCoilName, CoolingCoilInletNode, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            PackagedThermalStorageCoil::GetTESCoilAirOutletNode(
                                loc_coolingCoilName, CoolingCoilOutletNode, errFlag, cCurrentModuleObject);
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

                    if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                        thisSys.multiSpeedCoolingCoil = true;
                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit ||
                               thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                        thisSys.varSpeedCoolingCoil = true;
                    }

                    if (SetPointManager::NodeHasSPMCtrlVarType(thisSys.airOutNode, SetPointManager::iCtrlVarType_Temp))
                        thisSys.systemCoolControlNodeNum = thisSys.airOutNode;
                    if (SetPointManager::NodeHasSPMCtrlVarType(CoolingCoilOutletNode, SetPointManager::iCtrlVarType_Temp))
                        thisSys.systemCoolControlNodeNum = CoolingCoilOutletNode;

                    thisSys.coolCoilInletNodeNum = CoolingCoilInletNode;
                    thisSys.coolCoilOutletNodeNum = CoolingCoilOutletNode;
                    thisSys.coolingCoilName = loc_coolingCoilName;

                } else {
                    thisSys.validASHRAECoolCoil = false;
                }

                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple &&
                    thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {
                    thisSys.waterCyclingMode = DataHVACGlobals::WaterCycling;
                    WaterToAirHeatPumpSimple::SetSimpleWSHPData(
                        thisSys.coolingCoilIndex, errorsFound, thisSys.waterCyclingMode, _, thisSys.heatingCoilIndex);
                }

                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit &&
                    thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) {
                    VariableSpeedCoils::SetVarSpeedCoilData(thisSys.coolingCoilIndex, errorsFound, _, thisSys.heatingCoilIndex);
                }

                // Add cooling coil to component sets array
                if (thisSys.coolCoilExists) {
                    if (thisSys.coolingCoilType_Num != DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                        BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                             thisObjectName,
                                                             loc_coolingCoilType,
                                                             loc_coolingCoilName,
                                                             DataLoopNode::NodeID(CoolingCoilInletNode),
                                                             DataLoopNode::NodeID(CoolingCoilOutletNode));
                    } else {
                        BranchNodeConnections::SetUpCompSets(
                            cCurrentModuleObject, thisObjectName, loc_coolingCoilType, loc_coolingCoilName, "UNDEFINED", "UNDEFINED");
                    }
                }
                // Run as 100% DOAS DX coil
                if (!UtilityRoutines::SameString(loc_ISHundredPercentDOASDXCoil, "Yes")) {
                    thisSys.ISHundredPercentDOASDXCoil = false;
                } else {
                    if (UtilityRoutines::SameString(loc_ISHundredPercentDOASDXCoil, "Yes")) {
                        thisSys.ISHundredPercentDOASDXCoil = true;
                        if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                            ShowWarningError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Invalid entry for " + cAlphaFields(iDOASDXCoilAlphaNum) + " :" + Alphas(iDOASDXCoilAlphaNum));
                            ShowContinueError("Variable DX Cooling Coil is not supported as 100% DOAS DX coil.");
                            ShowContinueError("Variable DX Cooling Coil is reset as a regular DX coil and the simulation continues.");
                            thisSys.ISHundredPercentDOASDXCoil = false;
                        }
                    } else if (UtilityRoutines::SameString(loc_ISHundredPercentDOASDXCoil, "")) {
                        thisSys.ISHundredPercentDOASDXCoil = false;
                    } else if (UtilityRoutines::SameString(loc_ISHundredPercentDOASDXCoil, "No")) {
                        thisSys.ISHundredPercentDOASDXCoil = false;
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iDOASDXCoilAlphaNum) + " :" + Alphas(iDOASDXCoilAlphaNum));
                        ShowContinueError("Must be Yes or No.");
                        errorsFound = true;
                    }
                }

                // considered as as 100% DOAS DX cooling coil
                if (thisSys.ISHundredPercentDOASDXCoil) {
                    // set the system DX Coil application type to the child DX coil
                    if (!(thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed ||
                          thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {
                        DXCoils::SetDXCoilTypeData(thisSys.coolingCoilName);
                    }
                }
                // DOAS DX Cooling Coil Leaving Minimum Air Temperature
                // if (NumNumbers > 0) {
                // if (!lNumericBlanks(iDOASDXMinTempNumericNum)) {
                thisSys.designMinOutletTemp = loc_designMinOutletTemp;
                if (thisSys.controlType != controlTypeEnum::controlTypeCCMASHRAE && thisSys.designMinOutletTemp == DataSizing::AutoSize) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Invalid entry for " + cNumericFields(iDOASDXMinTempNumericNum) + " =DataSizing::AutoSize.");
                    // ShowContinueError("AutoSizing not allowed when " + cAlphaFields(iControlTypeAlphaNum) + " = " +
                    //                  Alphas(iControlTypeAlphaNum));
                    errorsFound = true;
                }
                if (thisSys.controlType != controlTypeEnum::controlTypeCCMASHRAE && thisSys.designMinOutletTemp > 7.5) {
                    ShowWarningError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Invalid entry for " + cNumericFields(iDOASDXMinTempNumericNum) + " = " +
                    //                  TrimSigDigits(Numbers(iDOASDXMinTempNumericNum), 3));
                    ShowContinueError("The minimum supply air temperature will be limited to 7.5C and the simulation continues.");
                    thisSys.designMinOutletTemp = 7.5;
                }
                //}
                //}

                // Get Latent Load Control flag
                if (loc_latentControlFlag != "") {
                    if (UtilityRoutines::SameString(loc_latentControlFlag, "SensibleOnlyLoadControl")) {
                        thisSys.runOnSensibleLoad = true;
                        thisSys.runOnLatentLoad = false;
                    } else if (UtilityRoutines::SameString(loc_latentControlFlag, "LatentOnlyLoadControl")) {
                        thisSys.runOnSensibleLoad = false;
                        thisSys.runOnLatentLoad = true;
                    } else if (UtilityRoutines::SameString(loc_latentControlFlag, "LatentOrSensibleLoadControl")) {
                        thisSys.runOnSensibleLoad = true;
                        thisSys.runOnLatentLoad = true;
                    } else if (UtilityRoutines::SameString(loc_latentControlFlag, "LatentWithSensibleLoadControl")) {
                        thisSys.runOnSensibleLoad = true;
                        thisSys.runOnLatentLoad = true;
                        thisSys.runOnLatentOnlyWithSensible = true;
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iRunOnLatentLoadAlphaNum) + " :" +
                        // Alphas(iRunOnLatentLoadAlphaNum));
                        ShowContinueError("Must be SensibleOnlyLoadControl, LatentOnlyLoadControl, LatentOrSensibleLoadControl, or "
                                          "LatentWithSensibleLoadControl.");
                    }
                }

                // Get reheat coil data if humidistat is used
                thisSys.suppHeatCoilName = loc_suppHeatCoilName;
                thisSys.suppHeatCoilTypeName = loc_suppHeatCoilType;
                errFlag = false;

                if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Water")) {
                    thisSys.suppHeatCoilType_Num = DataHVACGlobals::Coil_HeatingWater;
                } else if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Steam")) {
                    thisSys.suppHeatCoilType_Num = DataHVACGlobals::Coil_HeatingSteam;
                } else if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Fuel") ||
                           UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Electric") ||
                           UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:DesuperHeater")) {
                    thisSys.suppHeatCoilType_Num = HeatingCoils::GetHeatingCoilTypeNum(loc_suppHeatCoilType, loc_suppHeatCoilName, errFlag);
                } else if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:UserDefined")) {
                    thisSys.suppHeatCoilType_Num = DataHVACGlobals::Coil_UserDefined;
                }

                if (loc_suppHeatCoilType != "" && loc_suppHeatCoilName != "") {
                    thisSys.suppCoilExists = true;

                    if (thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingGasOrOtherFuel ||
                        thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingElectric ||
                        thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingDesuperheater) {

                        thisSys.suppHeatCoilType_Num = HeatingCoils::GetHeatingCoilTypeNum(loc_suppHeatCoilType, loc_suppHeatCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {

                            ValidateComponent(loc_suppHeatCoilType, loc_suppHeatCoilName, isNotOK, cCurrentModuleObject);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;

                            } else { // mine data from reheat coil

                                // Get the heating coil index
                                thisSys.suppHeatCoilIndex = HeatingCoils::GetHeatingCoilIndex(loc_suppHeatCoilType, loc_suppHeatCoilName, isNotOK);
                                if (isNotOK) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the design supplemental heating capacity
                                errFlag = false;
                                thisSys.designSuppHeatingCapacity =
                                    HeatingCoils::GetCoilCapacity(loc_suppHeatCoilType, loc_suppHeatCoilName, errFlag);
                                if (thisSys.designSuppHeatingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Reheat Coil Inlet Node
                                errFlag = false;
                                SupHeatCoilInletNode = HeatingCoils::GetCoilInletNode(loc_suppHeatCoilType, loc_suppHeatCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Reheat Coil Outlet Node
                                errFlag = false;
                                SupHeatCoilOutletNode = HeatingCoils::GetCoilOutletNode(loc_suppHeatCoilType, loc_suppHeatCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                            } // IF (IsNotOK) THEN
                        }

                        thisSys.suppCoilAirInletNode = SupHeatCoilInletNode;
                        thisSys.suppCoilAirOutletNode = SupHeatCoilOutletNode;

                    } else if (thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {

                        ValidateComponent(loc_suppHeatCoilType, loc_suppHeatCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from heating coil object

                            // Get the Heating Coil water Inlet or control Node number
                            errFlag = false;
                            thisSys.suppCoilFluidInletNode = WaterCoils::GetCoilWaterInletNode("Coil:Heating:Water", loc_suppHeatCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the ReHeat Coil hot water max volume flow rate
                            errFlag = false;
                            thisSys.maxSuppCoilFluidFlow = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", loc_suppHeatCoilName, errFlag);
                            if (thisSys.maxSuppCoilFluidFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the ReHeat Coil Inlet Node
                            errFlag = false;
                            SupHeatCoilInletNode = WaterCoils::GetCoilInletNode("Coil:Heating:Water", loc_suppHeatCoilName, errFlag);
                            thisSys.suppCoilAirInletNode = SupHeatCoilInletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the ReHeat Coil Outlet Node
                            errFlag = false;
                            SupHeatCoilOutletNode = WaterCoils::GetCoilOutletNode("Coil:Heating:Water", loc_suppHeatCoilName, errFlag);
                            thisSys.suppCoilAirOutletNode = SupHeatCoilOutletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }
                        }

                    } else if (thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {

                        ValidateComponent(loc_suppHeatCoilType, loc_suppHeatCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from heating coil object

                            errFlag = false;
                            thisSys.suppHeatCoilIndex = SteamCoils::GetSteamCoilIndex("COIL:HEATING:STEAM", loc_suppHeatCoilName, errFlag);
                            if (thisSys.suppHeatCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowSevereError("Illegal " + cAlphaFields(iSuppHeatCoilNameAlphaNum) + " = " + SuppHeatCoilName);
                                errorsFound = true;
                            }

                            // Get the Heating Coil steam inlet node number
                            errFlag = false;
                            thisSys.suppCoilFluidInletNode = SteamCoils::GetCoilSteamInletNode("Coil:Heating:Steam", loc_suppHeatCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }
                            // Get the Heating Coil steam max volume flow rate
                            thisSys.maxSuppCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(thisSys.suppHeatCoilIndex, errFlag);
                            if (thisSys.maxSuppCoilFluidFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                            if (thisSys.maxSuppCoilFluidFlow > 0.0) {
                                int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                                Real64 TempSteamIn = 100.0;
                                Real64 SteamDensity =
                                    FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getUnitarySystemInput);
                                thisSys.maxSuppCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(thisSys.suppHeatCoilIndex, errFlag) * SteamDensity;
                            }

                            // Get the Heating Coil Inlet Node
                            errFlag = false;
                            SupHeatCoilInletNode = SteamCoils::GetCoilAirInletNode(thisSys.suppHeatCoilIndex, loc_suppHeatCoilName, errFlag);
                            thisSys.suppCoilAirInletNode = SupHeatCoilInletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Heating Coil Outlet Node
                            errFlag = false;
                            SupHeatCoilOutletNode = SteamCoils::GetCoilAirOutletNode(thisSys.suppHeatCoilIndex, loc_suppHeatCoilName, errFlag);
                            thisSys.suppCoilAirOutletNode = SupHeatCoilOutletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }
                        }

                    } else if (thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                        ValidateComponent(loc_suppHeatCoilType, loc_suppHeatCoilName, isNotOK, cCurrentModuleObject);
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
                                loc_suppHeatCoilName, thisSys.suppHeatCoilIndex, errFlag, cCurrentModuleObject);
                            if (thisSys.suppHeatCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iSuppHeatCoilNameAlphaNum) + " = " + SuppHeatCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the supplemental heating Coil Inlet Node
                            errFlag = false;
                            UserDefinedComponents::GetUserDefinedCoilAirInletNode(
                                loc_suppHeatCoilName, SupHeatCoilInletNode, errFlag, cCurrentModuleObject);
                            thisSys.suppCoilAirInletNode = SupHeatCoilInletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the supplemenatal heating Coil Outlet Node
                            UserDefinedComponents::GetUserDefinedCoilAirOutletNode(
                                loc_suppHeatCoilName, SupHeatCoilOutletNode, errFlag, cCurrentModuleObject);
                            thisSys.suppCoilAirOutletNode = SupHeatCoilOutletNode;
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

                if (SetPointManager::NodeHasSPMCtrlVarType(thisSys.airOutNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.suppHeatControlNodeNum = thisSys.airOutNode;
                if (SetPointManager::NodeHasSPMCtrlVarType(SupHeatCoilOutletNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.suppHeatControlNodeNum = SupHeatCoilOutletNode;

                // Add supplemental heating coil to component sets array
                if (thisSys.suppCoilExists)
                    BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                         thisObjectName,
                                                         loc_suppHeatCoilType,
                                                         loc_suppHeatCoilName,
                                                         DataLoopNode::NodeID(SupHeatCoilInletNode),
                                                         DataLoopNode::NodeID(SupHeatCoilOutletNode));

                // set fan info for heating coils
                if (thisSys.suppCoilExists && thisSys.fanExists) {
                    if (thisSys.fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.suppHeatCoilName,
                                                                     thisSys.suppHeatCoilTypeName,
                                                                     thisSys.fanName,
                                                                     DataAirSystems::objectVectorOOFanSystemModel,
                                                                     thisSys.fanIndex);
                    } else {
                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.suppHeatCoilName,
                                                                     thisSys.suppHeatCoilTypeName,
                                                                     thisSys.fanName,
                                                                     DataAirSystems::structArrayLegacyFanModels,
                                                                     thisSys.fanIndex);
                    }
                }

                // Users may not provide SA flow input fields (below) and leave them blank. Check if other coil isDataSizing::AutoSized first to
                // alieviate input requirements. check if coil has no air flow input (VolFlow = 0) and other coil isDataSizing::AutoSized. If so,
                // useDataSizing::AutoSize for coil with 0 air flow rate. This means that the coils MUST mine the air flow rate if it exists
                if (thisSys.coolCoilExists && thisSys.heatCoilExists) {
                    if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize && thisSys.maxHeatAirVolFlow == 0 && loc_heatingSAFMethod == "") {
                        thisSys.maxHeatAirVolFlow = DataSizing::AutoSize;
                    } else if (thisSys.maxCoolAirVolFlow == 0 && thisSys.maxHeatAirVolFlow == DataSizing::AutoSize && loc_coolingSAFMethod == "") {
                        thisSys.maxCoolAirVolFlow = DataSizing::AutoSize;
                    }
                }

                // Determine supply air flow rate sizing method for cooling mode
                if (UtilityRoutines::SameString(loc_coolingSAFMethod, "SupplyAirFlowRate")) {
                    thisSys.coolingSAFMethod = SupplyAirFlowRate;

                    if (loc_coolingSAFMethod_SAFlow != -999.0) {
                        thisSys.maxCoolAirVolFlow = loc_coolingSAFMethod_SAFlow;
                        if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                        if ((thisSys.maxCoolAirVolFlow < 0.0 && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxCoolAirVolFlow == 0.0 && thisSys.coolCoilExists)) {
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
                } else if (UtilityRoutines::SameString(loc_coolingSAFMethod, "FlowPerFloorArea")) {

                    thisSys.coolingSAFMethod = FlowPerFloorArea;
                    if (loc_coolingSAFMethod_SAFlowPerFloorArea != -999.0) {
                        thisSys.maxCoolAirVolFlow = loc_coolingSAFMethod_SAFlowPerFloorArea;
                        if ((thisSys.maxCoolAirVolFlow < 0.0 && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxCoolAirVolFlow == 0.0 && thisSys.coolCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFloorAreaNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iCoolFlowPerFloorAreaNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFloorAreaNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.maxCoolAirVolFlow *= TotalFloorAreaOnAirLoop;
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iCoolFlowPerFloorAreaNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_coolingSAFMethod, "FractionOfAutosizedCoolingValue")) {

                    thisSys.coolingSAFMethod = FractionOfAutoSizedCoolingValue;
                    if (loc_coolingSAFMethod_FracOfAutosizedCoolingSAFlow != -999.0) {
                        thisSys.maxCoolAirVolFlow = loc_coolingSAFMethod_FracOfAutosizedCoolingSAFlow;
                        if ((thisSys.maxCoolAirVolFlow < 0.0 && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxCoolAirVolFlow == 0.0 && thisSys.coolCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFracCoolNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iCoolFlowPerFracCoolNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFracCoolNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iCoolFlowPerFracCoolNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_coolingSAFMethod, "FlowPerCoolingCapacity")) {

                    thisSys.coolingSAFMethod = FlowPerCoolingCapacity;
                    if (loc_coolingSAFMethod_FlowPerCoolingCapacity != -999.0) {
                        thisSys.maxCoolAirVolFlow = loc_coolingSAFMethod_FlowPerCoolingCapacity;
                        if ((thisSys.maxCoolAirVolFlow < 0.0 && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxCoolAirVolFlow == 0.0 && thisSys.coolCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerCoolCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iCoolFlowPerCoolCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerCoolCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iCoolFlowPerCoolCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_coolingSAFMethod, "None") || loc_coolingSAFMethod == "") {
                    thisSys.coolingSAFMethod = None;
                    //          thisSys%RequestAutosize = .TRUE. ! ??
                    if (thisSys.coolCoilExists && thisSys.maxCoolAirVolFlow == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " is blank and relates to " + loc_coolingCoilType +
                        // " = " +
                        //                  loc_coolingCoilName);
                        if (thisSys.heatCoilExists) {
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
                if (UtilityRoutines::SameString(loc_heatingSAFMethod, "SupplyAirFlowRate")) {
                    thisSys.heatingSAFMethod = SupplyAirFlowRate;
                    if (loc_heatingSAFMethod_SAFlow != -999.0) {
                        thisSys.maxHeatAirVolFlow = loc_heatingSAFMethod_SAFlow;
                        if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                        if ((thisSys.maxHeatAirVolFlow < 0.0 && thisSys.maxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxHeatAirVolFlow == 0.0 && thisSys.heatCoilExists)) {
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
                } else if (UtilityRoutines::SameString(loc_heatingSAFMethod, "FlowPerFloorArea")) {
                    thisSys.heatingSAFMethod = FlowPerFloorArea;
                    if (loc_heatingSAFMethod_SAFlowPerFloorArea != -999.0) {
                        thisSys.maxHeatAirVolFlow = loc_heatingSAFMethod_SAFlowPerFloorArea;
                        if ((thisSys.maxHeatAirVolFlow < 0.0 && thisSys.maxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxHeatAirVolFlow == 0.0 && thisSys.heatCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFloorAreaNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iHeatFlowPerFloorAreaNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFloorAreaNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.maxHeatAirVolFlow *= TotalFloorAreaOnAirLoop;
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatFlowPerFloorAreaNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_heatingSAFMethod, "FractionOfAutosizedHeatingValue")) {
                    thisSys.heatingSAFMethod = FractionOfAutoSizedHeatingValue;
                    if (loc_heatingSAFMethod_FracOfAutosizedHeatingSAFlow != -999.0) {
                        thisSys.maxHeatAirVolFlow = loc_heatingSAFMethod_FracOfAutosizedHeatingSAFlow;
                        if ((thisSys.maxHeatAirVolFlow < 0.0 && thisSys.maxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxHeatAirVolFlow == 0.0 && thisSys.heatCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFracCoolNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iHeatFlowPerFracCoolNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFracCoolNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatFlowPerFracCoolNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_heatingSAFMethod, "FlowPerHeatingCapacity")) {
                    thisSys.heatingSAFMethod = FlowPerHeatingCapacity;
                    if (loc_heatingSAFMethod_FlowPerHeatingCapacity != -999.0) {
                        thisSys.maxHeatAirVolFlow = loc_heatingSAFMethod_FlowPerHeatingCapacity;
                        if ((thisSys.maxHeatAirVolFlow < 0.0 && thisSys.maxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxHeatAirVolFlow == 0.0 && thisSys.heatCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerHeatCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iHeatFlowPerHeatCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerHeatCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatFlowPerHeatCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_heatingSAFMethod, "None") || loc_heatingSAFMethod == "") {
                    thisSys.heatingSAFMethod = None;
                    //          thisSys%RequestAutosize = .TRUE. ! ??
                    if (thisSys.heatCoilExists && thisSys.maxHeatAirVolFlow == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " is blank and relates to " + loc_heatingCoilType +
                        // " = " +
                        //                  loc_heatingCoilName);
                        if (thisSys.coolCoilExists) {
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
                if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "SupplyAirFlowRate")) {
                    thisSys.noCoolHeatSAFMethod = SupplyAirFlowRate;
                    if (loc_noCoolHeatSAFMethod_SAFlow != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_SAFlow;
                        if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
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
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "FlowPerFloorArea")) {
                    thisSys.noCoolHeatSAFMethod = FlowPerFloorArea;
                    if (loc_noCoolHeatSAFMethod_SAFlowPerFloorArea != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_SAFlowPerFloorArea;
                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerFloorAreaNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.maxNoCoolHeatAirVolFlow *= TotalFloorAreaOnAirLoop;
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "FractionOfAutosizedCoolingValue")) {
                    thisSys.noCoolHeatSAFMethod = FractionOfAutoSizedCoolingValue;
                    if (loc_noCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow;
                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerFracCoolNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "FractionOfAutosizedHeatingValue")) {
                    thisSys.noCoolHeatSAFMethod = FractionOfAutoSizedHeatingValue;
                    if (loc_noCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow;
                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerFracHeatNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "FlowPerCoolingCapacity")) {
                    thisSys.noCoolHeatSAFMethod = FlowPerCoolingCapacity;
                    if (loc_noCoolHeatSAFMethod_FlowPerCoolingCapacity != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_FlowPerCoolingCapacity;
                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerCoolCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "FlowPerHeatingCapacity")) {
                    thisSys.noCoolHeatSAFMethod = FlowPerHeatingCapacity;
                    if (loc_noCoolHeatSAFMethod_FlowPerHeatingCapacity != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_FlowPerHeatingCapacity;
                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerHeatCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "None") || loc_noCoolHeatSAFMethod == "") {
                    thisSys.noCoolHeatSAFMethod = None;
                    //          thisSys%RequestAutosize = .TRUE. ! ??
                } else {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                    ShowContinueError("Valid entries are: SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingValue, "
                                      "FractionOfAutosizedHeatingValue, FlowPerCoolingCapacity, FlowPerHeatingCapacity, or None ");
                    errorsFound = true;
                }

                //       Fan operating mode (cycling or constant) schedule. IF constant fan, then set AirFlowControl
                if (thisSys.fanOpModeSchedPtr > 0) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.fanOpModeSchedPtr, ">=", 0.0, "<=", 0.0)) {
                        //           set fan operating mode to continuous so sizing can set VS coil data
                        thisSys.fanOpMode = DataHVACGlobals::ContFanCycCoil;
                        //           set air flow control mode:
                        //             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
                        //             UseCompressorOffFlow = operate at value specified by user
                        //           AirFlowControl only valid if fan opmode = ContFanCycComp
                        if (thisSys.maxNoCoolHeatAirVolFlow == 0.0) {
                            thisSys.airFlowControl = useCompFlow::useCompressorOnFlow;
                        } else {
                            thisSys.airFlowControl = useCompFlow::useCompressorOffFlow;
                        }
                    }
                }

                if (thisSys.coolingCoilType_Num != DataHVACGlobals::CoilDX_CoolingHXAssisted &&
                    thisSys.coolingCoilType_Num != DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl &&
                    thisSys.dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_Multimode) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iDehumidControlAlphaNum) + " = " + Alphas(iDehumidControlAlphaNum));
                    ShowContinueError("Multimode control must be used with a Heat Exchanger Assisted or Multimode Cooling Coil.");
                    if (loc_suppHeatCoilName == "" && loc_suppHeatCoilType == "") {
                    } else {
                        if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                            ShowContinueError("Dehumidification control type is assumed to be None and the simulation continues.");
                            thisSys.dehumidControlType_Num = dehumCtrlTypeEnum::dehumidControl_None;
                        } else {
                            ShowContinueError("Dehumidification control type is assumed to be CoolReheat and the simulation continues.");
                            thisSys.dehumidControlType_Num = dehumCtrlTypeEnum::dehumidControl_CoolReheat;
                        }
                    }
                }

                //       Check placement of cooling coil with respect to fan placement and dehumidification control type

                if (thisSys.fanExists) {
                    if (thisSys.fanPlace == fanPlaceEnum::blowThru) {
                        if (FanOutletNode == HeatingCoilInletNode && thisSys.dehumidControlType_Num != dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
                            thisSys.coolingCoilUpstream = false;
                        }
                    } else if (thisSys.fanPlace == fanPlaceEnum::drawThru) {
                        if (HeatingCoilOutletNode == CoolingCoilInletNode &&
                            thisSys.dehumidControlType_Num != dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
                            thisSys.coolingCoilUpstream = false;
                        }
                    }
                } else {
                    if (HeatingCoilOutletNode == CoolingCoilInletNode &&
                        thisSys.dehumidControlType_Num != dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
                        thisSys.coolingCoilUpstream = false;
                    }
                    if (ZoneEquipmentFound) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("ZoneHVAC equipment must contain a fan object.");
                        // ShowContinueError("specified " + cAlphaFields(iFanTypeAlphaNum) + " = " + Alphas(iFanTypeAlphaNum));
                        // ShowContinueError("specified " + cAlphaFields(iFanNameAlphaNum) + " = " + Alphas(iFanNameAlphaNum));
                        errorsFound = true;
                    }
                }

                // check node connections
                if (thisSys.fanPlace == fanPlaceEnum::blowThru) {

                    if (FanInletNode != thisSys.airInNode) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("When a blow through fan is specified, the fan inlet node name must be the same as the unitary system "
                                          "inlet node name.");
                        ShowContinueError("...Fan inlet node name           = " + DataLoopNode::NodeID(FanInletNode));
                        ShowContinueError("...UnitarySystem inlet node name = " + DataLoopNode::NodeID(thisSys.airInNode));
                        errorsFound = true;
                    }
                    if (thisSys.coolingCoilUpstream) {
                        if (FanOutletNode != CoolingCoilInletNode && thisSys.coolCoilExists && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a blow through fan is specified, the fan outlet node name must be the same as the cooling coil "
                                              "inlet node name.");
                            ShowContinueError("...Fan outlet node name         = " + DataLoopNode::NodeID(FanOutletNode));
                            ShowContinueError("...Cooling coil inlet node name = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != HeatingCoilInletNode && thisSys.coolCoilExists && thisSys.heatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                            ShowContinueError("...Cooling coil outlet node name = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...Heating coil inlet node name  = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            errorsFound = true;
                        }
                        if (thisSys.suppCoilExists) {
                            if (SupHeatCoilOutletNode != thisSys.airOutNode) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError("The reheat coil outlet node name must be the same as the unitary system outlet node name.");
                                ShowContinueError("...Reheat coil outlet node name   = " + DataLoopNode::NodeID(SupHeatCoilOutletNode));
                                ShowContinueError("...UnitarySystem outlet node name = " + DataLoopNode::NodeID(thisSys.airOutNode));
                                //                ErrorsFound=.TRUE.
                            }
                        } else { // IF((thisSys%Humidistat ...
                            // Heating coil outlet node name must be the same as the Unitary system outlet node name
                            if (thisSys.heatCoilExists && HeatingCoilOutletNode != thisSys.airOutNode) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError("When a blow through fan is specified, the heating coil outlet node name must be the same as the "
                                                  "unitary system outlet node name.");
                                ShowContinueError("...Heating coil outlet node name  = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                                ShowContinueError("...Unitary system outlet node name = " + DataLoopNode::NodeID(thisSys.airOutNode));
                                errorsFound = true;
                            }
                        }
                    } else { // IF(thisSys%CoolingCoilUpstream)THEN
                        if (FanOutletNode != HeatingCoilInletNode && thisSys.fanExists && thisSys.heatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a blow through fan is specified, the fan outlet node name must be the same as the heating coil "
                                              "inlet node name.");
                            ShowContinueError("...Fan outlet node name         = " + DataLoopNode::NodeID(FanOutletNode));
                            ShowContinueError("...Heating coil inlet node name = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            errorsFound = true;
                        }
                        if (HeatingCoilOutletNode != CoolingCoilInletNode && thisSys.coolCoilExists && thisSys.heatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The heating coil outlet node name must be the same as the cooling coil inlet node name.");
                            ShowContinueError("...Heating coil outlet node name = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                            ShowContinueError("...Cooling coil inlet node name  = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != thisSys.airOutNode && thisSys.coolCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(
                                "When a blow through fan is specified, the cooling coil outlet node name must be the same as the unitary "
                                "system outlet node name.");
                            ShowContinueError("...Cooling coil outlet node name   = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...UnitarySystem outlet node name  = " + DataLoopNode::NodeID(thisSys.airOutNode));
                            errorsFound = true;
                        }
                    }

                } else { // ELSE from IF(thisSys%FanPlace .EQ. BlowThru)THEN

                    if (thisSys.coolingCoilUpstream) {
                        if (CoolingCoilInletNode != thisSys.airInNode && CoolingCoilInletNode != 0 && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(
                                "When a draw through fan is specified, the cooling coil inlet node name must be the same as the unitary "
                                "system inlet node name.");
                            ShowContinueError("...Cooling coil inlet node name  = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            ShowContinueError("...UnitarySystem inlet node name = " + DataLoopNode::NodeID(thisSys.airInNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != HeatingCoilInletNode && thisSys.coolCoilExists && thisSys.heatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                            ShowContinueError("...Cooling coil outlet node name = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...Heating coil inlet node name  = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            errorsFound = true;
                        }
                        if (HeatingCoilOutletNode != FanInletNode && thisSys.heatCoilExists && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a draw through fan is specified, the heating coil outlet node name must be the same as the fan "
                                              "inlet node name.");
                            ShowContinueError("...Heating coil outlet node name = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                            ShowContinueError("...Fan inlet node name           = " + DataLoopNode::NodeID(FanInletNode));
                            errorsFound = true;
                        }
                        if (thisSys.suppCoilExists) {
                            if (FanOutletNode != SupHeatCoilInletNode && thisSys.fanExists) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError(
                                    "When a draw through fan is specified, the fan outlet node name must be the same as the reheat coil "
                                    "inlet node name.");
                                ShowContinueError("...Fan outlet node name        = " + DataLoopNode::NodeID(FanOutletNode));
                                ShowContinueError("...Reheat coil inlet node name = " + DataLoopNode::NodeID(SupHeatCoilInletNode));
                                //                ErrorsFound=.TRUE.
                            }
                            if (SupHeatCoilOutletNode != thisSys.airOutNode) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError("The reheat coil outlet node name must be the same as the unitary system outlet node name.");
                                ShowContinueError("...Reheat coil outlet node name   = " + DataLoopNode::NodeID(SupHeatCoilOutletNode));
                                ShowContinueError("...UnitarySystem outlet node name = " + DataLoopNode::NodeID(thisSys.airOutNode));
                            }
                        } else {
                            if (FanOutletNode != thisSys.airOutNode && thisSys.fanExists) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError(
                                    "When a draw through fan is specified, the fan outlet node name must be the same as the unitary system "
                                    "outlet node name.");
                                ShowContinueError("...Fan outlet node name        = " + DataLoopNode::NodeID(FanOutletNode));
                                ShowContinueError("...Unitary system outlet node name = " + DataLoopNode::NodeID(thisSys.airOutNode));
                                errorsFound = true;
                            }
                        }
                    } else { // IF(thisSys%CoolingCoilUpstream)THEN
                        if (HeatingCoilInletNode != thisSys.airInNode && HeatingCoilInletNode != 0 && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(
                                "When a draw through fan is specified, the heating coil inlet node name must be the same as the unitary "
                                "system inlet node name.");
                            ShowContinueError("...Heating coil inlet node name  = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            ShowContinueError("...UnitarySystem inlet node name = " + DataLoopNode::NodeID(thisSys.airInNode));
                            errorsFound = true;
                        }
                        if (HeatingCoilOutletNode != CoolingCoilInletNode && thisSys.heatCoilExists && thisSys.coolCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The heating coil outlet node name must be the same as the cooling coil inlet node name.");
                            ShowContinueError("...Heating coil outlet node name = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                            ShowContinueError("...Cooling coil inlet node name  = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != FanInletNode && thisSys.coolCoilExists && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a draw through fan is specified, the cooling coil outlet node name must be the same as the fan "
                                              "inlet node name.");
                            ShowContinueError("...Cooling coil outlet node name = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...Fan inlet node name           = " + DataLoopNode::NodeID(FanInletNode));
                            errorsFound = true;
                        }
                        if (FanOutletNode != thisSys.airOutNode && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a draw through fan is specified, the fan outlet node name must be the same as the unitary system "
                                              "outlet node name.");
                            ShowContinueError("...Fan outlet node name           = " + DataLoopNode::NodeID(FanOutletNode));
                            ShowContinueError("...UnitarySystem outlet node name = " + DataLoopNode::NodeID(thisSys.airOutNode));
                            errorsFound = true;
                        }
                    }
                } // ELSE from IF(thisSys%FanPlace .EQ. BlowThru)THEN

                // Set the unitary system supplemental heater max outlet temperature
                // this field will be 0 if the input is not specified (included) in the input file
                // someone may use a default other than what we intended, allow it to be used
                // so if this field is blank, and the input field is included, read the default, otherwise use 80
                // if (!lNumericBlanks(iDesignMaxOutletTempNumericNum) && NumNumbers > (iDesignMaxOutletTempNumericNum - 1)) {
                thisSys.designMaxOutletTemp = loc_designMaxOutletTemp;
                if (thisSys.designMaxOutletTemp == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                //}

                // Set maximum Outdoor air temperature for supplemental heating coil operation
                // this field will be 0 if the input is not specified (included) in the input file
                // someone may use a default other than what we intended, allow it to be used
                // so if this field is blank, and the input field is included, read the default, otherwise use 9999
                // if (!lNumericBlanks(iMaxOATSuppHeatNumericNum) && NumNumbers > (iMaxOATSuppHeatNumericNum - 1)) {
                thisSys.maxOATSuppHeat = loc_maxOATSuppHeat;
                // Can't let MaxOATSuppHeat default to 21C if using cool reheat since it would shut off supp heater when dehumidifying
                // this may also allow supplemental heater to operate when in heating mode when it should not
                if (thisSys.maxOATSuppHeat == 21.0 && thisSys.dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
                    thisSys.maxOATSuppHeat = 999.0;
                }

                if (thisSys.maxCoolAirVolFlow > 0.0 && thisSys.maxHeatAirVolFlow > 0.0 && thisSys.maxNoCoolHeatAirVolFlow >= 0.0 &&
                    !thisSys.requestAutoSize) {
                    thisSys.designFanVolFlowRate = max(thisSys.maxCoolAirVolFlow, thisSys.maxHeatAirVolFlow, thisSys.maxNoCoolHeatAirVolFlow);
                } else if (thisSys.maxCoolAirVolFlow > 0.0 && thisSys.maxNoCoolHeatAirVolFlow >= 0.0 && !thisSys.requestAutoSize) {
                    thisSys.designFanVolFlowRate = max(thisSys.maxCoolAirVolFlow, thisSys.maxNoCoolHeatAirVolFlow);
                } else if (thisSys.maxHeatAirVolFlow > 0.0 && thisSys.maxNoCoolHeatAirVolFlow >= 0.0 && !thisSys.requestAutoSize) {
                    thisSys.designFanVolFlowRate = max(thisSys.maxHeatAirVolFlow, thisSys.maxNoCoolHeatAirVolFlow);
                } else {
                    if (thisSys.fanExists && thisSys.designFanVolFlowRate == 0.0) {
                        thisSys.designFanVolFlowRate = DataSizing::AutoSize;
                    }
                    // need more of this type of warning when flow cannot be determined
                    if (thisSys.maxHeatAirVolFlow == 0.0 && thisSys.heatCoilExists) {
                        if (thisSys.fanExists) {
                            if (thisSys.coolCoilExists && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize) {
                                if (thisSys.maxCoolAirVolFlow == 0.0) {
                                    thisSys.maxHeatAirVolFlow = thisSys.designFanVolFlowRate;
                                }
                            }
                        } else if (thisSys.coolCoilExists) {
                            thisSys.maxHeatAirVolFlow = thisSys.maxCoolAirVolFlow;
                        } else {
                            if (thisSys.heatingCoilType_Num != DataHVACGlobals::CoilDX_HeatingEmpirical &&
                                thisSys.heatingCoilType_Num != DataHVACGlobals::CoilDX_MultiSpeedHeating &&
                                thisSys.heatingCoilType_Num != DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("When non-DX heating coils are specified, the heating air flow rate must be entered in " +
                                //                  cAlphaFields(iHeatSAFMAlphaNum));
                                errorsFound = true;
                            }
                        }
                    } else if (thisSys.maxHeatAirVolFlow == 0.0 && !thisSys.fanExists && !thisSys.coolCoilExists) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("When non-DX heating coils are specified, the heating air flow rate must be entered in " +
                        //                  cAlphaFields(iHeatSAFMAlphaNum));
                    }
                }

                if (FanVolFlowRate != DataSizing::AutoSize && thisSys.fanExists) {
                    if (FanVolFlowRate < thisSys.maxCoolAirVolFlow && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize && thisSys.coolCoilExists) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... air flow rate = " + General::TrimSigDigits(FanVolFlowRate, 7) + " in fan object " + thisSys.fanName +
                                          " is less than the maximum HVAC system air flow rate in cooling mode.");
                        // ShowContinueError(" The " + cNumericFields(iMaxCoolAirVolFlowNumericNum) +
                        //                  " is reset to the fan flow rate and the simulation continues.");
                        thisSys.maxCoolAirVolFlow = FanVolFlowRate;
                        thisSys.designFanVolFlowRate = FanVolFlowRate;
                    }
                    if (FanVolFlowRate < thisSys.maxHeatAirVolFlow && thisSys.maxHeatAirVolFlow != DataSizing::AutoSize && thisSys.heatCoilExists) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... air flow rate = " + General::TrimSigDigits(FanVolFlowRate, 7) + " in fan object " + thisSys.fanName +
                                          " is less than the maximum HVAC system air flow rate in heating mode.");
                        // ShowContinueError(" The " + cNumericFields(3) + " is reset to the fan flow rate and the simulation continues.");
                        thisSys.maxHeatAirVolFlow = FanVolFlowRate;
                        thisSys.designFanVolFlowRate = FanVolFlowRate;
                    }
                }

                if (thisSys.fanOpModeSchedPtr > 0) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.fanOpModeSchedPtr, ">=", 0.0, "<=", 0.0)) {
                        //           set air flow control mode:
                        //             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
                        //             UseCompressorOffFlow = operate at value specified by user
                        //           AirFlowControl only valid IF fan opmode = ContFanCycComp
                        if (thisSys.maxNoCoolHeatAirVolFlow == 0.0) {
                            thisSys.airFlowControl = useCompFlow::useCompressorOnFlow;
                        } else {
                            thisSys.airFlowControl = useCompFlow::useCompressorOffFlow;
                        }
                    }
                }

                // Set minimum OAT for heat pump compressor operation in cooling mode
                // get from coil module
                errFlag = false;
                if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                    thisSys.minOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                    thisSys.minOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                    thisSys.minOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                    thisSys.minOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    thisSys.minOATCompressorCooling = VariableSpeedCoils::GetVSCoilMinOATCompressor(loc_coolingCoilName, errFlag);
                } else {
                    thisSys.minOATCompressorCooling = -1000.0;
                }
                if (errFlag) {
                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                    errorsFound = true;
                }

                // Set minimum OAT for heat pump compressor operation in heating mode
                // get from coil module
                errFlag = false;
                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                    thisSys.minOATCompressorHeating = VariableSpeedCoils::GetVSCoilMinOATCompressor(loc_heatingCoilName, errFlag);
                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                    thisSys.minOATCompressorHeating = DXCoils::GetMinOATCompressor(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    //       ELSEIF  ***... make sure we catch all possbile coil types here ...***
                } else {
                    thisSys.minOATCompressorHeating = -1000.0;
                }
                if (errFlag) {
                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                    errorsFound = true;
                }

                //       Mine heatpump Outdoor condenser node from DX coil object
                errFlag = false;
                if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                    thisSys.condenserNodeNum = DXCoils::GetCoilCondenserInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    thisSys.condenserNodeNum = VariableSpeedCoils::GetVSCoilCondenserInletNode(loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) {
                    // already filled
                    // UnitarySystem( UnitarySysNum ).CondenserNodeNum = GetDXCoilCondenserInletNode( "Coil:Cooling:DX:SingleSpeed",
                    // GetHXDXCoilName( CoolingCoilType, loc_coolingCoilName, errFlag ), errFlag );

                } else {
                    if (loc_condenserInletNodeName != "") {
                        thisSys.condenserNodeNum = NodeInputManager::GetOnlySingleNode(loc_condenserInletNodeName,
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
                thisSys.maxONOFFCyclesperHour = loc_maxONOFFCyclesperHour;
                // if (NumNumbers < iMaxONOFFCycPerHourNumericNum) {
                //    thisSys.maxONOFFCyclesperHour = 2.5;
                //}

                // Set the heat pump time constant
                thisSys.HPTimeConstant = loc_HPTimeConstant;
                // if (NumNumbers < iHPTimeConstantNumericNum) {
                //    thisSys.HPTimeConstant = 60.0;
                //}

                // Set the heat pump on-cycle power use fraction
                thisSys.onCyclePowerFraction = loc_onCyclePowerFraction;
                // if (NumNumbers < iOnCyclePowerFracNumericNum) {
                //    thisSys.OnCyclePowerFraction = 0.01;
                //}

                // Set the heat pump fan delay time
                thisSys.fanDelayTime = loc_fanDelayTime;
                // if (NumNumbers < iFanDelayTimeNumericNum) {
                //    thisSys.FanDelayTime = 60.0;
                //}

                thisSys.ancillaryOnPower = loc_ancillaryOnPower;
                thisSys.ancillaryOffPower = loc_ancillaryOffPower;

                thisSys.designHRWaterVolumeFlow = loc_designHRWaterVolumeFlow;
                thisSys.maxHROutletWaterTemp = loc_maxHROutletWaterTemp;

                if (thisSys.designHRWaterVolumeFlow > 0.0) {
                    thisSys.heatRecActive = true;
                    errFlag = false;
                    if (loc_heatRecoveryInletNodeName != "" && loc_heatRecoveryOutletNodeName != "") {
                        thisSys.heatRecoveryInletNodeNum = NodeInputManager::GetOnlySingleNode(loc_heatRecoveryInletNodeName,
                                                                                               errFlag,
                                                                                               cCurrentModuleObject,
                                                                                               thisObjectName,
                                                                                               DataLoopNode::NodeType_Water,
                                                                                               DataLoopNode::NodeConnectionType_Inlet,
                                                                                               3,
                                                                                               DataLoopNode::ObjectIsNotParent);
                        thisSys.heatRecoveryOutletNodeNum = NodeInputManager::GetOnlySingleNode(loc_heatRecoveryOutletNodeName,
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

                        if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                            DXCoils::SetMSHPDXCoilHeatRecoveryFlag(thisSys.coolingCoilIndex);
                        }
                        if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                            DXCoils::SetMSHPDXCoilHeatRecoveryFlag(thisSys.heatingCoilIndex);
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

                if (loc_designSpecMultispeedHPType != "" && loc_designSpecMultispeedHPName != "") {
                    thisSys.designSpecMultispeedHPType = loc_designSpecMultispeedHPType;
                    thisSys.designSpecMultispeedHPName = loc_designSpecMultispeedHPName;
                    int designSpecType_Num = 1;

                    DesignSpecMSHP thisDesignSpec;
                    thisSys.compPointerMSHP = thisDesignSpec.factory(designSpecType_Num, loc_designSpecMultispeedHPName);
                    thisSys.designSpecMSHPIndex = getDesignSpecMSHPIndex(thisSys.designSpecMultispeedHPName);

                    if (thisSys.compPointerMSHP != nullptr) {

                        thisSys.noLoadAirFlowRateRatio = thisSys.compPointerMSHP->noLoadAirFlowRateRatio;

                        if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                            thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                            thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {
                            thisSys.numOfSpeedHeating = thisSys.compPointerMSHP->numOfSpeedHeating;
                            thisSys.heatMassFlowRate.resize(thisSys.numOfSpeedHeating);
                            thisSys.heatVolumeFlowRate.resize(thisSys.numOfSpeedHeating);
                            thisSys.MSHeatingSpeedRatio.resize(thisSys.numOfSpeedHeating);
                            for (int i = 0; i < thisSys.numOfSpeedHeating; ++i) {
                                thisSys.heatMassFlowRate[i] = 0.0;
                                thisSys.heatVolumeFlowRate[i] = 0.0;
                                thisSys.MSHeatingSpeedRatio[i] = 1.0;
                            }
                        }

                        if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                            thisSys.numOfSpeedCooling = thisSys.compPointerMSHP->numOfSpeedCooling;
                            thisSys.coolMassFlowRate.resize(thisSys.numOfSpeedCooling);
                            thisSys.coolVolumeFlowRate.resize(thisSys.numOfSpeedCooling);
                            thisSys.MSCoolingSpeedRatio.resize(thisSys.numOfSpeedCooling);
                            for (int i = 0; i < thisSys.numOfSpeedCooling; ++i) {
                                thisSys.coolMassFlowRate[i] = 0.0;
                                thisSys.coolVolumeFlowRate[i] = 0.0;
                                thisSys.MSCoolingSpeedRatio[i] = 1.0;
                            }
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... one or both of the following inputs are invalid.");
                        // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPTypeAlphaNum) + " = " + Alphas(iDesignSpecMSHPTypeAlphaNum));
                        // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPNameAlphaNum) + " = " + Alphas(iDesignSpecMSHPNameAlphaNum));
                        errorsFound = true;
                    }
                } else if ((loc_designSpecMultispeedHPType == "" && loc_designSpecMultispeedHPName != "") ||
                           (loc_designSpecMultispeedHPType != "" && loc_designSpecMultispeedHPName == "")) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    ShowContinueError("... one or both of the following inputs are invalid.");
                    // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPTypeAlphaNum) + " = " + Alphas(iDesignSpecMSHPTypeAlphaNum));
                    // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPNameAlphaNum) + " = " + Alphas(iDesignSpecMSHPNameAlphaNum));
                    errorsFound = true;
                    //} else if (thisSys.numOfSpeedHeating > 0) { // how do these last 2 get called?
                    //    int numOfSpeedHeating = thisSys.numOfSpeedHeating;

                    //    thisSys.heatMassFlowRate.allocate(numOfSpeedHeating);
                    //    thisSys.heatVolumeFlowRate.allocate(numOfSpeedHeating);
                    //    thisSys.MSHeatingSpeedRatio.allocate(numOfSpeedHeating);
                    //    thisSys.MSHeatingSpeedRatio = 1.0;

                    //} else if (thisSys.numOfSpeedCooling > 0) {
                    //    int numOfSpeedCooling = thisSys.numOfSpeedCooling;

                    //    thisSys.coolMassFlowRate.allocate(numOfSpeedCooling);
                    //    thisSys.coolVolumeFlowRate.allocate(numOfSpeedCooling);
                    //    thisSys.MSCoolingSpeedRatio.allocate(numOfSpeedCooling);
                    //    thisSys.MSCoolingSpeedRatio = 1.0;
                }

                if (thisSys.multiSpeedCoolingCoil) {

                    // int designSpecIndex = thisSys.designSpecMSHPIndex;
                    // if (designSpecIndex > 0) thisSys.numOfSpeedCooling = DesignSpecMSHP.numOfSpeedCooling;

                    if (thisSys.numOfSpeedCooling == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... Cooling coil object type requires valid " + unitarySysHeatPumpPerformanceObjectType +
                                          " for cooling to be specified with number of speeds > 0");
                        errorsFound = true;
                    }
                }
                if (thisSys.multiSpeedHeatingCoil) {

                    // int designSpecIndex = thisSys.designSpecMSHPIndex;
                    // if (designSpecIndex > 0) thisSys.numOfSpeedHeating = DesignSpecMSHP(designSpecIndex).NumOfSpeedHeating;

                    if (thisSys.numOfSpeedHeating == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... Heating coil object type requires valid " + unitarySysHeatPumpPerformanceObjectType +
                                          " for heating to be specified with number of speeds > 0");
                        errorsFound = true;
                    }
                }

                if ((thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating &&
                     thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) ||
                    (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGasOrOtherFuel &&
                     thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling)) {
                    // Index = thisSys.designSpecMSHPIndex;
                    if (thisSys.compPointerMSHP != nullptr) {
                        if (thisSys.compPointerMSHP->singleModeFlag) {
                            thisSys.singleMode = 1;
                        }
                    }
                } else {
                    if (thisSys.compPointerMSHP != nullptr) {
                        if (thisSys.compPointerMSHP->singleModeFlag) {
                            ShowSevereError(cCurrentModuleObject + ": " + thisObjectName);
                            ShowContinueError(
                                "In order to perform Single Mode Operation, the valid cooling coil type is Coil:Cooling:DX:MultiSpeed and "
                                "the valid heating is Coil:Heating:DX:MultiSpeed or Coil:Heating:Fuel.");
                            // ShowContinueError("The input cooling coil type = " + Alphas(iCoolingCoilTypeAlphaNum) +
                            //                  " and the input heating coil type = " + Alphas(iHeatingCoilTypeAlphaNum));
                        }
                    }
                }

                if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    VariableSpeedCoils::SetVarSpeedCoilData(thisSys.coolingCoilIndex, errorsFound, _, _, thisSys.designSpecMSHPIndex);
                }

                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                    VariableSpeedCoils::SetVarSpeedCoilData(thisSys.heatingCoilIndex, errorsFound, _, _, thisSys.designSpecMSHPIndex);
                }

                // set global logicals that denote coil type
                if (thisSys.multiSpeedHeatingCoil || thisSys.varSpeedHeatingCoil) {
                    thisSys.multiOrVarSpeedHeatCoil = true;
                }
                if (thisSys.multiSpeedCoolingCoil || thisSys.varSpeedCoolingCoil) {
                    thisSys.multiOrVarSpeedCoolCoil = true;
                }

                // set global variables for multi-stage chilled and hot water coils
                if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                    thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {
                    // int designSpecIndex = thisSys.designSpecMSHPIndex;
                    if (thisSys.compPointerMSHP != nullptr) {
                        thisSys.numOfSpeedCooling = thisSys.compPointerMSHP->numOfSpeedCooling;
                        if (thisSys.numOfSpeedCooling > 1) {
                            thisSys.multiSpeedCoolingCoil = true;
                            thisSys.multiOrVarSpeedCoolCoil = true;
                        }
                    }
                }
                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                    // designSpecIndex = thisSys.designSpecMSHPIndex;
                    if (thisSys.compPointerMSHP != nullptr) {
                        thisSys.numOfSpeedHeating = thisSys.compPointerMSHP->numOfSpeedHeating;
                        if (thisSys.numOfSpeedHeating > 1) {
                            thisSys.multiSpeedHeatingCoil = true;
                            thisSys.multiOrVarSpeedHeatCoil = true;
                        }
                    }
                }

                // check for specific input requirements for ASHRAE90.1 model
                if (thisSys.controlType == controlTypeEnum::controlTypeCCMASHRAE) {

                    // only allowed for water and DX cooling coils at this time
                    if (thisSys.coolCoilExists && thisSys.coolingCoilType_Num != DataHVACGlobals::Coil_CoolingWater &&
                        thisSys.coolingCoilType_Num != DataHVACGlobals::Coil_CoolingWaterDetailed &&
                        thisSys.coolingCoilType_Num != DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                        if (DataGlobals::DisplayExtraWarnings) {
                            ShowWarningError(cCurrentModuleObject + ": " + thisObjectName);
                            ShowContinueError("ASHRAE90.1 control method requires specific cooling coil types.");
                            ShowContinueError("Valid cooling coil types are Coil:Cooling:Water, Coil:Cooling:Water:DetailedGeometry and "
                                              "Coil:Cooling:DX:SingleSpeed.");
                            // ShowContinueError("The input cooling coil type = " + Alphas(iCoolingCoilTypeAlphaNum) +
                            //                  ". This coil will not be modeled using the ASHRAE 90.1 algorithm.");
                        }
                        // mark this coil as non-ASHRAE90 type
                        thisSys.validASHRAECoolCoil = false;
                    }
                    // only allow for water, fuel, or electric at this time
                    if (thisSys.heatCoilExists && thisSys.heatingCoilType_Num != DataHVACGlobals::Coil_HeatingWater &&
                        thisSys.heatingCoilType_Num != DataHVACGlobals::Coil_HeatingGasOrOtherFuel &&
                        thisSys.heatingCoilType_Num != DataHVACGlobals::Coil_HeatingElectric &&
                        thisSys.heatingCoilType_Num != DataHVACGlobals::CoilDX_HeatingEmpirical) {
                        if (DataGlobals::DisplayExtraWarnings) {
                            ShowWarningError(cCurrentModuleObject + ": " + thisObjectName);
                            ShowContinueError("ASHRAE90.1 control method requires specific heating coil types.");
                            ShowContinueError("Valid heating coil types are Coil:Heating:Water, Coil:Heating:Fuel, Coil:Heating:Electric and "
                                              "Coil:Heating:DX:SingleSpeed.");
                            // ShowContinueError("The input heating coil type = " + Alphas(iHeatingCoilTypeAlphaNum) +
                            //                  ". This coil will not be modeled using the ASHRAE 90.1 algorithm.");
                        }
                        // mark this coil as non-ASHRAE90 type
                        thisSys.validASHRAEHeatCoil = false;
                    }
                    if (thisSys.dehumidControlType_Num == dehumidControl_Multimode || thisSys.dehumidControlType_Num == dehumidControl_CoolReheat) {
                        ShowWarningError(cCurrentModuleObject + ": " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iDehumidControlAlphaNum) + " = " +
                        // Alphas(iDehumidControlAlphaNum));
                        ShowContinueError(
                            "ASHRAE90.1 control method does not support dehumidification at this time. Dehumidification control type is "
                            "assumed to be None.");
                        thisSys.dehumidControlType_Num = dehumidControl_None;
                    }
                    if (thisSys.runOnLatentLoad) {
                        ShowWarningError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iRunOnLatentLoadAlphaNum) + " :" +
                        // Alphas(iRunOnLatentLoadAlphaNum));
                        ShowContinueError(
                            "ASHRAE90.1 control method does not support latent load control at this time. This input must be selected as "
                            "SensibleOnlyLoadControl.");
                        thisSys.runOnSensibleLoad = true;
                        thisSys.runOnLatentLoad = false;
                        thisSys.runOnLatentOnlyWithSensible = false;
                    }
                }

                // Setup Report variables for the Unitary System that are not reported in the components themselves
                //                if (GetUnitarySystemDoOnlyOnceFlag) {
                //                    for (UnitarySysNum = 1; UnitarySysNum <= NumUnitarySystem; ++UnitarySysNum) {
                SetupOutputVariable(
                    "Unitary System Part Load Ratio", OutputProcessor::Unit::None, thisSys.partLoadFrac, "System", "Average", thisSys.name);
                SetupOutputVariable(
                    "Unitary System Total Cooling Rate", OutputProcessor::Unit::W, thisSys.totCoolEnergyRate, "System", "Average", thisSys.name);
                SetupOutputVariable(
                    "Unitary System Sensible Cooling Rate", OutputProcessor::Unit::W, thisSys.sensCoolEnergyRate, "System", "Average", thisSys.name);
                SetupOutputVariable(
                    "Unitary System Latent Cooling Rate", OutputProcessor::Unit::W, thisSys.latCoolEnergyRate, "System", "Average", thisSys.name);
                SetupOutputVariable(
                    "Unitary System Total Heating Rate", OutputProcessor::Unit::W, thisSys.totHeatEnergyRate, "System", "Average", thisSys.name);
                SetupOutputVariable(
                    "Unitary System Sensible Heating Rate", OutputProcessor::Unit::W, thisSys.sensHeatEnergyRate, "System", "Average", thisSys.name);
                SetupOutputVariable(
                    "Unitary System Latent Heating Rate", OutputProcessor::Unit::W, thisSys.latHeatEnergyRate, "System", "Average", thisSys.name);
                SetupOutputVariable("Unitary System Ancillary Electric Power",
                                    OutputProcessor::Unit::W,
                                    thisSys.totalAuxElecPower,
                                    "System",
                                    "Average",
                                    thisSys.name);

                // report predicted load as determined by Unitary System for load control only
                if (thisSys.controlType != controlTypeEnum::controlTypeSetpoint) {
                    SetupOutputVariable("Unitary System Predicted Sensible Load to Setpoint Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        thisSys.sensibleLoadPredicted,
                                        "System",
                                        "Average",
                                        thisSys.name);
                    SetupOutputVariable("Unitary System Predicted Moisture Load to Setpoint Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        thisSys.moistureLoadPredicted,
                                        "System",
                                        "Average",
                                        thisSys.name);
                }

                //        IF(UnitarySystem(UnitarySysNum)%DehumidControlType_Num .EQ. DehumidControl_CoolReheat)THEN
                SetupOutputVariable("Unitary System Dehumidification Induced Heating Demand Rate",
                                    OutputProcessor::Unit::W,
                                    thisSys.dehumidInducedHeatingDemandRate,
                                    "System",
                                    "Average",
                                    thisSys.name);
                //        END IF

                if (thisSys.fanExists) {
                    SetupOutputVariable("Unitary System Fan Part Load Ratio",
                                        OutputProcessor::Unit::None,
                                        thisSys.fanPartLoadRatio,
                                        "System",
                                        "Average",
                                        thisSys.name);
                }

                SetupOutputVariable("Unitary System Compressor Part Load Ratio",
                                    OutputProcessor::Unit::None,
                                    thisSys.compPartLoadRatio,
                                    "System",
                                    "Average",
                                    thisSys.name);

                SetupOutputVariable("Unitary System Frost Control Status",
                                    OutputProcessor::Unit::None,
                                    thisSys.frostControlStatus,
                                    "System",
                                    "Average",
                                    thisSys.name);

                if (thisSys.heatCoilExists) {
                    SetupOutputVariable("Unitary System Heating Ancillary Electric Energy",
                                        OutputProcessor::Unit::J,
                                        thisSys.heatingAuxElecConsumption,
                                        "System",
                                        "Sum",
                                        thisSys.name,
                                        _,
                                        "Electric",
                                        "Heating",
                                        _,
                                        "System");
                }

                {
                    auto const SELECT_CASE_var(thisSys.coolingCoilType_Num);
                    if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                        SetupOutputVariable(
                            "Unitary System Cycling Ratio", OutputProcessor::Unit::None, thisSys.cycRatio, "System", "Average", thisSys.name);
                        SetupOutputVariable("Unitary System Compressor Speed Ratio",
                                            OutputProcessor::Unit::None,
                                            thisSys.speedRatio,
                                            "System",
                                            "Average",
                                            thisSys.name);
                    } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                        SetupOutputVariable("Unitary System Cooling Ancillary Electric Energy",
                                            OutputProcessor::Unit::J,
                                            thisSys.coolingAuxElecConsumption,
                                            "System",
                                            "Sum",
                                            thisSys.name,
                                            _,
                                            "Electric",
                                            "Cooling",
                                            _,
                                            "System");
                        SetupOutputVariable(
                            "Unitary System Electric Power", OutputProcessor::Unit::W, thisSys.elecPower, "System", "Average", thisSys.name);
                        SetupOutputVariable(
                            "Unitary System Electric Energy", OutputProcessor::Unit::J, thisSys.elecPowerConsumption, "System", "Sum", thisSys.name);
                        if (thisSys.heatRecActive) {
                            SetupOutputVariable("Unitary System Heat Recovery Rate",
                                                OutputProcessor::Unit::W,
                                                thisSys.heatRecoveryRate,
                                                "System",
                                                "Average",
                                                thisSys.name);
                            SetupOutputVariable("Unitary System Heat Recovery Inlet Temperature",
                                                OutputProcessor::Unit::C,
                                                thisSys.heatRecoveryInletTemp,
                                                "System",
                                                "Average",
                                                thisSys.name);
                            SetupOutputVariable("Unitary System Heat Recovery Outlet Temperature",
                                                OutputProcessor::Unit::C,
                                                thisSys.heatRecoveryOutletTemp,
                                                "System",
                                                "Average",
                                                thisSys.name);
                            SetupOutputVariable("Unitary System Heat Recovery Fluid Mass Flow Rate",
                                                OutputProcessor::Unit::kg_s,
                                                thisSys.heatRecoveryMassFlowRate,
                                                "System",
                                                "Average",
                                                thisSys.name);
                            SetupOutputVariable("Unitary System Heat Recovery Energy",
                                                OutputProcessor::Unit::J,
                                                thisSys.heatRecoveryEnergy,
                                                "System",
                                                "Sum",
                                                thisSys.name);
                        }
                    } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHP)) {
                        SetupOutputVariable("Unitary System Requested Sensible Cooling Rate",
                                            OutputProcessor::Unit::W,
                                            thisSys.coolingCoilSensDemand,
                                            "System",
                                            "Average",
                                            thisSys.name);
                        SetupOutputVariable("Unitary System Requested Latent Cooling Rate",
                                            OutputProcessor::Unit::W,
                                            thisSys.coolingCoilLatentDemand,
                                            "System",
                                            "Average",
                                            thisSys.name);
                    } else {
                    }
                }

                {
                    auto const SELECT_CASE_var(thisSys.heatingCoilType_Num);
                    if ((SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) ||
                        (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) ||
                        (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {
                    } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHP)) {
                        SetupOutputVariable("Unitary System Requested Heating Rate",
                                            OutputProcessor::Unit::W,
                                            thisSys.heatingCoilSensDemand,
                                            "System",
                                            "Average",
                                            thisSys.name);
                    } else {
                    }
                }

                if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling ||
                    thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {
                    SetupOutputVariable(
                        "Unitary System DX Coil Cycling Ratio", OutputProcessor::Unit::None, thisSys.cycRatio, "System", "Average", thisSys.name);
                    SetupOutputVariable(
                        "Unitary System DX Coil Speed Ratio", OutputProcessor::Unit::None, thisSys.speedRatio, "System", "Average", thisSys.name);
                    SetupOutputVariable(
                        "Unitary System DX Coil Speed Level", OutputProcessor::Unit::None, thisSys.speedNum, "System", "Average", thisSys.name);
                }

                if (((thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                      thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) &&
                     thisSys.multiSpeedCoolingCoil) ||
                    (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater && thisSys.multiSpeedHeatingCoil)) {
                    SetupOutputVariable(
                        "Unitary System Water Coil Cycling Ratio", OutputProcessor::Unit::None, thisSys.cycRatio, "System", "Average", thisSys.name);
                    SetupOutputVariable(
                        "Unitary System Water Coil Speed Ratio", OutputProcessor::Unit::None, thisSys.speedRatio, "System", "Average", thisSys.name);
                    SetupOutputVariable(
                        "Unitary System Water Coil Speed Level", OutputProcessor::Unit::None, thisSys.speedNum, "System", "Average", thisSys.name);
                }

                if (DataGlobals::AnyEnergyManagementSystemInModel) {
                    SetupEMSActuator("AirLoopHVAC:UnitarySystem",
                                     thisSys.name,
                                     "Autosized Supply Air Flow Rate",
                                     "[m3/s]",
                                     thisSys.designFanVolFlowRateEMSOverrideOn,
                                     thisSys.designFanVolFlowRateEMSOverrideValue);
                    SetupEMSActuator("AirLoopHVAC:UnitarySystem",
                                     thisSys.name,
                                     "Autosized Supply Air Flow Rate During Cooling Operation",
                                     "[m3/s]",
                                     thisSys.maxCoolAirVolFlowEMSOverrideOn,
                                     thisSys.maxCoolAirVolFlowEMSOverrideValue);
                    SetupEMSActuator("AirLoopHVAC:UnitarySystem",
                                     thisSys.name,
                                     "Autosized Supply Air Flow Rate During Heating Operation",
                                     "[m3/s]",
                                     thisSys.maxHeatAirVolFlowEMSOverrideOn,
                                     thisSys.maxHeatAirVolFlowEMSOverrideValue);
                    SetupEMSActuator("AirLoopHVAC:UnitarySystem",
                                     thisSys.name,
                                     "Autosized Supply Air Flow Rate During No Heating or Cooling Operation",
                                     "[m3/s]",
                                     thisSys.maxNoCoolHeatAirVolFlowEMSOverrideOn,
                                     thisSys.maxNoCoolHeatAirVolFlowEMSOverrideValue);

                    SetupEMSInternalVariable("Unitary System Control Zone Mass Flow Fraction", thisSys.name, "[]", thisSys.controlZoneMassFlowFrac);
                }
                //                    }
                if (DataGlobals::AnyEnergyManagementSystemInModel) {
                    //                        for (UnitarySysNum = 1; UnitarySysNum <= NumUnitarySystem; ++UnitarySysNum) {
                    SetupEMSInternalVariable("Unitary HVAC Design Heating Capacity", thisSys.name, "[W]", thisSys.designHeatingCapacity);
                    SetupEMSInternalVariable("Unitary HVAC Design Cooling Capacity", thisSys.name, "[W]", thisSys.designCoolingCapacity);
                    SetupEMSActuator("Unitary HVAC",
                                     thisSys.name,
                                     "Sensible Load Request",
                                     "[W]",
                                     thisSys.EMSOverrideSensZoneLoadRequest,
                                     thisSys.EMSSensibleZoneLoadValue);
                    SetupEMSActuator("Unitary HVAC",
                                     thisSys.name,
                                     "Moisture Load Request",
                                     "[W]",
                                     thisSys.EMSOverrideMoistZoneLoadRequest,
                                     thisSys.EMSMoistureZoneLoadValue);
                    //                        }
                }
                // can this be called each time a system is gottem?
                bool anyEMSRan;
                EMSManager::ManageEMS(DataGlobals::emsCallFromComponentGetInput, anyEMSRan);

                unitarySys.push_back(thisSys);
                break;
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

        std::string CompName = this->suppHeatCoilName;
        int CoilType_Num = this->suppHeatCoilType_Num;

        if ((CoilType_Num == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) || (CoilType_Num == DataHVACGlobals::Coil_HeatingElectric)) {
            HeatingCoils::SimulateHeatingCoilComponents(
                CompName, FirstHVACIteration, _, this->suppHeatCoilIndex, _, _, this->fanOpMode, this->suppHeatPartLoadFrac);
            //                             QCoilReq=(UnitarySystem(UnitarySysNum)%DesignSuppHeatingCapacity*UnitarySystem(UnitarySysNum)%SuppHeatPartLoadFrac)

        } else if (CoilType_Num == DataHVACGlobals::Coil_HeatingDesuperheater) {
            HeatingCoils::SimulateHeatingCoilComponents(
                CompName, FirstHVACIteration, _, this->suppHeatCoilIndex, _, _, this->fanOpMode, this->suppHeatPartLoadFrac);

        } else if (CoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
            WaterCoils::SimulateWaterCoilComponents(
                CompName, FirstHVACIteration, this->suppHeatCoilIndex, QActual, this->fanOpMode, this->suppHeatPartLoadFrac);

        } else if (CoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
            SteamCoils::SimulateSteamCoilComponents(CompName,
                                                    FirstHVACIteration,
                                                    this->suppHeatCoilIndex,
                                                    this->designSuppHeatingCapacity * this->suppHeatPartLoadFrac,
                                                    _,
                                                    this->fanOpMode,
                                                    this->suppHeatPartLoadFrac);

        } else {
        }
    }

    void UnitarySys::controlUnitarySystemtoSP(int const AirLoopNum,                  // Primary air loop number
                                              bool const FirstHVACIteration,         // True when first HVAC iteration
                                              int &CompOn,                           // compressor on/off control
                                              Optional<Real64 const> OAUCoilOutTemp, // the coil inlet temperature of OutdoorAirUnit
                                              Optional_bool HXUnitOn                 // Flag to control HX for HXAssisted Cooling Coil
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

        // CALL the series of components that simulate a Unitary System
        if (this->fanExists && this->fanPlace == fanPlaceEnum::blowThru) {
            if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {

                HVACFan::fanObjs[this->fanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->fanIndex, FanSpeedRatio);
            }
        }

        if (this->coolingCoilUpstream) {

            if (this->coolCoilExists) {
                this->updateUnitarySystemControl(
                    AirLoopNum, this->coolCoilOutletNodeNum, this->systemCoolControlNodeNum, OnOffAirFlowRatio, FirstHVACIteration, OAUCoilOutTemp);
                this->controlCoolingSystemToSP(AirLoopNum, FirstHVACIteration, HXUnitOn, CompOn);
                PartLoadRatio = this->coolingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }
            if (this->heatCoilExists) {
                this->updateUnitarySystemControl(AirLoopNum,
                                                 this->heatCoilOutletNodeNum,
                                                 this->systemHeatControlNodeNum,
                                                 OnOffAirFlowRatio,
                                                 FirstHVACIteration,
                                                 OAUCoilOutTemp,
                                                 _,
                                                 this->designMaxOutletTemp);
                this->controlHeatingSystemToSP(AirLoopNum, FirstHVACIteration, CompOn);
                PartLoadRatio = this->heatingPartLoadFrac;
                int CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio);
            }

        } else {

            if (this->heatCoilExists) {
                this->updateUnitarySystemControl(AirLoopNum,
                                                 this->heatCoilOutletNodeNum,
                                                 this->systemHeatControlNodeNum,
                                                 OnOffAirFlowRatio,
                                                 FirstHVACIteration,
                                                 OAUCoilOutTemp,
                                                 _,
                                                 this->designMaxOutletTemp);
                this->controlHeatingSystemToSP(AirLoopNum, FirstHVACIteration, CompOn);
                PartLoadRatio = this->heatingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio);
            }
            if (this->coolCoilExists) {
                this->updateUnitarySystemControl(
                    AirLoopNum, this->coolCoilOutletNodeNum, this->systemCoolControlNodeNum, OnOffAirFlowRatio, FirstHVACIteration, OAUCoilOutTemp);
                this->controlCoolingSystemToSP(AirLoopNum, FirstHVACIteration, HXUnitOn, CompOn);
                PartLoadRatio = this->coolingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }
        }

        if (this->fanExists && this->fanPlace == fanPlaceEnum::drawThru) {
            if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->fanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->fanIndex, FanSpeedRatio);
            }
        }

        if (this->suppCoilExists) {
            SuppHeatingCoilFlag = true;
            this->updateUnitarySystemControl(AirLoopNum,
                                             this->suppCoilAirOutletNode,
                                             this->suppHeatControlNodeNum,
                                             OnOffAirFlowRatio,
                                             FirstHVACIteration,
                                             OAUCoilOutTemp,
                                             _,
                                             this->designMaxOutletTemp);
            this->controlSuppHeatSystem(AirLoopNum, FirstHVACIteration);
            this->calcUnitarySuppSystemToSP(FirstHVACIteration);
            SuppHeatingCoilFlag = false;
        }

        this->initHeatPump = false;
    }

    void UnitarySys::updateUnitarySystemControl(int const AirLoopNum,  // number of the current air loop being simulated
                                                int const OutNode,     // coil outlet node number
                                                int const ControlNode, // control node number
                                                Real64 &OnOffAirFlowRatio,
                                                bool const FirstHVACIteration,
                                                Optional<Real64 const> OAUCoilOutletTemp, // "ONLY" for zoneHVAC:OutdoorAirUnit
                                                Optional<Real64> ZoneLoad,
                                                Optional<Real64 const> MaxOutletTemp // limits heating coil outlet temp [C]
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
            auto const SELECT_CASE_var(this->controlType);
            if (SELECT_CASE_var == controlTypeEnum::controlTypeLoad || SELECT_CASE_var == controlTypeEnum::controlTypeCCMASHRAE) {
                if (AirLoopNum == -1) { // This IF-THEN routine is just for ZoneHVAC:OutdoorAirUnit
                    ShowWarningError(this->unitType + " \"" + this->name + "\"");
                    ShowFatalError("...Load based control is not allowed when used with ZoneHVAC:OutdoorAirUnit");
                }

                // here we need to deal with sequenced zone equip
                HeatingLoad = false;
                CoolingLoad = false;
                if (this->zoneSequenceCoolingNum > 0 && this->zoneSequenceHeatingNum > 0) {
                    QToCoolSetPt = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->controlZoneNum)
                                       .SequencedOutputRequiredToCoolingSP(this->zoneSequenceCoolingNum);
                    QToHeatSetPt = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->controlZoneNum)
                                       .SequencedOutputRequiredToHeatingSP(this->zoneSequenceHeatingNum);
                    if (QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0 &&
                        DataHeatBalFanSys::TempControlType(this->controlZoneNum) != DataHVACGlobals::SingleCoolingSetPoint) {
                        ZoneLoad = QToHeatSetPt;
                        HeatingLoad = true;
                    } else if (QToHeatSetPt > 0.0 && QToCoolSetPt > 0.0 &&
                               DataHeatBalFanSys::TempControlType(this->controlZoneNum) == DataHVACGlobals::SingleCoolingSetPoint) {
                        ZoneLoad = 0.0;
                    } else if (QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0 &&
                               DataHeatBalFanSys::TempControlType(this->controlZoneNum) != DataHVACGlobals::SingleHeatingSetPoint) {
                        ZoneLoad = QToCoolSetPt;
                        CoolingLoad = true;
                    } else if (QToHeatSetPt < 0.0 && QToCoolSetPt < 0.0 &&
                               DataHeatBalFanSys::TempControlType(this->controlZoneNum) == DataHVACGlobals::SingleHeatingSetPoint) {
                        ZoneLoad = 0.0;
                    } else if (QToHeatSetPt <= 0.0 && QToCoolSetPt >= 0.0) {
                        ZoneLoad = 0.0;
                    }
                    MoistureLoad = DataZoneEnergyDemands::ZoneSysMoistureDemand(this->controlZoneNum)
                                       .SequencedOutputRequiredToDehumidSP(this->zoneSequenceCoolingNum);
                } else {
                    ZoneLoad = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->controlZoneNum).RemainingOutputRequired;
                    QToCoolSetPt = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->controlZoneNum).OutputRequiredToCoolingSP;
                    QToHeatSetPt = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->controlZoneNum).OutputRequiredToHeatingSP;
                    MoistureLoad = DataZoneEnergyDemands::ZoneSysMoistureDemand(this->controlZoneNum).OutputRequiredToDehumidifyingSP;
                }

                if (this->dehumidControlType_Num != dehumCtrlTypeEnum::dehumidControl_None) {
                    Real64 H2OHtOfVap = Psychrometrics::PsyHfgAirFnWTdb(DataLoopNode::Node(this->nodeNumOfControlledZone).HumRat,
                                                                        DataLoopNode::Node(this->nodeNumOfControlledZone).Temp);

                    // positive MoistureLoad means no dehumidification load
                    MoistureLoad = min(0.0, MoistureLoad * H2OHtOfVap);
                } else {
                    MoistureLoad = 0.0;
                }

                this->initLoadBasedControl(AirLoopNum, FirstHVACIteration, OnOffAirFlowRatio, ZoneLoad);

                // *** the location of this EMS override looks suspect. If EMS is active the load will be changed but the CoolingLoad and HeatingLoad
                // flags are not updated. suggest this be moved up above InitLoadBasedControl function on previous line so the EMS loads are used in
                // that routine EMS override point
                if (this->EMSOverrideSensZoneLoadRequest) ZoneLoad = this->EMSSensibleZoneLoadValue;
                if (this->EMSOverrideMoistZoneLoadRequest) MoistureLoad = this->EMSMoistureZoneLoadValue;

                this->simASHRAEModel = false; // flag used to envoke ASHRAE 90.1 model calculations
                // allows non-ASHSRAE compliant coil types to be modeled using non-ASHAR90 method. Constant fan operating mode is required.
                if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
                    if (CoolingLoad) {
                        if (this->validASHRAECoolCoil) this->simASHRAEModel = true;
                    } else if (HeatingLoad) {
                        if (this->validASHRAEHeatCoil) this->simASHRAEModel = true;
                    }
                }

            } else if (SELECT_CASE_var == controlTypeEnum::controlTypeSetpoint) {
                if (AirLoopNum == -1) { // This IF-THEN routine is just for ZoneHVAC:OutdoorAIRUNIT

                    if (ControlNode == 0) {
                        this->desiredOutletTemp = OAUCoilOutletTemp;
                        this->desiredOutletHumRat = 1.0;
                    } else if (ControlNode == OutNode) {
                        this->desiredOutletTemp = OAUCoilOutletTemp;
                    }
                    // If the unitary system is an equipment of Outdoor Air Unit, the desired coil outlet humidity level is set to zero
                    this->desiredOutletHumRat = 1.0;

                } else { // Not Outdoor Air Unit or zone equipment
                    if (AirLoopNum > 0) economizerFlag = DataAirLoop::AirLoopControlInfo(AirLoopNum).EconoActive;
                    if (ControlNode == 0) {
                        this->desiredOutletTemp = 0.0;
                        this->desiredOutletHumRat = 1.0;
                    } else if (ControlNode == OutNode) {
                        if (this->ISHundredPercentDOASDXCoil && this->runOnSensibleLoad) {
                            this->frostControlSetPointLimit(DataLoopNode::Node(ControlNode).TempSetPoint,
                                                            DataLoopNode::Node(ControlNode).HumRatMax,
                                                            DataEnvironment::OutBaroPress,
                                                            this->designMinOutletTemp,
                                                            1);
                        }
                        this->desiredOutletTemp = DataLoopNode::Node(ControlNode).TempSetPoint;
                        //  IF HumRatMax is zero, then there is no request from SetpointManager:SingleZone:Humidity:Maximum
                        if ((this->dehumidControlType_Num != dehumCtrlTypeEnum::dehumidControl_None) &&
                            (DataLoopNode::Node(ControlNode).HumRatMax > 0.0)) {
                            if (this->ISHundredPercentDOASDXCoil && this->runOnLatentLoad) {
                                this->frostControlSetPointLimit(DataLoopNode::Node(ControlNode).TempSetPoint,
                                                                DataLoopNode::Node(ControlNode).HumRatMax,
                                                                DataEnvironment::OutBaroPress,
                                                                this->designMinOutletTemp,
                                                                2);
                            }
                            this->desiredOutletHumRat = DataLoopNode::Node(ControlNode).HumRatMax;
                        } else {
                            this->desiredOutletHumRat = 1.0;
                        }
                    } else {
                        if (this->ISHundredPercentDOASDXCoil && this->runOnSensibleLoad) {
                            this->frostControlSetPointLimit(DataLoopNode::Node(ControlNode).TempSetPoint,
                                                            DataLoopNode::Node(ControlNode).HumRatMax,
                                                            DataEnvironment::OutBaroPress,
                                                            this->designMinOutletTemp,
                                                            1);
                        }
                        this->desiredOutletTemp =
                            DataLoopNode::Node(ControlNode).TempSetPoint - (DataLoopNode::Node(ControlNode).Temp - DataLoopNode::Node(OutNode).Temp);
                        if (this->dehumidControlType_Num != dehumCtrlTypeEnum::dehumidControl_None) {
                            if (this->ISHundredPercentDOASDXCoil && this->runOnLatentLoad) {
                                this->frostControlSetPointLimit(DataLoopNode::Node(ControlNode).TempSetPoint,
                                                                DataLoopNode::Node(ControlNode).HumRatMax,
                                                                DataEnvironment::OutBaroPress,
                                                                this->designMinOutletTemp,
                                                                2);
                            }
                            this->desiredOutletHumRat = DataLoopNode::Node(ControlNode).HumRatMax -
                                                        (DataLoopNode::Node(ControlNode).HumRat - DataLoopNode::Node(OutNode).HumRat);
                        } else {
                            this->desiredOutletHumRat = 1.0;
                        }
                    }
                }
                if (present(MaxOutletTemp)) this->desiredOutletTemp = min(this->desiredOutletTemp, MaxOutletTemp);

            } else {
            }
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
        // static Array1D_bool MyStagedFlag; // used for finding on staged thermostat
        //////////// hoisted into namespace ////////////////////////////////////////////////
        // static bool MyOneTimeFlag( true ); // one time allocation flag // InitLoadBasedControlOneTimeFlag
        // static bool MyAirLoopPass( true ); // one time allocation flag // InitLoadBasedControlAirLoopPass
        // static int AirLoopPass( 0 ); // Number of air loop pass // AirLoopPassCounter
        // static bool FlowFracFlagReady( true ); // one time flag for calculating flow fraction // InitLoadBasedControlFlowFracFlagReady
        // static Real64 CntrlZoneTerminalUnitMassFlowRateMax( 0.0 ); // Maximum mass flow rate through controlled zone //
        // InitLoadBasedControlCntrlZoneTerminalUnitMassFlowRateMax
        ////////////////////////////////////////////////////////////////////////////////////
        std::string FanType; // used in warning messages
        std::string FanName; // used in warning messages
        // inlet node and system outlet node
        Real64 MassFlowRate; // mass flow rate to calculate loss
        Real64 MaxTemp;      // Maximum temperature used in latent loss calculation
        Real64 QZnReq;
        Real64 QActual;
        Real64 CoilMaxVolFlowRate;
        Real64 SensOutputOff;
        Real64 LatOutputOff;

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
        if (DataGlobals::BeginEnvrnFlag && this->myEnvrnFlag) {

            // set fluid-side hardware limits
            if (this->heatCoilFluidInletNode > 0) {

                if (this->maxHeatCoilFluidFlow == DataSizing::AutoSize) {
                    // IF water coil max water flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                    if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                        WaterCoils::SimulateWaterCoilComponents(this->heatingCoilName, FirstHVACIteration, this->heatingCoilIndex);
                        Real64 CoilMaxVolFlowRate = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->heatingCoilName, errorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->heatCoilLoopNum).FluidName,
                                                                           DataGlobals::CWInitConvTemp,
                                                                           DataPlant::PlantLoop(this->heatCoilLoopNum).FluidIndex,
                                                                           routineName);
                            this->maxHeatCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                    }
                    // IF steam coil max steam flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                    if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                        SteamCoils::SimulateSteamCoilComponents(this->heatingCoilName,
                                                                FirstHVACIteration,
                                                                this->heatingCoilIndex,
                                                                1.0,
                                                                QActual); // QCoilReq, simulate any load > 0 to get max capacity
                        Real64 CoilMaxVolFlowRate = SteamCoils::GetCoilMaxSteamFlowRate(this->heatingCoilIndex, errorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            Real64 TempSteamIn = 100.0;
                            Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                            this->maxHeatCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                    }
                }

                PlantUtilities::InitComponentNodes(0.0,
                                                   this->maxHeatCoilFluidFlow,
                                                   this->heatCoilFluidInletNode,
                                                   this->heatCoilFluidOutletNodeNum,
                                                   this->heatCoilLoopNum,
                                                   this->heatCoilLoopSide,
                                                   this->heatCoilBranchNum,
                                                   this->heatCoilCompNum);
            }
            if (this->suppCoilFluidInletNode > 0) {
                if (this->maxSuppCoilFluidFlow == DataSizing::AutoSize) {
                    if (this->suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                        // IF water coil max water flow rate is DataSizing::AutoSized, simulate once in order to mine max flow rate
                        WaterCoils::SimulateWaterCoilComponents(this->suppHeatCoilName, FirstHVACIteration, this->suppHeatCoilIndex);
                        Real64 CoilMaxVolFlowRate = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", this->suppHeatCoilName, errorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->suppCoilLoopNum).FluidName,
                                                                           DataGlobals::CWInitConvTemp,
                                                                           DataPlant::PlantLoop(this->suppCoilLoopNum).FluidIndex,
                                                                           routineName);
                            this->maxSuppCoilFluidFlow = CoilMaxVolFlowRate * rho;
                        }
                    }
                    if (this->suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                        SteamCoils::SimulateSteamCoilComponents(this->suppHeatCoilName,
                                                                FirstHVACIteration,
                                                                this->suppHeatCoilIndex,
                                                                1.0,
                                                                QActual); // QCoilReq, simulate any load > 0 to get max capacity
                        Real64 CoilMaxVolFlowRate = SteamCoils::GetCoilMaxSteamFlowRate(this->suppHeatCoilIndex, errorsFound);
                        if (CoilMaxVolFlowRate != DataSizing::AutoSize) {
                            int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            Real64 TempSteamIn = 100.0;
                            Real64 SteamDensity = FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, routineName);
                            this->maxSuppCoilFluidFlow = CoilMaxVolFlowRate * SteamDensity;
                        }
                    }
                    PlantUtilities::InitComponentNodes(0.0,
                                                       this->maxSuppCoilFluidFlow,
                                                       this->suppCoilFluidInletNode,
                                                       this->suppCoilFluidOutletNodeNum,
                                                       this->suppCoilLoopNum,
                                                       this->suppCoilLoopSide,
                                                       this->suppCoilBranchNum,
                                                       this->suppCoilCompNum);
                }
            }
            this->myEnvrnFlag = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            this->myEnvrnFlag = true;
        }

        if (this->myFanFlag) {
            if (this->actualFanVolFlowRate != DataSizing::AutoSize) {
                if (this->actualFanVolFlowRate > 0.0) {
                    this->heatingFanSpeedRatio = this->maxHeatAirVolFlow / this->actualFanVolFlowRate;
                    this->coolingFanSpeedRatio = this->maxCoolAirVolFlow / this->actualFanVolFlowRate;
                    this->noHeatCoolSpeedRatio = this->maxNoCoolHeatAirVolFlow / this->actualFanVolFlowRate;
                    if (this->fanExists && !this->multiOrVarSpeedHeatCoil && !this->multiOrVarSpeedCoolCoil) {
                        bool fanHasPowerSpeedRatioCurve = false;
                        if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                            if (HVACFan::fanObjs[this->fanIndex]->powerModFuncFlowFractionCurveIndex > 0) fanHasPowerSpeedRatioCurve = true;
                        } else {
                            if (Fans::GetFanSpeedRatioCurveIndex(FanType, FanName, this->fanIndex) > 0) fanHasPowerSpeedRatioCurve = true;
                        }
                        if (fanHasPowerSpeedRatioCurve) {

                            if (this->actualFanVolFlowRate == this->maxHeatAirVolFlow && this->actualFanVolFlowRate == this->maxCoolAirVolFlow &&
                                this->actualFanVolFlowRate == this->maxNoCoolHeatAirVolFlow) {
                                ShowWarningError(this->unitType + " \"" + this->name + "\"");
                                ShowContinueError("...For fan type and name = " + FanType + " \"" + FanName + "\"");
                                ShowContinueError("...Fan power ratio function of speed ratio curve has no impact if fan volumetric flow rate is the "
                                                  "same as the unitary system volumetric flow rate.");
                                ShowContinueError(
                                    "...Fan volumetric flow rate            = " + General::RoundSigDigits(this->actualFanVolFlowRate, 5) + " m3/s.");
                                ShowContinueError("...Unitary system volumetric flow rate = " + General::RoundSigDigits(this->maxHeatAirVolFlow, 5) +
                                                  " m3/s.");
                            }
                        }
                    }
                }
                this->myFanFlag = false;
            } else {
                if (this->fanExists) {
                    if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        this->actualFanVolFlowRate = HVACFan::fanObjs[this->fanIndex]->designAirVolFlowRate;
                    } else {
                        this->actualFanVolFlowRate = Fans::GetFanDesignVolumeFlowRate(blankString, blankString, errorsFound, this->fanIndex);
                    }
                }
            }
        }

        if (allocated(DataZoneEquipment::ZoneEquipConfig) && this->myCheckFlag) {
            if (this->airLoopEquipment) {
                int zoneNum = DataHeatBalance::Zone(this->controlZoneNum).ZoneEqNum;
                int zoneInlet = this->zoneInletNode;
                int coolingPriority = 0;
                int heatingPriority = 0;
                // setup zone equipment sequence information based on finding matching air terminal
                if (DataZoneEquipment::ZoneEquipConfig(zoneNum).EquipListIndex > 0) {
                    DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(zoneNum).EquipListIndex)
                        .getPrioritiesforInletNode(zoneInlet, coolingPriority, heatingPriority);
                    this->zoneSequenceCoolingNum = coolingPriority;
                    this->zoneSequenceHeatingNum = heatingPriority;
                }
                this->myCheckFlag = false;
                if (this->zoneSequenceCoolingNum == 0) {
                    ShowSevereError(this->unitType + " \"" + this->name +
                                    "\": No matching air terminal found in the zone equipment list for zone = " +
                                    DataHeatBalance::Zone(this->controlZoneNum).Name + ".");
                    ShowFatalError("Subroutine InitLoadBasedControl: Errors found in getting " + this->unitType +
                                   " input.  Preceding condition(s) causes termination.");
                }
            }
            if (this->zoneInletNode == 0) {
                ShowSevereError(this->unitType + " \"" + this->name + "\": The zone inlet node in the controlled zone (" +
                                DataHeatBalance::Zone(this->controlZoneNum).Name + ") is not found.");
                ShowFatalError("Subroutine InitLoadBasedControl: Errors found in getting " + this->unitType +
                               " input.  Preceding condition(s) causes termination.");
            }
        }

        // What type of logic is this? Is the point to go through the main IF once? or every other time?
        // RR: This was used with AirflowNetwork to calculate duct losses.
        // RR: AFN counts the number of passes through airloop equipment (same logic in Furnaces and other modules) and resets the counter to 0 on
        // BeginEnvrnFlag. RR: This has been changed in this module and AFN to use AirflowNetworkFanActivated if AirflowNetworkUnitarySystem is seen
        // by AFN. RR: Search for AirflowNetworkFanActivated in this module to see usage. The following lines of code can probably be removed although
        // it would require a AFN input file to test.
        if (DataGlobals::BeginEnvrnFlag && initLoadBasedControlAirLoopPass) {
            airLoopPassCounter = 0;
            initLoadBasedControlAirLoopPass = false;
        }
        if (!DataGlobals::BeginEnvrnFlag) {
            initLoadBasedControlAirLoopPass = true;
        }

        ++airLoopPassCounter;
        if (airLoopPassCounter > 2) airLoopPassCounter = 1;

        // reset duct losses from previous iteration
        if (FirstHVACIteration) {
            this->senLoadLoss = 0.0;
            this->latLoadLoss = 0.0;
        }

        // Calcuate air distribution losses
        //  IF (.NOT. FirstHVACIteration .AND. AirLoopPass .EQ. 1 .AND. AirflowNetworkFanActivated) THEN
        if (!FirstHVACIteration && DataAirflowNetwork::AirflowNetworkFanActivated) {
            Real64 DeltaMassRate = 0.0;
            int ZoneInNode = this->zoneInletNode;
            Real64 MinHumRat = DataLoopNode::Node(ZoneInNode).HumRat;
            Real64 MassFlowRate = DataLoopNode::Node(ZoneInNode).MassFlowRate / this->controlZoneMassFlowFrac;
            if (DataLoopNode::Node(this->airOutNode).Temp < DataLoopNode::Node(this->nodeNumOfControlledZone).Temp)
                MinHumRat = DataLoopNode::Node(this->airOutNode).HumRat;
            if (DataAirflowNetwork::SimulateAirflowNetwork > DataAirflowNetwork::AirflowNetworkControlMultizone) {
                DeltaMassRate =
                    DataLoopNode::Node(this->airOutNode).MassFlowRate - DataLoopNode::Node(ZoneInNode).MassFlowRate / this->controlZoneMassFlowFrac;
                if (DeltaMassRate < 0.0) DeltaMassRate = 0.0;
            } else {
                MassFlowRate = DataLoopNode::Node(this->airOutNode).MassFlowRate;
                DeltaMassRate = 0.0;
            }
            this->senLoadLoss = MassFlowRate * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(this->airOutNode).Temp, MinHumRat) -
                                                Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(ZoneInNode).Temp, MinHumRat)) +
                                DeltaMassRate * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(this->airOutNode).Temp, MinHumRat) -
                                                 Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(this->nodeNumOfControlledZone).Temp, MinHumRat));
            if (std::abs(this->sensibleLoadMet) > 0.0) {
                if (std::abs(this->senLoadLoss / this->sensibleLoadMet) < 0.001) this->senLoadLoss = 0.0;
            }
            if (this->humidistat) {
                MaxTemp = DataLoopNode::Node(this->nodeNumOfControlledZone).Temp;
                this->latLoadLoss = MassFlowRate * (Psychrometrics::PsyHFnTdbW(MaxTemp, DataLoopNode::Node(this->airOutNode).HumRat) -
                                                    Psychrometrics::PsyHFnTdbW(MaxTemp, DataLoopNode::Node(ZoneInNode).HumRat)) +
                                    DeltaMassRate * (Psychrometrics::PsyHFnTdbW(MaxTemp, DataLoopNode::Node(this->airOutNode).HumRat) -
                                                     Psychrometrics::PsyHFnTdbW(MaxTemp, DataLoopNode::Node(this->nodeNumOfControlledZone).HumRat));
                if (std::abs(this->latentLoadMet) > 0.0) {
                    if (std::abs(this->latLoadLoss / this->latentLoadMet) < 0.001) this->latLoadLoss = 0.0;
                }
            }
        }

        if (this->fanOpModeSchedPtr > 0) {
            if (ScheduleManager::GetCurrentScheduleValue(this->fanOpModeSchedPtr) == 0.0) {
                this->fanOpMode = fanOpModeEnum::cycFanCycCoil;
            } else {
                this->fanOpMode = fanOpModeEnum::contFanCycCoil;
                DataHVACGlobals::OnOffFanPartLoadFraction = 1.0;
            }
        }

        //  OpMode = UnitarySystem(UnitarySysNum)%FanOpMode
        if (allocated(DataAirLoop::AirLoopControlInfo) && this->airLoopEquipment) {
            economizerFlag = DataAirLoop::AirLoopControlInfo(AirLoopNum).EconoActive;
        } else {
            economizerFlag = false;
        }

        // System load calculation for cycling fan systems
        if (this->controlZoneMassFlowFrac > 0.0) {
            QZnReq = ZoneLoad / this->controlZoneMassFlowFrac;
            MoistureLoad /= this->controlZoneMassFlowFrac;
            QToCoolSetPt /= this->controlZoneMassFlowFrac;
            QToHeatSetPt /= this->controlZoneMassFlowFrac;
            ZoneLoad = QZnReq;
        } else {
            QZnReq = ZoneLoad;
            this->controlZoneMassFlowFrac = 1.0;
        }

        CoolingLoad = false;
        HeatingLoad = false;

        if (QZnReq > Small5WLoad / this->controlZoneMassFlowFrac && !DataZoneEnergyDemands::CurDeadBandOrSetback(this->controlZoneNum)) {
            if (DataHeatBalFanSys::TempControlType(this->controlZoneNum) != DataHVACGlobals::SingleCoolingSetPoint) {
                HeatingLoad = true;
            }
        } else if (QZnReq < -Small5WLoad / this->controlZoneMassFlowFrac && !DataZoneEnergyDemands::CurDeadBandOrSetback(this->controlZoneNum)) {
            if (DataHeatBalFanSys::TempControlType(this->controlZoneNum) != DataHVACGlobals::SingleHeatingSetPoint) {
                CoolingLoad = true;
            }
        }

        // System load calculation for constant fan systems
        if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
            bool HXUnitOn = false;
            this->fanPartLoadRatio = 0.0; // sets fan to minimum for ASHRAE model
            this->setOnOffMassFlowRate(OnOffAirFlowRatio,
                                       0.0); // CompOnMassFlow and CompOffMassFlow are scalar, reset to this system's values
            this->calcUnitarySystemToLoad(AirLoopNum, FirstHVACIteration, 0.0, 0.0, OnOffAirFlowRatio, SensOutputOff, LatOutputOff, HXUnitOn);
            {
                auto const SELECT_CASE_var(DataHeatBalFanSys::TempControlType(this->controlZoneNum));
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

            if (CoolingLoad && this->iterationCounter <= 20) {
                this->iterationMode[this->iterationCounter] = CoolingMode;
            } else if (HeatingLoad && this->iterationCounter <= 20) {
                this->iterationMode[this->iterationCounter] = HeatingMode;
            } else if (this->iterationCounter <= 20) {
                this->iterationMode[this->iterationCounter] = NoCoolHeat;
            }
            // IF small loads to meet or not converging, just shut down unit
            if (std::abs(ZoneLoad) < Small5WLoad) {
                ZoneLoad = 0.0;
                CoolingLoad = false;
                HeatingLoad = false;
            } else if (this->iterationCounter > 6) {                // attempt to lock output (air flow) if oscillations are detected
                int OperatingMode = this->iterationMode[7];         // VS systems can take a few more iterations than single-speed systems
                int OperatingModeMinusOne = this->iterationMode[6]; // previously tested 5th iteration, now tests 7th
                int OperatingModeMinusTwo = this->iterationMode[5];
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

        // Determine the staged status
        if (allocated(DataZoneControls::StageZoneLogic) && this->designSpecMSHPIndex > 0) {
            if (DataZoneControls::StageZoneLogic(this->controlZoneNum)) {
                this->staged = true;
                this->stageNum = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->controlZoneNum).StageNum;
            } else {
                if (this->myStagedFlag) {
                    ShowWarningError("ZoneControl:Thermostat:StagedDualSetpoint is found, but is not applied to this AirLoopHVAC:UnitarySystem "
                                     "object with UnitarySystemPerformance:Multispeed type = ");
                    ShowContinueError(this->name + ". Please make correction. Simulation continues...");
                    this->myStagedFlag = false;
                }
            }
        }

        // Staged control
        if (this->staged) {
            if (this->stageNum == 0) {
                HeatingLoad = false;
                CoolingLoad = false;
                QZnReq = 0.0;
            } else {
                QZnReq = DataZoneEnergyDemands::ZoneSysEnergyDemand(this->controlZoneNum).RemainingOutputRequired / this->controlZoneMassFlowFrac;
                if (this->stageNum > 0) {
                    HeatingLoad = true;
                    CoolingLoad = false;
                } else {
                    HeatingLoad = false;
                    CoolingLoad = true;
                }
            }
        }

        if (this->dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_Multimode) {
            if (HeatingLoad) MoistureLoad = 0.0;
        }

        // Check load control
        if (this->runOnLatentOnlyWithSensible && ZoneLoad == 0.0) MoistureLoad = 0.0;
        if (!this->runOnSensibleLoad) {
            ZoneLoad = 0.0;
            CoolingLoad = false;
            HeatingLoad = false;
        }
        if (!this->runOnLatentLoad) MoistureLoad = 0.0;

        // Testing heat pump air to air with RH control with CoolReheat dehumidifaction control showed that when there was heating
        // and moisture load, the cooling coil was turning on to meet the moisture load and reheat was then turning on to meet both
        // heating load and excess cooling load caused by cooling coil. Adding the logic below caused the zone temperature,
        // relative humidity, cooling/heating rate to line up for both the orignal and new file with unitary system object.

        if (this->suppCoilExists) {
            if (this->dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
                if (MoistureLoad < 0.0 && this->heatPump) {
                    HeatingLoad = false;
                    CoolingLoad = true;
                }
            }
        }

        // set report variables for predicted sensible and latent load
        this->sensibleLoadPredicted = ZoneLoad;
        this->moistureLoadPredicted = MoistureLoad;
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

            this->lastMode = HeatingMode;

            if (this->multiOrVarSpeedHeatCoil) {

                HeatSpeedNum = this->heatingSpeedNum;

                if (HeatSpeedNum == 0) {
                    CompOnMassFlow = this->idleMassFlowRate;
                    CompOnFlowRatio = this->idleSpeedRatio;
                } else if (HeatSpeedNum == 1) {
                    CompOnMassFlow = this->heatMassFlowRate[1];
                    CompOnFlowRatio = this->MSHeatingSpeedRatio[1];
                } else if (HeatSpeedNum > 1) {
                    CompOnMassFlow = this->heatMassFlowRate[HeatSpeedNum];
                    CompOnFlowRatio = this->MSHeatingSpeedRatio[HeatSpeedNum];
                }
                // Set the compressor or coil OFF mass flow rate based on LOGICAL flag
                // UseCompressorOnFlow is used when the user does not enter a value for no cooling or heating flow rate
                if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
                    if (MoistureLoad < 0.0 && this->humidistat && this->dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
                        if (this->multiOrVarSpeedCoolCoil) {
                            CoolSpeedNum = this->coolingSpeedNum;
                            if (CoolSpeedNum < 1) {
                                CompOnMassFlow = this->idleMassFlowRate;
                                CompOffMassFlow = this->idleMassFlowRate;
                                CompOffFlowRatio = this->idleSpeedRatio;
                            } else if (CoolSpeedNum == 1) {
                                CompOnMassFlow = this->coolMassFlowRate[1];
                                CompOffMassFlow = this->coolMassFlowRate[1];
                                CompOffFlowRatio = this->MSCoolingSpeedRatio[1];
                            } else if (CoolSpeedNum > 1) {
                                CompOnMassFlow = this->coolMassFlowRate[CoolSpeedNum];
                                CompOffMassFlow = this->coolMassFlowRate[CoolSpeedNum - 1];
                                CompOffFlowRatio = this->MSCoolingSpeedRatio[CoolSpeedNum - 1];
                            }
                        } else {
                            CompOffMassFlow = this->maxCoolAirMassFlow;
                            CompOffFlowRatio = this->coolingFanSpeedRatio;
                        }
                    } else {
                        if (HeatSpeedNum == 0) {
                            CompOffMassFlow = this->idleMassFlowRate;
                            CompOffFlowRatio = this->idleSpeedRatio;
                        } else if (HeatSpeedNum == 1) {
                            CompOffMassFlow = this->heatMassFlowRate[HeatSpeedNum];
                            CompOffFlowRatio = this->heatMassFlowRate[HeatSpeedNum];
                        } else {
                            CompOffMassFlow = this->heatMassFlowRate[HeatSpeedNum - 1];
                            CompOffFlowRatio = this->MSHeatingSpeedRatio[HeatSpeedNum - 1];
                        }
                    }
                } else { // cycling fan mode
                    if (HeatSpeedNum <= 1) {
                        CompOffMassFlow = 0.0; // #5518
                        CompOffFlowRatio = 0.0;
                    } else {
                        CompOffMassFlow = this->heatMassFlowRate[HeatSpeedNum - 1];
                        CompOffFlowRatio = this->MSHeatingSpeedRatio[HeatSpeedNum - 1];
                    }
                }
            } else { // IF(MultiOrVarSpeedHeatCoil) THEN
                //   If a heating and moisture load exists, operate at the cooling mass flow rate ELSE operate at the heating flow rate
                if (MoistureLoad < 0.0 && this->humidistat && this->dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_CoolReheat &&
                    !this->DXHeatingCoil) {
                    if (this->multiOrVarSpeedCoolCoil) {
                        CoolSpeedNum = this->coolingSpeedNum;
                        if (CoolSpeedNum < 1) {
                            CompOnMassFlow = this->idleMassFlowRate;
                            CompOnFlowRatio = this->idleSpeedRatio;
                        } else if (CoolSpeedNum == 1) {
                            CompOnMassFlow = this->coolMassFlowRate[1];
                            CompOnFlowRatio = this->MSCoolingSpeedRatio[1];
                        } else {
                            CompOnMassFlow = this->coolMassFlowRate[CoolSpeedNum];
                            CompOnFlowRatio = this->MSCoolingSpeedRatio[CoolSpeedNum];
                        }
                    } else { // IF (MultiOrVarSpeedCoolCoil) THEN
                        CompOnMassFlow = this->maxCoolAirMassFlow;
                        CompOnFlowRatio = this->coolingFanSpeedRatio;
                        if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
                            CompOffMassFlow = this->maxNoCoolHeatAirMassFlow;
                            CompOffFlowRatio = this->coolingFanSpeedRatio;
                        }
                    }
                } else { // Heating load but no moisture load
                    CompOnMassFlow = this->maxHeatAirMassFlow;
                    CompOnFlowRatio = this->heatingFanSpeedRatio;
                    if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
                        if (this->airFlowControl == useCompressorOnFlow) {
                            CompOffMassFlow = this->maxHeatAirMassFlow;
                            CompOffFlowRatio = this->heatingFanSpeedRatio;
                        } else {
                            CompOffMassFlow = this->maxNoCoolHeatAirMassFlow;
                            CompOffFlowRatio = this->heatingFanSpeedRatio;
                        }
                    }
                }
            }

            // If a cooling load exists, operate at the cooling mass flow rate
        } else if (CoolingLoad) {

            this->lastMode = CoolingMode;

            if (this->multiOrVarSpeedCoolCoil) {

                CoolSpeedNum = this->coolingSpeedNum;

                if (CoolSpeedNum == 0) {
                    CompOnMassFlow = this->idleMassFlowRate;
                    CompOnFlowRatio = this->idleSpeedRatio;
                } else if (CoolSpeedNum == 1) {
                    CompOnMassFlow = this->coolMassFlowRate[1];
                    CompOnFlowRatio = this->MSCoolingSpeedRatio[1];
                } else if (CoolSpeedNum > 1) {
                    CompOnMassFlow = this->coolMassFlowRate[CoolSpeedNum];
                    CompOnFlowRatio = this->MSCoolingSpeedRatio[CoolSpeedNum];
                }
                // Set the compressor or coil OFF mass flow rate based on LOGICAL flag
                // UseCompressorOnFlow is used when the user does not enter a value for no cooling or heating flow rate
                //    IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
                //      IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
                if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
                    if (CoolSpeedNum == 0) {
                        CompOffMassFlow = this->idleMassFlowRate;
                        CompOffFlowRatio = this->idleSpeedRatio;
                    } else if (CoolSpeedNum == 1) {
                        CompOffMassFlow = this->coolMassFlowRate[CoolSpeedNum];
                        CompOffFlowRatio = this->coolMassFlowRate[CoolSpeedNum];
                    } else {
                        CompOffMassFlow = this->coolMassFlowRate[CoolSpeedNum - 1];
                        CompOffFlowRatio = this->MSCoolingSpeedRatio[CoolSpeedNum - 1];
                    }
                } else { // cycling fan mode
                    if (CoolSpeedNum <= 1) {
                        CompOffMassFlow = 0.0; // #5518
                        CompOffFlowRatio = 0.0;
                    } else {
                        CompOffMassFlow = this->coolMassFlowRate[CoolSpeedNum - 1];
                        CompOffFlowRatio = this->MSCoolingSpeedRatio[CoolSpeedNum - 1];
                    }
                }
            } else { // IF(MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
                CompOnMassFlow = this->maxCoolAirMassFlow;
                CompOnFlowRatio = this->coolingSpeedRatio;
                if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
                    if (this->airFlowControl == useCompressorOnFlow) {
                        CompOffMassFlow = this->maxCoolAirMassFlow;
                        CompOffFlowRatio = this->coolingFanSpeedRatio;
                    } else {
                        CompOffMassFlow = this->maxNoCoolHeatAirMassFlow;
                        CompOffFlowRatio = this->coolingFanSpeedRatio;
                    }
                }
            }

        } else { // No load
            // If no load exists, set the compressor on mass flow rate.
            // Set equal the mass flow rate when no heating or cooling is needed If no moisture load exists.
            // If the user has set the off mass flow rate to 0, set according to the last operating mode.

            if (MoistureLoad < 0.0 && this->humidistat && this->dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
                if (this->multiOrVarSpeedCoolCoil) {
                    CoolSpeedNum = this->coolingSpeedNum;
                    if (CoolSpeedNum < 1) {
                        CompOnMassFlow = this->idleMassFlowRate;
                        CompOnFlowRatio = this->idleSpeedRatio;
                    } else if (CoolSpeedNum == 1) {
                        CompOnMassFlow = this->coolMassFlowRate[1];
                        CompOnFlowRatio = this->MSCoolingSpeedRatio[1];
                    } else {
                        CompOnMassFlow = this->coolMassFlowRate[CoolSpeedNum];
                        CompOnFlowRatio = this->MSCoolingSpeedRatio[CoolSpeedNum];
                    }

                    if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
                        if (this->airFlowControl == useCompressorOnFlow) {
                            if (CoolSpeedNum <= 1) {
                                CompOffMassFlow = this->idleMassFlowRate;
                                CompOffFlowRatio = this->idleSpeedRatio;
                            } else {
                                CompOffMassFlow = this->coolMassFlowRate[CoolSpeedNum - 1];
                                CompOffFlowRatio = this->MSCoolingSpeedRatio[CoolSpeedNum - 1];
                            }
                        } else {
                            CompOffMassFlow = this->idleMassFlowRate;
                            CompOffFlowRatio = this->idleSpeedRatio;
                        }
                    }

                } else { // IF (MultiOrVarSpeedCoolCoil(UnitarySysNum)) THEN
                    CompOnMassFlow = this->maxCoolAirMassFlow;
                    CompOnFlowRatio = this->coolingFanSpeedRatio;
                    if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
                        if (this->airFlowControl == useCompressorOnFlow) {
                            CompOffMassFlow = this->maxCoolAirMassFlow;
                            CompOffFlowRatio = this->coolingFanSpeedRatio;
                        } else {
                            CompOffMassFlow = this->maxNoCoolHeatAirMassFlow;
                            CompOffFlowRatio = this->coolingFanSpeedRatio;
                        }
                    }
                }

            } else { // No Moisture Load

                if (this->lastMode == HeatingMode) {
                    if (this->multiOrVarSpeedHeatCoil) {
                        CompOnMassFlow = this->idleMassFlowRate;
                        CompOnFlowRatio = this->idleSpeedRatio;
                    } else {
                        CompOnMassFlow = this->maxNoCoolHeatAirMassFlow;
                        CompOnFlowRatio = 1.0;
                    }
                } else {
                    if (this->multiOrVarSpeedCoolCoil) {
                        CompOnMassFlow = this->idleMassFlowRate;
                        CompOnFlowRatio = this->idleSpeedRatio;
                    } else {
                        CompOnMassFlow = this->maxNoCoolHeatAirMassFlow;
                        CompOnFlowRatio = 1.0;
                    }
                }
                if (CompOnMassFlow == 0.0) {
                    if (this->lastMode == HeatingMode) {
                        if (this->multiOrVarSpeedHeatCoil) {
                            HeatSpeedNum = this->heatingSpeedNum;
                            if (HeatSpeedNum == 0) {
                                CompOnMassFlow = this->idleMassFlowRate;
                                CompOnFlowRatio = this->idleSpeedRatio;
                            } else if (HeatSpeedNum == 1) {
                                CompOnMassFlow = this->heatMassFlowRate[1];
                                CompOnFlowRatio = this->MSHeatingSpeedRatio[1];
                            } else if (HeatSpeedNum > 1) {
                                CompOnMassFlow = this->heatMassFlowRate[HeatSpeedNum];
                                CompOnFlowRatio = this->MSHeatingSpeedRatio[HeatSpeedNum];
                            }
                        } else { // IF(MultiOrVarSpeedHeatCoil) THEN
                            CompOnMassFlow = this->maxHeatAirMassFlow;
                            CompOnFlowRatio = this->heatingFanSpeedRatio;
                        }
                    } else { // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
                        if (this->multiOrVarSpeedCoolCoil) {
                            CoolSpeedNum = this->coolingSpeedNum;
                            if (CoolSpeedNum == 0) {
                                CompOnMassFlow = this->idleMassFlowRate;
                                CompOnFlowRatio = this->idleSpeedRatio;
                            } else if (CoolSpeedNum == 1) {
                                CompOnMassFlow = this->coolMassFlowRate[1];
                                CompOnFlowRatio = this->MSCoolingSpeedRatio[1];
                            } else if (CoolSpeedNum > 1) {
                                CompOnMassFlow = this->coolMassFlowRate[CoolSpeedNum];
                                CompOnFlowRatio = this->MSCoolingSpeedRatio[CoolSpeedNum];
                            }
                        } else { // IF(MultiOrVarSpeedCoolCoil) THEN
                            CompOnMassFlow = this->maxCoolAirMassFlow;
                            CompOnFlowRatio = this->coolingFanSpeedRatio;
                        } // IF(MultiOrVarSpeedCoolCoil) THEN
                    }     // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
                }         // IF(CompOnMassFlow .EQ. 0.0d0)THEN

                if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
                    if (this->airFlowControl == useCompressorOnFlow) {
                        if (this->lastMode == HeatingMode) {
                            if (this->multiOrVarSpeedHeatCoil) {
                                HeatSpeedNum = this->heatingSpeedNum;
                                if (HeatSpeedNum < 1) {
                                    CompOffMassFlow = this->idleMassFlowRate;
                                    CompOffFlowRatio = this->idleSpeedRatio;
                                } else if (HeatSpeedNum == 1) {
                                    CompOffMassFlow = this->heatMassFlowRate[1];
                                    CompOffFlowRatio = this->MSHeatingSpeedRatio[1];
                                } else {
                                    CompOffMassFlow = this->heatMassFlowRate[HeatSpeedNum - 1];
                                    CompOffFlowRatio = this->MSHeatingSpeedRatio[HeatSpeedNum - 1];
                                }
                            } else {
                                CompOffMassFlow = this->maxHeatAirMassFlow;
                                CompOffFlowRatio = this->heatingFanSpeedRatio;
                            }
                        } else { // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
                            if (this->multiOrVarSpeedCoolCoil) {
                                CoolSpeedNum = this->coolingSpeedNum;
                                if (CoolSpeedNum < 1) {
                                    CompOffMassFlow = this->idleMassFlowRate;
                                    CompOffFlowRatio = this->idleSpeedRatio;
                                } else if (CoolSpeedNum == 1) {
                                    CompOffMassFlow = this->coolMassFlowRate[1];
                                    CompOffFlowRatio = this->MSCoolingSpeedRatio[1];
                                } else {
                                    CompOffMassFlow = this->coolMassFlowRate[CoolSpeedNum - 1];
                                    CompOffFlowRatio = this->MSCoolingSpeedRatio[CoolSpeedNum - 1];
                                }
                            } else {
                                CompOffMassFlow = this->maxCoolAirMassFlow;
                                CompOffFlowRatio = this->coolingFanSpeedRatio;
                            }
                        }    // IF(UnitarySystem(UnitarySysNum)%LastMode .EQ. HeatingMode)THEN
                    } else { // IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
                        if (this->lastMode == HeatingMode) {
                            if (this->multiOrVarSpeedHeatCoil) {
                                CompOffMassFlow = this->idleMassFlowRate;
                                CompOffFlowRatio = this->idleSpeedRatio;
                            } else {
                                CompOffMassFlow = this->maxNoCoolHeatAirMassFlow;
                                CompOffFlowRatio = this->heatingFanSpeedRatio;
                            }
                        } else {
                            if (this->multiOrVarSpeedCoolCoil) {
                                CompOffMassFlow = this->idleMassFlowRate;
                                CompOffFlowRatio = this->idleSpeedRatio;
                            } else {
                                CompOffMassFlow = this->maxNoCoolHeatAirMassFlow;
                                CompOffFlowRatio = this->coolingFanSpeedRatio;
                            }
                        }
                    } // IF (UnitarySystem(UnitarySysNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
                }     // IF(UnitarySystem(UnitarySysNum)%FanOpMode == ContFanCycCoil)THEN
            }         // ELSE ! No Moisture Load
        }             // No Heating/Cooling Load

        if (this->multiSpeedHeatingCoil && (HeatingLoad && HeatSpeedNum == 1)) {
            if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
                DataHVACGlobals::MSHPMassFlowRateLow = CompOnMassFlow; // #5737
            } else {
                DataHVACGlobals::MSHPMassFlowRateLow = CompOnMassFlow * PartLoadRatio; // proportional to PLR when speed = 1,  #5518
            }
        } else if (this->multiSpeedCoolingCoil && (CoolingLoad && CoolSpeedNum == 1)) {
            if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
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

        Real64 fanPartLoadRatio = PartLoadRatio;
        if (this->simASHRAEModel) fanPartLoadRatio = this->fanPartLoadRatio;
        int SpeedNum = max(this->coolingSpeedNum, this->heatingSpeedNum);
        int InletNode = this->airInNode;

        if (SpeedNum > 1) {
            if ((CoolingLoad && this->multiOrVarSpeedCoolCoil) || (HeatingLoad && this->multiOrVarSpeedHeatCoil)) {
                AverageUnitMassFlow = fanPartLoadRatio * CompOnMassFlow + (1.0 - fanPartLoadRatio) * CompOffMassFlow;
            } else {
                AverageUnitMassFlow = CompOnMassFlow;
            }
        } else {
            AverageUnitMassFlow = (fanPartLoadRatio * CompOnMassFlow) + ((1.0 - fanPartLoadRatio) * CompOffMassFlow);
        }

        if (CompOffFlowRatio > 0.0) {
            if (SpeedNum > 1) {
                if ((CoolingLoad && this->multiOrVarSpeedCoolCoil) || (HeatingLoad && this->multiOrVarSpeedHeatCoil)) {
                    FanSpeedRatio = fanPartLoadRatio * CompOnFlowRatio + (1.0 - fanPartLoadRatio) * CompOffFlowRatio;
                    m_runTimeFraction1 = fanPartLoadRatio;
                    m_runTimeFraction2 = 1.0 - fanPartLoadRatio;
                } else {
                    FanSpeedRatio = CompOnFlowRatio;
                    m_runTimeFraction1 = fanPartLoadRatio;
                    m_runTimeFraction2 = 0.0;
                }
            } else {
                FanSpeedRatio = (fanPartLoadRatio * CompOnFlowRatio) + ((1.0 - fanPartLoadRatio) * CompOffFlowRatio);
                m_runTimeFraction1 = fanPartLoadRatio;
                m_runTimeFraction2 = 1.0 - fanPartLoadRatio;
            }
        } else {
            FanSpeedRatio = CompOnFlowRatio;
            m_runTimeFraction1 = fanPartLoadRatio;
            m_runTimeFraction2 = 0.0;
        }

        if (!(HeatingLoad && this->numOfSpeedHeating == 0)) {
            if (this->singleMode == 1) {
                if (this->fanOpMode == fanOpModeEnum::contFanCycCoil) {
                    AverageUnitMassFlow = CompOnMassFlow;
                    FanSpeedRatio = CompOnFlowRatio;
                    m_runTimeFraction1 = 1.0;
                    m_runTimeFraction2 = 0.0;
                } else {
                    AverageUnitMassFlow = fanPartLoadRatio * CompOnMassFlow;
                    FanSpeedRatio = fanPartLoadRatio * CompOnFlowRatio;
                    m_runTimeFraction1 = fanPartLoadRatio;
                    m_runTimeFraction2 = 0.0;
                }
            }
        }

        // If the unitary system is scheduled on or nightime cycle overrides fan schedule. Uses same logic as fan.
        if (this->fanExists) {
            FanOn = false;
            if (ScheduleManager::GetCurrentScheduleValue(this->fanAvailSchedPtr) > 0) FanOn = true;
        } else {
            FanOn = true;
        }
        if (ScheduleManager::GetCurrentScheduleValue(this->sysAvailSchedPtr) > 0.0 &&
            ((FanOn || DataHVACGlobals::TurnFansOn) && !DataHVACGlobals::TurnFansOff)) {
            if (this->controlType == controlTypeEnum::controlTypeSetpoint) {
                // set point based equipment should use VAV terminal units to set the flow.
                // zone equipment needs to set flow since no other device regulates flow (ZoneHVAC /= AirLoopEquipment)
                if (!this->airLoopEquipment) {
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
                if (!this->airLoopEquipment) {
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
                                             Optional_bool HXUnitOn,        // Flag to control HX for HXAssisted Cooling Coil
                                             Optional<Real64> HeatCoilLoad, // Adjusted load to heating coil when SAT exceeds max limit (W)
                                             Optional<Real64> SuppCoilLoad, // Adjusted load to supp heating coil when SAT exceeds max limit (W)
                                             Optional_int_const CompOn      // Determines if compressor is on or off
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the resulting performance of the unitary system given
        // the operating PLR. System output is calculated with respect to zone condition.

        Real64 CoilCoolHeatRat = 0.0; // ratio of cooling to heating PLR for cycling fan RH control

        int OutletNode = this->airOutNode;
        int InletNode = this->airInNode;
        int ZoneNode = this->nodeNumOfControlledZone;

        int CoolingCompOn = 0;
        if (CoolPLR > 0) {
            CoolingCompOn = 1;
            // let logical override compressor status if present (tests if economizer can meet load without compressor)
            if (present(CompOn)) CoolingCompOn = CompOn;
            // for multispeed coils, comp is on IF speed > 1
        } else if (this->coolingSpeedNum > 1) {
            CoolingCompOn = 1;
            // let logical override compressor status if present (tests if economizer can meet load without compressor)
            if (present(CompOn)) CoolingCompOn = CompOn;
        }

        int HeatingCompOn = 0;
        if (HeatPLR > 0) {
            HeatingCompOn = 1;
            // let logical override compressor status if present (tests if economizer can meet load without compressor)
            // probably don't need this for heating mode
            if (present(CompOn)) HeatingCompOn = CompOn;
            CoilCoolHeatRat = min(1.0, CoolPLR / HeatPLR);
        } else {
            CoilCoolHeatRat = 1.0;
        }
        // for multispeed coils, comp is on at PLR=0 IF speed > 1
        if (this->heatingSpeedNum > 1) HeatingCompOn = 1;

        // set the operating flow rate
        if (this->numOfSpeedCooling > 0 || this->numOfSpeedHeating > 0) {
            this->setOnOffMassFlowRate(OnOffAirFlowRatio, max(CoolPLR, HeatPLR));
        } else {
            this->setAverageAirFlow(max(CoolPLR, HeatPLR), OnOffAirFlowRatio);
        }

        // Call the series of components that simulate a Unitary System
        if (this->ATMixerExists) {
            // There is an air terminal mixer
            if (this->ATMixerType == DataHVACGlobals::ATMixer_InletSide) { // if there is an inlet side air terminal mixer
                // set the primary air inlet mass flow rate
                DataLoopNode::Node(this->ATMixerPriNode).MassFlowRate =
                    min(DataLoopNode::Node(this->ATMixerPriNode).MassFlowRateMaxAvail, DataLoopNode::Node(InletNode).MassFlowRate);
                // now calculate the the mixer outlet conditions (and the secondary air inlet flow rate)
                // the mixer outlet flow rate has already been set above (it is the "inlet" node flow rate)
                SingleDuct::SimATMixer(this->ATMixerName, FirstHVACIteration, this->ATMixerIndex);
            }
        }

        if (this->fanExists && this->fanPlace == fanPlaceEnum::blowThru) {
            if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->fanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->fanIndex, FanSpeedRatio);
            }
        }

        if (this->coolingCoilUpstream) {

            if (this->coolCoilExists) {
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }
            if (this->heatCoilExists) {
                // operate the heating coil without regard to coil outlet temperature
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad);
                if (DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp > this->designMaxOutletTemp && !this->simASHRAEModel) {
                    Real64 MDotAir = DataLoopNode::Node(this->heatCoilInletNodeNum).MassFlowRate;
                    Real64 CpAirIn = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->heatCoilInletNodeNum).HumRat,
                                                                    DataLoopNode::Node(this->heatCoilInletNodeNum).Temp);
                    Real64 CpAirOut = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->heatCoilOutletNodeNum).HumRat,
                                                                     DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp);
                    Real64 CpAir = (CpAirIn + CpAirOut) / 2;
                    Real64 HCDeltaT = this->designMaxOutletTemp - DataLoopNode::Node(this->heatCoilInletNodeNum).Temp;
                    Real64 MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
                    this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad);
                    if (present(HeatCoilLoad)) HeatCoilLoad = MaxHeatCoilLoad;
                }
            }

            // If blow thru fan is used, the fan must be simulated after coil sets OnOffFanPartLoadFraction
            if (this->fanExists && this->fanPlace == fanPlaceEnum::blowThru && DataHVACGlobals::OnOffFanPartLoadFraction < 1.0) {
                if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[this->fanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
                } else {
                    Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->fanIndex, FanSpeedRatio);
                }
                if (this->coolCoilExists) {
                    this->calcUnitaryCoolingSystem(
                        AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
                }
                if (this->heatCoilExists) {
                    this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad);
                    if (DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp > this->designMaxOutletTemp && !this->simASHRAEModel) {
                        Real64 MDotAir = DataLoopNode::Node(this->heatCoilInletNodeNum).MassFlowRate;
                        Real64 CpAirIn = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->heatCoilInletNodeNum).HumRat,
                                                                        DataLoopNode::Node(this->heatCoilInletNodeNum).Temp);
                        Real64 CpAirOut = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->heatCoilOutletNodeNum).HumRat,
                                                                         DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp);
                        Real64 CpAir = (CpAirIn + CpAirOut) / 2;
                        Real64 HCDeltaT = this->designMaxOutletTemp - DataLoopNode::Node(this->heatCoilInletNodeNum).Temp;
                        Real64 MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
                        this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad);
                    }
                }
            }

        } else {

            if (this->heatCoilExists) {
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad);
                if (DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp > this->designMaxOutletTemp && !this->simASHRAEModel) {
                    Real64 MDotAir = DataLoopNode::Node(this->heatCoilInletNodeNum).MassFlowRate;
                    Real64 CpAirIn = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->heatCoilInletNodeNum).HumRat,
                                                                    DataLoopNode::Node(this->heatCoilInletNodeNum).Temp);
                    Real64 CpAirOut = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->heatCoilOutletNodeNum).HumRat,
                                                                     DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp);
                    Real64 CpAir = (CpAirIn + CpAirOut) / 2;
                    Real64 HCDeltaT = this->designMaxOutletTemp - DataLoopNode::Node(this->heatCoilInletNodeNum).Temp;
                    Real64 MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
                    this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad);
                }
            }
            if (this->coolCoilExists) {
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }

            // If blow thru fan is used, the fan must be simulated after coil sets OnOffFanPartLoadFraction
            if (this->fanExists && this->fanPlace == fanPlaceEnum::blowThru && DataHVACGlobals::OnOffFanPartLoadFraction < 1.0) {
                if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    HVACFan::fanObjs[this->fanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
                } else {
                    Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->fanIndex, FanSpeedRatio);
                }
                if (this->heatCoilExists) {
                    this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, HeatCoilLoad);
                    if (DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp > this->designMaxOutletTemp && !this->simASHRAEModel) {
                        Real64 MDotAir = DataLoopNode::Node(this->heatCoilInletNodeNum).MassFlowRate;
                        Real64 CpAirIn = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->heatCoilInletNodeNum).HumRat,
                                                                        DataLoopNode::Node(this->heatCoilInletNodeNum).Temp);
                        Real64 CpAirOut = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->heatCoilOutletNodeNum).HumRat,
                                                                         DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp);
                        Real64 CpAir = (CpAirIn + CpAirOut) / 2;
                        Real64 HCDeltaT = this->designMaxOutletTemp - DataLoopNode::Node(this->heatCoilInletNodeNum).Temp;
                        Real64 MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
                        this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, HeatPLR, HeatingCompOn, OnOffAirFlowRatio, MaxHeatCoilLoad);
                    }
                }
                if (this->coolCoilExists) {
                    this->calcUnitaryCoolingSystem(
                        AirLoopNum, FirstHVACIteration, CoolPLR, CoolingCompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
                }
            }
        }

        if (this->fanExists && this->fanPlace == fanPlaceEnum::drawThru) {
            if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->fanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->fanIndex, FanSpeedRatio);
            }
        }

        Real64 SuppPLR = this->suppHeatPartLoadFrac;
        if (this->suppCoilExists) {
            this->calcUnitarySuppHeatingSystem(FirstHVACIteration, SuppPLR, SuppCoilLoad);
            if ((DataLoopNode::Node(this->suppCoilAirOutletNode).Temp > this->designMaxOutletTemp) && SuppPLR > 0.0 && !this->simASHRAEModel) {
                Real64 MDotAir = DataLoopNode::Node(this->suppCoilAirInletNode).MassFlowRate;
                Real64 CpAirIn = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->suppCoilAirInletNode).HumRat,
                                                                DataLoopNode::Node(this->suppCoilAirInletNode).Temp);
                Real64 CpAirOut = Psychrometrics::PsyCpAirFnWTdb(DataLoopNode::Node(this->suppCoilAirOutletNode).HumRat,
                                                                 DataLoopNode::Node(this->suppCoilAirOutletNode).Temp);
                Real64 CpAir = (CpAirIn + CpAirOut) / 2;
                Real64 HCDeltaT = max(0.0, this->designMaxOutletTemp - DataLoopNode::Node(this->suppCoilAirInletNode).Temp);
                Real64 MaxHeatCoilLoad = MDotAir * CpAir * HCDeltaT;
                this->calcUnitarySuppHeatingSystem(FirstHVACIteration, SuppPLR, MaxHeatCoilLoad);
                if (present(SuppCoilLoad)) SuppCoilLoad = MaxHeatCoilLoad;
            }
        }

        // If there is a supply side air terminal mixer, calculate its output
        if (this->ATMixerExists) {
            if (this->ATMixerType == DataHVACGlobals::ATMixer_SupplySide) {
                SingleDuct::SimATMixer(this->ATMixerName, FirstHVACIteration, this->ATMixerIndex);
            }
        }

        calculateCapacity(SensOutput, LatOutput);
    }

    void UnitarySys::calculateCapacity(Real64 &SensOutput, Real64 &LatOutput)
    {

        // Check delta T (outlet to space), IF positive use space HumRat ELSE outlet humrat to calculate
        // sensible capacity as MdotDeltaH at constant humidity ratio
        Real64 MinHumRatio = DataLoopNode::Node(this->nodeNumOfControlledZone).HumRat;
        int OutletNode = this->airOutNode;
        Real64 AirMassFlow = DataLoopNode::Node(OutletNode).MassFlowRate;
        Real64 ZoneTemp = DataLoopNode::Node(this->nodeNumOfControlledZone).Temp;
        Real64 ZoneHumRat = DataLoopNode::Node(this->nodeNumOfControlledZone).HumRat;
        if (DataLoopNode::Node(OutletNode).Temp < ZoneTemp) MinHumRatio = DataLoopNode::Node(OutletNode).HumRat;

        // calculate sensible load met
        if (this->ATMixerExists) {
            if (this->ATMixerType == DataHVACGlobals::ATMixer_SupplySide) {
                // Air terminal supply side mixer
                int ATMixOutNode = this->ATMixerOutNode;
                SensOutput =
                    DataLoopNode::Node(ATMixOutNode).MassFlowRate * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(ATMixOutNode).Temp, MinHumRatio) -
                                                                     Psychrometrics::PsyHFnTdbW(ZoneTemp, MinHumRatio));
                if (this->humidistat) {
                    //   Calculate latent load met (at constant temperature)
                    LatOutput = DataLoopNode::Node(ATMixOutNode).MassFlowRate *
                                    (Psychrometrics::PsyHFnTdbW(ZoneTemp, DataLoopNode::Node(ATMixOutNode).HumRat) -
                                     Psychrometrics::PsyHFnTdbW(ZoneTemp, ZoneHumRat)) -
                                this->latLoadLoss;
                } else {
                    LatOutput = 0.0;
                }
            } else {
                // Air terminal inlet side mixer
                SensOutput = AirMassFlow * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, MinHumRatio) -
                                            Psychrometrics::PsyHFnTdbW(ZoneTemp, MinHumRatio));
                if (this->humidistat) {
                    //   Calculate latent load met (at constant temperature)
                    LatOutput = AirMassFlow * (Psychrometrics::PsyHFnTdbW(ZoneTemp, DataLoopNode::Node(OutletNode).HumRat) -
                                               Psychrometrics::PsyHFnTdbW(ZoneTemp, ZoneHumRat)) -
                                this->latLoadLoss;
                } else {
                    LatOutput = 0.0;
                }
            }
        } else {
            // Calculate sensible load met (at constant humidity ratio)
            SensOutput = AirMassFlow * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, MinHumRatio) -
                                        Psychrometrics::PsyHFnTdbW(ZoneTemp, MinHumRatio)) -
                         this->senLoadLoss;

            if (this->humidistat) {

                //   Calculate latent load met (at constant temperature)
                LatOutput = AirMassFlow * (Psychrometrics::PsyHFnTdbW(ZoneTemp, DataLoopNode::Node(OutletNode).HumRat) -
                                           Psychrometrics::PsyHFnTdbW(ZoneTemp, ZoneHumRat)) -
                            this->latLoadLoss;
            } else {
                LatOutput = 0.0;
            }
        }
        this->sensibleLoadMet = SensOutput;
        this->latentLoadMet = LatOutput;
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
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages unitary cooling system component simulation.

        // Using/Aliasing
        // using DXCoils::SimDXCoil;
        // using DXCoils::SimDXCoilMultiMode;
        // using DXCoils::SimDXCoilMultiSpeed;
        // using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        // using PackagedThermalStorageCoil::SimTESCoil;
        // using UserDefinedComponents::SimCoilUserDefined;
        // using VariableSpeedCoils::SimVariableSpeedCoils;
        // using WaterCoils::SimulateWaterCoilComponents;
        // using WaterToAirHeatPump::SimWatertoAirHP;
        // using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;

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
        std::string CompName = this->coolingCoilName;
        int CompIndex = this->coolingCoilIndex;
        Real64 CoilPLR = 1.0;
        if (this->condenserNodeNum != 0) {
            OutdoorPressure = DataLoopNode::Node(this->condenserNodeNum).Press;
            // IF node is not connected to anything, pressure = default, use weather data
            if (OutdoorPressure == DataLoopNode::DefaultNodeValues.Press) {
                OutsideDryBulbTemp = DataEnvironment::OutDryBulbTemp;
                //      OutdoorHumRat   = OutHumRat
                //      OutdoorPressure = OutBaroPress
                //      OutdoorWetBulb  = OutWetBulbTemp
            } else {
                OutsideDryBulbTemp = DataLoopNode::Node(this->condenserNodeNum).Temp;
                //      OutdoorHumRat   = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%HumRat
                //      OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb,OutdoorHumRat,OutdoorPressure,RoutineName)
            }
        } else {
            OutsideDryBulbTemp = DataEnvironment::OutDryBulbTemp;
            //    OutdoorHumRat   = OutHumRat
            //    OutdoorPressure = OutBaroPress
            //    OutdoorWetBulb  = OutWetBulbTemp
        }
        //  PartLoadRatio = UnitarySystem(UnitarySysNum)%CoolingPartLoadFrac

        {
            auto const SELECT_CASE_var(this->coolingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingSingleSpeed) { // Coil:Cooling:DX:SingleSpeed

                DXCoils::SimDXCoil(
                    blankString, CompOn, FirstHVACIteration, CompIndex, this->fanOpMode, PartLoadRatio, OnOffAirFlowRatio, CoilCoolHeatRat);
                this->coolCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if ((SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingHXAssisted) ||
                       (SELECT_CASE_var == DataHVACGlobals::CoilWater_CoolingHXAssisted)) {

                if (this->coolingCoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted) {
                    Real64 mdot =
                        min(DataLoopNode::Node(this->coolCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->maxCoolCoilFluidFlow * PartLoadRatio);
                    DataLoopNode::Node(this->coolCoilFluidInletNode).MassFlowRate = mdot;
                }
                HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(
                    blankString, FirstHVACIteration, CompOn, PartLoadRatio, CompIndex, this->fanOpMode, HXUnitOn, OnOffAirFlowRatio, economizerFlag);
                if (this->coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted)
                    this->coolCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) { // Coil:Cooling:DX:TwoSpeed
                // formerly (v3 and beyond)COIL:DX:MULTISPEED:COOLINGEMPIRICAL

                DXCoils::SimDXCoilMultiSpeed(blankString, this->coolingSpeedRatio, this->coolingCycRatio, CompIndex);
                if (this->coolingSpeedRatio > 0.0) {
                    this->coolCompPartLoadRatio = this->coolingSpeedRatio * double(CompOn);
                } else {
                    this->coolCompPartLoadRatio = this->coolingCycRatio * double(CompOn);
                }

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) { // Coil:Cooling:DX:Multispeed

                if (OutsideDryBulbTemp > this->minOATCompressorCooling) {
                    DXCoils::SimDXCoilMultiSpeed(CompName,
                                                 this->coolingSpeedRatio,
                                                 this->coolingCycRatio,
                                                 CompIndex,
                                                 this->coolingSpeedNum,
                                                 this->fanOpMode,
                                                 CompOn,
                                                 this->singleMode);
                    if (this->coolingSpeedNum > 1) {
                        if (this->singleMode == 0) {
                            this->coolCompPartLoadRatio = double(CompOn);
                        } else {
                            this->coolCompPartLoadRatio = this->coolingCycRatio * double(CompOn);
                        }
                    } else {
                        this->coolCompPartLoadRatio = this->coolingCycRatio * double(CompOn);
                    }
                } else {
                    DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, 0.0, CompIndex, this->coolingSpeedNum, this->fanOpMode, CompOn);
                    this->coolCompPartLoadRatio = 0.0;
                }

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                // formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL

                DXCoils::SimDXCoilMultiMode(
                    CompName, CompOn, FirstHVACIteration, PartLoadRatio, this->dehumidificationMode, CompIndex, this->fanOpMode);
                this->coolCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) {

                HeatingActive = false; // set to arbitrary value on entry to function
                CoolingActive = false; // set to arbitrary value on entry to function

                UserDefinedComponents::SimCoilUserDefined(CompName, CompIndex, AirLoopNum, HeatingActive, CoolingActive);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWater) || (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterDetailed)) {

                if (this->coolCoilWaterFlowRatio == 0.0) {
                    mdot = this->maxCoolCoilFluidFlow * PartLoadRatio;
                } else {
                    mdot = this->coolCoilWaterFlowRatio * this->maxCoolCoilFluidFlow;
                }
                DataLoopNode::Node(this->coolCoilFluidInletNode).MassFlowRate = mdot;
                WaterCoils::SimulateWaterCoilComponents(
                    CompName, FirstHVACIteration, this->coolingCoilIndex, QActual, this->fanOpMode, PartLoadRatio);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {
                if (this->coolingSpeedNum > 1) {
                    CoilPLR = 1.0;
                } else {
                    CoilPLR = PartLoadRatio;
                }
                VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                          CompIndex,
                                                          this->fanOpMode,
                                                          this->maxONOFFCyclesperHour,
                                                          this->HPTimeConstant,
                                                          this->fanDelayTime,
                                                          CompOn,
                                                          CoilPLR,
                                                          this->coolingSpeedNum,
                                                          this->coolingSpeedRatio,
                                                          this->coolingCoilSensDemand,
                                                          this->coolingCoilLatentDemand,
                                                          OnOffAirFlowRatio);
                if (this->coolingSpeedNum > 1) {
                    this->coolCompPartLoadRatio = 1.0;
                } else {
                    this->coolCompPartLoadRatio = PartLoadRatio * double(CompOn);
                }

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {

                if (PartLoadRatio > 0.0 && this->WSHPRuntimeFrac > 0.0 && this->fanOpMode == fanOpModeEnum::cycFanCycCoil) {
                    DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / this->WSHPRuntimeFrac;
                }

                WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                this->coolingCoilIndex,
                                                                this->coolingCoilSensDemand,
                                                                this->coolingCoilLatentDemand,
                                                                this->fanOpMode,
                                                                this->WSHPRuntimeFrac,
                                                                this->maxONOFFCyclesperHour,
                                                                this->HPTimeConstant,
                                                                this->fanDelayTime,
                                                                CompOn,
                                                                PartLoadRatio,
                                                                FirstHVACIteration);

                this->coolCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterToAirHP) {

                this->heatPumpRunFrac(PartLoadRatio, errFlag, this->WSHPRuntimeFrac);

                if (PartLoadRatio > 0.0 && this->WSHPRuntimeFrac > 0.0 && this->fanOpMode == fanOpModeEnum::cycFanCycCoil) {
                    DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / this->WSHPRuntimeFrac;
                }

                WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                    this->coolingCoilIndex,
                                                    this->maxCoolAirMassFlow,
                                                    this->fanOpMode,
                                                    FirstHVACIteration,
                                                    this->WSHPRuntimeFrac,
                                                    this->maxONOFFCyclesperHour,
                                                    this->HPTimeConstant,
                                                    this->fanDelayTime,
                                                    this->initHeatPump,
                                                    this->coolingCoilSensDemand,
                                                    this->coolingCoilLatentDemand,
                                                    CompOn,
                                                    PartLoadRatio);

                this->coolCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {

                PackagedThermalStorageCoil::SimTESCoil(CompName, this->coolingCoilIndex, this->fanOpMode, this->TESOpMode, PartLoadRatio);
            }
        }

        this->coolingPartLoadFrac = PartLoadRatio;
    }

    void UnitarySys::calcUnitaryHeatingSystem(int const AirLoopNum,               // index to air loop
                                              bool const FirstHVACIteration,      // True when first HVAC iteration
                                              Real64 const PartLoadRatio,         // coil operating part-load ratio
                                              int const CompOn,                   // compressor control (0=off, 1=on)
                                              Real64 const OnOffAirFlowRatio,     // ratio of on to off flow rate
                                              Optional<Real64 const> HeatCoilLoad // adjusted heating coil load if outlet temp exceeds max (W)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   February 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages unitary heating system component simulation.

        // Using/Aliasing
        // using DXCoils::SimDXCoil;
        // using DXCoils::SimDXCoilMultiSpeed;
        // using HeatingCoils::SimulateHeatingCoilComponents;
        // using SteamCoils::SimulateSteamCoilComponents;
        // using UserDefinedComponents::SimCoilUserDefined;
        // using VariableSpeedCoils::SimVariableSpeedCoils;
        // using WaterCoils::SimulateWaterCoilComponents;
        // using WaterToAirHeatPump::SimWatertoAirHP;
        // using WaterToAirHeatPumpSimple::SimWatertoAirHPSimple;

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

        std::string CompName = this->heatingCoilName;
        Real64 dummy = 0.0;
        Real64 HeatPLR = 1.0;
        if (this->condenserNodeNum != 0) {
            OutdoorPressure = DataLoopNode::Node(this->condenserNodeNum).Press;
            // IF node is not connected to anything, pressure = default, use weather data
            if (OutdoorPressure == DataLoopNode::DefaultNodeValues.Press) {
                OutsideDryBulbTemp = DataEnvironment::OutDryBulbTemp;
                //      OutdoorHumRat   = OutHumRat
                //      OutdoorPressure = OutBaroPress
                //      OutdoorWetBulb  = OutWetBulbTemp
            } else {
                OutsideDryBulbTemp = DataLoopNode::Node(this->condenserNodeNum).Temp;
                //      OutdoorHumRat   = Node(UnitarySystem(UnitarySysNum)%CondenserNodeNum)%HumRat
                //      OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb,OutdoorHumRat,OutdoorPressure,RoutineName)
            }
        } else {
            OutsideDryBulbTemp = DataEnvironment::OutDryBulbTemp;
            //    OutdoorHumRat   = OutHumRat
            //    OutdoorPressure = OutBaroPress
            //    OutdoorWetBulb  = OutWetBulbTemp
        }

        {
            auto const SELECT_CASE_var(this->heatingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_HeatingEmpirical) { // COIL:HEATING:DX:SINGLESPEED

                DXCoils::SimDXCoil(CompName, CompOn, FirstHVACIteration, this->heatingCoilIndex, this->fanOpMode, PartLoadRatio, OnOffAirFlowRatio);
                this->heatCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) {

                HeatingActive = false; // set to arbitrary value on entry to function
                CoolingActive = true;  // set to arbitrary value on entry to function

                UserDefinedComponents::SimCoilUserDefined(CompName, this->heatingCoilIndex, AirLoopNum, HeatingActive, CoolingActive);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric)) {
                if (present(HeatCoilLoad)) {
                    HeatingCoils::SimulateHeatingCoilComponents(
                        CompName, FirstHVACIteration, HeatCoilLoad, this->heatingCoilIndex, _, false, this->fanOpMode, PartLoadRatio);
                } else {
                    HeatingCoils::SimulateHeatingCoilComponents(CompName,
                                                                FirstHVACIteration,
                                                                this->designHeatingCapacity * PartLoadRatio,
                                                                this->heatingCoilIndex,
                                                                _,
                                                                false,
                                                                this->fanOpMode,
                                                                PartLoadRatio);
                }
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater) {
                if (present(HeatCoilLoad)) {
                    HeatingCoils::SimulateHeatingCoilComponents(
                        CompName, FirstHVACIteration, HeatCoilLoad, this->heatingCoilIndex, _, false, this->fanOpMode, PartLoadRatio);
                } else {
                    HeatingCoils::SimulateHeatingCoilComponents(CompName,
                                                                FirstHVACIteration,
                                                                this->designHeatingCapacity * PartLoadRatio,
                                                                this->heatingCoilIndex,
                                                                _,
                                                                false,
                                                                this->fanOpMode,
                                                                PartLoadRatio);
                }

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) {

                if (OutsideDryBulbTemp > this->minOATCompressorHeating) {
                    DXCoils::SimDXCoilMultiSpeed(CompName,
                                                 this->heatingSpeedRatio,
                                                 this->heatingCycRatio,
                                                 this->heatingCoilIndex,
                                                 this->heatingSpeedNum,
                                                 this->fanOpMode,
                                                 CompOn,
                                                 this->singleMode);
                    this->heatCompPartLoadRatio = PartLoadRatio * double(CompOn);
                } else {
                    DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, 0.0, this->heatingCoilIndex, this->heatingSpeedNum, this->fanOpMode, CompOn);
                    this->heatCompPartLoadRatio = 0.0;
                }

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {
                HeatingCoils::SimulateHeatingCoilComponents(
                    CompName, FirstHVACIteration, _, 0, _, _, this->fanOpMode, PartLoadRatio, this->heatingSpeedNum, this->heatingSpeedRatio);
                this->heatingCycRatio = PartLoadRatio;
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {
                if (this->heatCoilWaterFlowRatio == 0.0) {
                    mdot = this->maxHeatCoilFluidFlow * PartLoadRatio;
                } else {
                    mdot = this->heatCoilWaterFlowRatio * this->maxHeatCoilFluidFlow;
                }
                DataLoopNode::Node(this->heatCoilFluidInletNode).MassFlowRate = mdot;
                WaterCoils::SimulateWaterCoilComponents(
                    CompName, FirstHVACIteration, this->heatingCoilIndex, QActual, this->fanOpMode, PartLoadRatio);
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {
                // this same CALL is made in the steam coil calc routine
                mdot = min(DataLoopNode::Node(this->heatCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->maxHeatCoilFluidFlow * PartLoadRatio);
                DataLoopNode::Node(this->heatCoilFluidInletNode).MassFlowRate = mdot;
                SteamCoils::SimulateSteamCoilComponents(CompName,
                                                        FirstHVACIteration,
                                                        this->heatingCoilIndex,
                                                        this->designHeatingCapacity * PartLoadRatio,
                                                        _,
                                                        this->fanOpMode,
                                                        PartLoadRatio);

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit)) {

                if (this->heatingSpeedNum > 1) {
                    HeatPLR = 1.0;
                } else {
                    HeatPLR = PartLoadRatio;
                }
                VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                          this->heatingCoilIndex,
                                                          this->fanOpMode,
                                                          this->maxONOFFCyclesperHour,
                                                          this->HPTimeConstant,
                                                          this->fanDelayTime,
                                                          CompOn,
                                                          HeatPLR,
                                                          this->heatingSpeedNum,
                                                          this->heatingSpeedRatio,
                                                          this->heatingCoilSensDemand,
                                                          dummy,
                                                          OnOffAirFlowRatio);
                if (this->heatingSpeedNum > 1) {
                    this->heatCompPartLoadRatio = 1.0;
                } else {
                    this->heatCompPartLoadRatio = PartLoadRatio * double(CompOn);
                }
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) {

                if (PartLoadRatio > 0.0 && this->WSHPRuntimeFrac > 0.0 && this->fanOpMode == fanOpModeEnum::cycFanCycCoil) {
                    DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / this->WSHPRuntimeFrac;
                }

                WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                this->heatingCoilIndex,
                                                                this->heatingCoilSensDemand,
                                                                dummy,
                                                                this->fanOpMode,
                                                                this->WSHPRuntimeFrac,
                                                                this->maxONOFFCyclesperHour,
                                                                this->HPTimeConstant,
                                                                this->fanDelayTime,
                                                                CompOn,
                                                                PartLoadRatio,
                                                                FirstHVACIteration);
                this->heatCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHP) {

                this->heatPumpRunFrac(PartLoadRatio, errFlag, this->WSHPRuntimeFrac);

                if (PartLoadRatio > 0.0 && this->WSHPRuntimeFrac > 0.0 && this->fanOpMode == fanOpModeEnum::cycFanCycCoil) {
                    DataHVACGlobals::OnOffFanPartLoadFraction = PartLoadRatio / this->WSHPRuntimeFrac;
                }

                WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                    this->heatingCoilIndex,
                                                    this->maxHeatAirMassFlow,
                                                    this->fanOpMode,
                                                    FirstHVACIteration,
                                                    this->WSHPRuntimeFrac,
                                                    this->maxONOFFCyclesperHour,
                                                    this->HPTimeConstant,
                                                    this->fanDelayTime,
                                                    this->initHeatPump,
                                                    this->heatingCoilSensDemand,
                                                    dummy,
                                                    CompOn,
                                                    PartLoadRatio);
                this->heatCompPartLoadRatio = PartLoadRatio * double(CompOn);

            } else {
                ShowFatalError("CalcUnitaryHeatingSystem: Invalid Unitary System coil type = " +
                               DataHVACGlobals::cAllCoilTypes(this->heatingCoilType_Num));
            }
        }

        this->heatingPartLoadFrac = PartLoadRatio;
    }

    void UnitarySys::calcUnitarySuppHeatingSystem(bool const FirstHVACIteration,      // True when first HVAC iteration
                                                  Real64 const PartLoadRatio,         // coil operating part-load ratio
                                                  Optional<Real64 const> SuppCoilLoad // adjusted supp coil load when outlet temp exceeds max (W)
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
        std::string CompName = this->suppHeatCoilName;
        if (DataEnvironment::OutDryBulbTemp <= this->maxOATSuppHeat || (MoistureLoad < 0.0 && this->coolingPartLoadFrac > 0.0)) {
            if (present(SuppCoilLoad)) {
                SuppHeatCoilLoad = SuppCoilLoad;
            } else {
                SuppHeatCoilLoad = this->designSuppHeatingCapacity * PartLoadRatio;
            }
        } else {
            SuppHeatCoilLoad = 0.0;
        }

        {
            auto const SELECT_CASE_var(this->suppHeatCoilType_Num);

            if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) || (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric)) {
                {
                    auto const SELECT_CASE_var1(this->controlType);
                    if (SELECT_CASE_var1 == controlTypeEnum::controlTypeSetpoint) {
                        HeatingCoils::SimulateHeatingCoilComponents(
                            CompName, FirstHVACIteration, _, this->suppHeatCoilIndex, _, true, this->fanOpMode, PartLoadRatio);
                    } else {
                        HeatingCoils::SimulateHeatingCoilComponents(
                            CompName, FirstHVACIteration, SuppHeatCoilLoad, this->suppHeatCoilIndex, _, true, this->fanOpMode, PartLoadRatio);
                    }
                }
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater) {
                HeatingCoils::SimulateHeatingCoilComponents(
                    CompName, FirstHVACIteration, SuppHeatCoilLoad, this->suppHeatCoilIndex, _, true, this->fanOpMode, PartLoadRatio);

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {
                if (present(SuppCoilLoad)) {
                    if (SuppHeatCoilLoad > 0.0) {
                        // see if HW coil has enough capacity to meet the load
                        mdot = min(DataLoopNode::Node(this->suppCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->maxSuppCoilFluidFlow);
                        DataLoopNode::Node(this->suppCoilFluidInletNode).MassFlowRate = mdot;
                        //     simulate water coil to find operating capacity
                        WaterCoils::SimulateWaterCoilComponents(
                            this->suppHeatCoilName, FirstHVACIteration, this->suppHeatCoilIndex, QActual, this->fanOpMode, PartLoadRatio);
                        if (QActual > SuppHeatCoilLoad) {
                            Par[1] = double(1);
                            if (FirstHVACIteration) {
                                Par[2] = 1.0;
                            } else {
                                Par[2] = 0.0;
                            }
                            Par[3] = SuppHeatCoilLoad;
                            Par[4] = 1.0; // SuppHeatingCoilFlag
                            Par[5] = 1.0; // Load based control
                            // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->hotWaterHeatingCoilResidual, 0.0, 1.0, &Par);
                            PartLoadFrac = 0.0; // temporary while SolveRoot is commented out
                            this->suppHeatPartLoadFrac = PartLoadFrac;
                        } else {
                            this->suppHeatPartLoadFrac = 1.0;
                        }
                    }
                } else {
                    mdot = min(DataLoopNode::Node(this->suppCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->maxSuppCoilFluidFlow * PartLoadRatio);
                    DataLoopNode::Node(this->suppCoilFluidInletNode).MassFlowRate = mdot;

                    WaterCoils::SimulateWaterCoilComponents(
                        CompName, FirstHVACIteration, this->suppHeatCoilIndex, QActual, this->fanOpMode, PartLoadRatio);
                }
            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {
                mdot = min(DataLoopNode::Node(this->suppCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->maxSuppCoilFluidFlow * PartLoadRatio);
                DataLoopNode::Node(this->suppCoilFluidInletNode).MassFlowRate = mdot;
                SteamCoils::SimulateSteamCoilComponents(
                    CompName, FirstHVACIteration, this->suppHeatCoilIndex, SuppHeatCoilLoad, _, this->fanOpMode, PartLoadRatio);

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
        int OutletNode = this->coolCoilOutletNodeNum;
        int InletNode = this->coolCoilInletNodeNum;
        int ControlNode = this->systemCoolControlNodeNum;
        Real64 DesOutTemp = this->desiredOutletTemp;
        Real64 DesOutHumRat = this->desiredOutletHumRat;
        int CoilType_Num = this->coolingCoilType_Num;
        Real64 LoopDXCoilMaxRTFSave = DataAirLoop::LoopDXCoilRTF;
        DataAirLoop::LoopDXCoilRTF = 0.0;

        std::string CompName = this->coolingCoilName;
        int FanOpMode = this->fanOpMode;
        Real64 SpeedRatio = 0.0;
        int SpeedNum = 0;
        Real64 CycRatio = 0.0;
        Real64 PartLoadFrac = 0.0;
        int DehumidMode = 0;
        bool SensibleLoad = false;
        bool LatentLoad = false;
        Real64 WSHPRuntimeFrac = 0.0;
        Real64 dummy = 0.0;
        Real64 SensLoad = 0.0;
        int SolFla = 0;
        int SolFlaLat = 0;
        Real64 NoLoadTempOut = 0.0;
        Real64 NoLoadHumRatOut = 0.0;
        Real64 OnOffAirFlowRatio = 0.0; // Autodesk:Init Patch to prevent use uninitialized in calls to SimVariableSpeedCoils

        if (this->condenserNodeNum != 0) {
            OutdoorDryBulb = DataLoopNode::Node(this->condenserNodeNum).Temp;
        } else {
            OutdoorDryBulb = DataEnvironment::OutDryBulbTemp;
        }

        // Check the dehumidification control type. IF it's multimode, turn off the HX to find the sensible PLR. Then check to
        // see if the humidity load is met without the use of the HX. Always run the HX for the other modes.
        if (this->dehumidControlType_Num != dehumCtrlTypeEnum::dehumidControl_Multimode) {
            HXUnitOn = true;
        } else {
            HXUnitOn = false;
        }

        // IF there is a fault of coil SAT Sensor (zrp_Nov2016)
        if (this->faultyCoilSATFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            // calculate the sensor offset using fault information
            int FaultIndex = this->faultyCoilSATIndex;
            this->faultyCoilSATOffset = FaultsManager::FaultsCoilSATSensor(FaultIndex).CalFaultOffsetAct();
            // update the DesOutTemp
            DesOutTemp -= this->faultyCoilSATOffset;
        }

        // IF DXCoolingSystem is scheduled on and there is flow
        if ((ScheduleManager::GetCurrentScheduleValue(this->sysAvailSchedPtr) > 0.0) &&
            ScheduleManager::GetCurrentScheduleValue(this->coolingCoilAvailSchPtr) > 0.0 &&
            (DataLoopNode::Node(InletNode).MassFlowRate > MinAirMassFlow)) {

            // Determine if there is a sensible load on this system
            if (DataLoopNode::Node(InletNode).Temp - DesOutTemp > DataHVACGlobals::TempControlTol) SensibleLoad = true;
            // if a heat pump and other coil is on, disable this coil
            if (this->heatPump && this->heatingPartLoadFrac > 0.0) SensibleLoad = false;

            // Determine if there is a latent load on this system - for future use to serve latent-only loads
            if (DataLoopNode::Node(InletNode).HumRat > DesOutHumRat) LatentLoad = true;

            // disable latent dehumidification if there is no sensible load and latent only is not allowed
            if (this->runOnLatentOnlyWithSensible && !SensibleLoad) LatentLoad = false;

            // disable compressor if OAT is below minimum outdoor temperature
            if (OutdoorDryBulb < this->minOATCompressorCooling) {
                SensibleLoad = false;
                LatentLoad = false;
            }

            // IF DXCoolingSystem runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
            // Multimode coil will switch to enhanced dehumidification IF available and needed, but it
            // still runs to meet the sensible load. Multimode applies to Multimode or HXAssistedCooling coils.
            if ((SensibleLoad && this->runOnSensibleLoad) || (LatentLoad && this->runOnLatentLoad)) {
                // calculate sensible PLR, don't care IF latent is true here but need to gaurd for
                // when LatentLoad=TRUE and SensibleLoad=FALSE
                ReqOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                            (Psychrometrics::PsyHFnTdbW(DesOutTemp, DataLoopNode::Node(OutletNode).HumRat) -
                             Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(OutletNode).HumRat));

                PartLoadFrac = 0.0;
                CompOn = 0;

                if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL
                    this->compPartLoadRatio = PartLoadFrac;

                    DXCoils::SimDXCoil(CompName, On, FirstHVACIteration, this->coolingCoilIndex, FanOpMode, PartLoadFrac);

                } else if ((CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) ||
                           (CoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted)) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

                    if (this->coolCoilFluidInletNode > 0) DataLoopNode::Node(this->coolCoilFluidInletNode).MassFlowRate = 0.0;

                    HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(
                        CompName, FirstHVACIteration, On, PartLoadFrac, this->coolingCoilIndex, FanOpMode, HXUnitOn, _, economizerFlag);
                    if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) this->compPartLoadRatio = PartLoadFrac;
                } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                    DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, PartLoadFrac, this->coolingCoilIndex);

                } else if (CoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                    this->simMultiSpeedCoils(AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, CoolingCoil);

                } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                           (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {

                    this->coolingCoilSensDemand = ReqOutput;
                    VariableSpeedCoils::SimVariableSpeedCoils("",
                                                              this->coolingCoilIndex,
                                                              FanOpMode,
                                                              this->maxONOFFCyclesperHour,
                                                              this->HPTimeConstant,
                                                              this->fanDelayTime,
                                                              CompOn,
                                                              CycRatio,
                                                              SpeedNum,
                                                              SpeedRatio,
                                                              SensLoad,
                                                              dummy,
                                                              OnOffAirFlowRatio);

                } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {

                    DXCoils::SimDXCoilMultiMode(CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, this->coolingCoilIndex, FanOpMode);
                    this->compPartLoadRatio = PartLoadFrac;
                } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWater) ||
                           (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed)) { // COIL:COOLING:WATER

                    WaterCoils::SimulateWaterCoilComponents(CompName, FirstHVACIteration, this->coolingCoilIndex, _, this->fanOpMode, PartLoadFrac);

                } else if (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {

                    WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                    this->coolingCoilIndex,
                                                                    ReqOutput,
                                                                    dummy,
                                                                    FanOpMode,
                                                                    WSHPRuntimeFrac,
                                                                    this->maxONOFFCyclesperHour,
                                                                    this->HPTimeConstant,
                                                                    this->fanDelayTime,
                                                                    0,
                                                                    PartLoadFrac,
                                                                    FirstHVACIteration);
                    this->coolingCoilSensDemand = 0.0;

                } else if (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP) {

                    WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                        this->coolingCoilIndex,
                                                        this->maxCoolAirMassFlow,
                                                        FanOpMode,
                                                        FirstHVACIteration,
                                                        WSHPRuntimeFrac,
                                                        this->maxONOFFCyclesperHour,
                                                        this->HPTimeConstant,
                                                        this->fanDelayTime,
                                                        this->initHeatPump,
                                                        ReqOutput,
                                                        dummy,
                                                        0,
                                                        PartLoadFrac);

                } else if (CoilType_Num == DataHVACGlobals::Coil_UserDefined) {

                    HeatingActive = false; // set to arbitrary value on entry to function
                    CoolingActive = true;  // set to arbitrary value on entry to function
                    UserDefinedComponents::SimCoilUserDefined(CompName, this->coolingCoilIndex, AirLoopNum, HeatingActive, CoolingActive);
                    if (CoolingActive) PartLoadFrac = 1.0;

                } else if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {

                    PackagedThermalStorageCoil::SimTESCoil(CompName, this->coolingCoilIndex, FanOpMode, this->TESOpMode, PartLoadFrac);

                } else {
                }

                //      NoOutput = Node(InletNode)%MassFlowRate *  &
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
                    WSHPRuntimeFrac = 1.0;

                    if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) { // COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

                        DXCoils::SimDXCoil(CompName, On, FirstHVACIteration, this->coolingCoilIndex, FanOpMode, PartLoadFrac);
                        this->compPartLoadRatio = PartLoadFrac;

                    } else if ((CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) ||
                               (CoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted)) { // CoilSystem:Cooling:DX:HeatExchangerAssisted

                        if (this->coolCoilFluidInletNode > 0)
                            DataLoopNode::Node(this->coolCoilFluidInletNode).MassFlowRate = max(0.0, this->maxCoolCoilFluidFlow);
                        HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(
                            CompName, FirstHVACIteration, On, PartLoadFrac, this->coolingCoilIndex, FanOpMode, HXUnitOn, _, economizerFlag);

                        if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) this->compPartLoadRatio = PartLoadFrac;

                    } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                        CycRatio = 1.0;
                        for (SpeedNum = 1; SpeedNum <= this->numOfSpeedCooling; ++SpeedNum) {
                            SpeedRatio = double(SpeedNum) - 1.0;
                            DXCoils::SimDXCoilMultiSpeed(CompName, SpeedRatio, CycRatio, this->coolingCoilIndex);
                            OutletTemp = DXCoils::DXCoilOutletTemp(this->coolingCoilIndex);
                            if (OutletTemp < DesOutTemp && SensibleLoad) break; // this isn't going to work IF dehumidIFying
                        }

                    } else if (CoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                        CycRatio = 1.0;
                        SpeedRatio = 0.0;
                        for (SpeedNum = 1; SpeedNum <= this->numOfSpeedCooling; ++SpeedNum) {
                            if (SpeedNum > 1) CycRatio = 0.0;
                            if (SpeedNum > 1) SpeedRatio = 1.0;
                            this->coolingSpeedNum = SpeedNum;
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
                        this->coolingSpeedRatio = SpeedRatio;
                        this->coolingPartLoadFrac = PartLoadFrac;
                        for (SpeedNum = 1; SpeedNum <= this->numOfSpeedCooling; ++SpeedNum) {
                            this->coolingSpeedNum = SpeedNum;
                            VariableSpeedCoils::SimVariableSpeedCoils("",
                                                                      this->coolingCoilIndex,
                                                                      FanOpMode,
                                                                      this->maxONOFFCyclesperHour,
                                                                      this->HPTimeConstant,
                                                                      this->fanDelayTime,
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

                        DXCoils::SimDXCoilMultiMode(CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, this->coolingCoilIndex, FanOpMode);
                        this->compPartLoadRatio = PartLoadFrac;

                    } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWater) ||
                               (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed)) { // COIL:COOLING:WATER

                        mdot = this->maxCoolCoilFluidFlow;
                        PlantUtilities::SetComponentFlowRate(mdot,
                                                             this->coolCoilFluidInletNode,
                                                             this->coolCoilFluidOutletNodeNum,
                                                             this->coolCoilLoopNum,
                                                             this->coolCoilLoopSide,
                                                             this->coolCoilBranchNum,
                                                             this->coolCoilCompNum);

                        WaterCoils::SimulateWaterCoilComponents(
                            CompName, FirstHVACIteration, this->coolingCoilIndex, _, this->fanOpMode, PartLoadFrac);

                    } else if (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {

                        WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                        this->coolingCoilIndex,
                                                                        ReqOutput,
                                                                        dummy,
                                                                        FanOpMode,
                                                                        WSHPRuntimeFrac,
                                                                        this->maxONOFFCyclesperHour,
                                                                        this->HPTimeConstant,
                                                                        this->fanDelayTime,
                                                                        1,
                                                                        PartLoadFrac,
                                                                        FirstHVACIteration);
                        this->coolingCoilSensDemand = ReqOutput;

                    } else if (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP) {

                        WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                            this->coolingCoilIndex,
                                                            this->maxCoolAirMassFlow,
                                                            FanOpMode,
                                                            FirstHVACIteration,
                                                            WSHPRuntimeFrac,
                                                            this->maxONOFFCyclesperHour,
                                                            this->HPTimeConstant,
                                                            this->fanDelayTime,
                                                            this->initHeatPump,
                                                            ReqOutput,
                                                            dummy,
                                                            0,
                                                            PartLoadFrac);

                    } else if (CoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                        HeatingActive = false; // set to arbitrary value on entry to function
                        CoolingActive = false; // set to arbitrary value on entry to function

                        UserDefinedComponents::SimCoilUserDefined(CompName, this->coolingCoilIndex, AirLoopNum, HeatingActive, CoolingActive);
                        if (CoolingActive) PartLoadFrac = 1.0;

                    } else if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {

                        // TES coil simulated above with PLR=0. Operating mode is known here, no need to simulate again to determine operating mode.
                        if (this->TESOpMode == PackagedThermalStorageCoil::OffMode ||
                            this->TESOpMode == PackagedThermalStorageCoil::ChargeOnlyMode) { // cannot cool
                            PartLoadFrac = 0.0;
                        } else {
                            // Get full load result
                            PackagedThermalStorageCoil::SimTESCoil(CompName, this->coolingCoilIndex, FanOpMode, this->TESOpMode, PartLoadFrac);
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
                            (this->TESOpMode == PackagedThermalStorageCoil::OffMode ||
                             this->TESOpMode == PackagedThermalStorageCoil::ChargeOnlyMode)) {
                            PartLoadFrac = 0.0;
                        }
                    } else if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling &&
                               (this->TESOpMode == PackagedThermalStorageCoil::OffMode ||
                                this->TESOpMode == PackagedThermalStorageCoil::ChargeOnlyMode)) {
                        PartLoadFrac = 0.0;
                    } else {

                        Par[9] = double(AirLoopNum);
                        Par[10] = 0.0;
                        if (FirstHVACIteration) Par[10] = 1.0;

                        if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {

                            Par[1] = double(this->coolingCoilIndex);
                            Par[2] = DesOutTemp;
                            Par[5] = double(FanOpMode);
                            // auto &functionRef(this->DOE2DXCoilResidual);
                            // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, functionRef, 0.0, 1.0, Par);
                            this->compPartLoadRatio = PartLoadFrac;

                        } else if ((CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) ||
                                   (CoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted)) {

                            Par[1] = double(this->coolingCoilIndex);
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
                            Par[6] = double(1);
                            // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->HXAssistedCoolCoilTempResidual, 0.0, 1.0, Par);
                            if (SolFla == -1) {

                                //                 RegulaFalsi may not find sensible PLR when the latent degradation model is used.
                                //                 IF iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                                TempMaxPLR = -0.1;
                                TempOutletTempDXCoil = DataLoopNode::Node(InletNode).Temp;
                                while ((TempOutletTempDXCoil - DesOutTemp) > 0.0 && TempMaxPLR <= 1.0) {
                                    //                   find upper limit of PLR
                                    TempMaxPLR += 0.1;
                                    HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(
                                        CompName, FirstHVACIteration, On, TempMaxPLR, this->coolingCoilIndex, FanOpMode, HXUnitOn, _, economizerFlag);
                                    TempOutletTempDXCoil = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletTemp(this->coolingCoilIndex);
                                }
                                TempMinPLR = TempMaxPLR;
                                while ((TempOutletTempDXCoil - DesOutTemp) < 0.0 && TempMinPLR >= 0.0) {
                                    //                  pull upper limit of PLR DOwn to last valid limit (i.e. outlet temp still exceeds DesOutTemp)
                                    TempMaxPLR = TempMinPLR;
                                    //                   find minimum limit of PLR
                                    TempMinPLR -= 0.01;
                                    HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(
                                        CompName, FirstHVACIteration, On, TempMinPLR, this->coolingCoilIndex, FanOpMode, HXUnitOn, _, economizerFlag);
                                    TempOutletTempDXCoil = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletTemp(this->coolingCoilIndex);
                                }
                                //                 Relax boundary slightly to assure a solution can be found using RegulaFalsi (i.e. one boundary may
                                //                 be very near the desired result)
                                TempMinPLR = max(0.0, (TempMinPLR - 0.01));
                                TempMaxPLR = min(1.0, (TempMaxPLR + 0.01));
                                //                 tighter boundary of solution has been found, CALL RegulaFalsi a second time
                                // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->HXAssistedCoolCoilTempResidual, TempMinPLR, TempMaxPLR,
                                // Par);
                                if (SolFla == -1) {
                                    if (!DataGlobals::WarmupFlag) {
                                        if (this->HXAssistedSensPLRIter < 1) {
                                            ++this->HXAssistedSensPLRIter;
                                            ShowWarningError(
                                                this->unitType +
                                                " - Iteration limit exceeded calculating DX unit sensible part-load ratio for unit = " + this->name);
                                            ShowContinueError("Estimated part-load ratio   = " +
                                                              General::RoundSigDigits((ReqOutput / FullOutput), 3));
                                            ShowContinueError("Calculated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                            ShowContinueErrorTimeStamp(
                                                "The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            this->unitType + " \"" + this->name +
                                                "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR "
                                                "statistics follow.",
                                            this->HXAssistedSensPLRIterIndex,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                } else if (SolFla == -2) {
                                    PartLoadFrac = ReqOutput / FullOutput;
                                    if (!DataGlobals::WarmupFlag) {
                                        if (this->HXAssistedSensPLRFail < 1) {
                                            ++this->HXAssistedSensPLRFail;
                                            ShowWarningError(this->unitType +
                                                             " - DX unit sensible part-load ratio calculation unexpectedly failed: part-load ratio "
                                                             "limits exceeded, for unit = " +
                                                             this->name);
                                            ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                            ShowContinueErrorTimeStamp(
                                                "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            this->unitType + " \"" + this->name +
                                                "\" - DX unit sensible part-load ratio calculation unexpectedly failed error continues. Sensible PLR "
                                                "statistics follow.",
                                            this->HXAssistedSensPLRFailIndex,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                }
                            } else if (SolFla == -2) {
                                PartLoadFrac = ReqOutput / FullOutput;
                                if (!DataGlobals::WarmupFlag) {
                                    if (this->HXAssistedSensPLRFail2 < 1) {
                                        ++this->HXAssistedSensPLRFail2;
                                        ShowWarningError(
                                            this->unitType +
                                            " - DX unit sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " +
                                            this->name);
                                        ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                        ShowContinueErrorTimeStamp(
                                            "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                    }
                                    ShowRecurringWarningErrorAtEnd(this->unitType + " \"" + this->name +
                                                                       "\" - DX unit sensible part-load ratio calculation failed error continues. "
                                                                       "Sensible PLR statistics follow.",
                                                                   this->HXAssistedSensPLRFailIndex2,
                                                                   PartLoadFrac,
                                                                   PartLoadFrac);
                                }
                            }
                            if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) this->compPartLoadRatio = PartLoadFrac;

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                            Par[1] = double(this->coolingCoilIndex);
                            Par[2] = DesOutTemp;
                            // Par(3) is only needed for variable speed coils (see DXCoilVarSpeedResidual and DXCoilCyclingResidual)
                            Par[3] = 1.0; // UnitarySysNum;
                            this->coolingSpeedRatio = SpeedRatio;
                            if (SpeedRatio == 1.0) {
                                // General::SolveRoot(Acc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedResidual, 0.0, 1.0, Par);
                                PartLoadFrac = SpeedRatio;
                            } else {
                                // General::SolveRoot(Acc, MaxIte, SolFla, CycRatio, this->DXCoilCyclingResidual, 0.0, 1.0, Par);
                                PartLoadFrac = CycRatio;
                            }

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                            Par[1] = double(this->coolingCoilIndex);
                            Par[2] = DesOutTemp;
                            Par[3] = double(1);
                            // Par[4] = CycRatio or SpeedRatio
                            Par[5] = this->coolingSpeedNum;
                            Par[6] = 1.0; // UnitarySystem(UnitarySysNum)%FanOpMode
                            Par[7] = 1.0; // CompOp
                            Par[8] = ReqOutput;

                            if (this->coolingSpeedNum > 1.0) {
                                Par[4] = CycRatio;
                                // General::SolveRoot(Acc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedResidual, 0.0, 1.0, Par);
                                PartLoadFrac = SpeedRatio;
                            } else {
                                SpeedRatio = 0.0;
                                this->coolingSpeedRatio = SpeedRatio;
                                Par[4] = SpeedRatio;
                                //General::SolveRoot(Acc, MaxIte, SolFla, CycRatio, DXCoilCyclingResidual, 0.0, 1.0, Par);
                                PartLoadFrac = CycRatio;
                            }

                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {

                            Par[1] = double(this->coolingCoilIndex);
                            Par[2] = DesOutTemp;
                            Par[3] = double(1);
                            // Par[4] = CycRatio or SpeedRatio
                            Par[5] = this->coolingSpeedNum;
                            Par[6] = double(fanOpMode);
                            Par[7] = 1.0; // CompOp
                            Par[8] = ReqOutput;

                            if (this->coolingSpeedNum > 1.0) {
                                Par[4] = CycRatio;
                                // General::SolveRoot(Acc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedResidual, 0.0, 1.0, Par);
                                this->coolingCycRatio = CycRatio;
                                this->coolingSpeedRatio = SpeedRatio;
                                this->coolingPartLoadFrac = SpeedRatio;
                                this->calcPassiveSystem(AirLoopNum, FirstHVACIteration);
                                PartLoadFrac = SpeedRatio;
                            } else {
                                this->coolingSpeedRatio = SpeedRatio;
                                Par[4] = SpeedRatio;
                                // General::SolveRoot(Acc, MaxIte, SolFla, CycRatio, this->DXCoilCyclingResidual, 0.0, 1.0, Par);
                                this->coolingCycRatio = CycRatio;
                                this->coolingPartLoadFrac = CycRatio;
                                this->calcPassiveSystem(AirLoopNum, FirstHVACIteration);
                                PartLoadFrac = CycRatio;
                            }

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {

                            Par[1] = double(this->coolingCoilIndex);
                            Par[2] = DesOutTemp;
                            // dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                            Par[3] = double(DehumidMode);
                            Par[4] = double(FanOpMode);
                            // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->MultiModeDXCoilResidual, 0.0, 1.0, Par);
                            this->compPartLoadRatio = PartLoadFrac;

                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWater) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed)) {

                            Par[1] = double(1);
                            if (FirstHVACIteration) {
                                Par[2] = 1.0;
                            } else {
                                Par[2] = 0.0;
                            }
                            Par[3] = DesOutTemp;

                            // calculate max waterside PLR from mdot request above in case plant chokes water flow
                            maxPartLoadFrac =
                                min(1.0,
                                    ((mdot / this->maxCoolCoilFluidFlow) +
                                     0.001)); // plant can limit flow and RegulaFalsi could hit max iteration limit (leave a little slop, 0.001)
                            // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->CoolWaterTempResidual, 0.0, maxPartLoadFrac, Par);

                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP)) {
                            Par[1] = double(1);
                            if (FirstHVACIteration) {
                                Par[2] = 1.0;
                            } else {
                                Par[2] = 0.0;
                            }
                            Par[3] = DesOutTemp;
                            Par[4] = ReqOutput;
                            this->coolingCoilSensDemand = ReqOutput;
                            // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->CoolWatertoAirHPTempResidual, 0.0, 1.0, Par);

                        } else if (CoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                            // do nothing, user defined coil cannot be controlled

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {

                            Par[1] = double(1);
                            Par[2] = DesOutTemp;
                            Par[3] = 0.0; // DesOutHumRat; set to 0 if temp controlled
                            // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->TESIceStorageCoilOutletResidual, 0.0, 1.0, Par);

                        } else {
                            ShowMessage(" For :" + this->unitType + "=\"" + this->name + "\"");
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
                    (this->dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_Multimode)) {

                    if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) { // CoilSystem:Cooling:DX:HeatExchangerAssisted
                        // Determine required part load when heat exchanger is ON
                        HXUnitOn = true;
                        PartLoadFrac = 1.0;
                        HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil(
                            CompName, FirstHVACIteration, On, PartLoadFrac, this->coolingCoilIndex, FanOpMode, HXUnitOn, _, economizerFlag);

                        OutletTempDXCoil = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletTemp(this->coolingCoilIndex);

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
                            Par[1] = double(this->coolingCoilIndex);
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
                            // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->HXAssistedCoolCoilTempResidual, 0.0, 1.0, Par);
                        }
                        this->compPartLoadRatio = PartLoadFrac;

                    } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {

                        // Get full load result
                        PartLoadFrac = 1.0;
                        DehumidMode = 1;
                        this->dehumidificationMode = DehumidMode;
                        DXCoils::SimDXCoilMultiMode(CompName, On, FirstHVACIteration, PartLoadFrac, DehumidMode, this->coolingCoilIndex, FanOpMode);
                        FullOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                                     (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(InletNode).HumRat) -
                                      Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat));

                        // Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
                        // Check that this is the case; IF not set PartLoadFrac = 0.0 (off) and return
                        // Calculate the part load fraction
                        if (FullOutput >= 0) {
                            PartLoadFrac = 0.0;
                        } else {
                            OutletTempDXCoil = DXCoils::DXCoilOutletTemp(this->coolingCoilIndex);
                            OutletHumRatDXCoil = DXCoils::DXCoilOutletHumRat(this->coolingCoilIndex);
                            // If sensible load and setpoint cannot be met, set PLR = 1. if no sensible load and
                            // latent load exists and setpoint cannot be met, set PLR = 1.
                            // why is our logic different? Did we figure something out that reduced the logic?
                            //                  IF ((SensibleLoad .and. LatentLoad .AND. .NOT. UnitarySystem(UnitarySysNum)%RunOnLatentLoad .AND. &
                            //                       OutletHumRatDXCoil >= DesOutHumRat)) THEN
                            if ((OutletTempDXCoil > (DesOutTemp - (Acc * 2.0)) && SensibleLoad && this->runOnSensibleLoad) ||
                                (OutletHumRatDXCoil > (DesOutHumRat - (HumRatAcc * 2.0)) && !SensibleLoad && LatentLoad && this->runOnLatentLoad)) {
                                PartLoadFrac = 1.0;
                                //                  ELSEIF ((SensibleLoad .and. LatentLoad .AND. .NOT. UnitarySystem(UnitarySysNum)%RunOnLatentLoad
                                //                  .AND. &
                                //                       OutletHumRatDXCoil < DesOutHumRat)) THEN
                            } else if (!SensibleLoad && (OutletHumRatDXCoil < DesOutHumRat && LatentLoad && this->runOnLatentLoad)) {
                                PartLoadFrac = ReqOutput / FullOutput;
                                Par[1] = double(this->coolingCoilIndex);
                                Par[2] = DesOutHumRat;
                                // dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                                Par[3] = double(DehumidMode);
                                Par[4] = double(FanOpMode);
                                // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->MultiModeDXCoilHumRatResidual, 0.0, 1.0, Par);
                            } else { // must be a sensible load so find PLR
                                PartLoadFrac = ReqOutput / FullOutput;
                                Par[1] = double(this->coolingCoilIndex);
                                Par[2] = DesOutTemp;
                                // Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                                Par[3] = double(DehumidMode);
                                Par[4] = double(FanOpMode);
                                // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->MultiModeDXCoilResidual, 0.0, 1.0, Par);
                            }
                        }
                        this->compPartLoadRatio = PartLoadFrac;

                    } else {
                    }
                } // END IF humidity ratio setpoint not met - Multimode humidity control

                // IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
                // then overcool to meet moisture load

                if ((OutletHumRatDXCoil > DesOutHumRat) && (PartLoadFrac < 1.0) && LatentLoad &&
                    (this->dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_CoolReheat)) {

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

                            Par[1] = double(this->coolingCoilIndex);
                            Par[2] = DesOutHumRat;
                            Par[5] = double(FanOpMode);
                            // General::SolveRoot(HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, this->DOE2DXCoilHumRatResidual, 0.0, 1.0, Par);
                            this->compPartLoadRatio = PartLoadFrac;

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
                                Par[1] = double(this->coolingCoilIndex);
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
                                // General::SolveRoot(HumRatAcc, MaxIte, SolFla, PartLoadFrac, this->HXAssistedCoolCoilHRResidual, 0.0, 1.0, Par);
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
                                                                                            this->coolingCoilIndex,
                                                                                            FanOpMode,
                                                                                            HXUnitOn,
                                                                                            _,
                                                                                            economizerFlag);
                                        OutletHumRatDXCoil = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletHumRat(this->coolingCoilIndex);
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
                                                                                            this->coolingCoilIndex,
                                                                                            FanOpMode,
                                                                                            HXUnitOn,
                                                                                            _,
                                                                                            economizerFlag);
                                        OutletHumRatDXCoil = HVACHXAssistedCoolingCoil::HXAssistedCoilOutletHumRat(this->coolingCoilIndex);
                                    }
                                    //                   tighter boundary of solution has been found, CALL RegulaFalsi a second time
                                    // General::SolveRoot(HumRatAcc, MaxIte, SolFla, PartLoadFrac, this->HXAssistedCoolCoilHRResidual, TempMinPLR,
                                    // TempMaxPLR, Par);
                                    if (SolFla == -1) {
                                        if (!DataGlobals::WarmupFlag) {
                                            if (this->HXAssistedCRLatPLRIter < 1) {
                                                ++this->HXAssistedCRLatPLRIter;
                                                ShowWarningError(
                                                    this->unitType +
                                                    " - Iteration limit exceeded calculating DX unit latent part-load ratio for unit = " +
                                                    this->name);
                                                ShowContinueError("Estimated latent part-load ratio  = " +
                                                                  General::RoundSigDigits((ReqOutput / FullOutput), 3));
                                                ShowContinueError("Calculated latent part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                                ShowContinueErrorTimeStamp("The calculated latent part-load ratio will be used and the simulation "
                                                                           "continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                this->unitType + " \"" + this->name +
                                                    "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR "
                                                    "statistics follow.",
                                                this->HXAssistedCRLatPLRIterIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }

                                    } else if (SolFla == -2) {

                                        PartLoadFrac = ReqOutput / FullOutput;
                                        if (!DataGlobals::WarmupFlag) {
                                            if (this->HXAssistedCRLatPLRFail < 1) {
                                                ++this->HXAssistedCRLatPLRFail;
                                                ShowWarningError(this->unitType +
                                                                 " - DX unit latent part-load ratio calculation failed unexpectedly: part-load ratio "
                                                                 "limits exceeded, for unit = " +
                                                                 this->name);
                                                ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                                ShowContinueErrorTimeStamp(
                                                    "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                            }
                                            ShowRecurringWarningErrorAtEnd(
                                                this->unitType + " \"" + this->name +
                                                    "\" - DX unit latent part-load ratio calculation failed unexpectedly error continues. Latent PLR "
                                                    "statistics follow.",
                                                this->HXAssistedCRLatPLRFailIndex,
                                                PartLoadFrac,
                                                PartLoadFrac);
                                        }
                                    }
                                } else if (SolFla == -2) {
                                    PartLoadFrac = ReqOutput / FullOutput;
                                    if (!DataGlobals::WarmupFlag) {
                                        if (this->HXAssistedCRLatPLRFail2 < 1) {
                                            ++this->HXAssistedCRLatPLRFail2;
                                            ShowWarningError(
                                                this->unitType +
                                                " - DX unit latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " +
                                                this->name);
                                            ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                                            ShowContinueErrorTimeStamp(
                                                "The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                                        }
                                        ShowRecurringWarningErrorAtEnd(
                                            this->unitType + " \"" + this->name +
                                                "\" - DX unit latent part-load ratio calculation failed error continues. Latent PLR statistics "
                                                "follow.",
                                            this->HXAssistedCRLatPLRFailIndex2,
                                            PartLoadFrac,
                                            PartLoadFrac);
                                    }
                                }
                            }
                            this->compPartLoadRatio = PartLoadFrac;

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                            //               Simulate MultiSpeed DX coil at sensible result
                            DXCoils::SimDXCoilMultiSpeed(CompName, SpeedRatio, CycRatio, this->coolingCoilIndex);

                            OutletHumRatDXCoil = DXCoils::DXCoilOutletHumRat(this->coolingCoilIndex);
                            // IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
                            // then overcool to meet moisture load

                            if (OutletHumRatDXCoil > DesOutHumRat) {

                                CycRatio = 0.0;
                                SpeedRatio = 0.0;

                                DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, 1.0, this->coolingCoilIndex);
                                OutletHumRatLS = DXCoils::DXCoilOutletHumRat(this->coolingCoilIndex);
                                if (OutletHumRatLS > DesOutHumRat) {
                                    CycRatio = 1.0;
                                    DXCoils::SimDXCoilMultiSpeed(CompName, 1.0, 1.0, this->coolingCoilIndex);
                                    OutletHumRatHS = DXCoils::DXCoilOutletHumRat(this->coolingCoilIndex);
                                    if (OutletHumRatHS < DesOutHumRat) {
                                        Par[1] = double(this->coolingCoilIndex);
                                        Par[2] = DesOutHumRat;
                                        // General::SolveRoot(HumRatAcc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedHumRatResidual, 0.0, 1.0,
                                        // Par);
                                    } else {
                                        SpeedRatio = 1.0;
                                    }
                                } else {
                                    SpeedRatio = 0.0;
                                    Par[1] = double(this->coolingCoilIndex);
                                    Par[2] = DesOutHumRat;
                                    // General::SolveRoot(HumRatAcc, MaxIte, SolFla, CycRatio, this->DXCoilCyclingHumRatResidual, 0.0, 1.0, Par);
                                }
                            }

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                            DXCoils::SimDXCoilMultiSpeed(CompName, SpeedRatio, CycRatio, this->coolingCoilIndex);
                            OutletHumRatDXCoil = DXCoils::DXCoilOutletHumRat(this->coolingCoilIndex);

                            // IF humidity setpoint is not satisfied and humidity control type is CoolReheat,
                            // then overcool to meet moisture load

                            if (OutletHumRatDXCoil > DesOutHumRat) {

                                CycRatio = 0.0;
                                SpeedRatio = 0.0;

                                DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, 1.0, this->coolingCoilIndex);
                                OutletHumRatLS = DXCoils::DXCoilOutletHumRat(this->coolingCoilIndex);
                                if (OutletHumRatLS > DesOutHumRat) {
                                    CycRatio = 1.0;
                                    DXCoils::SimDXCoilMultiSpeed(CompName, 1.0, 1.0, this->coolingCoilIndex);
                                    OutletHumRatHS = DXCoils::DXCoilOutletHumRat(this->coolingCoilIndex);
                                    if (OutletHumRatHS < DesOutHumRat) {
                                        Par[1] = double(this->coolingCoilIndex);
                                        Par[2] = DesOutHumRat;
                                        Par[3] = ReqOutput;
                                        // General::SolveRoot(HumRatAcc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedHumRatResidual, 0.0, 1.0,
                                        // Par);
                                    } else {
                                        SpeedRatio = 1.0;
                                    }
                                } else {
                                    SpeedRatio = 0.0;
                                    Par[1] = double(this->coolingCoilIndex);
                                    Par[2] = DesOutHumRat;
                                    Par[3] = ReqOutput;
                                    // General::SolveRoot(HumRatAcc, MaxIte, SolFla, CycRatio, this->DXCoilCyclingHumRatResidual, 0.0, 1.0, Par);
                                }
                            }
                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {
                            VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                                      this->coolingCoilIndex,
                                                                      this->fanOpMode,
                                                                      this->maxONOFFCyclesperHour,
                                                                      this->HPTimeConstant,
                                                                      this->fanDelayTime,
                                                                      1,
                                                                      CycRatio,
                                                                      SpeedNum,
                                                                      SpeedRatio,
                                                                      ReqOutput,
                                                                      dummy,
                                                                      OnOffAirFlowRatio);
                            OutletHumRatLS = DataLoopNode::Node(this->coolCoilOutletNodeNum).HumRat;

                            if (OutletHumRatLS > DesOutHumRat) {
                                CycRatio = 1.0;

                                VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                                          this->coolingCoilIndex,
                                                                          this->fanOpMode,
                                                                          this->maxONOFFCyclesperHour,
                                                                          this->HPTimeConstant,
                                                                          this->fanDelayTime,
                                                                          1,
                                                                          1.0,
                                                                          SpeedNum,
                                                                          1.0,
                                                                          ReqOutput,
                                                                          dummy,
                                                                          OnOffAirFlowRatio);

                                OutletHumRatHS = DataLoopNode::Node(this->coolCoilOutletNodeNum).HumRat;

                                if (OutletHumRatHS < DesOutHumRat) {
                                    Par[1] = double(this->coolingCoilIndex);
                                    Par[2] = DesOutHumRat;
                                    Par[3] = double(1);
                                    if (SpeedNum == 1) {
                                        // General::SolveRoot(HumRatAcc, MaxIte, SolFla, CycRatio, this->DXCoilCyclingHumRatResidual, 0.0, 1.0, Par);
                                    } else {
                                        // General::SolveRoot(HumRatAcc, MaxIte, SolFla, SpeedRatio, this->DXCoilVarSpeedHumRatResidual, 0.0, 1.0,
                                        // Par);
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
                                Par[1] = double(this->coolingCoilIndex);
                                Par[2] = DesOutHumRat;
                                Par[3] = double(1);
                                // General::SolveRoot(HumRatAcc, MaxIte, SolFla, CycRatio, this->DXCoilVarSpeedHumRatResidual, 0.0, 1.0, Par);
                            }
                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {

                            Par[1] = double(this->coolingCoilIndex);
                            Par[2] = DesOutHumRat;
                            // dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                            Par[3] = double(DehumidMode);
                            Par[4] = double(FanOpMode);
                            // General::SolveRoot(Acc, MaxIte, SolFlaLat, PartLoadFrac, this->MultiModeDXCoilHumRatResidual, 0.0, 1.0, Par);
                            this->compPartLoadRatio = PartLoadFrac;

                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWater) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed)) { // COIL:COOLING:WATER

                            Par[1] = double(1);
                            if (FirstHVACIteration) {
                                Par[2] = 1.0;
                            } else {
                                Par[2] = 0.0;
                            }
                            Par[3] = DesOutHumRat;

                            // General::SolveRoot(HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, this->CoolWaterHumRatResidual, 0.0, 1.0, Par);

                        } else if ((CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) ||
                                   (CoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP)) {

                            Par[1] = double(1);
                            if (FirstHVACIteration) {
                                Par[2] = 1.0;
                            } else {
                                Par[2] = 0.0;
                            }
                            Par[3] = DesOutHumRat;
                            Par[4] = ReqOutput;

                            // General::SolveRoot(HumRatAcc, MaxIte, SolFlaLat, PartLoadFrac, this->CoolWatertoAirHPHumRatResidual, 0.0, 1.0, Par);

                        } else if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {

                            if (CoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling &&
                                (this->TESOpMode != PackagedThermalStorageCoil::OffMode &&
                                 this->TESOpMode != PackagedThermalStorageCoil::ChargeOnlyMode)) {
                                Par[1] = double(1);
                                Par[2] = 0.0; // DesOutTemp; set to 0 if humrat controlled
                                Par[3] = DesOutHumRat;
                                // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->TESIceStorageCoilOutletResidual, 0.0, 1.0, Par);
                            }

                        } else {
                        }
                    }
                }
            }
        }

        if (SolFla == -1) {
            if (!DataGlobals::WarmupFlag) {
                if (this->SensPLRIter < 1) {
                    ++this->SensPLRIter;
                    ShowWarningError(this->unitType + " - Iteration limit exceeded calculating part-load ratio for unit = " + this->name);
                    ShowContinueError("Estimated part-load ratio  = " + General::RoundSigDigits((ReqOutput / FullOutput), 3));
                    ShowContinueError("Calculated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->unitType + " \"" + this->name +
                            "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.",
                        this->SensPLRIterIndex,
                        PartLoadFrac,
                        PartLoadFrac);
                }
            }
        } else if (SolFla == -2) {
            PartLoadFrac = ReqOutput / FullOutput;
            if (!DataGlobals::WarmupFlag) {
                if (this->SensPLRFail < 1) {
                    ++this->SensPLRFail;
                    ShowWarningError(this->unitType +
                                     " - sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + this->name);
                    ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->unitType + " \"" + this->name +
                            "\" - sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.",
                        this->SensPLRFailIndex,
                        PartLoadFrac,
                        PartLoadFrac);
                }
            }
        }

        if (SolFlaLat == -1 && SolFla != -1) {
            if (!DataGlobals::WarmupFlag) {
                if (this->LatPLRIter < 1) {
                    ++this->LatPLRIter;
                    ShowWarningError(this->unitType + " - Iteration limit exceeded calculating latent part-load ratio for unit = " + this->name);
                    ShowContinueError("Estimated part-load ratio   = " + General::RoundSigDigits((ReqOutput / FullOutput), 3));
                    ShowContinueError("Calculated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(
                    this->unitType + " \"" + this->name +
                        "\" - Iteration limit exceeded calculating latent part-load ratio error continues. Latent PLR statistics follow.",
                    this->LatPLRIterIndex,
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
                if (this->LatPLRFail < 1) {
                    ++this->LatPLRFail;
                    ShowWarningError(this->unitType +
                                     " - latent part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + this->name);
                    ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                }
                ShowRecurringWarningErrorAtEnd(this->unitType + " \"" + this->name +
                                                   "\" - latent part-load ratio calculation failed error continues. Latent PLR statistics follow.",
                                               this->LatPLRFailIndex,
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

        this->coolingPartLoadFrac = PartLoadFrac;
        this->coolingSpeedRatio = SpeedRatio;
        this->coolingCycRatio = CycRatio;
        this->dehumidificationMode = DehumidMode;

        DataAirLoop::LoopDXCoilRTF = max(DataAirLoop::LoopDXCoilRTF, LoopDXCoilMaxRTFSave);

        if (this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
            this->coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {
            mdot = PartLoadFrac * this->maxCoolCoilFluidFlow;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->coolCoilFluidInletNode,
                                                 this->coolCoilFluidOutletNodeNum,
                                                 this->coolCoilLoopNum,
                                                 this->coolCoilLoopSide,
                                                 this->coolCoilBranchNum,
                                                 this->coolCoilCompNum);
        }
    }

    void UnitarySys::controlHeatingSystemToSP(int const AirLoopNum,          // index to air loop
                                              bool const FirstHVACIteration, // First HVAC iteration flag
                                              int &CompOn                    // compressor on/off control
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

        std::vector<Real64> Par(11); // Parameter array passed to solver
        Real64 WSHPRuntimeFrac;     // Run time fraction of water to air hp
        Real64 OutdoorDryBulb;      // local variable for OutDryBulbTemp
        Real64 OutdoorHumRat;       // local variable for OutHumRat
        Real64 OutdoorPressure;     // local variable for OutBaroPress
        Real64 OutdoorWetBulb;      // local variable for OutWetBulbTemp
        bool HeatingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil
                            // is wrapped by UnitarySystem.
        bool CoolingActive; // dummy variable for UserDefined coil which are passed back indicating if coil is on or off. Not needed here since coil
                            // is wrapped by UnitarySystem.
        Real64 mdot;        // water coil water flow rate [kg/s]
        Real64 maxPartLoadFrac; // calculated maximum water side PLR for RegulaFalsi call (when plant limits flow max PLR != 1)

        // Set local variables
        // Retrieve the load on the controlled zone
        int InletNode = this->heatCoilInletNodeNum;
        int OutletNode = this->heatCoilOutletNodeNum;
        int ControlNode = this->systemHeatControlNodeNum;
        Real64 DesOutTemp = this->desiredOutletTemp;
        Real64 LoopHeatingCoilMaxRTFSave = DataAirLoop::LoopHeatingCoilMaxRTF;
        Real64 LoopHeatingCoilMaxRTF = 0.0;
        Real64 LoopDXCoilMaxRTFSave = DataAirLoop::LoopDXCoilRTF;
        DataAirLoop::LoopDXCoilRTF = 0.0;

        std::string CompName = this->heatingCoilName;
        int CompIndex = this->heatingCoilIndex;
        int FanOpMode = this->fanOpMode;

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

        if (this->condenserNodeNum != 0) {
            OutdoorDryBulb = DataLoopNode::Node(this->condenserNodeNum).Temp;
            if (this->condenserType == DataHVACGlobals::WaterCooled) {
                OutdoorHumRat = DataEnvironment::OutHumRat;
                OutdoorPressure = DataEnvironment::OutBaroPress;
                OutdoorWetBulb = DataEnvironment::OutWetBulbTemp;
            } else {
                OutdoorPressure = DataLoopNode::Node(this->condenserNodeNum).Press;
                // IF node is not connected to anything, pressure = default, use weather data
                if (OutdoorPressure == DataLoopNode::DefaultNodeValues.Press) {
                    OutdoorDryBulb = DataEnvironment::OutDryBulbTemp;
                    OutdoorHumRat = DataEnvironment::OutHumRat;
                    OutdoorPressure = DataEnvironment::OutBaroPress;
                    OutdoorWetBulb = DataEnvironment::OutWetBulbTemp;
                } else {
                    OutdoorHumRat = DataLoopNode::Node(this->condenserNodeNum).HumRat;
                    //     this should use Node%WetBulbTemp or a PSYC function, not OAWB
                    OutdoorWetBulb = DataLoopNode::Node(this->condenserNodeNum).OutAirWetBulb;
                }
            }
        } else {
            OutdoorDryBulb = DataEnvironment::OutDryBulbTemp;
            OutdoorHumRat = DataEnvironment::OutHumRat;
            OutdoorPressure = DataEnvironment::OutBaroPress;
            OutdoorWetBulb = DataEnvironment::OutWetBulbTemp;
        }

        // IF there is a fault of coil SAT Sensor (zrp_Nov2016)
        if (this->faultyCoilSATFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            // calculate the sensor offset using fault information
            int FaultIndex = this->faultyCoilSATIndex;
            this->faultyCoilSATOffset = FaultsManager::FaultsCoilSATSensor(FaultIndex).CalFaultOffsetAct();
            // update the DesOutTemp
            DesOutTemp -= this->faultyCoilSATOffset;
        }

        // IF DXHeatingSystem is scheduled on and there is flow
        if (ScheduleManager::GetCurrentScheduleValue(this->sysAvailSchedPtr) > 0.0 &&
            ScheduleManager::GetCurrentScheduleValue(this->heatingCoilAvailSchPtr) > 0.0 &&
            DataLoopNode::Node(InletNode).MassFlowRate > MinAirMassFlow) {

            // Determine if there is a sensible load on this system
            if (DesOutTemp - DataLoopNode::Node(InletNode).Temp > DataHVACGlobals::TempControlTol) SensibleLoad = true;
            // if a heat pump and other coil is on, disable this coil
            if (this->heatPump && this->coolingPartLoadFrac > 0.0) SensibleLoad = false;

            // disable compressor if OAT is below minimum outdoor temperature
            if (OutdoorDryBulb < this->minOATCompressorHeating) {
                SensibleLoad = false;
            }

            // IF DXHeatingSystem runs with a heating load then set PartLoadFrac on Heating System
            if (SensibleLoad) {

                ReqOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                            (Psychrometrics::PsyHFnTdbW(DesOutTemp, DataLoopNode::Node(InletNode).HumRat) -
                             Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat));

                // Get no load result
                PartLoadFrac = 0.0;
                WSHPRuntimeFrac = 0.0;
                CompOn = 0;

                {
                    auto const SELECT_CASE_var(this->heatingCoilType_Num);

                    if (SELECT_CASE_var == DataHVACGlobals::CoilDX_HeatingEmpirical) {

                        DXCoils::SimDXCoil(CompName, On, FirstHVACIteration, CompIndex, FanOpMode, PartLoadFrac);
                        this->compPartLoadRatio = PartLoadFrac;

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) { // do nothing, user defined coil cannot be controlled

                        HeatingActive = false; // set to arbitrary value on entry to function
                        CoolingActive = false; // set to arbitrary value on entry to function
                        UserDefinedComponents::SimCoilUserDefined(CompName, CompIndex, AirLoopNum, HeatingActive, CoolingActive);
                        if (HeatingActive) PartLoadFrac = 1.0;

                    } else if ((SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {

                        this->simMultiSpeedCoils(AirLoopNum, FirstHVACIteration, CompOn, SensibleLoad, LatentLoad, PartLoadFrac, HeatingCoil);

                    } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit)) {

                        this->heatingCoilSensDemand = ReqOutput;
                        VariableSpeedCoils::SimVariableSpeedCoils("",
                                                                  this->heatingCoilIndex,
                                                                  FanOpMode,
                                                                  this->maxONOFFCyclesperHour,
                                                                  this->HPTimeConstant,
                                                                  this->fanDelayTime,
                                                                  CompOn,
                                                                  CycRatio,
                                                                  SpeedNum,
                                                                  SpeedRatio,
                                                                  SensLoad,
                                                                  dummy);

                    } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric) ||
                               (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater)) {

                        HeatingCoils::SimulateHeatingCoilComponents(CompName, FirstHVACIteration, 0.0, CompIndex, _, _, FanOpMode);

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                        WaterCoils::SimulateWaterCoilComponents(
                            CompName, FirstHVACIteration, this->heatingCoilIndex, _, this->fanOpMode, PartLoadFrac);

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                        SteamCoils::SimulateSteamCoilComponents(CompName,
                                                                FirstHVACIteration,
                                                                this->heatingCoilIndex,
                                                                1.0,
                                                                _,
                                                                this->fanOpMode,
                                                                PartLoadFrac); // QCoilReq, simulate any load > 0 to get max capacity

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) {

                        if (FirstHVACIteration) this->compPartLoadRatio = 1;
                        WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                        CompIndex,
                                                                        ReqOutput,
                                                                        dummy,
                                                                        FanOpMode,
                                                                        this->compPartLoadRatio,
                                                                        this->maxONOFFCyclesperHour,
                                                                        this->HPTimeConstant,
                                                                        this->fanDelayTime,
                                                                        0,
                                                                        PartLoadFrac,
                                                                        FirstHVACIteration);
                        this->compPartLoadRatio = PartLoadFrac;
                        this->heatingCoilSensDemand = 0.0;

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHP) {

                        WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                            CompIndex,
                                                            this->maxHeatAirMassFlow,
                                                            FanOpMode,
                                                            FirstHVACIteration,
                                                            WSHPRuntimeFrac,
                                                            this->maxONOFFCyclesperHour,
                                                            this->HPTimeConstant,
                                                            this->fanDelayTime,
                                                            this->initHeatPump,
                                                            ReqOutput,
                                                            dummy,
                                                            0,
                                                            PartLoadFrac);
                        this->compPartLoadRatio = PartLoadFrac;

                    } else {
                    }
                }

                //     IF outlet temp at no load is within ACC of set point, do not run the coil
                if (std::abs(DataLoopNode::Node(OutletNode).Temp - DesOutTemp) < Acc ||
                    this->heatingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                    // do nothing, coil is at the set point.
                } else if ((DataLoopNode::Node(OutletNode).Temp - DesOutTemp) > Acc) { // IF outlet temp is above set point turn off coil
                    PartLoadFrac = 0.0;
                } else { // ELSE get full load result

                    // Get full load result
                    PartLoadFrac = 1.0;
                    WSHPRuntimeFrac = 1.0;
                    CompOn = 1;

                    {
                        auto const SELECT_CASE_var(this->heatingCoilType_Num);

                        if (SELECT_CASE_var == DataHVACGlobals::CoilDX_HeatingEmpirical) { // Coil:Heating:DX:SingleSpeed

                            DXCoils::SimDXCoil(CompName, On, FirstHVACIteration, this->heatingCoilIndex, FanOpMode, PartLoadFrac);
                            this->compPartLoadRatio = PartLoadFrac;

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) {

                            //  should never get here, coil cannot be controlled and has already been simulated

                        } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) {

                            CycRatio = 1.0;
                            SpeedRatio = 0.0;
                            for (SpeedNum = 1; SpeedNum <= this->numOfSpeedHeating; ++SpeedNum) {
                                if (SpeedNum > 1) CycRatio = 0.0;
                                if (SpeedNum > 1) SpeedRatio = 1.0;
                                this->heatingSpeedNum = SpeedNum;
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
                            this->heatingSpeedRatio = SpeedRatio;
                            this->heatingPartLoadFrac = PartLoadFrac;
                            for (SpeedNum = 1; SpeedNum <= this->numOfSpeedHeating; ++SpeedNum) {
                                this->heatingSpeedNum = SpeedNum;
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
                            this->heatingSpeedRatio = SpeedRatio;
                            this->heatingPartLoadFrac = PartLoadFrac;
                            for (SpeedNum = 1; SpeedNum <= this->numOfSpeedHeating; ++SpeedNum) {
                                this->heatingSpeedNum = SpeedNum;
                                VariableSpeedCoils::SimVariableSpeedCoils("",
                                                                          this->heatingCoilIndex,
                                                                          FanOpMode,
                                                                          this->maxONOFFCyclesperHour,
                                                                          this->HPTimeConstant,
                                                                          this->fanDelayTime,
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
                                CompName, FirstHVACIteration, this->designHeatingCapacity, CompIndex, _, _, FanOpMode);

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater) {

                            HeatingCoils::SimulateHeatingCoilComponents(CompName, FirstHVACIteration, ReqOutput, CompIndex, _, _, FanOpMode);

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                            mdot = this->maxHeatCoilFluidFlow;
                            PlantUtilities::SetComponentFlowRate(mdot,
                                                                 this->heatCoilFluidInletNode,
                                                                 this->heatCoilFluidOutletNodeNum,
                                                                 this->heatCoilLoopNum,
                                                                 this->heatCoilLoopSide,
                                                                 this->heatCoilBranchNum,
                                                                 this->heatCoilCompNum);

                            WaterCoils::SimulateWaterCoilComponents(
                                CompName, FirstHVACIteration, this->heatingCoilIndex, _, this->fanOpMode, PartLoadFrac);

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                            mdot = this->maxHeatCoilFluidFlow;
                            PlantUtilities::SetComponentFlowRate(mdot,
                                                                 this->heatCoilFluidInletNode,
                                                                 this->heatCoilFluidOutletNodeNum,
                                                                 this->heatCoilLoopNum,
                                                                 this->heatCoilLoopSide,
                                                                 this->heatCoilBranchNum,
                                                                 this->heatCoilCompNum);

                            SteamCoils::SimulateSteamCoilComponents(CompName,
                                                                    FirstHVACIteration,
                                                                    this->heatingCoilIndex,
                                                                    1.0,
                                                                    _,
                                                                    this->fanOpMode,
                                                                    PartLoadFrac); // QCoilReq, simulate any load > 0 to get max capacity

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) {

                            WaterToAirHeatPumpSimple::SimWatertoAirHPSimple(blankString,
                                                                            CompIndex,
                                                                            ReqOutput,
                                                                            dummy,
                                                                            FanOpMode,
                                                                            WSHPRuntimeFrac,
                                                                            this->maxONOFFCyclesperHour,
                                                                            this->HPTimeConstant,
                                                                            this->fanDelayTime,
                                                                            1,
                                                                            PartLoadFrac,
                                                                            FirstHVACIteration);
                            this->heatingCoilSensDemand = ReqOutput;
                            this->compPartLoadRatio = PartLoadFrac;

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHP) {
                            WaterToAirHeatPump::SimWatertoAirHP(blankString,
                                                                CompIndex,
                                                                this->maxHeatAirMassFlow,
                                                                FanOpMode,
                                                                FirstHVACIteration,
                                                                WSHPRuntimeFrac,
                                                                this->maxONOFFCyclesperHour,
                                                                this->HPTimeConstant,
                                                                this->fanDelayTime,
                                                                this->initHeatPump,
                                                                ReqOutput,
                                                                dummy,
                                                                0,
                                                                PartLoadFrac);
                            this->compPartLoadRatio = PartLoadFrac;

                        } else {
                        }
                    }

                    FullOutput = DataLoopNode::Node(InletNode).MassFlowRate *
                                 (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, DataLoopNode::Node(InletNode).HumRat) -
                                  Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, DataLoopNode::Node(InletNode).HumRat));

                    //       If the outlet temp is within ACC of set point,
                    if (std::abs(DataLoopNode::Node(OutletNode).Temp - DesOutTemp) < Acc ||
                        this->heatingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                        // do nothing, coil is at set point
                    } else if (DataLoopNode::Node(OutletNode).Temp < (DesOutTemp - Acc)) { // IF outlet temp is below set point coil must be on
                        PartLoadFrac = 1.0;
                    } else { // ELSE find the PLR to meet the set point

                        {
                            auto const SELECT_CASE_var(this->heatingCoilType_Num);

                            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_HeatingEmpirical) { // Coil:Heating:DX:SingleSpeed

                                Par[1] = double(CompIndex);
                                Par[2] = DesOutTemp;
                                Par[3] = 1.0;               // OnOffAirFlowFrac assume = 1.0 for continuous fan dx system
                                Par[5] = double(FanOpMode); // this does nothing since set point based control requires constant fan
                                // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->DXHeatingCoilResidual, 0.0, 1.0, Par);
                                this->compPartLoadRatio = PartLoadFrac;

                            } else if ((SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {

                                Par[1] = double(this->heatingCoilIndex);
                                Par[2] = DesOutTemp;
                                Par[3] = 1.0;
                                // Par(4) = CycRatio or SpeedRatio
                                Par[5] = this->heatingSpeedNum;
                                Par[6] = double(FanOpMode);
                                Par[7] = 1.0; // UnitarySystem(UnitarySysNum)%CompOp
                                Par[8] = ReqOutput;
                                if (this->heatingSpeedNum > 1.0) {
                                    Par[4] = CycRatio;
                                    // General::SolveRoot(Acc, MaxIte, SolFla, SpeedRatio, this->HeatingCoilVarSpeedResidual, 0.0, 1.0, Par);
                                    this->heatingCycRatio = CycRatio;
                                    this->heatingSpeedRatio = SpeedRatio;
                                    this->heatingPartLoadFrac = SpeedRatio;
                                    this->calcPassiveSystem(AirLoopNum, FirstHVACIteration);
                                    PartLoadFrac = SpeedRatio;
                                } else {
                                    SpeedRatio = 0.0;
                                    this->heatingSpeedRatio = SpeedRatio;
                                    Par[4] = SpeedRatio;
                                    // General::SolveRoot(Acc, MaxIte, SolFla, CycRatio, this->HeatingCoilVarSpeedCycResidual, 0.0, 1.0, Par);
                                    this->heatingCycRatio = CycRatio;
                                    this->heatingPartLoadFrac = CycRatio;
                                    this->calcPassiveSystem(AirLoopNum, FirstHVACIteration);
                                    PartLoadFrac = CycRatio;
                                }

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) {

                                HeatingCoils::SimulateHeatingCoilComponents(
                                    this->heatingCoilName, FirstHVACIteration, ReqOutput, CompIndex, _, true, FanOpMode, PartLoadFrac);
                                PartLoadFrac = ReqOutput / FullOutput;

                            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater)) {

                                Par[1] = double(1);
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
                                Par[5] = FanOpMode;
                                // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->GasElecHeatingCoilResidual, 0.0, 1.0, Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                                Par[1] = double(1); // UnitarySysNum);
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
                                        ((mdot / this->maxHeatCoilFluidFlow) +
                                         0.001)); // plant can limit flow and RegulaFalsi could hit max iteration limit (leave a little slop, 0.001)
                                // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->HotWaterHeatingCoilResidual, 0.0, maxPartLoadFrac,
                                // Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                                Par[1] = double(1); // UnitarySysNum);
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
                                        ((mdot / this->maxHeatCoilFluidFlow) +
                                         0.001)); // plant can limit flow and RegulaFalsi could hit max iteration limit (leave a little slop, 0.001)
                                // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->SteamHeatingCoilResidual, 0.0, maxPartLoadFrac, Par);

                            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) ||
                                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWaterToAirHP)) {

                                Par[1] = double(1); // UnitarySysNum);
                                if (FirstHVACIteration) {
                                    Par[2] = 1.0;
                                } else {
                                    Par[2] = 0.0;
                                }
                                Par[3] = DesOutTemp;
                                Par[4] = ReqOutput;
                                this->heatingCoilSensDemand = ReqOutput;

                                // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->HeatWatertoAirHPTempResidual, 0.0, 1.0, Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) {

                                // should never get here, user defined coil cannot be controlled and has already been simulated

                            } else {
                                ShowMessage(" For :" + this->unitType + "=\"" + this->name + "\"");
                                ShowFatalError("ControlHeatingSystemToSP: Invalid heating coil type = " +
                                               DataHVACGlobals::cAllCoilTypes(this->heatingCoilType_Num));
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
                    if (this->HeatCoilSensPLRIter < 1) {
                        ++this->HeatCoilSensPLRIter;
                        ShowWarningError(this->unitType +
                                         " - Iteration limit exceeded calculating sensible part-load ratio for unit = " + this->name);
                        ShowContinueError("Estimated part-load ratio  = " + General::RoundSigDigits((ReqOutput / FullOutput), 3));
                        ShowContinueError("Calculated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                        ShowContinueErrorTimeStamp("The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(
                            this->unitType + " \"" + this->name +
                                "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.",
                            this->HeatCoilSensPLRIterIndex,
                            PartLoadFrac,
                            PartLoadFrac);
                    }
                }
            } else if (SolFla == -2) {
                PartLoadFrac = ReqOutput / FullOutput;
                if (!DataGlobals::WarmupFlag) {
                    if (this->HeatCoilSensPLRFail < 1) {
                        ++this->HeatCoilSensPLRFail;
                        ShowWarningError(this->unitType +
                                         " - sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + this->name);
                        ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                        ShowContinueErrorTimeStamp("The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                    } else {
                        ShowRecurringWarningErrorAtEnd(
                            this->unitType + " \"" + this->name +
                                "\" - sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.",
                            this->HeatCoilSensPLRFailIndex,
                            PartLoadFrac,
                            PartLoadFrac);
                    }
                }
            }
        }

        // Set the final results
        this->heatingPartLoadFrac = PartLoadFrac;
        this->heatingSpeedRatio = SpeedRatio;
        this->heatingCycRatio = CycRatio;

        DataAirLoop::LoopHeatingCoilMaxRTF = max(DataAirLoop::LoopHeatingCoilMaxRTF, LoopHeatingCoilMaxRTFSave);
        DataAirLoop::LoopDXCoilRTF = max(DataAirLoop::LoopDXCoilRTF, LoopDXCoilMaxRTFSave);

        if (this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater || this->heatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
            mdot = PartLoadFrac * this->maxHeatCoilFluidFlow;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->heatCoilFluidInletNode,
                                                 this->heatCoilFluidOutletNodeNum,
                                                 this->heatCoilLoopNum,
                                                 this->heatCoilLoopSide,
                                                 this->heatCoilBranchNum,
                                                 this->heatCoilCompNum);
        }
    }

    void UnitarySys::controlSuppHeatSystem(int const AirLoopNum,         // index to air loop
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
        std::string CompName; // Name of the heating coil
        int CompIndex;        // Index to the heating coil
        Real64 FullOutput;    // Sensible capacity (outlet - inlet) when the compressor is on
        Real64 ReqOutput;     // Sensible capacity (outlet - inlet) required to meet load or set point temperature
        int InletNode;        // Inlet node number of the DX cooling coil
        int OutletNode;       // Outlet node number of the DX cooling coil
        int ControlNode;      // The node number where a set point is placed to control the DX cooling coil
        Real64 PartLoadFrac;  // The part-load fraction of the compressor

        Real64 DesOutTemp;  // Desired outlet temperature of the DX cooling coil
        Real64 QCoilActual; // Heating coil operating capacity [W]

        int SolFla;                       // Flag of solver, num iterations if >0, else error index
        Array1D<Real64> Par(8);           // Parameter array passed to solver
        bool SensibleLoad;                // True if there is a sensible cooling load on this system
        int FanOpMode;                    // Supply air fan operating mode
        Real64 LoopHeatingCoilMaxRTFSave; // Used to find RTF of heating coils without overwriting globabl variable
        Real64 LoopDXCoilMaxRTFSave;      // Used to find RTF of DX heating coils without overwriting globabl variable
        Real64 NoLoadTempOut;             // save outlet temp when coil is off (C)
        bool HeatingActive;               // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.
        bool CoolingActive;               // dummy variable for UserDefined coil which are passed back indicating if coil is on or off.
        Real64 mdot;                      // water coil water flow rate [kg/s]
        Real64 maxPartLoadFrac;           // calculated maximum water side PLR for RegulaFalsi call (when plant limits flow max PLR != 1)

        // Set local variables

        OutletNode = this->suppCoilAirOutletNode;
        InletNode = this->suppCoilAirInletNode;
        ControlNode = this->suppCoilAirOutletNode;
        DesOutTemp = this->desiredOutletTemp;
        CompName = this->suppHeatCoilName;
        CompIndex = this->suppHeatCoilIndex;
        FanOpMode = this->fanOpMode;
        SolFla = 0.0;

        PartLoadFrac = 0.0;

        SensibleLoad = false;

        LoopHeatingCoilMaxRTFSave = DataAirLoop::LoopHeatingCoilMaxRTF;
        DataAirLoop::LoopHeatingCoilMaxRTF = 0.0;
        LoopDXCoilMaxRTFSave = DataAirLoop::LoopDXCoilRTF;
        DataAirLoop::LoopDXCoilRTF = 0.0;

        // IF there is a fault of coil SAT Sensor (zrp_Nov2016)
        if (this->faultyCoilSATFlag && (!DataGlobals::WarmupFlag) && (!DataGlobals::DoingSizing) && (!DataGlobals::KickOffSimulation)) {
            // calculate the sensor offset using fault information
            int FaultIndex = this->faultyCoilSATIndex;
            this->faultyCoilSATOffset = FaultsManager::FaultsCoilSATSensor(FaultIndex).CalFaultOffsetAct();
            // update the DesOutTemp
            DesOutTemp -= this->faultyCoilSATOffset;
        }

        if ((ScheduleManager::GetCurrentScheduleValue(this->sysAvailSchedPtr) > 0.0) &&
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
                    auto const SELECT_CASE_var(this->suppHeatCoilType_Num);

                    if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) ||
                        (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric) ||
                        (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater)) {
                        HeatingCoils::SimulateHeatingCoilComponents(CompName,
                                                                    FirstHVACIteration,
                                                                    _,
                                                                    CompIndex,
                                                                    QCoilActual,
                                                                    SuppHeatingCoilFlag,
                                                                    FanOpMode,
                                                                    PartLoadFrac); // QCoilReq= 0.0d0,  &
                        if (!(SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater)) {
                            PartLoadFrac = QCoilActual / this->designSuppHeatingCapacity;
                        } else {
                            if (QCoilActual > DataHVACGlobals::SmallLoad) {
                                PartLoadFrac = 1.0;
                            } else {
                                PartLoadFrac = 0.0;
                            }
                        }
                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                        WaterCoils::SimulateWaterCoilComponents(
                            CompName, FirstHVACIteration, this->suppHeatCoilIndex, _, this->fanOpMode, PartLoadFrac);

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                        SteamCoils::SimulateSteamCoilComponents(CompName,
                                                                FirstHVACIteration,
                                                                this->suppHeatCoilIndex,
                                                                1.0,
                                                                _,
                                                                this->fanOpMode,
                                                                PartLoadFrac); // QCoilReq, simulate any load > 0 to get max capacity

                    } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) { // do nothing, user defined coil cannot be controlled
                        HeatingActive = false;                                         // set to arbitrary value on entry to function
                        CoolingActive = false;                                         // set to arbitrary value on entry to function
                        UserDefinedComponents::SimCoilUserDefined(CompName, CompIndex, AirLoopNum, HeatingActive, CoolingActive);
                        if (HeatingActive) PartLoadFrac = 1.0;

                    } else {
                    }
                }

                NoLoadTempOut = DataLoopNode::Node(OutletNode).Temp;
                //      NoOutput = Node(InletNode)%MassFlowRate *  &
                //                       (PsyHFnTdbW(NoLoadTempOut,Node(OutletNode)%HumRat)  &
                //                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

                //     If OutletTemp is within ACC of set point, either coil operated or is not needed
                if (std::abs(DataLoopNode::Node(OutletNode).Temp - DesOutTemp) < Acc ||
                    this->suppHeatCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                    // do nothing, coil is at set point (i.e., gas/elec/steam/user coil will try to hit set point
                } else if (PartLoadFrac > 0.0) {
                    // do nothing, coil tried to hit set point (i.e., gas/elec/steam/user coil tried to hit set point but missed
                } else if (NoLoadTempOut > (DesOutTemp - Acc)) {
                    PartLoadFrac = 0.0; // outlet temp > set point, coil is not needed
                } else {                // outlet temp too low, turn on coil

                    // Get full load result
                    PartLoadFrac = 1.0;

                    {
                        auto const SELECT_CASE_var(this->suppHeatCoilType_Num);

                        if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) ||
                            (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric)) {

                            HeatingCoils::SimulateHeatingCoilComponents(
                                CompName, FirstHVACIteration, _, CompIndex, QCoilActual, SuppHeatingCoilFlag, FanOpMode, PartLoadFrac);
                            PartLoadFrac = QCoilActual / this->designSuppHeatingCapacity;

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater) {

                            HeatingCoils::SimulateHeatingCoilComponents(
                                CompName, FirstHVACIteration, ReqOutput, CompIndex, _, SuppHeatingCoilFlag, FanOpMode);

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                            mdot = this->maxSuppCoilFluidFlow;
                            PlantUtilities::SetComponentFlowRate(mdot,
                                                                 this->suppCoilFluidInletNode,
                                                                 this->suppCoilFluidOutletNodeNum,
                                                                 this->suppCoilLoopNum,
                                                                 this->suppCoilLoopSide,
                                                                 this->suppCoilBranchNum,
                                                                 this->suppCoilCompNum);

                            WaterCoils::SimulateWaterCoilComponents(
                                CompName, FirstHVACIteration, this->suppHeatCoilIndex, _, this->fanOpMode, PartLoadFrac);

                        } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                            mdot = this->maxSuppCoilFluidFlow;
                            PlantUtilities::SetComponentFlowRate(mdot,
                                                                 this->suppCoilFluidInletNode,
                                                                 this->suppCoilFluidOutletNodeNum,
                                                                 this->suppCoilLoopNum,
                                                                 this->suppCoilLoopSide,
                                                                 this->suppCoilBranchNum,
                                                                 this->suppCoilCompNum);

                            SteamCoils::SimulateSteamCoilComponents(CompName,
                                                                    FirstHVACIteration,
                                                                    this->suppHeatCoilIndex,
                                                                    1.0,
                                                                    _,
                                                                    this->fanOpMode,
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
                            auto const SELECT_CASE_var(this->suppHeatCoilType_Num);

                            if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGasOrOtherFuel) ||
                                (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric) ||
                                (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingDesuperheater)) {

                                Par[1] = double(1);
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
                                // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->GasElecHeatingCoilResidual, 0.0, 1.0, Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingWater) {

                                Par[1] = double(1);
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
                                Par(5) = 0.0;

                                // calculate max waterside PLR from mdot request above in case plant chokes water flow
                                maxPartLoadFrac =
                                    min(1.0,
                                        ((mdot / this->maxSuppCoilFluidFlow) +
                                         0.001)); // plant can limit flow and RegulaFalsi could hit max iteration limit (leave a little slop, 0.001)
                                // General::SolveRoot(Acc, SolveMaxIter, SolFla, PartLoadFrac, this->HotWaterHeatingCoilResidual, 0.0,
                                // maxPartLoadFrac, Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingSteam) {

                                Par[1] = double(1);
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
                                        ((mdot / this->maxSuppCoilFluidFlow) +
                                         0.001)); // plant can limit flow and RegulaFalsi could hit max iteration limit (leave a little slop, 0.001)
                                // General::SolveRoot(Acc, MaxIte, SolFla, PartLoadFrac, this->SteamHeatingCoilResidual, 0.0, maxPartLoadFrac, Par);

                            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_UserDefined) {

                                //  do nothing, coil has already been simulated

                            } else {
                            }
                        }

                    } // IF ((FullOutput - ReqOutput) < Acc) THEN
                }     // IF ((NoOutput-ReqOutput) > Acc) THEN
            }         // IF (SensibleLoad ) THEN
        }             // IF((GetCurrentScheduleValue(UnitarySystem(UnitarySysNum)%SysAvailSchedPtr) > 0.0d0) .AND. &

        if (PartLoadFrac > 1.0) {
            PartLoadFrac = 1.0;
        } else if (PartLoadFrac < 0.0) {
            PartLoadFrac = 0.0;
        }

        if (SolFla == -1) {
            if (!DataGlobals::WarmupFlag) {
                if (this->SuppHeatCoilSensPLRIter < 1) {
                    ++this->SuppHeatCoilSensPLRIter;
                    ShowWarningError(this->unitType + " - Iteration limit exceeded calculating sensible part-load ratio for unit = " + this->name);
                    ShowContinueError("Estimated part-load ratio  = " + General::RoundSigDigits((ReqOutput / FullOutput), 3));
                    ShowContinueError("Calculated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The calculated part-load ratio will be used and the simulation continues. Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->unitType + " \"" + this->name +
                            "\" - Iteration limit exceeded calculating sensible part-load ratio error continues. Sensible PLR statistics follow.",
                        this->SuppHeatCoilSensPLRIterIndex,
                        PartLoadFrac,
                        PartLoadFrac);
                }
            } // IF(.NOT. WarmupFlag)THEN
        } else if (SolFla == -2) {
            PartLoadFrac = ReqOutput / FullOutput;
            if (!DataGlobals::WarmupFlag) {
                if (this->SuppHeatCoilSensPLRFail < 1) {
                    ++this->SuppHeatCoilSensPLRFail;
                    ShowWarningError(this->unitType +
                                     " - sensible part-load ratio calculation failed: part-load ratio limits exceeded, for unit = " + this->name);
                    ShowContinueError("Estimated part-load ratio = " + General::RoundSigDigits(PartLoadFrac, 3));
                    ShowContinueErrorTimeStamp("The estimated part-load ratio will be used and the simulation continues. Occurrence info:");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        this->unitType + " \"" + this->name +
                            "\" - sensible part-load ratio calculation failed error continues. Sensible PLR statistics follow.",
                        this->SuppHeatCoilSensPLRFailIndex,
                        PartLoadFrac,
                        PartLoadFrac);
                }
            } // IF(.NOT. WarmupFlag)THEN
        }     // IF (SolFla == -1) THEN

        this->suppHeatPartLoadFrac = PartLoadFrac;

        // LoopHeatingCoilMaxRTF used for AirflowNetwork gets set in child components (gas and fuel)
        DataAirLoop::LoopHeatingCoilMaxRTF = max(DataAirLoop::LoopHeatingCoilMaxRTF, LoopHeatingCoilMaxRTFSave);
        DataAirLoop::LoopDXCoilRTF = max(DataAirLoop::LoopDXCoilRTF, LoopDXCoilMaxRTFSave);

        if (this->suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater || this->suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
            mdot = PartLoadFrac * this->maxSuppCoilFluidFlow;
            PlantUtilities::SetComponentFlowRate(mdot,
                                                 this->suppCoilFluidInletNode,
                                                 this->suppCoilFluidOutletNodeNum,
                                                 this->suppCoilLoopNum,
                                                 this->suppCoilLoopSide,
                                                 this->suppCoilBranchNum,
                                                 this->suppCoilCompNum);
        }
    }

    void UnitarySys::simMultiSpeedCoils(int const AirLoopNum,          // Index to air loop
                                        bool const FirstHVACIteration, // True when first HVAC iteration
                                        int &CompOn,                   // compresor on/off control
                                        bool const SensibleLoad,
                                        bool const LatentLoad,
                                        Real64 const PartLoadFrac,
                                        int const CoilType,
                                        Optional_int_const SpeedNumber)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma, FSEC
        //       DATE WRITTEN   March 2013

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages multispeed and variable speed cooling coil simulation.

        // Locals
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string CompName; // Name of Unitary System object
        Real64 dummy;
        Real64 SensLoad;
        Real64 LatLoad;
        Real64 OnOffAirFlowRatio;
        int CoilTypeNum;
        int SpeedNum;
        int CoilOutletNodeNum;
        int CompIndex;
        Real64 SpeedRatio;
        Real64 CycRatio;

        dummy = 0.0;

        if (present(SpeedNumber)) {
            SpeedNum = SpeedNumber;
        } else {
            SpeedNum = 1;
        }

        if (CoilType == CoolingCoil) {

            CompName = this->coolingCoilName;
            CompIndex = this->coolingCoilIndex;
            CoilTypeNum = this->coolingCoilType_Num;
            CoilOutletNodeNum = this->coolCoilOutletNodeNum;
            if (SensibleLoad) {
                SensLoad = -1.0;
                CoolingLoad = true;
                HeatingLoad = false;
            }
            if (LatentLoad) LatLoad = -1.0;

        } else {

            CompName = this->heatingCoilName;
            CompIndex = this->heatingCoilIndex;
            CoilTypeNum = this->heatingCoilType_Num;
            CoilOutletNodeNum = this->heatCoilOutletNodeNum;

            if (SensibleLoad) {
                SensLoad = 1.0;
                CoolingLoad = false;
                HeatingLoad = true;
            } else {
                SensLoad = 0.0;
                HeatingLoad = false;
            }
            LatLoad = 0.0;
            this->fanOpMode = 1; // why is this here?
        }

        OnOffAirFlowRatio = 1.0;
        this->setOnOffMassFlowRate(OnOffAirFlowRatio, PartLoadFrac); // 1.0d0 = PartLoadRatio

        this->calcPassiveSystem(AirLoopNum, FirstHVACIteration);

        if ((CoilTypeNum == DataHVACGlobals::CoilDX_MultiSpeedCooling) || (CoilTypeNum == DataHVACGlobals::CoilDX_MultiSpeedHeating)) {

            if (CoilType == DataHVACGlobals::Cooling) {
                if (this->coolingSpeedNum <= 1.0) {
                    SpeedRatio = 0.0;
                    CycRatio = PartLoadFrac;
                } else {
                    if (this->singleMode == 0) {
                        SpeedRatio = PartLoadFrac;
                        CycRatio = 0.0;
                    } else {
                        SpeedRatio = 1.0;
                        CycRatio = PartLoadFrac;
                    }
                }
            } else {
                if (this->heatingSpeedNum <= 1.0) {
                    SpeedRatio = 0.0;
                    CycRatio = PartLoadFrac;
                } else {
                    if (this->singleMode == 0) {
                        SpeedRatio = PartLoadFrac;
                        CycRatio = 0.0;
                    } else {
                        SpeedRatio = 1.0;
                        CycRatio = PartLoadFrac;
                    }
                }
            }
            DXCoils::SimDXCoilMultiSpeed(CompName, 0.0, PartLoadFrac, CompIndex, SpeedNum, this->fanOpMode, 1, this->singleMode);

        } else if (CoilTypeNum == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {

            VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                      CompIndex,
                                                      this->fanOpMode,
                                                      this->maxONOFFCyclesperHour,
                                                      this->HPTimeConstant,
                                                      this->fanDelayTime,
                                                      CompOn,
                                                      PartLoadFrac,
                                                      SpeedNum,
                                                      this->coolingSpeedRatio,
                                                      SensLoad,
                                                      dummy,
                                                      OnOffAirFlowRatio);

        } else if (CoilTypeNum == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {

            VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                      CompIndex,
                                                      this->fanOpMode,
                                                      this->maxONOFFCyclesperHour,
                                                      this->HPTimeConstant,
                                                      this->fanDelayTime,
                                                      CompOn,
                                                      PartLoadFrac,
                                                      SpeedNum,
                                                      this->heatingSpeedRatio,
                                                      SensLoad,
                                                      dummy,
                                                      OnOffAirFlowRatio);

        } else if (CoilTypeNum == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) {

            VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                      CompIndex,
                                                      this->fanOpMode,
                                                      this->maxONOFFCyclesperHour,
                                                      this->HPTimeConstant,
                                                      this->fanDelayTime,
                                                      CompOn,
                                                      PartLoadFrac,
                                                      SpeedNum,
                                                      this->coolingSpeedRatio,
                                                      SensLoad,
                                                      dummy,
                                                      OnOffAirFlowRatio);

        } else if (CoilTypeNum == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) {

            VariableSpeedCoils::SimVariableSpeedCoils(CompName,
                                                      CompIndex,
                                                      this->fanOpMode,
                                                      this->maxONOFFCyclesperHour,
                                                      this->HPTimeConstant,
                                                      this->fanDelayTime,
                                                      CompOn,
                                                      PartLoadFrac,
                                                      SpeedNum,
                                                      this->heatingSpeedRatio,
                                                      SensLoad,
                                                      dummy,
                                                      OnOffAirFlowRatio);

        } else if ((CoilTypeNum == DataHVACGlobals::Coil_HeatingElectric_MultiStage) ||
                   (CoilTypeNum == DataHVACGlobals::Coil_HeatingGas_MultiStage)) {

            HeatingCoils::SimulateHeatingCoilComponents(
                CompName, FirstHVACIteration, _, CompIndex, _, _, this->fanOpMode, PartLoadFrac, SpeedNum, this->heatingSpeedRatio);
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

        // Locals
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 PartLoadRatio; // coil operating part-load ratio
        int CompOn;           // compressor control (0=off, 1=on)
        bool HXUnitOn;

        Real64 OnOffAirFlowRatio = 1.0;
        Real64 CoilCoolHeatRat = 1.0;
        Real64 QZnReq = 0.0;
        // CALL the series of components that simulate a Unitary System
        if (this->fanExists && this->fanPlace == fanPlaceEnum::blowThru) {
            if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->fanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->fanIndex, FanSpeedRatio);
            }
        }

        if (this->coolingCoilUpstream) {

            if (this->coolCoilExists) {
                PartLoadRatio = this->coolingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                HXUnitOn = false;
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }
            if (this->heatCoilExists) {
                PartLoadRatio = this->heatingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio);
            }

        } else {

            if (this->heatCoilExists) {
                PartLoadRatio = this->heatingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                this->calcUnitaryHeatingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio);
            }
            if (this->coolCoilExists) {
                PartLoadRatio = this->coolingPartLoadFrac;
                CompOn = 0;
                if (PartLoadRatio > 0.0) CompOn = 1;
                HXUnitOn = false;
                this->calcUnitaryCoolingSystem(AirLoopNum, FirstHVACIteration, PartLoadRatio, CompOn, OnOffAirFlowRatio, CoilCoolHeatRat, HXUnitOn);
            }
        }

        if (this->fanExists && this->fanPlace == fanPlaceEnum::drawThru) {
            if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->fanIndex]->simulate(_, _, _, _, m_massFlow1, m_runTimeFraction1, m_massFlow2, m_runTimeFraction2, _);
            } else {
                Fans::SimulateFanComponents(blankString, FirstHVACIteration, this->fanIndex, FanSpeedRatio);
            }
        }

        // CALL reheat coils next
        if (this->suppCoilExists) {
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
        this->partLoadFrac = 0.0;
        this->compPartLoadRatio = 0.0;
        this->cycRatio = 0.0;
        this->speedRatio = 0.0;
        this->fanPartLoadRatio = 0.0;
        this->totalAuxElecPower = 0.0;
        this->heatingAuxElecConsumption = 0.0;
        this->coolingAuxElecConsumption = 0.0;
        this->elecPower = 0.0;
        this->elecPowerConsumption = 0.0;

        int OutletNode = this->airOutNode;
        Real64 AirMassFlow = DataLoopNode::Node(OutletNode).MassFlowRate;

        {
            auto const SELECT_CASE_var(this->controlType);
            if (SELECT_CASE_var == controlTypeEnum::controlTypeSetpoint) {
                int InletNode = this->airInNode;
                Real64 MinHumRatio = DataLoopNode::Node(InletNode).HumRat;
                QSensUnitOut = AirMassFlow * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, MinHumRatio) -
                                              Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(InletNode).Temp, MinHumRatio)) -
                               this->senLoadLoss;
                QTotUnitOut = AirMassFlow * (DataLoopNode::Node(OutletNode).Enthalpy - DataLoopNode::Node(InletNode).Enthalpy);

            } else {
                Real64 MinHumRatio = DataLoopNode::Node(this->nodeNumOfControlledZone).HumRat;
                QSensUnitOut = AirMassFlow * (Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(OutletNode).Temp, MinHumRatio) -
                                              Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(this->nodeNumOfControlledZone).Temp, MinHumRatio)) -
                               this->senLoadLoss;
                QTotUnitOut = AirMassFlow * (DataLoopNode::Node(OutletNode).Enthalpy - DataLoopNode::Node(this->nodeNumOfControlledZone).Enthalpy);
            }
        }

        // set the system part-load ratio report variable
        this->partLoadFrac = max(this->coolingPartLoadFrac, this->heatingPartLoadFrac);
        // set the compressor part-load ratio report variable
        this->compPartLoadRatio = max(this->coolCompPartLoadRatio, this->heatCompPartLoadRatio);

        if (HeatingLoad) {
            if (QTotUnitOut > 0.0) { // heating
                this->totCoolEnergyRate = 0.0;
                this->sensCoolEnergyRate = 0.0;
                this->latCoolEnergyRate = 0.0;
                this->totHeatEnergyRate = QTotUnitOut;
                this->sensHeatEnergyRate = std::abs(max(0.0, QSensUnitOut));
                this->latHeatEnergyRate = std::abs(max(0.0, (QTotUnitOut - QSensUnitOut)));
            } else {
                this->totCoolEnergyRate = std::abs(QTotUnitOut);
                this->sensCoolEnergyRate = std::abs(min(0.0, QSensUnitOut));
                this->latCoolEnergyRate = std::abs(min(0.0, (QTotUnitOut - QSensUnitOut)));
                this->totHeatEnergyRate = 0.0;
                this->sensHeatEnergyRate = 0.0;
                this->latHeatEnergyRate = 0.0;
            }
        } else {
            if (QTotUnitOut <= 0.0) { // cooling
                this->totCoolEnergyRate = std::abs(min(0.0, QTotUnitOut));
                this->sensCoolEnergyRate = std::abs(min(0.0, QSensUnitOut));
                this->latCoolEnergyRate = std::abs(min(0.0, (QTotUnitOut - QSensUnitOut)));
                this->totHeatEnergyRate = 0.0;
                this->sensHeatEnergyRate = 0.0;
                this->latHeatEnergyRate = 0.0;
            } else {
                this->totCoolEnergyRate = 0.0;
                this->sensCoolEnergyRate = 0.0;
                this->latCoolEnergyRate = 0.0;
                this->totHeatEnergyRate = QTotUnitOut;
                this->sensHeatEnergyRate = std::abs(max(0.0, QSensUnitOut));
                this->latHeatEnergyRate = std::abs(max(0.0, (QTotUnitOut - QSensUnitOut)));
            }
        }

        if (this->fanExists) {
            if (CompOnMassFlow > 0.0) {
                this->fanPartLoadRatio = DataLoopNode::Node(OutletNode).MassFlowRate / CompOnMassFlow;
            } else {
                this->fanPartLoadRatio = 0.0;
            }
            if (AirLoopNum > 0) {
                if (this->fanOpMode == fanOpModeEnum::cycFanCycCoil) {
                    DataAirLoop::AirLoopFlow(AirLoopNum).FanPLR = this->fanPartLoadRatio;
                } else {
                    DataAirLoop::AirLoopFlow(AirLoopNum).FanPLR = 0.0;
                }
            }
        }

        Real64 locFanElecPower = 0.0;
        if (this->fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            locFanElecPower = HVACFan::fanObjs[this->fanIndex]->fanPower();
        } else {
            locFanElecPower = Fans::GetFanPower(this->fanIndex);
        }

        {
            auto const SELECT_CASE_var(this->coolingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                // need to make sure these are 0 for non-variable speed coils (or not report these variables)
                this->cycRatio = max(this->coolingCycRatio, this->heatingCycRatio);
                this->speedRatio = max(this->coolingSpeedRatio, this->heatingSpeedRatio);
                this->speedNum = max(this->coolingSpeedNum, this->heatingSpeedNum);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                this->cycRatio = max(this->coolingCycRatio, this->heatingCycRatio);
                this->speedRatio = max(this->coolingSpeedRatio, this->heatingSpeedRatio);
                this->speedNum = max(this->coolingSpeedNum, this->heatingSpeedNum);

                int CompPartLoadFrac = this->compPartLoadRatio;
                if (CoolingLoad) {

                    this->totalAuxElecPower = this->ancillaryOnPower * CompPartLoadFrac + this->ancillaryOffPower * (1.0 - CompPartLoadFrac);
                    this->coolingAuxElecConsumption = this->ancillaryOnPower * CompPartLoadFrac * ReportingConstant;
                }
                if (this->lastMode == CoolingMode) {
                    this->coolingAuxElecConsumption += this->ancillaryOffPower * (1.0 - CompPartLoadFrac) * ReportingConstant;
                }
                this->elecPower = locFanElecPower + DataHVACGlobals::DXElecCoolingPower + DataHVACGlobals::DXElecHeatingPower +
                                  DataHVACGlobals::ElecHeatingCoilPower + this->totalAuxElecPower;
                this->elecPowerConsumption = this->elecPower * ReportingConstant;

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWater || SELECT_CASE_var == DataHVACGlobals::Coil_CoolingWaterDetailed) {

                if (this->multiSpeedCoolingCoil) {
                    this->cycRatio = max(this->coolingCycRatio, this->heatingCycRatio);
                    this->speedRatio = max(this->coolingSpeedRatio, this->heatingSpeedRatio);
                    this->speedNum = max(this->coolingSpeedNum, this->heatingSpeedNum);
                }
                this->elecPower = locFanElecPower;
                this->elecPowerConsumption = this->elecPower * ReportingConstant;

            } else {
            }
        }

        {
            auto const SELECT_CASE_var(this->heatingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                this->cycRatio = max(this->coolingCycRatio, this->heatingCycRatio);
                this->speedRatio = max(this->coolingSpeedRatio, this->heatingSpeedRatio);
                this->speedNum = max(this->coolingSpeedNum, this->heatingSpeedNum);

                int CompPartLoadFrac = this->compPartLoadRatio;
                if (HeatingLoad) {

                    this->totalAuxElecPower = this->ancillaryOnPower * CompPartLoadFrac + this->ancillaryOffPower * (1.0 - CompPartLoadFrac);
                    this->heatingAuxElecConsumption = this->ancillaryOnPower * CompPartLoadFrac * ReportingConstant;
                }
                if (this->lastMode == HeatingMode) {
                    this->heatingAuxElecConsumption += this->ancillaryOffPower * (1.0 - CompPartLoadFrac) * ReportingConstant;
                }
                this->elecPower =
                    locFanElecPower + DataHVACGlobals::DXElecCoolingPower + DataHVACGlobals::DXElecHeatingPower + this->totalAuxElecPower;
                this->elecPowerConsumption = this->elecPower * ReportingConstant;

            } else if ((SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage) ||
                       (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage)) {
                this->cycRatio = max(this->coolingCycRatio, this->heatingCycRatio);
                this->speedRatio = max(this->coolingSpeedRatio, this->heatingSpeedRatio);

                this->elecPower =
                    locFanElecPower + DataHVACGlobals::DXElecCoolingPower + DataHVACGlobals::ElecHeatingCoilPower + this->totalAuxElecPower;
                this->elecPowerConsumption = this->elecPower * ReportingConstant;

            } else {
            }
        }

        DataAirLoop::LoopSystemOnMassFlowrate = CompOnMassFlow;
        DataAirLoop::LoopSystemOffMassFlowrate = CompOffMassFlow;
        DataAirLoop::LoopFanOperationMode = this->fanOpMode;
        DataAirLoop::LoopOnOffFanPartLoadRatio = this->fanPartLoadRatio;
        DataAirLoop::LoopCompCycRatio = this->cycRatio;

        if (this->firstPass) {

            if (!DataGlobals::SysSizingCalc) {

                if (DataSizing::CurOASysNum > 0) {
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).AirFlow = false;
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).CoolingAirFlow = false;
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).HeatingAirFlow = false;
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).Capacity = false;
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).CoolingCapacity = false;
                    DataSizing::OASysEqSizing(DataSizing::CurOASysNum).HeatingCapacity = false;
                } else if (DataSizing::CurSysNum > 0) {
                    DataSizing::UnitarySysEqSizing(DataSizing::CurSysNum).AirFlow = false;
                    DataSizing::UnitarySysEqSizing(DataSizing::CurSysNum).CoolingAirFlow = false;
                    DataSizing::UnitarySysEqSizing(DataSizing::CurSysNum).HeatingAirFlow = false;
                    DataSizing::UnitarySysEqSizing(DataSizing::CurSysNum).Capacity = false;
                    DataSizing::UnitarySysEqSizing(DataSizing::CurSysNum).CoolingCapacity = false;
                    DataSizing::UnitarySysEqSizing(DataSizing::CurSysNum).HeatingCapacity = false;
                    DataAirLoop::AirLoopControlInfo(DataSizing::CurSysNum).UnitarySysSimulating = false;
                } else if (DataSizing::CurZoneEqNum > 0) {
                    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).AirFlow = false;
                    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).CoolingAirFlow = false;
                    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingAirFlow = false;
                    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).Capacity = false;
                    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).CoolingCapacity = false;
                    DataSizing::ZoneEqSizing(DataSizing::CurZoneEqNum).HeatingCapacity = false;
                }
                this->firstPass = false;
            }
        }

        // reset to 1 in case blow through fan configuration (fan resets to 1, but for blow thru fans coil sets back down < 1)
        DataHVACGlobals::OnOffFanPartLoadFraction = 1.0;
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

        int HeatRecInNode = this->heatRecoveryInletNodeNum;
        int HeatRecOutNode = this->heatRecoveryOutletNodeNum;

        Real64 HeatRecInletTemp = DataLoopNode::Node(HeatRecInNode).Temp;
        Real64 HeatRecOutletTemp = 0.0;

        // Set heat recovery mass flow rates
        Real64 HeatRecMassFlowRate = DataLoopNode::Node(HeatRecInNode).MassFlowRate;

        Real64 QHeatRec = DataHVACGlobals::MSHPWasteHeat;

        if (HeatRecMassFlowRate > 0.0) {

            Real64 CpHeatRec = FluidProperties::GetSpecificHeatGlycol(
                DataPlant::PlantLoop(this->HRLoopNum).FluidName, HeatRecInletTemp, DataPlant::PlantLoop(this->HRLoopNum).FluidIndex, routineName);

            HeatRecOutletTemp = QHeatRec / (HeatRecMassFlowRate * CpHeatRec) + HeatRecInletTemp;
            // coil model should be handling max outlet water temp (via limit to heat transfer) since heat rejection needs to be accounted for by the
            // coil
            if (HeatRecOutletTemp > this->maxHROutletWaterTemp) {
                HeatRecOutletTemp = max(HeatRecInletTemp, this->maxHROutletWaterTemp);
                QHeatRec = HeatRecMassFlowRate * CpHeatRec * (HeatRecOutletTemp - HeatRecInletTemp);
            }
        } else {
            HeatRecOutletTemp = HeatRecInletTemp;
            QHeatRec = 0.0;
        }

        PlantUtilities::SafeCopyPlantNode(HeatRecInNode, HeatRecOutNode);

        DataLoopNode::Node(HeatRecOutNode).Temp = HeatRecOutletTemp;

        this->heatRecoveryRate = QHeatRec;
        this->heatRecoveryEnergy = this->heatRecoveryRate * ReportingConstant;
        this->heatRecoveryInletTemp = HeatRecInletTemp;
        this->heatRecoveryOutletTemp = HeatRecOutletTemp;
        this->heatRecoveryMassFlowRate = HeatRecMassFlowRate;
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

        Nmax = this->maxONOFFCyclesperHour;
        tau = this->HPTimeConstant;
        pr = this->onCyclePowerFraction;

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

        if (!SuppHeatingCoilFlag) {
            Real64 mdot = min(DataLoopNode::Node(this->heatCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->maxHeatCoilFluidFlow * PartLoadFrac);
            DataLoopNode::Node(this->heatCoilFluidInletNode).MassFlowRate = mdot;
            WaterCoils::SimulateWaterCoilComponents(
                this->heatingCoilName, FirstHVACIteration, this->heatingCoilIndex, QActual, this->fanOpMode, PartLoadFrac);
            OutletAirTemp = DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp;
        } else {
            Real64 mdot = min(DataLoopNode::Node(this->suppCoilFluidOutletNodeNum).MassFlowRateMaxAvail, this->maxSuppCoilFluidFlow * PartLoadFrac);
            DataLoopNode::Node(this->suppCoilFluidInletNode).MassFlowRate = mdot;
            WaterCoils::SimulateWaterCoilComponents(
                this->suppHeatCoilName, FirstHVACIteration, this->suppHeatCoilIndex, QActual, this->fanOpMode, PartLoadFrac);
            OutletAirTemp = DataLoopNode::Node(this->suppCoilAirOutletNode).Temp;
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
        // par(5) = supply air fan operating mode (ContFanCycCoil)

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
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        int CoilIndex = int(Par[1]);
        int FanOpMode = int(Par[5]);
        DXCoils::CalcDoe2DXCoil(CoilIndex, On, true, PartLoadRatio, FanOpMode);
        Real64 OutletAirHumRat = DXCoils::DXCoilOutletHumRat(CoilIndex);
        Residuum = Par[2] - OutletAirHumRat;

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
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        int CoilIndex = int(Par[1]);
        // FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
        bool FirstHVACIteration = (Par[3] == 1.0);
        bool HXUnitOn = (Par[4] == 1.0);
        int FanOpMode = int(Par[5]);
        int UnitarySysNum = int(Par[6]);
        if (this->coolCoilFluidInletNode > 0) {
            DataLoopNode::Node(this->coolCoilFluidInletNode).MassFlowRate = maxCoolCoilFluidFlow * PartLoadRatio;
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
        // par(5) = supply air fan operating mode (ContFanCycCoil)

        int CoilIndex = int(Par[1]);
        // FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
        bool FirstHVACIteration = (Par[3] == 1.0);
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

        {
            auto const SELECT_CASE_var(this->coolingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                DXCoils::CalcMultiSpeedDXCoil(CoilIndex, SpeedRatio, 1.0);
                OutletAirTemp = DXCoils::DXCoilOutletTemp(CoilIndex);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                OnOffAirFlowRatio = 1.0;

                this->setAverageAirFlow(SpeedRatio, OnOffAirFlowRatio);
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
                                                          this->maxONOFFCyclesperHour,
                                                          this->HPTimeConstant,
                                                          this->fanDelayTime,
                                                          CompOp,
                                                          CycRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          SensLoad,
                                                          dummy,
                                                          OnOffAirFlowRatio);

                OutletAirTemp = DataLoopNode::Node(this->coolCoilOutletNodeNum).Temp;

            } else {
                assert(false);
            }
        }

        Residuum = Par[2] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::HeatingCoilVarSpeedResidual(Real64 const SpeedRatio,       // compressor speed ratio (1.0 is max, 0.0 is min)
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

        {
            auto const SELECT_CASE_var(this->heatingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                OnOffAirFlowRatio = 1.0;

                this->setAverageAirFlow(SpeedRatio, OnOffAirFlowRatio);

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
                                                          this->maxONOFFCyclesperHour,
                                                          this->HPTimeConstant,
                                                          this->fanDelayTime,
                                                          CompOp,
                                                          CycRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          SensLoad,
                                                          LatLoad,
                                                          OnOffAirFlowRatio);

                OutletAirTemp = DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp;

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);

                HeatingCoils::CalcMultiStageElectricHeatingCoil(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode);

                OutletAirTemp = DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp;

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);

                HeatingCoils::CalcMultiStageElectricHeatingCoil(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode);

                OutletAirTemp = DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp;

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
        {
            auto const SELECT_CASE_var(this->coolingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                DXCoils::CalcMultiSpeedDXCoil(CoilIndex, SpeedRatio, 1.0);
                OutletAirHumRat = DXCoils::DXCoilOutletHumRat(CoilIndex);

            } else if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedCooling) {

                CycRatio = Par[4];
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                OnOffAirFlowRatio = 1.0;

                this->setAverageAirFlow(SpeedRatio, OnOffAirFlowRatio);
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
                                                          this->maxONOFFCyclesperHour,
                                                          this->HPTimeConstant,
                                                          this->fanDelayTime,
                                                          CompOp,
                                                          CycRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          SensLoad,
                                                          LatLoad,
                                                          OnOffAirFlowRatio);

                OutletAirHumRat = DataLoopNode::Node(this->coolCoilOutletNodeNum).HumRat;

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

        {
            auto const SELECT_CASE_var(this->coolingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {

                if (this->fanPlace == fanPlaceEnum::blowThru) { // must simulate fan if blow through since OnOffFanPartLoadFrac affects fan heat
                    this->coolingCycRatio = CycRatio;
                    this->coolingPartLoadFrac = CycRatio;
                    this->calcPassiveSystem(AirloopNum, FirstHVACIteration);
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

                this->setAverageAirFlow(CycRatio, OnOffAirFlowRatio);
                if (this->fanPlace == fanPlaceEnum::blowThru) { // must simulate fan if blow through since OnOffFanPartLoadFrac affects fan heat
                    this->coolingCycRatio = CycRatio;
                    this->coolingPartLoadFrac = CycRatio;
                    this->calcPassiveSystem(AirloopNum, FirstHVACIteration);
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
                                                          this->maxONOFFCyclesperHour,
                                                          this->HPTimeConstant,
                                                          this->fanDelayTime,
                                                          CompOp,
                                                          CycRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          SensLoad,
                                                          dummy,
                                                          OnOffAirFlowRatio);

                OutletAirTemp = DataLoopNode::Node(this->coolCoilOutletNodeNum).Temp;

            } else {
                assert(false);
            }
        }

        Residuum = Par[2] - OutletAirTemp;

        return Residuum;
    }

    Real64 UnitarySys::HeatingCoilVarSpeedCycResidual(Real64 const CycRatio,         // compressor cycling ratio (1.0 is continuous, 0.0 is off)
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

        {
            auto const SELECT_CASE_var(this->heatingCoilType_Num);

            if (SELECT_CASE_var == DataHVACGlobals::CoilDX_MultiSpeedHeating) {

                SpeedRatio = int(Par[4]);
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);
                CompOp = int(Par[7]);
                OnOffAirFlowRatio = 1.0;

                this->setAverageAirFlow(CycRatio, OnOffAirFlowRatio);
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
                                                          this->maxONOFFCyclesperHour,
                                                          this->HPTimeConstant,
                                                          this->fanDelayTime,
                                                          CompOp,
                                                          CycRatio,
                                                          SpeedNum,
                                                          SpeedRatio,
                                                          SensLoad,
                                                          LatLoad,
                                                          OnOffAirFlowRatio);

                OutletAirTemp = DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp;

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingElectric_MultiStage) {

                SpeedRatio = int(Par[4]);
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);

                HeatingCoils::CalcMultiStageElectricHeatingCoil(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode);

                OutletAirTemp = DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp;

            } else if (SELECT_CASE_var == DataHVACGlobals::Coil_HeatingGas_MultiStage) {

                SpeedRatio = int(Par[4]);
                SpeedNum = int(Par[5]);
                FanOpMode = int(Par[6]);

                HeatingCoils::CalcMultiStageGasHeatingCoil(CoilIndex, SpeedRatio, CycRatio, SpeedNum, FanOpMode);

                OutletAirTemp = DataLoopNode::Node(this->heatCoilOutletNodeNum).Temp;

            } else {
                assert(false);
            }
        }

        Residuum = Par[2] - OutletAirTemp;

        return Residuum;
    }

} // namespace UnitarySystems
} // namespace EnergyPlus
