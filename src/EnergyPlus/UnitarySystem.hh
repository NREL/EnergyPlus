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

#ifndef ENERGYPLUS_UNITARYSYSTEM_HH
#define ENERGYPLUS_UNITARYSYSTEM_HH

#include <string>
#include <vector>

namespace EnergyPlus {

namespace UnitarySystems {

    void clear_state();

    extern int numUnitarySystems;
    extern bool economizerFlag;      // holds air loop economizer status
    extern bool SuppHeatingCoilFlag; // set to TRUE when simulating supplemental heating coil

    // why are these external?
    // Last mode of operation
    extern int const CoolingMode;  // last compressor operating mode was in cooling
    extern int const HeatingMode;  // last compressor operating mode was in heating
    extern bool HeatingLoad;       // True when zone needs heating
    extern bool CoolingLoad;       // True when zone needs cooling
    extern Real64 MoistureLoad;    // Dehumidification Load (W)
    extern Real64 CompOnMassFlow;  // Supply air mass flow rate w/ compressor ON [kg/s]
    extern Real64 CompOffMassFlow; // Supply air mass flow rate w/ compressor OFF [kg/s]

    // Compressor operation
    extern int const On;  // normal compressor operation
    extern int const Off; // signal DXCoil that compressor shouldn't run

    // Coil type for SimWater and SimSteamCoil
    extern int const CoolingCoil;
    extern int const HeatingCoil;
    extern int const SuppHeatCoil;

    // Supply Air Sizing Option
    extern int const None;
    extern int const SupplyAirFlowRate;
    extern int const FlowPerFloorArea;
    extern int const FractionOfAutoSizedCoolingValue;
    extern int const FractionOfAutoSizedHeatingValue;
    extern int const FlowPerCoolingCapacity;
    extern int const FlowPerHeatingCapacity;

    struct DesignSpecMSHP
    {
        // friend class UnitarySys;

    public:
        DesignSpecMSHP(); // constructor
        ~DesignSpecMSHP() // destructor
        {
        }

        std::string name;
        static DesignSpecMSHP *factory(int object_type_of_num, std::string const objectName);
        int numOfSpeedHeating;
        int numOfSpeedCooling;
        Real64 noLoadAirFlowRateRatio;
        std::vector<Real64> coolingVolFlowRatio; // The ratio of flow to max for this speed
        std::vector<Real64> heatingVolFlowRatio; // The ratio of flow to max for this speed

        //    private:
        int m_DesignSpecMSHPType_Num;
        bool m_SingleModeFlag;

        static void getDesignSpecMSHP();
        static void getDesignSpecMSHPdata(bool errorsFound);
    };

    struct UnitarySys
    {

        enum class ControlType : int
        {
            None,
            Load,
            Setpoint,
            CCMASHRAE
        };

        enum class DehumCtrlType : int
        {
            None,
            CoolReheat,
            Multimode
        };

        enum class FanPlace : int
        {
            NotYetSet,
            BlowThru,
            DrawThru
        };

        // Airflow control for contant fan mode
        enum class UseCompFlow : int
        {
            FlowNotYetSet,
            UseCompressorOnFlow, // set compressor OFF air flow rate equal to compressor ON air flow rate
            UseCompressorOffFlow // set compressor OFF air flow rate equal to user defined value
        };

        // friend class DesignSpecMSHP;

        int m_UnitarySysNum;
        int m_unitarySystemType_Num;
        bool m_ThisSysInputShouldBeGotten;
        int m_SysAvailSchedPtr; // Pointer to the availability schedule
        ControlType m_ControlType;
        DehumCtrlType m_DehumidControlType_Num;
        bool m_Humidistat;
        bool m_ValidASHRAECoolCoil;
        bool m_ValidASHRAEHeatCoil;
        bool m_SimASHRAEModel; // flag denoting that ASHRAE model (SZVAV) should be used
        bool m_setFaultModelInput;
        int m_FanIndex;
        FanPlace m_FanPlace;
        int m_FanOpModeSchedPtr;
        bool m_FanExists;
        int m_FanType_Num;
        bool m_RequestAutoSize;
        Real64 m_ActualFanVolFlowRate;
        Real64 m_DesignFanVolFlowRate;
        Real64 m_DesignMassFlowRate;
        int m_FanAvailSchedPtr;
        int m_FanOpMode;
        int m_ATMixerIndex;
        int m_ATMixerPriNode;
        int m_ATMixerSecNode;
        bool m_AirLoopEquipment;
        int m_ZoneInletNode;
        int m_ZoneSequenceCoolingNum;
        int m_ZoneSequenceHeatingNum;
        bool m_HeatCoilExists;
        Real64 m_HeatingSizingRatio;
        int m_HeatingCoilType_Num;
        bool m_DXHeatingCoil;
        int m_HeatingCoilIndex;
        int m_HeatingCoilAvailSchPtr;
        Real64 m_DesignHeatingCapacity;
        Real64 m_MaxHeatAirVolFlow;
        int m_NumOfSpeedHeating;
        bool m_MultiSpeedHeatingCoil;
        bool m_VarSpeedHeatingCoil;
        int m_SystemHeatControlNodeNum;
        bool m_CoolCoilExists;
        int m_CoolingCoilType_Num;
        int m_NumOfSpeedCooling;
        int m_CoolingCoilAvailSchPtr;
        Real64 m_DesignCoolingCapacity;
        Real64 m_MaxCoolAirVolFlow;
        int m_CondenserNodeNum;
        int m_CondenserType;
        int m_CoolingCoilIndex;
        bool m_HeatPump;
        int m_ActualDXCoilIndexForHXAssisted;
        bool m_MultiSpeedCoolingCoil;
        bool m_VarSpeedCoolingCoil;
        int m_SystemCoolControlNodeNum;
        int m_WaterCyclingMode;
        bool m_ISHundredPercentDOASDXCoil;
        bool m_RunOnSensibleLoad;
        bool m_RunOnLatentLoad;
        bool m_RunOnLatentOnlyWithSensible;
        int m_DehumidificationMode;
        int m_SuppHeatCoilType_Num;
        bool m_SuppCoilExists;
        Real64 m_DesignSuppHeatingCapacity;
        int m_SuppCoilAirInletNode;
        int m_SuppCoilAirOutletNode;
        int m_SuppCoilFluidInletNode;
        Real64 m_MaxSuppCoilFluidFlow;
        int m_SuppHeatCoilIndex;
        int m_SuppHeatControlNodeNum;
        Real64 m_SupHeaterLoad;
        int m_CoolingSAFMethod;
        int m_HeatingSAFMethod;
        int m_NoCoolHeatSAFMethod;
        Real64 m_MaxNoCoolHeatAirVolFlow;
        UseCompFlow m_AirFlowControl;
        bool m_CoolingCoilUpstream;
        Real64 m_MaxOATSuppHeat;
        Real64 m_MinOATCompressorCooling;
        Real64 m_MinOATCompressorHeating;
        Real64 m_MaxONOFFCyclesperHour;
        Real64 m_HPTimeConstant;
        Real64 m_OnCyclePowerFraction;
        Real64 m_FanDelayTime;
        Real64 m_AncillaryOnPower;
        Real64 m_AncillaryOffPower;
        Real64 m_DesignHRWaterVolumeFlow;
        Real64 m_MaxHROutletWaterTemp;
        bool m_HeatRecActive;
        int m_HeatRecoveryInletNodeNum;
        int m_HeatRecoveryOutletNodeNum;
        int m_DesignSpecMSHPIndex;
        Real64 m_NoLoadAirFlowRateRatio;
        Real64 m_IdleMassFlowRate;
        Real64 m_IdleVolumeAirRate; // idle air flow rate [m3/s]
        Real64 m_IdleSpeedRatio;
        int m_SingleMode;
        bool m_MultiOrVarSpeedHeatCoil;
        bool m_MultiOrVarSpeedCoolCoil;
        Real64 m_PartLoadFrac;
        Real64 m_CoolingPartLoadFrac;
        Real64 m_HeatingPartLoadFrac;
        Real64 m_SuppHeatPartLoadFrac;
        Real64 m_HeatCompPartLoadRatio;
        Real64 m_CoolCompPartLoadRatio;
        Real64 m_SpeedRatio;
        Real64 m_CycRatio;

        bool m_MyEnvrnFlag;
        bool m_MyEnvrnFlag2;
        bool m_MyPlantScanFlag;
        bool m_MySuppCoilPlantScanFlag;
        bool m_MySetPointCheckFlag;
        bool m_MySizingCheckFlag;
        bool m_InitHeatPump; // Heat pump initialization flag (for error reporting)

        int m_HRLoopNum;
        int m_HRLoopSideNum;
        int m_HRBranchNum;
        int m_HRCompNum;
        int m_SuppCoilLoopNum;
        int m_SuppCoilLoopSide;
        int m_SuppCoilBranchNum;
        int m_SuppCoilCompNum;
        int m_SuppCoilFluidOutletNodeNum;

        Real64 m_WSHPRuntimeFrac;
        Real64 m_CompPartLoadRatio;
        Real64 m_CoolingCoilSensDemand;
        Real64 m_CoolingCoilLatentDemand;
        Real64 m_HeatingCoilSensDemand;
        Real64 m_SenLoadLoss;
        Real64 m_LatLoadLoss;
        Real64 m_DesignHeatRecMassFlowRate;
        Real64 m_HeatRecoveryMassFlowRate;
        Real64 m_HeatRecoveryRate;
        Real64 m_HeatRecoveryEnergy;
        Real64 m_HeatRecoveryInletTemp;
        Real64 m_HeatRecoveryOutletTemp;

        int m_IterationCounter;

        Real64 m_DesiredOutletTemp;
        Real64 m_DesiredOutletHumRat;
        int m_FrostControlStatus;

        Real64 m_CoolingCycRatio;
        Real64 m_CoolingSpeedRatio;
        int m_CoolingSpeedNum;
        Real64 m_HeatingCycRatio;
        Real64 m_HeatingSpeedRatio;
        int m_HeatingSpeedNum;
        int m_SpeedNum;

        Real64 m_DehumidInducedHeatingDemandRate;

        Real64 m_TotalAuxElecPower;
        Real64 m_HeatingAuxElecConsumption;
        Real64 m_CoolingAuxElecConsumption;
        Real64 m_ElecPower;
        Real64 m_ElecPowerConsumption;

        int m_LastMode;
        bool m_FirstPass;

        Real64 m_TotCoolEnergyRate;
        Real64 m_SensCoolEnergyRate;
        Real64 m_LatCoolEnergyRate;
        Real64 m_TotHeatEnergyRate;
        Real64 m_SensHeatEnergyRate;
        Real64 m_LatHeatEnergyRate;

        bool m_DesignFanVolFlowRateEMSOverrideOn;         // If true, then EMS is calling to override autosize fan flow
        bool m_MaxHeatAirVolFlowEMSOverrideOn;            // If true, then EMS is calling to override autosize fan flow
        bool m_MaxCoolAirVolFlowEMSOverrideOn;            // If true, then EMS is calling to override autosize fan flow
        bool m_MaxNoCoolHeatAirVolFlowEMSOverrideOn;      // If true, then EMS is calling to override autosize fan flow
        Real64 m_DesignFanVolFlowRateEMSOverrideValue;    // EMS value for override of fan flow rate autosize [m3/s]
        Real64 m_MaxHeatAirVolFlowEMSOverrideValue;       // EMS value for override of fan flow rate autosize [m3/s]
        Real64 m_MaxCoolAirVolFlowEMSOverrideValue;       // EMS value for override of fan flow rate autosize [m3/s]
        Real64 m_MaxNoCoolHeatAirVolFlowEMSOverrideValue; // EMS value for override of fan flow rate autosize [m3/s]
        bool m_EMSOverrideSensZoneLoadRequest;            // If true, then EMS is calling to override zone load
        bool m_EMSOverrideMoistZoneLoadRequest;           // If true, then EMS is calling to override zone load
        Real64 m_EMSSensibleZoneLoadValue;                // Value EMS is directing to use
        Real64 m_EMSMoistureZoneLoadValue;                // Value EMS is directing to use
        // Staged thermostat control
        int m_StageNum; // Stage number specified by staged thermostat
        bool m_Staged;  // Using Staged thermostat

        Real64 m_HeatingFanSpeedRatio;
        Real64 m_CoolingFanSpeedRatio;
        Real64 m_NoHeatCoolSpeedRatio;
        bool m_MyFanFlag;
        bool m_MyCheckFlag;
        Real64 m_SensibleLoadMet;
        Real64 m_LatentLoadMet;
        bool m_MyStagedFlag;
        Real64 m_SensibleLoadPredicted;
        Real64 m_MoistureLoadPredicted;

        // Fault model of coil SAT sensor
        bool m_FaultyCoilSATFlag;     // True if the coil has SAT sensor fault
        int m_FaultyCoilSATIndex;     // Index of the fault object corresponding to the coil
        Real64 m_FaultyCoilSATOffset; // Coil SAT sensor offset

        int m_TESOpMode; // operating mode of TES DX cooling coil
        bool m_initLoadBasedControlAirLoopPass;
        int m_airLoopPassCounter;
        int m_airLoopReturnCounter;
        bool m_FanCompNotSetYet;
        bool m_CoolCompNotSetYet;
        bool m_HeatCompNotSetYet;
        bool m_SuppCompNotSetYet;
        bool m_OKToPrintSizing;

    public:
        // SZVAV variables
        int UnitarySystemType_Num;
        int MaxIterIndex;
        int RegulaFalsiFailedIndex;
        int NodeNumOfControlledZone;
        Real64 FanPartLoadRatio;
        Real64 CoolCoilWaterFlowRatio;
        Real64 HeatCoilWaterFlowRatio;
        int ControlZoneNum;              // index of unit in ZoneEquipConfig
        int AirInNode;                   // Parent inlet air node number
        int AirOutNode;                  // Parent outlet air node number
        Real64 MaxCoolAirMassFlow;       // Maximum coil air mass flow for cooling [kg/s]
        Real64 MaxHeatAirMassFlow;       // Maximum coil air mass flow for heating [kg/s]
        Real64 MaxNoCoolHeatAirMassFlow; // Maximum coil air mass flow for no cooling or heating [kg/s]
        Real64 DesignMinOutletTemp;      // DOAS DX Cooling or SZVAV coil outlet air minimum temperature [C]
        Real64 DesignMaxOutletTemp;      // Maximum supply air temperature from heating coil [C]
        Real64 LowSpeedCoolFanRatio;     // cooling mode ratio of low speed fan flow to full flow rate
        Real64 LowSpeedHeatFanRatio;     // heating mode ratio of low speed fan flow to full flow rate
        Real64 MaxCoolCoilFluidFlow;     // Maximum cooling coil fluid flow for chilled water coil
        Real64 MaxHeatCoilFluidFlow;     // Maximum heating coil fluid flow for hot water or steam coil
        int CoolCoilInletNodeNum;        // Cooling coil air inlet node number
        int CoolCoilOutletNodeNum;       // Cooling coil air outlet node number
        int CoolCoilFluidOutletNodeNum;  // Cooling coil fluid outlet node number (from Plant Loop data)
        int CoolCoilLoopNum;             // Plant loop num of chilled water coil
        int CoolCoilLoopSide;            // Supply side or demand side
        int CoolCoilBranchNum;           // Branch of number of the cooling coil in the plant loop
        int CoolCoilCompNum;             // Comp num of the cooling coil in the plant loop
        int CoolCoilFluidInletNode;      // Cooling coil fluid inlet node
        int HeatCoilLoopNum;             // Plant loop num of hot water or steam coil
        int HeatCoilLoopSide;            // Supply side or demand side
        int HeatCoilBranchNum;           // Branch of number of the heating coil in the plant loop
        int HeatCoilCompNum;             // Comp num of the heating coil in the plant loop
        int HeatCoilFluidInletNode;      // Heating coil fluid inlet node
        int HeatCoilFluidOutletNodeNum;  // Heating coil fluid outlet node number (from Plant Loop data)
        int HeatCoilInletNodeNum;        // Heating coil air inlet node number
        int HeatCoilOutletNodeNum;       // Heating coil air outlet node number
        bool ATMixerExists;              // true if AT mixer is connected to Unitary System
        int ATMixerType;                 // type of AT mixer, inlet-side or supply-side
        int ATMixerOutNode;              // AT mixer outlet node number
        Real64 ControlZoneMassFlowFrac;  // fraction of air flow to the control zone
        DesignSpecMSHP *m_CompPointerMSHP;
        std::string Name;
        std::string UnitType;

        //    private:
        // private members not initialized in constructor
        std::string m_FanName;
        std::string m_ATMixerName;
        std::string m_HeatingCoilName;
        std::string m_HeatingCoilTypeName;
        std::string m_CoolingCoilName;
        std::string m_SuppHeatCoilName;
        std::string m_SuppHeatCoilTypeName;
        std::string m_DesignSpecMultispeedHPType;
        std::string m_DesignSpecMultispeedHPName;
        std::vector<Real64> m_CoolVolumeFlowRate;
        std::vector<Real64> m_CoolMassFlowRate;
        std::vector<Real64> m_MSCoolingSpeedRatio;
        std::vector<Real64> m_HeatVolumeFlowRate;
        std::vector<Real64> m_HeatMassFlowRate;
        std::vector<Real64> m_MSHeatingSpeedRatio;
        std::vector<Real64> m_HeatingVolFlowRatio;
        std::vector<int> m_IterationMode; // array of operating mode each iteration

        struct WarnMessages
        {
            // Warning message variables
            int m_HXAssistedSensPLRIter = 0;        // used in HX Assisted calculations
            int m_HXAssistedSensPLRIterIndex = 0;   // used in HX Assisted calculations
            int m_HXAssistedSensPLRFail = 0;        // used in HX Assisted calculations
            int m_HXAssistedSensPLRFailIndex = 0;   // used in HX Assisted calculations
            int m_HXAssistedSensPLRFail2 = 0;       // used in HX Assisted calculations
            int m_HXAssistedSensPLRFailIndex2 = 0;  // used in HX Assisted calculations
            int m_HXAssistedLatPLRIter = 0;         // used in HX Assisted calculations
            int m_HXAssistedLatPLRIterIndex = 0;    // used in HX Assisted calculations
            int m_HXAssistedLatPLRFail = 0;         // used in HX Assisted calculations
            int m_HXAssistedLatPLRFailIndex = 0;    // used in HX Assisted calculations
            int m_HXAssistedCRLatPLRIter = 0;       // used in HX Assisted calculations
            int m_HXAssistedCRLatPLRIterIndex = 0;  // used in HX Assisted calculations
            int m_HXAssistedCRLatPLRFail = 0;       // used in HX Assisted calculations
            int m_HXAssistedCRLatPLRFailIndex = 0;  // used in HX Assisted calculations
            int m_HXAssistedCRLatPLRFail2 = 0;      // used in HX Assisted calculations
            int m_HXAssistedCRLatPLRFailIndex2 = 0; // used in HX Assisted calculations
            int m_SensPLRIter = 0;                  // used in cool coil calculations
            int m_SensPLRIterIndex = 0;             // used in cool coil calculations
            int m_SensPLRFail = 0;                  // used in cool coil calculations
            int m_SensPLRFailIndex = 0;             // used in cool coil calculations
            int m_LatPLRIter = 0;                   // used in cool coil calculations
            int m_LatPLRIterIndex = 0;              // used in cool coil calculations
            int m_LatPLRFail = 0;                   // used in cool coil calculations
            int m_LatPLRFailIndex = 0;              // used in cool coil calculations
            int m_HeatCoilSensPLRIter = 0;          // used in heat coil calculations
            int m_HeatCoilSensPLRIterIndex = 0;     // used in heat coil calculations
            int m_HeatCoilSensPLRFail = 0;          // used in heat coil calculations
            int m_HeatCoilSensPLRFailIndex = 0;     // used in heat coil calculations
            int m_SuppHeatCoilSensPLRIter = 0;      // used in supp heat coil calculations
            int m_SuppHeatCoilSensPLRIterIndex = 0; // used in supp heat coil calculations
            int m_SuppHeatCoilSensPLRFail = 0;      // used in supp heat coil calculations
            int m_SuppHeatCoilSensPLRFailIndex = 0; // used in supp heat coil calculations
            int m_LatMaxIterIndex = 0;              // used in PLR calculations for moisture load
            int m_LatRegulaFalsiFailedIndex = 0;    // used in PLR calculations for moisture load
        };
        WarnMessages warnIndex;

        static void getUnitarySystemInput(std::string const &Name, bool const ZoneEquipment, int const ZoneOAUnitNum);

        static Real64 DOE2DXCoilResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                         std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 DOE2DXCoilHumRatResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                               std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 calcUnitarySystemLoadResidual(Real64 const PartLoadRatio,    // DX cooling coil part load ratio
                                                    std::vector<Real64> const &Par // Function parameters
        );

        static Real64 HXAssistedCoolCoilTempResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                     std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 hotWaterHeatingCoilResidual(Real64 const PartLoadFrac,     // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                  std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 HXAssistedCoolCoilHRResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                   std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 DXCoilVarSpeedResidual(Real64 const SpeedRatio,       // compressor speed ratio (1.0 is max, 0.0 is min)
                                             std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 heatingCoilVarSpeedResidual(Real64 const SpeedRatio,       // compressor speed ratio (1.0 is max, 0.0 is min)
                                                  std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 DXCoilVarSpeedHumRatResidual(Real64 const SpeedRatio,       // compressor speed ratio (1.0 is max, 0.0 is min)
                                                   std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 DXCoilCyclingResidual(Real64 const CycRatio,         // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                            std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 DXCoilCyclingHumRatResidual(Real64 const CycRatio,         // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                  std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 heatingCoilVarSpeedCycResidual(Real64 const CycRatio,         // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                     std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 TESIceStorageCoilOutletResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                      std::vector<Real64> const &Par // par( 1 ) = double( UnitarySysNum );
        );

        static Real64 multiModeDXCoilResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                              std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 multiModeDXCoilHumRatResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                    std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 coolWaterHumRatResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                              std::vector<Real64> const &Par // par(1) = CoolWater coil number
        );

        static Real64 coolWaterTempResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                            std::vector<Real64> const &Par // par(1) = CoolWater coil number
        );

        static Real64 gasElecHeatingCoilResidual(Real64 const PartLoadFrac,     // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                 std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 steamHeatingCoilResidual(Real64 const PartLoadFrac,     // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                               std::vector<Real64> const &Par // par(1) = DX coil number
        );

        static Real64 heatWatertoAirHPTempResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                   std::vector<Real64> const &Par // par(1) = HeatWatertoAirHP coil number
        );

        static Real64 coolWatertoAirHPHumRatResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                     std::vector<Real64> const &Par // par(1) = CoolWatertoAirHP coil number
        );

        static Real64 coolWatertoAirHPTempResidual(Real64 const PartLoadRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                                   std::vector<Real64> const &Par // par(1) = CoolWatertoAirHP coil number
        );

        static Real64 DXHeatingCoilResidual(Real64 const PartLoadFrac,     // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                            std::vector<Real64> const &Par // par(1) = DX coil number
        );

        void initUnitarySystems(int const &AirLoopNum, bool const &FirstHVACIteration, int const ZoneOAUnitNum, Real64 const OAUCoilOutTemp);

        void checkNodeSetPoint(int const AirLoopNum,       // number of the current air loop being simulated
                               int const ControlNode,      // Node to test for set point
                               int const CoilType,         // True if cooling coil, then test for HumRatMax set point
                               Real64 const OAUCoilOutTemp // the coil inlet temperature of OutdoorAirUnit
        );

        void frostControlSetPointLimit(Real64 &TempSetPoint,       // temperature setpoint of the sensor node
                                       Real64 &HumRatSetPoint,     // humidity ratio setpoint of the sensor node
                                       Real64 const BaroPress,     // baromtric pressure, Pa [N/m^2]
                                       Real64 const TfrostControl, // minimum temperature limit for forst control
                                       int const ControlMode       // temperature or humidity control mode
        );

        void reportUnitarySystem(int const AirLoopNum);

        void unitarySystemHeatRecovery();

        void controlUnitarySystemtoSP(int const AirLoopNum,          // Primary air loop number
                                      bool const FirstHVACIteration, // True when first HVAC iteration
                                      int &CompOn,                   // compressor on/off control
                                      Real64 const OAUCoilOutTemp,   // the coil inlet temperature of OutdoorAirUnit
                                      bool HXUnitOn                  // Flag to control HX for HXAssisted Cooling Coil
        );

        void controlUnitarySystemtoLoad(int const AirLoopNum,          // Primary air loop number
                                        bool const FirstHVACIteration, // True when first HVAC iteration
                                        int &CompOn,                   // Determines if compressor is on or off
                                        Real64 const OAUCoilOutTemp,   // the coil inlet temperature of OutdoorAirUnit
                                        bool HXUnitOn                  // Flag to control HX for HXAssisted Cooling Coil
        );

        void updateUnitarySystemControl(int const AirLoopNum,  // number of the current air loop being simulated
                                        int const OutNode,     // coil outlet node number
                                        int const ControlNode, // control node number
                                        Real64 &OnOffAirFlowRatio,
                                        bool const FirstHVACIteration,
                                        Real64 const OAUCoilOutletTemp, // "ONLY" for zoneHVAC:OutdoorAirUnit
                                        Real64 &ZoneLoad,
                                        Real64 const MaxOutletTemp // limits heating coil outlet temp [C]
        );

        void controlUnitarySystemOutput(int const AirLoopNum,          // Index to air loop
                                        bool const FirstHVACIteration, // True when first HVAC iteration
                                        Real64 &OnOffAirFlowRatio,     // ratio of heating PLR to cooling PLR (is this correct?)
                                        Real64 const ZoneLoad,
                                        Real64 &FullSensibleOutput,
                                        bool HXUnitOn, // Flag to control HX for HXAssisted Cooling Coil
                                        int CompOn);

        void initLoadBasedControl(int const AirLoopNum, // number of the current air loop being simulated
                                  bool const FirstHVACIteration,
                                  Real64 &OnOffAirFlowRatio,
                                  Real64 &ZoneLoad);

        void sizeUnitarySystem(bool const FirstHVACIteration, int const AirLoopNum);

        void setOnOffMassFlowRate(Real64 &OnOffAirFlowRatio, // ratio of coil on to coil off air flow rate
                                  Real64 const PartLoadRatio // coil part-load ratio
        );

        void setAverageAirFlow(Real64 const PartLoadRatio, // unit part load ratio
                               Real64 &OnOffAirFlowRatio   // ratio of compressor ON airflow to AVERAGE airflow over timestep
        );

        void calculateCapacity(Real64 &SensOutput, // sensible output of AirloopHVAC:UnitarySystem
                               Real64 &LatOutput   // latent output of AirloopHVAC:UnitarySystem
        );

        void calcUnitaryCoolingSystem(int const AirLoopNum,          // index to air loop
                                      bool const FirstHVACIteration, // True when first HVAC iteration
                                      Real64 const PartLoadRatio,    // coil operating part-load ratio
                                      int const CompOn,              // compressor control (0=off, 1=on)
                                      Real64 const OnOffAirFlowRatio,
                                      Real64 const CoilCoolHeatRat, // ratio of cooling to heating PLR for cycling fan RH control
                                      bool const HXUnitOn           // Flag to control HX for HXAssisted Cooling Coil
        );

        void calcUnitaryHeatingSystem(int const AirLoopNum,           // index to air loop
                                      bool const FirstHVACIteration,  // True when first HVAC iteration
                                      Real64 const PartLoadRatio,     // coil operating part-load ratio
                                      int const CompOn,               // comrpressor control (0=off, 1=on)
                                      Real64 const OnOffAirFlowRatio, // ratio of on to off flow rate
                                      Real64 HeatCoilLoad             // adjusted heating coil load if outlet temp exceeds max (W)
        );

        void calcUnitarySuppHeatingSystem(bool const FirstHVACIteration, // True when first HVAC iteration
                                          Real64 const PartLoadRatio,    // coil operating part-load ratio
                                          Real64 const SuppCoilLoad      // adjusted supp coil load when outlet temp exceeds max (W)
        );

        void calcUnitarySuppSystemToSP(bool const FirstHVACIteration // True when first HVAC iteration
        );

        void controlCoolingSystemToSP(int const AirLoopNum,          // index to air loop
                                      bool const FirstHVACIteration, // First HVAC iteration flag
                                      bool &HXUnitOn,                // flag to enable heat exchanger heat recovery
                                      int &CompOp                    // compressor on/off control
        );

        void controlHeatingSystemToSP(int const AirLoopNum,          // index to air loop
                                      bool const FirstHVACIteration, // First HVAC iteration flag
                                      int &CompOn,                   // compressor on/off control
                                      Real64 &HeatCoilLoad           // load met by heating coil
        );

        void controlSuppHeatSystemToSP(int const AirLoopNum,         // index to air loop
                                       bool const FirstHVACIteration // First HVAC iteration flag
        );

        void simMultiSpeedCoils(int const AirLoopNum,          // Index to air loop
                                bool const FirstHVACIteration, // True when first HVAC iteration
                                int &CompOn,                   // compressor on/off control
                                bool const SensibleLoad,
                                bool const LatentLoad,
                                Real64 const PartLoadFrac,
                                int const CoilType,
                                int const SpeedNumber);

        void calcPassiveSystem(int const AirLoopNum,         // Index to air loop
                               bool const FirstHVACIteration // True when first HVAC iteration
        );

        void heatPumpRunFrac(Real64 const PLR,   // part load ratio
                             bool &errFlag,      // part load factor out of range flag
                             Real64 &RuntimeFrac // the required run time fraction to meet part load
        );

        void setSpeedVariables(bool const SensibleLoad,   // True when meeting a sensible load (not a moisture load)
                               Real64 const PartLoadRatio // operating PLR
        );

    public:
        UnitarySys(); // constructor

        ~UnitarySys() // destructor
        {
        }

        static void getUnitarySystemInputData(std::string const &Name, bool const ZoneEquipment, int const ZoneOAUnitNum, bool &errorsFound);

        static UnitarySys *factory(int const object_type_of_num, std::string const objectName, bool const ZoneEquipment, int const ZoneOAUnitNum);

        void simulate(std::string const &Name,
                      bool const firstHVACIteration,
                      int const &AirLoopNum,
                      int &CompIndex,
                      bool &HeatActive,
                      bool &CoolActive,
                      int const OAUnitNum,         // If the system is an equipment of OutdoorAirUnit
                      Real64 const OAUCoilOutTemp, // the coil inlet temperature of OutdoorAirUnit
                      bool const ZoneEquipment     // TRUE if called as zone equipment
        );

        void calcUnitarySystemToLoad(int const AirLoopNum,          // index to air loop
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
        );

        static void checkUnitarySysCoilInOASysExists(std::string const &UnitarySysName, int const ZoneOAUnitNum);

        static void getUnitarySysHeatCoolCoil(std::string const &UnitarySysName, // Name of Unitary System object
                                              bool &CoolingCoil,                 // Cooling coil exists
                                              bool &HeatingCoil,                 // Heating coil exists
                                              int const ZoneOAUnitNum            // index to zone OA unit
        );

        static Real64 calcUnitarySystemWaterFlowResidual(Real64 const PartLoadRatio,    // water mass flow rate [kg/s]
                                                         std::vector<Real64> const &Par // Function parameters
        );
    };

    extern std::vector<UnitarySys> unitarySys;
    extern std::vector<DesignSpecMSHP> designSpecMSHP;
    int getDesignSpecMSHPIndex(std::string const &objectName);
    int getUnitarySystemIndex(std::string const &objectName);

} // namespace UnitarySystems
} // namespace EnergyPlus
#endif // ENERGYPLUS_UNITARYSYSTEM_HH
