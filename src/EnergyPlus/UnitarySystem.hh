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

    //extern int numDesignSpecMultiSpeedHP;
    extern int numUnitarySystems;
    //bool getInputFlag;
    extern bool economizerFlag; // holds air loop economizer status
    extern bool SuppHeatingCoilFlag; // set to TRUE when simulating supplemental heating coil
    
    // why are these external?
    // Last mode of operation
    extern int const CoolingMode; // last compressor operating mode was in cooling
    extern int const HeatingMode; // last compressor operating mode was in heating

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

    class DesignSpecMSHP
    {
        friend class UnitarySys;

    public:
        DesignSpecMSHP(); // constructor
        ~DesignSpecMSHP() // destructor
        {
        }

        std::string name;
        static DesignSpecMSHP *factory(int object_type_of_num, std::string const objectName);

    private:
        int designSpecMSHPType_Num;
        Real64 noLoadAirFlowRateRatio;
        int numOfSpeedHeating;
        int numOfSpeedCooling;
        std::vector<Real64> coolingVolFlowRatio; // The ratio of flow to max for this speed
        std::vector<Real64> heatingVolFlowRatio; // The ratio of flow to max for this speed
        // std::vector<Real64> coolVolumeFlowRate;
        //std::vector<Real64> coolMassFlowRate;
        std::vector<Real64> MSCoolingSpeedRatio;
        //std::vector<Real64> heatVolumeFlowRate;
        //std::vector<Real64> heatMassFlowRate;
        std::vector<Real64> MSHeatingSpeedRatio;
        bool singleModeFlag;

        static void getDesignSpecMSHP();
        static void getDesignSpecMSHPdata(bool errorsFound);
    };

    class UnitarySys
    {

        // bool myOneTimeFlag;
        // bool getInputOnceFlag;
        int fanSpeedRatio;

        enum controlTypeEnum : int
        {
            controlTypeNone,
            controlTypeLoad,
            controlTypeSetpoint,
            controlTypeCCMASHRAE,
            default = controlTypeNone
        };

        enum dehumCtrlTypeEnum : int
        {
            dehumidControl_None,
            dehumidControl_CoolReheat,
            dehumidControl_Multimode,
            //            default = dehumidControl_None
        };

        enum fanOpModeEnum : int
        {
            fanOpModeNotYetSet,
            contFanCycCoil,
            cycFanCycCoil
        };

        enum fanPlaceEnum : int
        {
            notYetSet,
            blowThru,
            drawThru
        };

        // Airflow control for contant fan mode
        enum useCompFlow : int
        {
            flowNotYetSet,
            useCompressorOnFlow, // set compressor OFF air flow rate equal to compressor ON air flow rate
            useCompressorOffFlow // set compressor OFF air flow rate equal to user defined value
        };

        friend class DesignSpecMSHP;
        UnitarySys *compPointer; // don't need this here
        std::string unitarySystemName;
        std::string unitType;
        int unitarySysNum;
        bool myGetInputSuccessfulFlag;
        bool thisSysInputShouldBeGotten;
        int sysAvailSchedPtr; // Pointer to the availability schedule
        controlTypeEnum controlType;
        int controlZoneNum;
        dehumCtrlTypeEnum dehumidControlType_Num;
        bool humidistat;
        int airInNode;  // system air node at inlet
        int airOutNode; // system air node at outlet
        bool validASHRAECoolCoil;
        bool validASHRAEHeatCoil;
        bool simASHRAEModel; // flag denoting that ASHRAE model (SZVAV) should be used
        std::string fanName;
        int fanIndex;
        fanPlaceEnum fanPlace;
        int fanOpModeSchedPtr;
        bool fanExists;
        int fanType_Num;
        bool requestAutoSize;
        Real64 actualFanVolFlowRate;
        Real64 designFanVolFlowRate;
        Real64 designMassFlowRate;
        int fanAvailSchedPtr;
        int fanOpMode;
        std::string ATMixerName;
        int ATMixerIndex;
        int ATMixerType;
        int ATMixerPriNode;
        int ATMixerSecNode;
        int ATMixerOutNode;
        bool ATMixerExists;
        int nodeNumOfControlledZone;
        bool airLoopEquipment;
        int zoneInletNode;
        int zoneSequenceCoolingNum;
        int zoneSequenceHeatingNum;
        std::string heatingCoilName;
        std::string heatingCoilTypeName;
        bool heatCoilExists;
        Real64 heatingSizingRatio;
        int heatingCoilType_Num;
        bool DXHeatingCoil;
        int heatingCoilIndex;
        int heatingCoilAvailSchPtr;
        Real64 designHeatingCapacity;
        Real64 maxHeatAirVolFlow;
        int numOfSpeedHeating;
        int heatCoilFluidInletNode;
        Real64 maxHeatCoilFluidFlow;
        bool multiSpeedHeatingCoil;
        bool varSpeedHeatingCoil;
        int systemHeatControlNodeNum;
        int heatCoilInletNodeNum;
        int heatCoilOutletNodeNum;
        bool coolCoilExists;
        int coolingCoilType_Num;
        int numOfSpeedCooling;
        int coolingCoilAvailSchPtr;
        Real64 designCoolingCapacity;
        Real64 maxCoolAirVolFlow;
        int condenserNodeNum;
        int condenserType;
        int coolingCoilIndex;
        bool heatPump;
        int actualDXCoilIndexForHXAssisted;
        Real64 maxCoolCoilFluidFlow;
        int coolCoilFluidInletNode;
        bool multiSpeedCoolingCoil;
        bool varSpeedCoolingCoil;
        int systemCoolControlNodeNum;
        std::string coolingCoilName;
        int coolCoilInletNodeNum;
        int coolCoilOutletNodeNum;
        int waterCyclingMode;
        bool ISHundredPercentDOASDXCoil;
        Real64 designMinOutletTemp;
        bool runOnSensibleLoad;
        bool runOnLatentLoad;
        bool runOnLatentOnlyWithSensible;
        int dehumidificationMode;
        std::string suppHeatCoilName;
        std::string suppHeatCoilTypeName;
        int suppHeatCoilType_Num;
        bool suppCoilExists;
        Real64 designSuppHeatingCapacity;
        int suppCoilAirInletNode;
        int suppCoilAirOutletNode;
        int suppCoilFluidInletNode;
        Real64 maxSuppCoilFluidFlow;
        int suppHeatCoilIndex;
        int suppHeatControlNodeNum;
        Real64 supHeaterLoad;
        int coolingSAFMethod;
        int heatingSAFMethod;
        int noCoolHeatSAFMethod;
        Real64 maxNoCoolHeatAirVolFlow;
        useCompFlow airFlowControl;
        bool coolingCoilUpstream;
        Real64 designMaxOutletTemp;
        Real64 maxOATSuppHeat;
        Real64 minOATCompressorCooling;
        Real64 minOATCompressorHeating;
        Real64 maxONOFFCyclesperHour;
        Real64 HPTimeConstant;
        Real64 onCyclePowerFraction;
        Real64 fanDelayTime;
        Real64 ancillaryOnPower;
        Real64 ancillaryOffPower;
        Real64 designHRWaterVolumeFlow;
        Real64 maxHROutletWaterTemp;
        bool heatRecActive;
        int heatRecoveryInletNodeNum;
        int heatRecoveryOutletNodeNum;
        std::string designSpecMultispeedHPType;
        std::string designSpecMultispeedHPName;
        int designSpecMSHPIndex;
        Real64 noLoadAirFlowRateRatio;
        DesignSpecMSHP *compPointerMSHP;
        std::vector<Real64> coolVolumeFlowRate;
        std::vector<Real64> coolMassFlowRate;
        std::vector<Real64> MSCoolingSpeedRatio;
        std::vector<Real64> heatVolumeFlowRate;
        std::vector<Real64> heatMassFlowRate;
        std::vector<Real64> MSHeatingSpeedRatio;
        std::vector<Real64> heatingVolFlowRatio;
        Real64 idleMassFlowRate;
        Real64 idleVolumeAirRate; // idle air flow rate [m3/s]
        Real64 idleSpeedRatio;
        int singleMode;
        bool multiOrVarSpeedHeatCoil;
        bool multiOrVarSpeedCoolCoil;
        Real64 partLoadFrac;
        Real64 coolingPartLoadFrac;
        Real64 heatingPartLoadFrac;
        Real64 suppHeatPartLoadFrac;
        Real64 heatCompPartLoadRatio;
        Real64 coolCompPartLoadRatio;
        Real64 speedRatio;
        Real64 cycRatio;

        bool myEnvrnFlag;
        bool myPlantScanFlag;
        bool mySuppCoilPlantScanFlag;
        bool mySetPointCheckFlag;
        bool mySizingCheckFlag;
        bool initHeatPump; // Heat pump initialization flag (for error reporting)

        int HRLoopNum;
        int HRLoopSideNum;
        int HRBranchNum;
        int HRCompNum;
        int coolCoilLoopNum;
        int coolCoilLoopSide;
        int coolCoilBranchNum;
        int coolCoilCompNum;
        int coolCoilFluidOutletNodeNum;
        int heatCoilLoopNum;
        int heatCoilLoopSide;
        int heatCoilBranchNum;
        int heatCoilCompNum;
        int heatCoilFluidOutletNodeNum;
        int suppCoilLoopNum;
        int suppCoilLoopSide;
        int suppCoilBranchNum;
        int suppCoilCompNum;
        int suppCoilFluidOutletNodeNum;

        Real64 maxCoolAirMassFlow;
        Real64 maxHeatAirMassFlow;
        Real64 maxNoCoolHeatAirMassFlow;
        Real64 WSHPRuntimeFrac;
        Real64 compPartLoadRatio;
        Real64 coolingCoilSensDemand;
        Real64 coolingCoilLatentDemand;
        Real64 heatingCoilSensDemand;
        Real64 senLoadLoss;
        Real64 latLoadLoss;
        Real64 designHeatRecMassFlowRate;
        Real64 heatRecoveryMassFlowRate;
        Real64 heatRecoveryRate;
        Real64 heatRecoveryEnergy;
        Real64 heatRecoveryInletTemp;
        Real64 heatRecoveryOutletTemp;

        int iterationCounter;

        Real64 desiredOutletTemp;
        Real64 desiredOutletHumRat;
        int frostControlStatus;
        
        Real64 coolingCycRatio;
        Real64 coolingSpeedRatio;
        int coolingSpeedNum;
        Real64 heatingCycRatio;
        Real64 heatingSpeedRatio;
        int heatingSpeedNum;
        int speedNum;

        Real64 dehumidInducedHeatingDemandRate;
        Real64 coolCoilWaterFlowRatio;
        Real64 heatCoilWaterFlowRatio;

        Real64 fanPartLoadRatio;
        Real64 totalAuxElecPower;
        Real64 heatingAuxElecConsumption;
        Real64 coolingAuxElecConsumption;
        Real64 elecPower;
        Real64 elecPowerConsumption;
       
        int lastMode;
        bool firstPass;
        
        Real64 totCoolEnergyRate;
        Real64 sensCoolEnergyRate;
        Real64 latCoolEnergyRate;
        Real64 totHeatEnergyRate;
        Real64 sensHeatEnergyRate;
        Real64 latHeatEnergyRate;
        
        // Warning message variables
        int HXAssistedSensPLRIter;                      // used in HX Assisted calculations
        int HXAssistedSensPLRIterIndex;                 // used in HX Assisted calculations
        int HXAssistedSensPLRFail;                      // used in HX Assisted calculations
        int HXAssistedSensPLRFailIndex;                 // used in HX Assisted calculations
        int HXAssistedSensPLRFail2;                     // used in HX Assisted calculations
        int HXAssistedSensPLRFailIndex2;                // used in HX Assisted calculations
        int HXAssistedLatPLRIter;                       // used in HX Assisted calculations
        int HXAssistedLatPLRIterIndex;                  // used in HX Assisted calculations
        int HXAssistedLatPLRFail;                       // used in HX Assisted calculations
        int HXAssistedLatPLRFailIndex;                  // used in HX Assisted calculations
        int HXAssistedCRLatPLRIter;                     // used in HX Assisted calculations
        int HXAssistedCRLatPLRIterIndex;                // used in HX Assisted calculations
        int HXAssistedCRLatPLRFail;                     // used in HX Assisted calculations
        int HXAssistedCRLatPLRFailIndex;                // used in HX Assisted calculations
        int HXAssistedCRLatPLRFail2;                    // used in HX Assisted calculations
        int HXAssistedCRLatPLRFailIndex2;               // used in HX Assisted calculations
        int SensPLRIter;                                // used in cool coil calculations
        int SensPLRIterIndex;                           // used in cool coil calculations
        int SensPLRFail;                                // used in cool coil calculations
        int SensPLRFailIndex;                           // used in cool coil calculations
        int LatPLRIter;                                 // used in cool coil calculations
        int LatPLRIterIndex;                            // used in cool coil calculations
        int LatPLRFail;                                 // used in cool coil calculations
        int LatPLRFailIndex;                            // used in cool coil calculations
        int HeatCoilSensPLRIter;                        // used in heat coil calculations
        int HeatCoilSensPLRIterIndex;                   // used in heat coil calculations
        int HeatCoilSensPLRFail;                        // used in heat coil calculations
        int HeatCoilSensPLRFailIndex;                   // used in heat coil calculations
        int SuppHeatCoilSensPLRIter;                    // used in supp heat coil calculations
        int SuppHeatCoilSensPLRIterIndex;               // used in supp heat coil calculations
        int SuppHeatCoilSensPLRFail;                    // used in supp heat coil calculations
        int SuppHeatCoilSensPLRFailIndex;               // used in supp heat coil calculations
        int DXCoilSensPLRIter;                          // used in DXCoil calculations
        int DXCoilSensPLRIterIndex;                     // used in DXCoil calculations
        int DXCoilSensPLRFail;                          // used in DXCoil calculations
        int DXCoilSensPLRFailIndex;                     // used in DXCoil calculations
        int MSpdSensPLRIter;                            // used in MultiSpeed calculations
        int MSpdSensPLRIterIndex;                       // used in MultiSpeed calculations
        int MSpdCycSensPLRIter;                         // used in MultiSpeed calculations
        int MSpdCycSensPLRIterIndex;                    // used in MultiSpeed calculations
        int MSpdLatPLRIter;                             // used in MultiSpeed calculations
        int MSpdLatPLRIterIndex;                        // used in MultiSpeed calculations
        int MSpdCycLatPLRIter;                          // used in MultiSpeed calculations
        int MSpdCycLatPLRIterIndex;                     // used in MultiSpeed calculations
        int LatMaxIterIndex;                            // used in PLR calculations for moisture load
        int LatRegulaFalsIFailedIndex;                  // used in PLR calculations for moisture load
                                                        // EMS variables
        bool designFanVolFlowRateEMSOverrideOn;         // If true, then EMS is calling to override autosize fan flow
        bool maxHeatAirVolFlowEMSOverrideOn;            // If true, then EMS is calling to override autosize fan flow
        bool maxCoolAirVolFlowEMSOverrideOn;            // If true, then EMS is calling to override autosize fan flow
        bool maxNoCoolHeatAirVolFlowEMSOverrideOn;      // If true, then EMS is calling to override autosize fan flow
        Real64 designFanVolFlowRateEMSOverrideValue;    // EMS value for override of fan flow rate autosize [m3/s]
        Real64 maxHeatAirVolFlowEMSOverrideValue;       // EMS value for override of fan flow rate autosize [m3/s]
        Real64 maxCoolAirVolFlowEMSOverrideValue;       // EMS value for override of fan flow rate autosize [m3/s]
        Real64 maxNoCoolHeatAirVolFlowEMSOverrideValue; // EMS value for override of fan flow rate autosize [m3/s]
        bool EMSOverrideSensZoneLoadRequest;            // If true, then EMS is calling to override zone load
        bool EMSOverrideMoistZoneLoadRequest;           // If true, then EMS is calling to override zone load
        Real64 EMSSensibleZoneLoadValue;                // Value EMS is directing to use
        Real64 EMSMoistureZoneLoadValue;                // Value EMS is directing to use
        // Staged thermostat control
        int stageNum;       // Stage number specified by staged thermostat
        bool staged;        // Using Staged thermostat
        int coolCountAvail; // Counter used to minimize the occurrence of output warnings
        int coolIndexAvail; // Index used to minimize the occurrence of output warnings
        int heatCountAvail; // Counter used to minimize the occurrence of output warnings
        int heatIndexAvail; // Index used to minimize the occurrence of output warnings

        Real64 heatingFanSpeedRatio;
        Real64 coolingFanSpeedRatio;
        Real64 noHeatCoolSpeedRatio;
        bool myFanFlag;
        bool myCheckFlag;
        Real64 controlZoneMassFlowFrac;
        Real64 sensibleLoadMet;
        Real64 latentLoadMet;
        bool myStagedFlag;
        std::vector<int> iterationMode; // array of operating mode each iteration
        Real64 sensibleLoadPredicted;
        Real64 moistureLoadPredicted;
        
        // Fault model of coil SAT sensor
        bool faultyCoilSATFlag;     // True if the coil has SAT sensor fault
        int faultyCoilSATIndex;     // Index of the fault object corresponding to the coil
        Real64 faultyCoilSATOffset; // Coil SAT sensor offset

        int TESOpMode; // operating mode of TES DX cooling coil
        Real64 lowSpeedCoolFanRatio;
        Real64 lowSpeedHeatFanRatio;

        static void getUnitarySystemInput(std::string const &unitarySystemName);
        static void getUnitarySystemInputData(std::string const &unitarySystemName, bool errorsFound);

    public:
        UnitarySys(); // constructor

        ~UnitarySys() // destructor
        {
        }

        std::string name; // user identifier
        int unitarySystemType_Num;

        static UnitarySys *factory(int object_type_of_num, std::string const objectName);

        void simulate(std::string const &unitarySystemName,
                      bool const firstHVACIteration,
                      int const &AirLoopNum,
                      int &CompIndex,
                      bool &HeatActive,
                      bool &CoolActive,
                      int const OAUnitNum,          // If the system is an equipment of OutdoorAirUnit
                      Real64 const OAUCoilOutTemp, // the coil inlet temperature of OutdoorAirUnit
                      bool const ZoneEquipment);    // TRUE if called as zone equipment

        void initUnitarySystems(int const &AirLoopNum,
                                bool const &FirstHVACIteration,
                                Optional_int_const OAUnitNum,
                                Optional<Real64 const> OAUCoilOutTemp
        );

        void checkNodeSetPoint(int const AirLoopNum,                     // number of the current air loop being simulated
                               int const ControlNode,                    // Node to test for set point
                               int const CoilType,                       // True if cooling coil, then test for HumRatMax set point
                               Optional<Real64 const> OAUCoilOutTemp = _ // the coil inlet temperature of OutdoorAirUnit
        );

        void frostControlSetPointLimit(Real64 &TempSetPoint,       // temperature setpoint of the sensor node
                                       Real64 &HumRatSetPoint,     // humidity ratio setpoint of the sensor node
                                       Real64 const BaroPress,     // baromtric pressure, Pa [N/m^2]
                                       Real64 const TfrostControl, // minimum temperature limit for forst control
                                       int const ControlMode       // temperature or humidity control mode
        );

        void reportUnitarySystem(int const AirLoopNum);

        void unitarySystemHeatRecovery();

        void controlUnitarySystemtoSP(int const AirLoopNum,                      // Primary air loop number
                                      bool const FirstHVACIteration,             // True when first HVAC iteration
                                      int &CompOn,                               // compressor on/off control
                                      Optional<Real64 const> OAUCoilOutTemp = _, // the coil inlet temperature of OutdoorAirUnit
                                      Optional_bool HXUnitOn = _                 // Flag to control HX for HXAssisted Cooling Coil
        );

        void controlUnitarySystemtoLoad(int const AirLoopNum,                      // Primary air loop number
                                        bool const FirstHVACIteration,             // True when first HVAC iteration
                                        int &CompOn,                               // Determines if compressor is on or off
                                        Optional<Real64 const> OAUCoilOutTemp = _, // the coil inlet temperature of OutdoorAirUnit
                                        Optional_bool HXUnitOn = _                 // Flag to control HX for HXAssisted Cooling Coil
        );

        void updateUnitarySystemControl(int const AirLoopNum,  // number of the current air loop being simulated
                                        int const OutNode,       // coil outlet node number
                                        int const ControlNode,   // control node number
                                        Real64 &OnOffAirFlowRatio,
                                        bool const FirstHVACIteration,
                                        Optional<Real64 const> OAUCoilOutletTemp = _, // "ONLY" for zoneHVAC:OutdoorAirUnit
                                        Optional<Real64> ZoneLoad = _,
                                        Optional<Real64 const> MaxOutletTemp = _ // limits heating coil outlet temp [C]
        );

        void controlUnitarySystemOutput(int const AirLoopNum,          // Index to air loop
                                        bool const FirstHVACIteration, // True when first HVAC iteration
                                        Real64 &OnOffAirFlowRatio,     // ratio of heating PLR to cooling PLR (is this correct?)
                                        Real64 const ZoneLoad,
                                        Real64 &FullSensibleOutput,
                                        Optional_bool HXUnitOn = _, // Flag to control HX for HXAssisted Cooling Coil
                                        Optional_int CompOn = _);

        void initLoadBasedControl(int const AirLoopNum, // number of the current air loop being simulated
                                  bool const FirstHVACIteration,
                                  Real64 &OnOffAirFlowRatio,
                                  Real64 &ZoneLoad
        );

        void sizeUnitarySystem(bool const FirstHVACIteration,
                               int const AirLoopNum // does this need to be optional?
        );

        void setOnOffMassFlowRate(Real64 &OnOffAirFlowRatio, // ratio of coil on to coil off air flow rate
                                  Real64 const PartLoadRatio // coil part-load ratio
        );

        void setAverageAirFlow(Real64 const PartLoadRatio, // unit part load ratio
                               Real64 &OnOffAirFlowRatio   // ratio of compressor ON airflow to AVERAGE airflow over timestep
        );

        void calcUnitarySystemToLoad(int const AirLoopNum,              // index to air loop
                                     bool const FirstHVACIteration,     // True when first HVAC iteration
                                     Real64 const CoolPLR,              // operating cooling part-load ratio []
                                     Real64 const HeatPLR,              // operating cooling part-load ratio []
                                     Real64 &OnOffAirFlowRatio,         // ratio of heating PLR to cooling PLR (is this correct?)
                                     Real64 &SensOutput,                // sensible capacity (W)
                                     Real64 &LatOutput,                 // latent capacity (W)
                                     Optional_bool HXUnitOn = _,        // Flag to control HX for HXAssisted Cooling Coil
                                     Optional<Real64> HeatCoilLoad = _, // Adjusted load to heating coil when SAT exceeds max limit (W)
                                     Optional<Real64> SuppCoilLoad = _, // Adjusted load to supp heating coil when SAT exceeds max limit (W)
                                     Optional_int_const CompOn = _      // Determines if compressor is on or off
        );

        void calculateCapacity(Real64 &SensOutput,      // sensible output of AirloopHVAC:UnitarySystem
                               Real64 &LatOutput        // latent output of AirloopHVAC:UnitarySystem
        );

        void calcUnitaryCoolingSystem(int const AirLoopNum,          // index to air loop
                                      bool const FirstHVACIteration, // True when first HVAC iteration
                                      Real64 const PartLoadRatio,    // coil operating part-load ratio
                                      int const CompOn,              // compressor control (0=off, 1=on)
                                      Real64 const OnOffAirFlowRatio,
                                      Real64 const CoilCoolHeatRat, // ratio of cooling to heating PLR for cycling fan RH control
                                      bool const HXUnitOn           // Flag to control HX for HXAssisted Cooling Coil
        );

        void calcUnitaryHeatingSystem(int const AirLoopNum,                   // index to air loop
                                      bool const FirstHVACIteration,          // True when first HVAC iteration
                                      Real64 const PartLoadRatio,             // coil operating part-load ratio
                                      int const CompOn,                       // comrpressor control (0=off, 1=on)
                                      Real64 const OnOffAirFlowRatio,         // ratio of on to off flow rate
                                      Optional<Real64 const> HeatCoilLoad = _ // adjusted heating coil load if outlet temp exceeds max (W)
        );

        void calcUnitarySuppHeatingSystem(bool const FirstHVACIteration,          // True when first HVAC iteration
                                          Real64 const PartLoadRatio,             // coil operating part-load ratio
                                          Optional<Real64 const> SuppCoilLoad = _ // adjusted supp coil load when outlet temp exceeds max (W)
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
                                      int &CompOn                    // compressor on/off control
        );

        void controlSuppHeatSystem(int const AirLoopNum,         // index to air loop
                                   bool const FirstHVACIteration // First HVAC iteration flag
        );

        void simMultiSpeedCoils(int const AirLoopNum,          // Index to air loop
                                bool const FirstHVACIteration, // True when first HVAC iteration
                                int &CompOn,                   // compressor on/off control
                                bool const SensibleLoad,
                                bool const LatentLoad,
                                Real64 const PartLoadFrac,
                                int const CoilType,
                                Optional_int_const SpeedNumber = _);

        void calcPassiveSystem(int const AirLoopNum,         // Index to air loop
                               bool const FirstHVACIteration // True when first HVAC iteration
        );

        void heatPumpRunFrac(Real64 const PLR,   // part load ratio
                             bool &errFlag,           // part load factor out of range flag
                             Real64 &RuntimeFrac      // the required run time fraction to meet part load
        );

        Real64 hotWaterHeatingCoilResidual(Real64 const PartLoadFrac,     // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                           std::vector<Real64> const &Par  // par(1) = DX coil number
        );

        Real64 DOE2DXCoilResidual(Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                  std::vector<Real64> const &Par   // par(1) = DX coil number
        );

        Real64 DOE2DXCoilHumRatResidual(Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                        std::vector<Real64> const &Par   // par(1) = DX coil number
        );

        Real64 HXAssistedCoolCoilTempResidual(Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                              std::vector<Real64> const &Par   // par(1) = DX coil number
        );

        Real64 HXAssistedCoolCoilHRResidual(Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                            std::vector<Real64> const &Par   // par(1) = DX coil number
        );

        Real64 DXCoilVarSpeedResidual(Real64 const SpeedRatio,  // compressor speed ratio (1.0 is max, 0.0 is min)
                                      std::vector<Real64> const &Par // par(1) = DX coil number
        );

        Real64 HeatingCoilVarSpeedResidual(Real64 const SpeedRatio,  // compressor speed ratio (1.0 is max, 0.0 is min)
                                           std::vector<Real64> const &Par // par(1) = DX coil number
        );

        Real64 DXCoilVarSpeedHumRatResidual(Real64 const SpeedRatio,  // compressor speed ratio (1.0 is max, 0.0 is min)
                                            std::vector<Real64> const &Par // par(1) = DX coil number
        );

        Real64 DXCoilCyclingResidual(Real64 const CycRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                     std::vector<Real64> const &Par // par(1) = DX coil number
        );

        Real64 HeatingCoilVarSpeedCycResidual(Real64 const CycRatio,    // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                                              std::vector<Real64> const &Par // par(1) = DX coil number
        );

        Real64 calcUnitarySystemLoadResidual(Real64 const PartLoadRatio, // DX cooling coil part load ratio
                                             std::vector<Real64> const &Par   // Function parameters
        );

        void setSpeedVariables(bool const SensibleLoad,   // True when meeting a sensible load (not a moisture load)
                               Real64 const PartLoadRatio // operating PLR
        );
    };

    extern std::vector<UnitarySys> unitarySys;
    extern std::vector<DesignSpecMSHP> designSpecMSHP;
    static int getDesignSpecMSHPIndex(std::string const &objectName);
    static int getUnitarySystemIndex(std::string const &objectName);


} // namespace UnitarySystems
} // namespace EnergyPlus
#endif // ENERGYPLUS_UNITARYSYSTEM_HH
