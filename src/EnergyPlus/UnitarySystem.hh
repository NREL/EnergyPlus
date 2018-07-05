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
        std::vector<Real64> coolVolumeFlowRate;
        std::vector<Real64> coolMassFlowRate;
        std::vector<Real64> MSCoolingSpeedRatio;
        std::vector<Real64> heatVolumeFlowRate;
        std::vector<Real64> heatMassFlowRate;
        std::vector<Real64> MSHeatingSpeedRatio;
        bool singleModeFlag;

        static void getDesignSpecMSHP();
        static void getDesignSpecMSHPdata(bool errorsFound);
    };

    class UnitarySys
    {

        // bool myOneTimeFlag;
        // bool getInputOnceFlag;

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
        std::string unitType;
        int sysAvailSchedPtr; // Pointer to the availability schedule
        controlTypeEnum controlType;
        int controlZoneNum;
        dehumCtrlTypeEnum dehumidControlType_Num;
        bool humidistat;
        int airInNode;  // system air node at inlet
        int airOutNode; // system air node at outlet
        bool validASHRAECoolCoil;
        bool validASHRAEHeatCoil;
        std::string fanName;
        int fanIndex;
        fanPlaceEnum fanPlace;
        int fanOpModeSchedPtr;
        bool fanExists;
        int fanType_Num;
        bool requestAutoSize;
        Real64 actualFanVolFlowRate;
        Real64 designFanVolFlowRate;
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
        bool myGetInputSuccessfulFlag;
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
        int singleMode;
        bool multiOrVarSpeedHeatCoil;
        bool multiOrVarSpeedCoolCoil;

        static void getInput();
        static void getInputData(bool errorsFound);

    public:
        UnitarySys(); // constructor

        ~UnitarySys() // destructor
        {
        }

        std::string name; // user identifier
        int unitarySystemType_Num;

        static UnitarySys *factory(int object_type_of_num, std::string const objectName);

        void simulate(std::string const &objectName,
                      bool const firstHVACIteration,
                      int const &AirLoopNum,
                      int &CompIndex,
                      bool &HeatingActive,
                      bool &CoolingActive);

        void init(bool const firstHVACIteration);

    };

    extern std::vector<UnitarySys> unitarySys;
    extern std::vector<DesignSpecMSHP> designSpecMSHP;

} // namespace UnitarySystems
} // namespace EnergyPlus
#endif // ENERGYPLUS_UNITARYSYSTEM_HH
