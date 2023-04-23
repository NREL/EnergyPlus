// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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

#ifndef Fans_hh_INCLUDED
#define Fans_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace Fans {

    enum class AvailabilityManagerCoupling
    {
        Invalid = -1,
        Coupled,
        Decoupled,
        Num
    };

    struct FanEquipConditions
    {
        // Members
        std::string FanName;               // Name of the fan
        std::string FanType;               // Type of Fan ie. Simple, Vane axial, Centrifugal, etc.
        std::string AvailSchedName;        // Fan Operation Schedule
        int FanType_Num = 0;               // DataHVACGlobals fan type
        int AvailSchedPtrNum = 0;          // Pointer to the availability schedule
        Real64 InletAirMassFlowRate = 0.0; // MassFlow through the Fan being Simulated [kg/Sec]
        Real64 OutletAirMassFlowRate = 0.0;
        Real64 MaxAirFlowRate = 0.0;                        // Max Specified Volume Flow Rate of Fan [m3/sec]
        bool MaxAirFlowRateIsAutosizable = false;           // if true, then this type of fan could be autosize
        bool MaxAirFlowRateEMSOverrideOn = false;           // if true, EMS wants to override fan size for Max Volume Flow Rate
        Real64 MaxAirFlowRateEMSOverrideValue = 0.0;        // EMS value to use for override of  Max Volume Flow Rate
        Real64 MinAirFlowRate = 0.0;                        // Min Specified Volume Flow Rate of Fan [m3/sec]
        Real64 MaxAirMassFlowRate = 0.0;                    // Max flow rate of fan in kg/sec
        Real64 MinAirMassFlowRate = 0.0;                    // Min flow rate of fan in kg/sec
        int FanMinAirFracMethod = DataHVACGlobals::MinFrac; // parameter for what method is used for min flow fraction
        Real64 FanMinFrac = 0.0;                            // Minimum fan air flow fraction
        Real64 FanFixedMin = 0.0;                           // Absolute minimum fan air flow [m3/s]
        bool EMSMaxMassFlowOverrideOn = false;              // if true, then EMS is calling to override mass flow
        Real64 EMSAirMassFlowValue = 0.0;                   // value EMS is directing to use [kg/s]
        Real64 InletAirTemp = 0.0;
        Real64 OutletAirTemp = 0.0;
        Real64 InletAirHumRat = 0.0;
        Real64 OutletAirHumRat = 0.0;
        Real64 InletAirEnthalpy = 0.0;
        Real64 OutletAirEnthalpy = 0.0;
        Real64 FanPower = 0.0;                             // Power of the Fan being Simulated [kW]
        Real64 FanEnergy = 0.0;                            // Fan energy in [kJ]
        Real64 FanRuntimeFraction = 0.0;                   // Fraction of the timestep that the fan operates
        Real64 DeltaTemp = 0.0;                            // Temp Rise across the Fan [C]
        Real64 DeltaPress = 0.0;                           // Delta Pressure Across the Fan [N/m2]
        Real64 PowerLossToAir = 0.0;                       // Fan heat gain to air stream [W]
        bool EMSFanPressureOverrideOn = false;             // if true, then EMS is calling to override
        Real64 EMSFanPressureValue = 0.0;                  // EMS value for Delta Pressure Across the Fan [Pa]
        Real64 FanEff = 0.0;                               // Fan total system efficiency (fan*belt*motor*VFD)
        bool EMSFanEffOverrideOn = false;                  // if true, then EMS is calling to override
        Real64 EMSFanEffValue = 0.0;                       // EMS value for total efficiency of the Fan, fraction on 0..1
        bool FaultyFilterFlag = false;                     // Indicate whether there is a fouling air filter corresponding to the fan
        int FaultyFilterIndex = 0;                         // Index of the fouling air filter corresponding to the fan
        Real64 MotEff = 0.0;                               // Fan motor efficiency
        Real64 MotInAirFrac = 0.0;                         // Fraction of motor heat entering air stream
        Array1D<Real64> FanCoeff{0.0, 0.0, 0.0, 0.0, 0.0}; // Fan Part Load Coefficients to match fan type
        // Mass Flow Rate Control Variables
        Real64 MassFlowRateMaxAvail = 0.0;
        Real64 MassFlowRateMinAvail = 0.0;
        Real64 RhoAirStdInit = 0.0;
        int InletNodeNum = 0;
        int OutletNodeNum = 0;
        int NVPerfNum = 0;
        int FanPowerRatAtSpeedRatCurveIndex = 0;
        int FanEffRatioCurveIndex = 0;
        std::string EndUseSubcategoryName;
        bool OneTimePowerRatioCheck = true; // one time flag used for error message
        bool OneTimeEffRatioCheck = true;   // one time flag used for error message
        Real64 FanWheelDia = 0.0;           // Fan wheel outer diameter [m]
        Real64 FanOutletArea = 0.0;         // Fan outlet area [m2]
        Real64 FanMaxEff = 0.0;             // Fan maximum static efficiency [-]
        Real64 EuMaxEff = 0.0;              // Euler number at fan maximum static efficiency [-]
        Real64 FanMaxDimFlow = 0.0;         // Fan maximum dimensionless airflow [-]
        Real64 FanShaftPwrMax = 0.0;        // Fan shaft maximum input power [W]
        Real64 FanSizingFactor = 0.0;       // Fan sizing factor [-]
        Real64 PulleyDiaRatio = 0.0;        // Motor/fan pulley diameter ratio [-]
        Real64 BeltMaxTorque = 0.0;         // Belt maximum torque [N-m]
        Real64 BeltSizingFactor = 0.0;      // Belt sizing factor [-]
        Real64 BeltTorqueTrans = 0.0;       // Belt fractional torque transition Region 1-2 [-]
        Real64 MotorMaxSpd = 0.0;           // Motor maximum speed [rpm]
        Real64 MotorMaxOutPwr = 0.0;        // Motor maximum output power [W]
        Real64 MotorSizingFactor = 0.0;     // Motor sizing factor [-]
        std::string VFDEffType;             // VFD efficiency type [Speed or Power]
        Real64 VFDMaxOutPwr = 0.0;          // VFD maximum output power [W]
        Real64 VFDSizingFactor = 0.0;       // VFD sizing factor [-]
        int PressRiseCurveIndex = 0;        // Fan pressure rise curve index
        int PressResetCurveIndex = 0;       // Duct static pressure reset curve index
        int PLFanEffNormCurveIndex = 0;     // Fan part-load efficiency (normal) curve index
        int PLFanEffStallCurveIndex = 0;    // Fan part-load efficiency (stall) curve index
        int DimFlowNormCurveIndex = 0;      // Fan dimensionless airflow (normal) curve index
        int DimFlowStallCurveIndex = 0;     // Fan dimensionless airflow (stall) curve index
        int BeltMaxEffCurveIndex = 0;       // Belt maximum efficiency curve index
        int PLBeltEffReg1CurveIndex = 0;    // Belt part-load efficiency (Region 1) curve index
        int PLBeltEffReg2CurveIndex = 0;    // Belt part-load efficiency (Region 2) curve index
        int PLBeltEffReg3CurveIndex = 0;    // Belt part-load efficiency (Region 3) curve index
        int MotorMaxEffCurveIndex = 0;      // Motor maximum efficiency curve index
        int PLMotorEffCurveIndex = 0;       // Motor part-load efficiency curve index
        int VFDEffCurveIndex = 0;           // VFD efficiency curve index
        Real64 DeltaPressTot = 0.0;         // Total pressure rise across fan [N/m2]
        Real64 FanAirPower = 0.0;           // Air power for fan being Simulated [W]
        Real64 FanSpd = 0.0;                // Fan shaft rotational speed [rpm]
        Real64 FanTrq = 0.0;                // Fan shaft torque [N-m]
        Real64 FanWheelEff = 0.0;           // Fan efficiency (mechanical)
        Real64 FanShaftPower = 0.0;         // Shaft input power for fan being Simulated [W]
        Real64 BeltMaxEff = 0.0;            // Belt maximum efficiency (mechanical)
        Real64 BeltEff = 0.0;               // Belt efficiency (mechanical)
        Real64 BeltInputPower = 0.0;        // Belt input power for fan being Simulated [W]
        Real64 MotorMaxEff = 0.0;           // Motor maximum efficiency (electrical)
        Real64 MotorInputPower = 0.0;       // Motor input power for fan being Simulated [W]
        Real64 VFDEff = 0.0;                // VFD efficiency (electrical)
        Real64 VFDInputPower = 0.0;         // VFD input power for fan being Simulated [W]
        // zone exhaust fan
        int FlowFractSchedNum = 0; // schedule index flow rate modifier schedule
        AvailabilityManagerCoupling AvailManagerMode =
            AvailabilityManagerCoupling::Invalid; // mode for how exhaust fan should react to availability managers
        int MinTempLimitSchedNum = 0;             // schedule index minimum temperature limit
        int BalancedFractSchedNum = 0;            // schedule index portion recirculated
        Real64 UnbalancedOutletMassFlowRate = 0.0;
        Real64 BalancedOutletMassFlowRate = 0.0;
        int AirLoopNum = 0;          // Airloop number
        Real64 DesignPointFEI = 0.0; // Fan Energy Index for the fan at the design operating point
    };

    struct NightVentPerfData
    {
        // Members
        std::string FanName;             // Name of the fan that will use this data
        Real64 FanEff = 0.0;             // Fan total efficiency; motor and mechanical
        Real64 DeltaPress = 0.0;         // Delta Pressure Across the Fan [N/m2]
        Real64 MaxAirFlowRate = 0.0;     // Max Specified Volume Flow Rate of Fan [m3/s]
        Real64 MaxAirMassFlowRate = 0.0; // Max flow rate of fan in kg/sec
        Real64 MotEff = 0.0;             // Fan motor efficiency
        Real64 MotInAirFrac = 0.0;       // Fraction of motor heat entering air stream
    };

    struct FanNumericFieldData
    {
        // Members
        Array1D_string FieldNames;
    };

    void SimulateFanComponents(EnergyPlusData &state,
                               std::string_view CompName,
                               bool FirstHVACIteration,
                               int &CompIndex,
                               ObjexxFCL::Optional<Real64 const> SpeedRatio = _,
                               ObjexxFCL::Optional<Real64 const> PressureRise = _ // Pressure difference to use for DeltaPress
    );

    void GetFanInput(EnergyPlusData &state);

    void InitFan(EnergyPlusData &state,
                 int FanNum,
                 bool FirstHVACIteration // unused1208
    );

    void SizeFan(EnergyPlusData &state, int FanNum);

    void SimSimpleFan(EnergyPlusData &state, int FanNum);

    void SimVariableVolumeFan(EnergyPlusData &state, int FanNum, ObjexxFCL::Optional<Real64 const> PressureRise = _);

    void SimOnOffFan(EnergyPlusData &state, int FanNum, ObjexxFCL::Optional<Real64 const> SpeedRatio = _);

    void SimZoneExhaustFan(EnergyPlusData &state, int FanNum);

    void SimComponentModelFan(EnergyPlusData &state, int FanNum);

    void UpdateFan(EnergyPlusData &state, int FanNum);

    void ReportFan(EnergyPlusData &state, int FanNum);

    void GetFanIndex(EnergyPlusData &state, std::string const &FanName, int &FanIndex, bool &ErrorsFound, std::string_view ThisObjectType = {});

    void GetFanVolFlow(EnergyPlusData &state, int FanIndex, Real64 &FanVolFlow);

    Real64 GetFanPower(EnergyPlusData &state, int FanIndex);

    void GetFanType(EnergyPlusData &state,
                    std::string const &FanName,           // Fan name
                    int &FanType,                         // returned fantype number
                    bool &ErrorsFound,                    // error indicator
                    std::string_view ThisObjectType = {}, // parent object type (for error message)
                    std::string_view ThisObjectName = {}  // parent object name (for error message)
    );

    Real64 GetFanDesignVolumeFlowRate(EnergyPlusData &state,
                                      std::string_view FanType,                  // must match fan types in this module
                                      std::string_view FanName,                  // must match fan names for the fan type
                                      bool &ErrorsFound,                         // set to true if problem
                                      ObjexxFCL::Optional_int_const FanIndex = _ // index to fan
    );

    int GetFanInletNode(EnergyPlusData &state,
                        std::string_view FanType, // must match fan types in this module
                        std::string_view FanName, // must match fan names for the fan type
                        bool &ErrorsFound         // set to true if problem
    );

    int GetFanOutletNode(EnergyPlusData &state,
                         std::string const &FanType, // must match fan types in this module
                         std::string const &FanName, // must match fan names for the fan type
                         bool &ErrorsFound           // set to true if problem
    );

    int GetFanAvailSchPtr(EnergyPlusData &state,
                          std::string const &FanType, // must match fan types in this module
                          std::string const &FanName, // must match fan names for the fan type
                          bool &ErrorsFound           // set to true if problem
    );

    int GetFanSpeedRatioCurveIndex(EnergyPlusData &state,
                                   std::string &FanType,               // must match fan types in this module (set if nonzero index passed)
                                   std::string &FanName,               // must match fan names for the fan type (set if nonzero index passed)
                                   ObjexxFCL::Optional_int IndexIn = _ // optional fan index if fan type and name are unknown or index needs setting
    );

    void SetFanData(EnergyPlusData &state,
                    int FanNum,                                          // Index of fan
                    bool &ErrorsFound,                                   // Set to true if certain errors found
                    std::string const &FanName,                          // Name of fan
                    ObjexxFCL::Optional<Real64 const> MaxAirVolFlow = _, // Fan air volumetric flow rate    [m3/s]
                    ObjexxFCL::Optional<Real64 const> MinAirVolFlow = _  // Fan air volumetric flow rate    [m3/s]
    );

    Real64 CalFaultyFanAirFlowReduction(EnergyPlusData &state,
                                        std::string const &FanName,    // Name of the Fan
                                        Real64 FanDesignAirFlowRate,   // Fan Design Volume Flow Rate [m3/s]
                                        Real64 FanDesignDeltaPress,    // Fan Design Delta Pressure [Pa]
                                        Real64 FanFaultyDeltaPressInc, // Increase of Fan Delta Pressure in the Faulty Case [Pa]
                                        int FanCurvePtr                // Fan Curve Pointer
    );

    Real64 FanDesHeatGain(EnergyPlusData &state,
                          int FanNum,       // index of fan in Fan array
                          Real64 FanVolFlow // fan volumetric flow rate [m3/s]
    );

    void SetFanAirLoopNumber(EnergyPlusData &state, int FanIndex, int AirLoopNum);

    void FanInputsForDesHeatGain(EnergyPlusData &state,
                                 int fanIndex,
                                 Real64 &deltaP,
                                 Real64 &motEff,
                                 Real64 &totEff,
                                 Real64 &motInAirFrac,
                                 Real64 &fanShaftPow,
                                 Real64 &motInPower,
                                 bool &fanCompModel);

} // namespace Fans

struct FansData : BaseGlobalStruct
{
    int NumFans = 0;
    int NumNightVentPerf = 0;    // number of FAN:NIGHT VENT PERFORMANCE objects found in the input
    bool GetFanInputFlag = true; // Flag set to make sure you get input once
    bool MyOneTimeFlag = true;
    bool ZoneEquipmentListChecked = false;
    Array1D_bool MySizeFlag;
    Array1D_bool MyEnvrnFlag;
    Array1D_bool CheckEquipName;
    Array1D<Fans::FanEquipConditions> Fan;
    std::unordered_map<std::string, std::string> UniqueFanNames;
    Array1D<Fans::NightVentPerfData> NightVentPerf;
    Array1D<Fans::FanNumericFieldData> FanNumericFields;
    int ErrCount = 0;

    void clear_state() override
    {
        *this = FansData();
    }
};

} // namespace EnergyPlus

#endif
