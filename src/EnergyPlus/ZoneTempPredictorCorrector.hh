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

#ifndef ZoneTempPredictorCorrector_hh_INCLUDED
#define ZoneTempPredictorCorrector_hh_INCLUDED

// C++ Headers
#include <string>
#include <unordered_set>
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace ZoneTempPredictorCorrector {

    // iZControlTypes

    enum class ZControlTypes
    {
        TStat = 1,
        TCTStat = 2,
        OTTStat = 3,
        HStat = 4,
        TandHStat = 5,
        StagedDual = 6
    };

    enum class AdaptiveComfortModel
    {
        ADAP_NONE = 1,
        ASH55_CENTRAL = 2,
        ASH55_UPPER_90 = 3,
        ASH55_UPPER_80 = 4,
        CEN15251_CENTRAL = 5,
        CEN15251_UPPER_I = 6,
        CEN15251_UPPER_II = 7,
        CEN15251_UPPER_III = 8
    };

    // The numbers are used to access zone comfort control type, see ValidComfortControlTypes
    enum class ComfortControl
    {
        SglHeatSetPoint = 1,
        SglCoolSetPoint = 2,
        SglHCSetPoint = 3,
        DualSetPoint = 4,
        SglHeatSetPointFanger = 1,
        SglCoolSetPointFanger = 2,
        SglHCSetPointFanger = 3,
        DualSetPointFanger = 4,
        SglHeatSetPointPierce = 5,
        SglCoolSetPointPierce = 6,
        SglHCSetPointPierce = 7,
        DualSetPointPierce = 8,
        SglHeatSetPointKSU = 9,
        SglCoolSetPointKSU = 10,
        SglHCSetPointKSU = 11,
        DualSetPointKSU = 12
    };

    // Average method parameter with multiple people objects in a zone
    enum class AverageMethod
    {
        NO = 0,  // No multiple people objects
        SPE = 1, // Specific people object
        OBJ = 2, // People object average
        PEO = 3  // People number average
    };

    struct ZoneTempControlType
    {
        // Members
        std::string Name;          // Name of the zone
        std::string TempSchedName; // Name of the schedule which determines the zone temp setpoint
        int TempSchedIndex;
        std::string HeatTempSetptSchedName;
        int HeatTempSchedIndex;
        std::string CoolTempSetptSchedName;
        int CoolTempSchedIndex;

        // Default Constructor
        ZoneTempControlType() : TempSchedIndex(0), HeatTempSchedIndex(0), CoolTempSchedIndex(0)
        {
        }
    };

    struct ZoneComfortFangerControlType
    {
        // Members
        std::string Name;                  // Name of the zone
        std::string PMVSchedName;          // Name of the schedule which determines the zone temp setpoint
        int PMVSchedIndex;                 // Index to PMV dual set point schedule
        std::string HeatPMVSetptSchedName; // Name of PMV heating set point schedule
        int HeatPMVSchedIndex;             // Index to PMV heating set point schedule
        std::string CoolPMVSetptSchedName; // Name of PMV cooling set point schedule
        int CoolPMVSchedIndex;             // INdex to PMV cooling set point schedule

        // Default Constructor
        ZoneComfortFangerControlType() : PMVSchedIndex(0), HeatPMVSchedIndex(0), CoolPMVSchedIndex(0)
        {
        }
    };

    struct AdaptiveComfortDailySetPointSchedule
    {
        // Members
        bool initialized;
        Array1D<Real64> ThermalComfortAdaptiveASH55_Upper_90;
        Array1D<Real64> ThermalComfortAdaptiveASH55_Upper_80;
        Array1D<Real64> ThermalComfortAdaptiveASH55_Central;
        Array1D<Real64> ThermalComfortAdaptiveCEN15251_Upper_I;
        Array1D<Real64> ThermalComfortAdaptiveCEN15251_Upper_II;
        Array1D<Real64> ThermalComfortAdaptiveCEN15251_Upper_III;
        Array1D<Real64> ThermalComfortAdaptiveCEN15251_Central;

        // Default Constructor
        AdaptiveComfortDailySetPointSchedule() : initialized(false)
        {
        }
    };

    // Functions

    void ManageZoneAirUpdates(EnergyPlusData &state,
                              DataHeatBalFanSys::PredictorCorrectorCtrl const UpdateType, // Can be iGetZoneSetPoints, iPredictStep, iCorrectStep
                              Real64 &ZoneTempChange,                                     // Temp change in zone air btw previous and current timestep
                              bool const ShortenTimeStepSys,
                              bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
                              Real64 const PriorTimeStep         // the old value for timestep length is passed for possible use in interpolating
    );

    void GetZoneAirSetPoints(EnergyPlusData &state);

    void InitZoneAirSetPoints(EnergyPlusData &state);

    void PredictSystemLoads(EnergyPlusData &state,
                            bool const ShortenTimeStepSys,
                            bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
                            Real64 const PriorTimeStep         // the old value for timestep length is passed for possible use in interpolating
    );

    void CalcZoneAirTempSetPoints(EnergyPlusData &state);

    void CalculateMonthlyRunningAverageDryBulb(EnergyPlusData &state, Array1D<Real64> &runningAverageASH, Array1D<Real64> &runningAverageCEN);

    void
    CalculateAdaptiveComfortSetPointSchl(EnergyPlusData &state, Array1D<Real64> const &runningAverageASH, Array1D<Real64> const &runningAverageCEN);

    void CalcPredictedSystemLoad(EnergyPlusData &state, int const ZoneNum, Real64 RAFNFrac);

    void ReportSensibleLoadsZoneMultiplier(Real64 &TotalLoad,
                                           Real64 &TotalHeatLoad,
                                           Real64 &TotalCoolLoad,
                                           Real64 &SensLoadSingleZone,
                                           Real64 &SensLoadHeatSingleZone,
                                           Real64 &SensLoadCoolSingleZone,
                                           Real64 const OutputHeatSP,
                                           Real64 const OutputCoolSP,
                                           Real64 const LoadCorrFactor,
                                           Real64 const ZoneMultiplier,
                                           Real64 const ZoneMultiplierList);

    void CalcPredictedHumidityRatio(EnergyPlusData &state, int const ZoneNum, Real64 RAFNFrac);

    void ReportMoistLoadsZoneMultiplier(Real64 &TotalLoad,
                                        Real64 &TotalHumidLoad,
                                        Real64 &TotalDehumidLoad,
                                        Real64 &MoistLoadSingleZone,
                                        Real64 &MoistLoadHumidSingleZone,
                                        Real64 &MoistLoadDehumidSingleZone,
                                        Real64 const ZoneMultiplier,
                                        Real64 const ZoneMultiplierList);

    void CorrectZoneAirTemp(EnergyPlusData &state,
                            Real64 &ZoneTempChange, // Temperature change in zone air between previous and current timestep
                            bool const ShortenTimeStepSys,
                            bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step history
                            Real64 const PriorTimeStep         // the old value for timestep length is passed for possible use in interpolating
    );

    void PushZoneTimestepHistories(EnergyPlusData &state);

    void PushSystemTimestepHistories(EnergyPlusData &state);

    void RevertZoneTimestepHistories(EnergyPlusData &state);

    void CorrectZoneHumRat(EnergyPlusData &state, int const ZoneNum);

    void DownInterpolate4HistoryValues(Real64 const OldTimeStep,
                                       Real64 const NewTimeStep,
                                       Real64 &oldVal0,
                                       Real64 &oldVal1,
                                       Real64 &oldVal2,
                                       Real64 &oldVal3,
                                       Real64 &oldVal4,
                                       Real64 &newVal0,
                                       Real64 &newVal1,
                                       Real64 &newVal2,
                                       Real64 &newVal3, // unused 1208
                                       Real64 &newVal4  // unused 1208
    );

    void InverseModelTemperature(EnergyPlusData &state,
                                 int const ZoneNum,              // Zone number
                                 Real64 &SumIntGain,             // Zone sum of convective internal gains
                                 Real64 &SumIntGainExceptPeople, // Zone sum of convective internal gains except for people
                                 Real64 &SumHA,                  // Zone sum of Hc*Area
                                 Real64 &SumHATsurf,             // Zone sum of Hc*Area*Tsurf
                                 Real64 &SumHATref,              // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
                                 Real64 &SumMCp,                 // Zone sum of MassFlowRate*Cp
                                 Real64 &SumMCpT,                // Zone sum of MassFlowRate*Cp*T
                                 Real64 &SumSysMCp,              // Zone sum of air system MassFlowRate*Cp
                                 Real64 &SumSysMCpT,             // Zone sum of air system MassFlowRate*Cp*T
                                 Real64 &AirCap                  // Formerly CoefAirrat, coef in zone temp eqn with dim of "air power capacity"rd
    );

    void InverseModelHumidity(EnergyPlusData &state,
                              int const ZoneNum,              // Zone number
                              Real64 &LatentGain,             // Zone sum of latent gain
                              Real64 &LatentGainExceptPeople, // Zone sum of latent gain except for people
                              Real64 &ZoneMassFlowRate,       // Zone air mass flow rate
                              Real64 &MoistureMassFlowRate,   // Zone moisture mass flow rate
                              Real64 &H2OHtOfVap,             // Heat of vaporization of air
                              Real64 &RhoAir                  // Air density
    );

    void CalcZoneSums(EnergyPlusData &state,
                      int const ZoneNum,              // Zone number
                      Real64 &SumIntGain,             // Zone sum of convective internal gains
                      Real64 &SumHA,                  // Zone sum of Hc*Area
                      Real64 &SumHATsurf,             // Zone sum of Hc*Area*Tsurf
                      Real64 &SumHATref,              // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
                      Real64 &SumMCp,                 // Zone sum of MassFlowRate*Cp
                      Real64 &SumMCpT,                // Zone sum of MassFlowRate*Cp*T
                      Real64 &SumSysMCp,              // Zone sum of air system MassFlowRate*Cp
                      Real64 &SumSysMCpT,             // Zone sum of air system MassFlowRate*Cp*T
                      bool const CorrectorFlag = true // Corrector call flag
    );

    void CalcZoneComponentLoadSums(EnergyPlusData &state,
                                   int const ZoneNum,        // Zone number
                                   Real64 const TempDepCoef, // Dependent coefficient
                                   Real64 const TempIndCoef, // Independent coefficient
                                   Real64 &SumIntGains,      // Zone sum of convective internal gains
                                   Real64 &SumHADTsurfs,     // Zone sum of Hc*Area*(Tsurf - Tz)
                                   Real64 &SumMCpDTzones,    // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
                                   Real64 &SumMCpDtInfil,    // Zone sum of MassFlowRate*Cp*(Tout - Tz) transfer from outside, ventil, earth tube
                                   Real64 &SumMCpDTsystem,   // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
                                   Real64 &SumNonAirSystem,  // Zone sum of non air system convective heat gains
                                   Real64 &CzdTdt,           // Zone air energy storage term.
                                   Real64 &imBalance,        // put all terms in eq. 5 on RHS , should be zero
                                   Real64 &SumEnthalpyM,     // Zone sum of phase change material melting enthlpy
                                   Real64 &SumEnthalpyH      // Zone sum of phase change material freezing enthalpy
    );

    bool VerifyThermostatInZone(EnergyPlusData &state, std::string const &ZoneName); // Zone to verify

    bool VerifyControlledZoneForThermostat(EnergyPlusData &state, std::string const &ZoneName); // Zone to verify

    void DetectOscillatingZoneTemp(EnergyPlusData &state);

    void AdjustAirSetPointsforOpTempCntrl(EnergyPlusData &state, int const TempControlledZoneID, int const ActualZoneNum, Real64 &ZoneAirSetPoint);

    void AdjustOperativeSetPointsforAdapComfort(EnergyPlusData &state, int const TempControlledZoneID, Real64 &ZoneAirSetPoint);

    void CalcZoneAirComfortSetPoints(EnergyPlusData &state);

    void GetComfortSetPoints(EnergyPlusData &state,
                             int const PeopleNum,
                             int const ComfortControlNum,
                             Real64 const PMVSet,
                             Real64 &Tset // drybulb setpoint temperature for a given PMV value
    );

    Real64 PMVResidual(EnergyPlusData &state,
                       Real64 const Tset,
                       Array1D<Real64> const &Par // par(1) = PMV set point
    );

    void AdjustCoolingSetPointforTempAndHumidityControl(EnergyPlusData &state,
                                                        int const TempControlledZoneID,
                                                        int const ActualZoneNum // controlled zone actual zone number
    );

    void OverrideAirSetPointsforEMSCntrl(EnergyPlusData &state);

    void FillPredefinedTableOnThermostatSetpoints(EnergyPlusData &state);

    std::tuple<Real64, int, std::string>
    temperatureAndCountInSch(EnergyPlusData &state, int const &scheduleIndex, bool const &isSummer, int const &dayOfWeek, int const &hourOfDay);

} // namespace ZoneTempPredictorCorrector

struct ZoneTempPredictorCorrectorData : BaseGlobalStruct
{
    Array1D_string const ValidControlTypes;

    Array1D_string const ValidComfortControlTypes;

    Array1D_string const cZControlTypes;

    int NumSingleTempHeatingControls;
    int NumSingleTempCoolingControls;
    int NumSingleTempHeatCoolControls;
    int NumDualTempHeatCoolControls;

    // Number of Thermal comfort control types
    int NumSingleFangerHeatingControls;
    int NumSingleFangerCoolingControls;
    int NumSingleFangerHeatCoolControls;
    int NumDualFangerHeatCoolControls;

    // Number of zone with staged controlled objects
    int NumStageCtrZone;
    // Number of zone with onoff thermostat
    int NumOnOffCtrZone;

    Array1D<Real64> ZoneSetPointLast;
    Array1D<Real64> TempIndZnLd;
    Array1D<Real64> TempDepZnLd;
    Array1D<Real64> ZoneAirRelHum; // Zone relative humidity in percent

    // Zone temperature history - used only for oscillation test
    Array2D<Real64> ZoneTempHist;
    Array1D<Real64> ZoneTempOscillate;
    Array1D<Real64> ZoneTempOscillateDuringOccupancy;
    Array1D<Real64> ZoneTempOscillateInDeadband;
    Real64 AnyZoneTempOscillate;
    Real64 AnyZoneTempOscillateDuringOccupancy;
    Real64 AnyZoneTempOscillateInDeadband;
    Real64 AnnualAnyZoneTempOscillate;
    Real64 AnnualAnyZoneTempOscillateDuringOccupancy;
    Real64 AnnualAnyZoneTempOscillateInDeadband;
    bool OscillationVariablesNeeded;

    bool InitZoneAirSetPointsOneTimeFlag;
    bool SetupOscillationOutputFlag;

    // Object Data
    std::unordered_set<std::string> HumidityControlZoneUniqueNames;
    EPVector<ZoneTempPredictorCorrector::ZoneTempControlType> SetPointSingleHeating;
    EPVector<ZoneTempPredictorCorrector::ZoneTempControlType> SetPointSingleCooling;
    EPVector<ZoneTempPredictorCorrector::ZoneTempControlType> SetPointSingleHeatCool;
    EPVector<ZoneTempPredictorCorrector::ZoneTempControlType> SetPointDualHeatCool;
    EPVector<ZoneTempPredictorCorrector::ZoneComfortFangerControlType> SetPointSingleHeatingFanger;
    EPVector<ZoneTempPredictorCorrector::ZoneComfortFangerControlType> SetPointSingleCoolingFanger;
    EPVector<ZoneTempPredictorCorrector::ZoneComfortFangerControlType> SetPointSingleHeatCoolFanger;
    EPVector<ZoneTempPredictorCorrector::ZoneComfortFangerControlType> SetPointDualHeatCoolFanger;
    ZoneTempPredictorCorrector::AdaptiveComfortDailySetPointSchedule AdapComfortDailySetPointSchedule;

    Array1D<Real64> AdapComfortSetPointSummerDesDay;

    bool CalcZoneAirComfortSetPointsFirstTimeFlag = true; // Flag set to make sure you get input once
    bool MyEnvrnFlag = true;
    bool MyDayFlag = true;
    bool ErrorsFound = false;
    bool ControlledZonesChecked = false;

    int IterLimitExceededNum1 = 0;
    int IterLimitErrIndex1 = 0;
    int IterLimitExceededNum2 = 0;
    int IterLimitErrIndex2 = 0;

    void clear_state() override
    {
        this->HumidityControlZoneUniqueNames.clear();
        this->NumSingleTempHeatingControls = 0;
        this->NumSingleTempCoolingControls = 0;
        this->NumSingleTempHeatCoolControls = 0;
        this->NumDualTempHeatCoolControls = 0;
        this->NumSingleFangerHeatingControls = 0;
        this->NumSingleFangerCoolingControls = 0;
        this->NumSingleFangerHeatCoolControls = 0;
        this->NumDualFangerHeatCoolControls = 0;
        this->NumStageCtrZone = 0;
        this->InitZoneAirSetPointsOneTimeFlag = true;
        this->SetupOscillationOutputFlag = true;
        this->OscillationVariablesNeeded = false;
        this->ZoneSetPointLast.deallocate();
        this->TempIndZnLd.deallocate();
        this->TempDepZnLd.deallocate();
        this->ZoneAirRelHum.deallocate();
        this->ZoneTempHist.deallocate();
        this->ZoneTempOscillate.deallocate();
        this->AnyZoneTempOscillate = 0.0;
        this->AnyZoneTempOscillateDuringOccupancy = 0.0;
        this->AnyZoneTempOscillateInDeadband = 0.0;
        this->AnnualAnyZoneTempOscillate = 0.0;
        this->AnnualAnyZoneTempOscillateDuringOccupancy = 0.0;
        this->AnnualAnyZoneTempOscillateInDeadband = 0.0;
        this->SetPointSingleHeating.deallocate();
        this->SetPointSingleCooling.deallocate();
        this->SetPointSingleHeatCool.deallocate();
        this->SetPointDualHeatCool.deallocate();
        this->SetPointSingleHeatingFanger.deallocate();
        this->SetPointSingleCoolingFanger.deallocate();
        this->SetPointSingleHeatCoolFanger.deallocate();
        this->SetPointDualHeatCoolFanger.deallocate();
        this->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Central.deallocate();
        this->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_90.deallocate();
        this->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveASH55_Upper_80.deallocate();
        this->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Central.deallocate();
        this->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_I.deallocate();
        this->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_II.deallocate();
        this->AdapComfortDailySetPointSchedule.ThermalComfortAdaptiveCEN15251_Upper_III.deallocate();
        this->NumOnOffCtrZone = 0;
        this->AdapComfortSetPointSummerDesDay = Array1D<Real64>(7, -1);
        this->CalcZoneAirComfortSetPointsFirstTimeFlag = true;
        this->MyEnvrnFlag = true;
        this->MyDayFlag = true;
        this->ErrorsFound = false;
        this->ControlledZonesChecked = false;
        this->IterLimitExceededNum1 = 0;
        this->IterLimitErrIndex1 = 0;
        this->IterLimitExceededNum2 = 0;
        this->IterLimitErrIndex2 = 0;
    }

    // Default Constructor
    ZoneTempPredictorCorrectorData()
        : NumSingleTempHeatingControls(0), NumSingleTempCoolingControls(0), NumSingleTempHeatCoolControls(0), NumDualTempHeatCoolControls(0),
          NumSingleFangerHeatingControls(0), NumSingleFangerCoolingControls(0), NumSingleFangerHeatCoolControls(0), NumDualFangerHeatCoolControls(0),
          NumStageCtrZone(0), NumOnOffCtrZone(0), AnyZoneTempOscillate(0.0), AnyZoneTempOscillateDuringOccupancy(0.0),
          AnyZoneTempOscillateInDeadband(0.0), AnnualAnyZoneTempOscillate(0.0), AnnualAnyZoneTempOscillateDuringOccupancy(0.0),
          AnnualAnyZoneTempOscillateInDeadband(0.0), OscillationVariablesNeeded(false), InitZoneAirSetPointsOneTimeFlag(true),
          SetupOscillationOutputFlag(true), CalcZoneAirComfortSetPointsFirstTimeFlag(true), MyEnvrnFlag(true), MyDayFlag(true), ErrorsFound(false),
          ControlledZonesChecked(false)
    {
        AdapComfortSetPointSummerDesDay.allocate(7);
        AdapComfortSetPointSummerDesDay = -1;
    }
};

} // namespace EnergyPlus

#endif
