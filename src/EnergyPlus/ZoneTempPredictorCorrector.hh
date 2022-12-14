// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

    struct ZoneTempControl
    {
        std::string Name;          // Name of the zone
        std::string TempSchedName; // Name of the schedule which determines the zone temp setpoint
        int TempSchedIndex = 0;
        std::string HeatTempSetptSchedName;
        int HeatTempSchedIndex = 0;
        std::string CoolTempSetptSchedName;
        int CoolTempSchedIndex;
    };

    struct ZoneComfortFangerControl
    {
        std::string Name;                  // Name of the zone
        std::string PMVSchedName;          // Name of the schedule which determines the zone temp setpoint
        int PMVSchedIndex = 0;             // Index to PMV dual set point schedule
        std::string HeatPMVSetptSchedName; // Name of PMV heating set point schedule
        int HeatPMVSchedIndex = 0;         // Index to PMV heating set point schedule
        std::string CoolPMVSetptSchedName; // Name of PMV cooling set point schedule
        int CoolPMVSchedIndex;             // INdex to PMV cooling set point schedule
    };

    struct AdaptiveComfortDailySetPointSchedule
    {
        bool initialized = false;
        Array1D<Real64> ThermalComfortAdaptiveASH55_Upper_90;
        Array1D<Real64> ThermalComfortAdaptiveASH55_Upper_80;
        Array1D<Real64> ThermalComfortAdaptiveASH55_Central;
        Array1D<Real64> ThermalComfortAdaptiveCEN15251_Upper_I;
        Array1D<Real64> ThermalComfortAdaptiveCEN15251_Upper_II;
        Array1D<Real64> ThermalComfortAdaptiveCEN15251_Upper_III;
        Array1D<Real64> ThermalComfortAdaptiveCEN15251_Central;
    };

    // Functions

    void ManageZoneAirUpdates(EnergyPlusData &state,
                              DataHeatBalFanSys::PredictorCorrectorCtrl UpdateType, // Can be iGetZoneSetPoints, iPredictStep, iCorrectStep
                              Real64 &ZoneTempChange,                               // Temp change in zone air btw previous and current timestep
                              bool ShortenTimeStepSys,
                              bool UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
                              Real64 PriorTimeStep         // the old value for timestep length is passed for possible use in interpolating
    );

    void GetZoneAirSetPoints(EnergyPlusData &state);

    void InitZoneAirSetPoints(EnergyPlusData &state);

    void PredictSystemLoads(EnergyPlusData &state,
                            bool ShortenTimeStepSys,
                            bool UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
                            Real64 PriorTimeStep         // the old value for timestep length is passed for possible use in interpolating
    );

    void CalcZoneAirTempSetPoints(EnergyPlusData &state);

    void CalculateMonthlyRunningAverageDryBulb(EnergyPlusData &state, Array1D<Real64> &runningAverageASH, Array1D<Real64> &runningAverageCEN);

    void
    CalculateAdaptiveComfortSetPointSchl(EnergyPlusData &state, Array1D<Real64> const &runningAverageASH, Array1D<Real64> const &runningAverageCEN);

    void CalcPredictedSystemLoad(EnergyPlusData &state, int ZoneNum, Real64 RAFNFrac);

    void ReportSensibleLoadsZoneMultiplier(Real64 &TotalLoad,
                                           Real64 &TotalHeatLoad,
                                           Real64 &TotalCoolLoad,
                                           Real64 &SensLoadSingleZone,
                                           Real64 &SensLoadHeatSingleZone,
                                           Real64 &SensLoadCoolSingleZone,
                                           Real64 OutputHeatSP,
                                           Real64 OutputCoolSP,
                                           Real64 LoadCorrFactor,
                                           Real64 ZoneMultiplier,
                                           Real64 ZoneMultiplierList);

    void CalcPredictedHumidityRatio(EnergyPlusData &state, int ZoneNum, Real64 RAFNFrac);

    void ReportMoistLoadsZoneMultiplier(Real64 &TotalLoad,
                                        Real64 &TotalHumidLoad,
                                        Real64 &TotalDehumidLoad,
                                        Real64 &MoistLoadSingleZone,
                                        Real64 &MoistLoadHumidSingleZone,
                                        Real64 &MoistLoadDehumidSingleZone,
                                        Real64 ZoneMultiplier,
                                        Real64 ZoneMultiplierList);

    void CorrectZoneAirTemp(EnergyPlusData &state,
                            Real64 &ZoneTempChange,     // Temperature change in zone air between previous and current timestep
                            bool UseZoneTimeStepHistory // if true then use zone timestep history, if false use system time step history
    );

    void PushZoneTimestepHistories(EnergyPlusData &state);

    void PushSystemTimestepHistories(EnergyPlusData &state);

    void RevertZoneTimestepHistories(EnergyPlusData &state);

    void CorrectZoneHumRat(EnergyPlusData &state, int ZoneNum);

    void DownInterpolate4HistoryValues(Real64 OldTimeStep,
                                       Real64 NewTimeStep,
                                       Real64 &oldVal0,
                                       Real64 &oldVal1,
                                       Real64 &oldVal2,
                                       Real64 &newVal0,
                                       Real64 &newVal1,
                                       Real64 &newVal2,
                                       Real64 &newVal3, // unused 1208
                                       Real64 &newVal4  // unused 1208
    );

    void InverseModelTemperature(EnergyPlusData &state,
                                 int ZoneNum,                    // Zone number
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
                              int ZoneNum,                    // Zone number
                              Real64 &LatentGain,             // Zone sum of latent gain
                              Real64 &LatentGainExceptPeople, // Zone sum of latent gain except for people
                              Real64 &ZoneMassFlowRate,       // Zone air mass flow rate
                              Real64 &MoistureMassFlowRate,   // Zone moisture mass flow rate
                              Real64 &H2OHtOfVap,             // Heat of vaporization of air
                              Real64 &RhoAir                  // Air density
    );

    void CalcZoneSums(EnergyPlusData &state,
                      int ZoneNum,              // Zone number
                      Real64 &SumIntGain,       // Zone sum of convective internal gains
                      Real64 &SumHA,            // Zone sum of Hc*Area
                      Real64 &SumHATsurf,       // Zone sum of Hc*Area*Tsurf
                      Real64 &SumHATref,        // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
                      Real64 &SumMCp,           // Zone sum of MassFlowRate*Cp
                      Real64 &SumMCpT,          // Zone sum of MassFlowRate*Cp*T
                      Real64 &SumSysMCp,        // Zone sum of air system MassFlowRate*Cp
                      Real64 &SumSysMCpT,       // Zone sum of air system MassFlowRate*Cp*T
                      bool CorrectorFlag = true // Corrector call flag
    );

    void CalcZoneComponentLoadSums(EnergyPlusData &state,
                                   int ZoneNum,             // Zone number
                                   Real64 TempDepCoef,      // Dependent coefficient
                                   Real64 TempIndCoef,      // Independent coefficient
                                   Real64 &SumIntGains,     // Zone sum of convective internal gains
                                   Real64 &SumHADTsurfs,    // Zone sum of Hc*Area*(Tsurf - Tz)
                                   Real64 &SumMCpDTzones,   // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
                                   Real64 &SumMCpDtInfil,   // Zone sum of MassFlowRate*Cp*(Tout - Tz) transfer from outside, ventil, earth tube
                                   Real64 &SumMCpDTsystem,  // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
                                   Real64 &SumNonAirSystem, // Zone sum of non air system convective heat gains
                                   Real64 &CzdTdt,          // Zone air energy storage term.
                                   Real64 &imBalance,       // put all terms in eq. 5 on RHS , should be zero
                                   Real64 &SumEnthalpyM,    // Zone sum of phase change material melting enthlpy
                                   Real64 &SumEnthalpyH     // Zone sum of phase change material freezing enthalpy
    );

    bool VerifyThermostatInZone(EnergyPlusData &state, std::string const &ZoneName); // Zone to verify

    bool VerifyControlledZoneForThermostat(EnergyPlusData &state, std::string const &ZoneName); // Zone to verify

    void DetectOscillatingZoneTemp(EnergyPlusData &state);

    void AdjustAirSetPointsforOpTempCntrl(EnergyPlusData &state, int TempControlledZoneID, int ActualZoneNum, Real64 &ZoneAirSetPoint);

    void AdjustOperativeSetPointsforAdapComfort(EnergyPlusData &state, int TempControlledZoneID, Real64 &ZoneAirSetPoint);

    void CalcZoneAirComfortSetPoints(EnergyPlusData &state);

    void GetComfortSetPoints(EnergyPlusData &state,
                             int PeopleNum,
                             int ComfortControlNum,
                             Real64 PMVSet,
                             Real64 &Tset // drybulb setpoint temperature for a given PMV value
    );

    void AdjustCoolingSetPointforTempAndHumidityControl(EnergyPlusData &state,
                                                        int TempControlledZoneID,
                                                        int ActualZoneNum // controlled zone actual zone number
    );

    void OverrideAirSetPointsforEMSCntrl(EnergyPlusData &state);

    void FillPredefinedTableOnThermostatSetpoints(EnergyPlusData &state);

    std::tuple<Real64, int, std::string>
    temperatureAndCountInSch(EnergyPlusData &state, int scheduleIndex, bool isSummer, int dayOfWeek, int hourOfDay);

} // namespace ZoneTempPredictorCorrector

struct ZoneTempPredictorCorrectorData : BaseGlobalStruct
{
    int NumSingleTempHeatingControls = 0;
    int NumSingleTempCoolingControls = 0;
    int NumSingleTempHeatCoolControls = 0;
    int NumDualTempHeatCoolControls = 0;

    // Number of Thermal comfort control types
    int NumSingleFangerHeatingControls = 0;
    int NumSingleFangerCoolingControls = 0;
    int NumSingleFangerHeatCoolControls = 0;
    int NumDualFangerHeatCoolControls = 0;

    // Number of zone with staged controlled objects
    int NumStageCtrZone = 0;
    // Number of zone with onoff thermostat
    int NumOnOffCtrZone = 0;

    Array1D<Real64> ZoneSetPointLast;
    Array1D<Real64> TempIndZnLd;
    Array1D<Real64> TempDepZnLd;
    Array1D<Real64> ZoneAirRelHum; // Zone relative humidity in percent

    // Zone temperature history - used only for oscillation test
    Array2D<Real64> ZoneTempHist;
    Array1D<Real64> ZoneTempOscillate;
    Array1D<Real64> ZoneTempOscillateDuringOccupancy;
    Array1D<Real64> ZoneTempOscillateInDeadband;
    Real64 AnyZoneTempOscillate = 0.0;
    Real64 AnyZoneTempOscillateDuringOccupancy = 0.0;
    Real64 AnyZoneTempOscillateInDeadband = 0.0;
    Real64 AnnualAnyZoneTempOscillate = 0.0;
    Real64 AnnualAnyZoneTempOscillateDuringOccupancy = 0.0;
    Real64 AnnualAnyZoneTempOscillateInDeadband = 0.0;
    bool OscillationVariablesNeeded = false;

    bool InitZoneAirSetPointsOneTimeFlag = true;
    bool SetupOscillationOutputFlag = true;

    // Object Data
    std::unordered_set<std::string> HumidityControlZoneUniqueNames;
    EPVector<ZoneTempPredictorCorrector::ZoneTempControl> SetPointSingleHeating;
    EPVector<ZoneTempPredictorCorrector::ZoneTempControl> SetPointSingleCooling;
    EPVector<ZoneTempPredictorCorrector::ZoneTempControl> SetPointSingleHeatCool;
    EPVector<ZoneTempPredictorCorrector::ZoneTempControl> SetPointDualHeatCool;
    EPVector<ZoneTempPredictorCorrector::ZoneComfortFangerControl> SetPointSingleHeatingFanger;
    EPVector<ZoneTempPredictorCorrector::ZoneComfortFangerControl> SetPointSingleCoolingFanger;
    EPVector<ZoneTempPredictorCorrector::ZoneComfortFangerControl> SetPointSingleHeatCoolFanger;
    EPVector<ZoneTempPredictorCorrector::ZoneComfortFangerControl> SetPointDualHeatCoolFanger;
    ZoneTempPredictorCorrector::AdaptiveComfortDailySetPointSchedule AdapComfortDailySetPointSchedule;

    std::array<Real64, 7> AdapComfortSetPointSummerDesDay = {-1};

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
        *this = ZoneTempPredictorCorrectorData();
    }
};

} // namespace EnergyPlus

#endif
