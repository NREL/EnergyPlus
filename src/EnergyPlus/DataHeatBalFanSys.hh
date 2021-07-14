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

#ifndef DataHeatBalFanSys_hh_INCLUDED
#define DataHeatBalFanSys_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EPVector.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataHeatBalFanSys {

    // Data
    // -only module should be available to other modules and routines.
    // Thus, all variables in this module must be PUBLIC.

    // MODULE PARAMETER DEFINITIONS:
    constexpr bool UseSimpleAirFlow = true;
    constexpr Real64 MaxRadHeatFlux = 4000.0; // [W/m2] max limit for radiant heat flux at a surface due to HVAC equipment

    // Controls for PredictorCorrector

    enum class PredictorCorrectorCtrl
    {
        Unassigned = -1,
        GetZoneSetPoints,
        PredictStep,
        CorrectStep,
        RevertZoneTimestepHistories,
        PushZoneTimestepHistories,
        PushSystemTimestepHistories
    };

    struct ZoneComfortControlsFangerData
    {
        // Members
        int FangerType;      // Index for Fanger type
        Real64 LowPMV;       // Low PMV value
        Real64 HighPMV;      // High PMV Value
        int DualPMVErrCount; // Dual PMV setpoint error count
        int DualPMVErrIndex; // Dual PMV setpoint error index

        // Default Constructor
        ZoneComfortControlsFangerData() : FangerType(0), LowPMV(0.0), HighPMV(0.0), DualPMVErrCount(0), DualPMVErrIndex(0)
        {
        }
    };

} // namespace DataHeatBalFanSys

struct HeatBalFanSysData : BaseGlobalStruct
{
    Array1D<Real64> SumConvHTRadSys;         // Sum of convection to zone air from hi temp radiant heaters
    Array1D<Real64> SumLatentHTRadSys;       // Sum of latent gains from hi temp radiant heaters
    Array1D<Real64> SumConvPool;             // Sum of convection to zone air from pools
    Array1D<Real64> SumLatentPool;           // Sum of latent gains from pools
    Array1D<Real64> QHTRadSysToPerson;       // Sum of radiant gains to people from hi temp radiant heaters
    Array1D<Real64> QHWBaseboardToPerson;    // Sum of radiant gains to people from hot water baseboard heaters
    Array1D<Real64> QSteamBaseboardToPerson; // Sum of radiant gains to people from steam baseboard heaters
    Array1D<Real64> QElecBaseboardToPerson;  // Sum of radiant gains to people from electric baseboard heaters
    Array1D<Real64> QCoolingPanelToPerson;   // Sum of radiant losses to people from cooling panels
    // Zone air drybulb conditions variables
    Array1D<Real64> ZTAV;         // Zone Air Temperature Averaged over the Zone Time step
    Array1D<Real64> MAT;          // MEAN AIR TEMPERATURE (C)
    Array1D<Real64> TempTstatAir; // temperature of air near the thermo stat
    Array1D<Real64> ZT;           // Zone Air Temperature Averaged over the System Time Increment
    Array1D<Real64> XMAT;         // TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE
    Array1D<Real64> XM2T;
    Array1D<Real64> XM3T;
    Array1D<Real64> XM4T;
    Array1D<Real64> DSXMAT; // Down Stepped MAT history storage
    Array1D<Real64> DSXM2T; // Down Stepped MAT history storage
    Array1D<Real64> DSXM3T; // Down Stepped MAT history storage
    Array1D<Real64> DSXM4T; // Down Stepped MAT history storage
    Array1D<Real64> XMPT;   // Zone air temperature at previous time step

    Array1D<Real64> ZTAVComf; // Zone Air Temperature Averaged over the Zone Time step used
    // in thermal comfort models (currently Fang model only)
    Array1D<Real64> ZoneAirHumRatAvgComf; // AIR Humidity Ratio averaged over the zone time
    // step used in thermal comfort models (currently Fang model only)

    // Zone Air moisture conditions variables
    Array1D<Real64> ZoneAirHumRatAvg;  // AIR Humidity Ratio averaged over the zone time step
    Array1D<Real64> ZoneAirHumRat;     // AIR Humidity Ratio
    Array1D<Real64> WZoneTimeMinus1;   // Humidity ratio history terms for 3rd order derivative
    Array1D<Real64> WZoneTimeMinus2;   // Time Minus 2 Zone Time Steps Term
    Array1D<Real64> WZoneTimeMinus3;   // Time Minus 3 Zone Time Steps Term
    Array1D<Real64> WZoneTimeMinus4;   // Time Minus 4 Zone Time Steps Term
    Array1D<Real64> DSWZoneTimeMinus1; // DownStepped Humidity ratio history terms for 3rd order derivative
    Array1D<Real64> DSWZoneTimeMinus2; // DownStepped Time Minus 2 Zone Time Steps Term
    Array1D<Real64> DSWZoneTimeMinus3; // DownStepped Time Minus 3 Zone Time Steps Term
    Array1D<Real64> DSWZoneTimeMinus4; // DownStepped Time Minus 4 Zone Time Steps Term
    Array1D<Real64> WZoneTimeMinusP;   // Humidity ratio history terms at previous time step

    Array1D<Real64> ZoneAirHumRatTemp;   // Temp zone air humidity ratio at time plus 1
    Array1D<Real64> WZoneTimeMinus1Temp; // Zone air humidity ratio at previous timestep
    Array1D<Real64> WZoneTimeMinus2Temp; // Zone air humidity ratio at timestep T-2
    Array1D<Real64> WZoneTimeMinus3Temp; // Zone air humidity ratio at timestep T-3
    Array1D<Real64> ZoneAirHumRatOld;    // Last Time Steps Zone AIR Humidity Ratio

    Array1D<Real64> MCPI;                       // INFILTRATION MASS FLOW * AIR SPECIFIC HEAT
    Array1D<Real64> MCPTI;                      // INFILTRATION MASS FLOW * AIR CP * AIR TEMPERATURE
    Array1D<Real64> MCPV;                       // VENTILATION MASS FLOW * AIR SPECIFIC HEAT
    Array1D<Real64> MCPTV;                      // VENTILATION MASS FLOW * AIR CP * AIR TEMPERATURE
    Array1D<Real64> MCPM;                       // Mixing MASS FLOW * AIR SPECIFIC HEAT
    Array1D<Real64> MCPTM;                      // Mixing MASS FLOW * AIR CP * AIR TEMPERATURE
    Array1D<Real64> MCPE;                       // EARTHTUBE MASS FLOW * AIR SPECIFIC HEAT
    Array1D<Real64> EAMFL;                      // OUTDOOR AIR MASS FLOW for EarthTube
    Array1D<Real64> EAMFLxHumRat;               // OUTDOOR AIR MASS FLOW * Humidity Ratio for EarthTube (water vapor mass flow)
    Array1D<Real64> MCPTE;                      // EARTHTUBE MASS FLOW * AIR CP * AIR TEMPERATURE
    Array1D<Real64> MCPC;                       // COOLTOWER MASS FLOW * AIR SPECIFIC HEAT
    Array1D<Real64> CTMFL;                      // OUTDOOR AIR MASS FLOW for cooltower
    Array1D<Real64> MCPTC;                      // COOLTOWER MASS FLOW * AIR CP * AIR TEMPERATURE
    Array1D<Real64> ThermChimAMFL;              // OUTDOOR AIR MASS FLOW for THERMALCHIMNEY
    Array1D<Real64> MCPTThermChim;              // THERMALCHIMNEY MASS FLOW * AIR SPECIFIC HEAT
    Array1D<Real64> MCPThermChim;               // THERMALCHIMNEY MASS FLOW * AIR CP * AIR TEMPERATURE
    Array1D<Real64> ZoneLatentGain;             // Latent Energy from each Zone (People, equipment)
    Array1D<Real64> ZoneLatentGainExceptPeople; // Added for hybrid model -- Latent Energy from each Zone (equipment)
    Array1D<Real64> OAMFL;                      // OUTDOOR AIR MASS FLOW (kg/s) for infiltration
    Array1D<Real64> VAMFL;                      // OUTDOOR AIR MASS FLOW (kg/s) for ventilation
    Array1D<Real64> NonAirSystemResponse;       // Convective heat addition rate from non forced air
    // equipment such as baseboards plus heat from lights to
    Array1D<Real64> SysDepZoneLoads; // Convective heat addition or subtraction rate from sources that
    // depend on what is happening with the HVAC system. Such as:
    // heat gain from lights to return air when return flow = 0; heat gain
    // from air flow windows to return air when return air flow = 0;
    // and heat removed by return air from refrigeration cases when
    // return air flow = 0.
    Array1D<Real64> SysDepZoneLoadsLagged; // SysDepZoneLoads saved to be added to zone heat balance next
    // HVAC time step
    Array1D<Real64> MDotCPOA; // Airbalance MASS FLOW * AIR SPECIFIC HEAT used at Air Balance Method = Quadrature in the ZoneAirBalance:OutdoorAir
    Array1D<Real64> MDotOA;   // Airbalance MASS FLOW rate used at Air Balance Method = Quadrature in the ZoneAirBalance:OutdoorAir

    Array1D<Real64> MixingMassFlowZone;    // Mixing MASS FLOW (kg/s)
    Array1D<Real64> MixingMassFlowXHumRat; // Mixing MASS FLOW * Humidity Ratio

    Array1D_bool ZoneMassBalanceFlag;  // zone mass flow balance flag
    Array1D_bool ZoneInfiltrationFlag; // Zone Infiltration flag
    Array1D_int ZoneReOrder;           // zone number reordered for zone mass balance

    // REAL Variables for the Heat Balance Simulation

    Array1D<Real64> QRadSysSource;     // Current source/sink for a particular surface (radiant sys)
    Array1D<Real64> TCondFDSourceNode; // Temperature of source/sink location in surface from CondFD algo
    Array1D<Real64> QPVSysSource;      // Current source/sink for a surface (integrated PV sys)

    Array1D<Real64> CTFTsrcConstPart; // Constant Outside Portion of the CTF calculation of
    // temperature at source
    Array1D<Real64> CTFTuserConstPart; // Constant Outside Portion of the CTF calculation of
    // temperature at the user specified location
    Array1D<Real64> QHTRadSysSurf; // Current radiant heat flux at a surface due to the presence
    // of high temperature radiant heaters
    Array1D<Real64> QHWBaseboardSurf; // Current radiant heat flux at a surface due to the presence
    // of hot water baseboard heaters
    Array1D<Real64> QSteamBaseboardSurf; // Current radiant heat flux at a surface due to the presence
    // of steam baseboard heaters
    Array1D<Real64> QElecBaseboardSurf; // Current radiant heat flux at a surface due to the presence
    // of electric baseboard heaters
    Array1D<Real64> QCoolingPanelSurf; // Current radiant heat flux at a surface due to the presence
    // of simple cooling panels
    Array1D<Real64> QRadSurfAFNDuct;     // Current radiant heat flux at a surface due to radiation from AFN ducts
    Array1D<Real64> QPoolSurfNumerator;  // Current pool heat flux impact at the surface (numerator of surface heat balance)
    Array1D<Real64> PoolHeatTransCoefs;  // Current pool heat transfer coefficients (denominator of surface heat balance)
    Array1D<Real64> RadSysTiHBConstCoef; // Inside heat balance coefficient that is constant
    Array1D<Real64> RadSysTiHBToutCoef;  // Inside heat balance coefficient that modifies Toutside
    Array1D<Real64> RadSysTiHBQsrcCoef;  // Inside heat balance coefficient that modifies source/sink
    Array1D<Real64> RadSysToHBConstCoef; // Outside heat balance coefficient that is constant
    Array1D<Real64> RadSysToHBTinCoef;   // Outside heat balance coefficient that modifies Toutside
    Array1D<Real64> RadSysToHBQsrcCoef;  // Outside heat balance coefficient that modifies source/sink

    // Moisture variables to carry info from HB to the Zone Temp Predictor-Corrector for Fan System
    Array1D<Real64> SumHmAW;   // SUM OF ZONE AREA*Moist CONVECTION COEFF*INSIDE Humidity Ratio
    Array1D<Real64> SumHmARa;  // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air
    Array1D<Real64> SumHmARaW; // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air* Inside Humidity Ration
    Array1D<Real64> SumHmARaZ;

    Array1D<Real64> TempZoneThermostatSetPoint;
    Array1D<Real64> AdapComfortCoolingSetPoint;
    Array1D<Real64> ZoneThermostatSetPointHi;
    Array1D<Real64> ZoneThermostatSetPointLo;
    Array1D<Real64> ZoneThermostatSetPointHiAver;
    Array1D<Real64> ZoneThermostatSetPointLoAver;

    Array1D<Real64> LoadCorrectionFactor; // PH 3/3/04

    Array1D<Real64> AIRRAT; // "air power capacity"  PH 3/5/04
    Array1D<Real64> ZTM1;   // zone air temperature at previous timestep
    Array1D<Real64> ZTM2;   // zone air temperature at timestep T-2
    Array1D<Real64> ZTM3;   // zone air temperature at previous T-3
    // Hybrid Modeling
    Array1D<Real64> PreviousMeasuredZT1;     // Measured zone air temperature at previous timestep1
    Array1D<Real64> PreviousMeasuredZT2;     // Measured zone air temperature at previous timestep2
    Array1D<Real64> PreviousMeasuredZT3;     // Measured zone air temperature at previous timestep3
    Array1D<Real64> PreviousMeasuredHumRat1; // Hybrid model zone humidity ratio at previous timestep
    Array1D<Real64> PreviousMeasuredHumRat2; // Hybrid model zone humidity ratio at previous timestep
    Array1D<Real64> PreviousMeasuredHumRat3; // Hybrid model zone humidity ratio at previous timestep
    // Exact and Euler solutions
    Array1D<Real64> ZoneTMX; // TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE in Exact and Euler method
    Array1D<Real64> ZoneTM2; // TEMPORARY ZONE TEMPERATURE at timestep t-2 in Exact and Euler method
    Array1D<Real64> ZoneT1;  // Zone temperature at the previous time step used in Exact and Euler method
    Array1D<Real64> ZoneWMX; // TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE in Exact and Euler method
    Array1D<Real64> ZoneWM2; // TEMPORARY ZONE TEMPERATURE at timestep t-2 in Exact and Euler method
    Array1D<Real64> ZoneW1;  // Zone temperature at the previous time step used in Exact and Euler method
    Array1D_int TempControlType;
    Array1D_int ComfortControlType;

    Array1D<Real64> ZoneHeatIndex;
    Array1D<Real64> ZoneHumidex;
    Array1D_int ZoneNumOcc;

    Array1D<std::vector<Real64>> ZoneHeatIndexHourBins;
    Array1D<std::vector<Real64>> ZoneHumidexHourBins;
    Array1D<std::vector<Real64>> ZoneHeatIndexOccuHourBins;
    Array1D<std::vector<Real64>> ZoneHumidexOccuHourBins;
    Array1D<std::vector<Real64>> ZoneCO2LevelHourBins;
    Array1D<std::vector<Real64>> ZoneCO2LevelOccuHourBins;
    Array1D<std::vector<Real64>> ZoneLightingLevelHourBins;
    Array1D<std::vector<Real64>> ZoneLightingLevelOccuHourBins;

    Array1D<Real64> ZoneOccPierceSET;
    Array1D<Real64> ZoneOccPierceSETLastStep;
    Array1D<std::vector<Real64>> ZoneLowSETHours;
    Array1D<std::vector<Real64>> ZoneHighSETHours;

    EPVector<DataHeatBalFanSys::ZoneComfortControlsFangerData> ZoneComfortControlsFanger;

    void clear_state() override
    {
        this->SumConvHTRadSys.deallocate();
        this->SumLatentHTRadSys.deallocate();
        this->SumConvPool.deallocate();
        this->SumLatentPool.deallocate();
        this->QHTRadSysToPerson.deallocate();
        this->QHWBaseboardToPerson.deallocate();
        this->QSteamBaseboardToPerson.deallocate();
        this->QElecBaseboardToPerson.deallocate();
        this->ZTAV.deallocate();
        this->MAT.deallocate();
        this->TempTstatAir.deallocate();
        this->ZT.deallocate();
        this->XMAT.deallocate();
        this->XM2T.deallocate();
        this->XM3T.deallocate();
        this->XM4T.deallocate();
        this->DSXMAT.deallocate();
        this->DSXM2T.deallocate();
        this->DSXM3T.deallocate();
        this->DSXM4T.deallocate();
        this->XMPT.deallocate();
        this->ZTAVComf.deallocate();
        this->ZoneAirHumRatAvgComf.deallocate();
        this->ZoneAirHumRatAvg.deallocate();
        this->ZoneAirHumRat.deallocate();
        this->WZoneTimeMinus1.deallocate();
        this->WZoneTimeMinus2.deallocate();
        this->WZoneTimeMinus3.deallocate();
        this->WZoneTimeMinus4.deallocate();
        this->DSWZoneTimeMinus1.deallocate();
        this->DSWZoneTimeMinus2.deallocate();
        this->DSWZoneTimeMinus3.deallocate();
        this->DSWZoneTimeMinus4.deallocate();
        this->WZoneTimeMinusP.deallocate();
        this->ZoneAirHumRatTemp.deallocate();
        this->WZoneTimeMinus1Temp.deallocate();
        this->WZoneTimeMinus2Temp.deallocate();
        this->WZoneTimeMinus3Temp.deallocate();
        this->ZoneAirHumRatOld.deallocate();
        this->MCPI.deallocate();
        this->MCPTI.deallocate();
        this->MCPV.deallocate();
        this->MCPTV.deallocate();
        this->MCPM.deallocate();
        this->MCPTM.deallocate();
        this->MCPE.deallocate();
        this->EAMFL.deallocate();
        this->EAMFLxHumRat.deallocate();
        this->MCPTE.deallocate();
        this->MCPC.deallocate();
        this->CTMFL.deallocate();
        this->MCPTC.deallocate();
        this->ThermChimAMFL.deallocate();
        this->MCPTThermChim.deallocate();
        this->MCPThermChim.deallocate();
        this->ZoneLatentGain.deallocate();
        this->ZoneLatentGainExceptPeople.deallocate();
        this->OAMFL.deallocate();
        this->VAMFL.deallocate();
        this->NonAirSystemResponse.deallocate();
        this->SysDepZoneLoads.deallocate();
        this->SysDepZoneLoadsLagged.deallocate();
        this->MDotCPOA.deallocate();
        this->MDotOA.deallocate();
        this->MixingMassFlowZone.deallocate();
        this->MixingMassFlowXHumRat.deallocate();
        this->ZoneMassBalanceFlag.deallocate();
        this->ZoneInfiltrationFlag.deallocate();
        this->ZoneReOrder.deallocate();
        this->QRadSysSource.deallocate();
        this->TCondFDSourceNode.deallocate();
        this->QPVSysSource.deallocate();
        this->CTFTsrcConstPart.deallocate();
        this->CTFTuserConstPart.deallocate();
        this->QHTRadSysSurf.deallocate();
        this->QHWBaseboardSurf.deallocate();
        this->QSteamBaseboardSurf.deallocate();
        this->QElecBaseboardSurf.deallocate();
        this->QPoolSurfNumerator.deallocate();
        this->QRadSurfAFNDuct.deallocate();
        this->PoolHeatTransCoefs.deallocate();
        this->RadSysTiHBConstCoef.deallocate();
        this->RadSysTiHBToutCoef.deallocate();
        this->RadSysTiHBQsrcCoef.deallocate();
        this->RadSysToHBConstCoef.deallocate();
        this->RadSysToHBTinCoef.deallocate();
        this->RadSysToHBQsrcCoef.deallocate();
        this->SumHmAW.deallocate();
        this->SumHmARa.deallocate();
        this->SumHmARaW.deallocate();
        this->TempZoneThermostatSetPoint.deallocate();
        this->AdapComfortCoolingSetPoint.deallocate();
        this->ZoneThermostatSetPointHi.deallocate();
        this->ZoneThermostatSetPointLo.deallocate();
        this->ZoneThermostatSetPointHiAver.deallocate();
        this->ZoneThermostatSetPointLoAver.deallocate();
        this->LoadCorrectionFactor.deallocate();
        this->AIRRAT.deallocate();
        this->ZTM1.deallocate();
        this->ZTM2.deallocate();
        this->ZTM3.deallocate();
        this->PreviousMeasuredZT1.deallocate();
        this->PreviousMeasuredZT2.deallocate();
        this->PreviousMeasuredZT3.deallocate();
        this->PreviousMeasuredHumRat1.deallocate();
        this->PreviousMeasuredHumRat2.deallocate();
        this->PreviousMeasuredHumRat3.deallocate();
        this->ZoneTMX.deallocate();
        this->ZoneTM2.deallocate();
        this->ZoneT1.deallocate();
        this->ZoneWMX.deallocate();
        this->ZoneWM2.deallocate();
        this->ZoneW1.deallocate();
        this->TempControlType.deallocate();
        this->ComfortControlType.deallocate();
        this->ZoneHeatIndex.deallocate();
        this->ZoneHumidex.deallocate();
        this->ZoneNumOcc.deallocate();
        ;
        this->ZoneHeatIndexHourBins.deallocate();
        this->ZoneHeatIndexOccuHourBins.deallocate();
        this->ZoneHumidexHourBins.deallocate();
        this->ZoneHumidexOccuHourBins.deallocate();
        this->ZoneOccPierceSET.deallocate();
        this->ZoneOccPierceSETLastStep.deallocate();
        this->ZoneLowSETHours.deallocate();
        this->ZoneHighSETHours.deallocate();
        this->ZoneCO2LevelHourBins.deallocate();
        this->ZoneCO2LevelOccuHourBins.deallocate();
        this->ZoneLightingLevelHourBins.deallocate();
        this->ZoneLightingLevelOccuHourBins.deallocate();
        this->ZoneComfortControlsFanger.deallocate();
    }
};

} // namespace EnergyPlus

#endif
