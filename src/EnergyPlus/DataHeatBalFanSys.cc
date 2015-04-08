// EnergyPlus Headers
#include <DataHeatBalFanSys.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataHeatBalFanSys {

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   February 1997
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module should contains the information that is needed to pass from the
	// Heat Balance Module to the Fan Systems

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	int const UseSimpleAirFlow( 1 );
	Real64 const MaxRadHeatFlux( 4000.0 ); // [W/m2] max limit for radiant heat flux at a surface due to HVAC equipment

	// Controls for PredictorCorrector
	int const iGetZoneSetPoints( 1 );
	int const iPredictStep( 2 );
	int const iCorrectStep( 3 );
	int const iRevertZoneTimestepHistories( 4 );
	int const iPushZoneTimestepHistories( 5 );
	int const iPushSystemTimestepHistories( 6 );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	Array1D< Real64 > SumConvHTRadSys; // Sum of convection to zone air from hi temp radiant heaters
	Array1D< Real64 > SumLatentHTRadSys; // Sum of latent gains from hi temp radiant heaters
	Array1D< Real64 > SumConvPool; // Sum of convection to zone air from pools
	Array1D< Real64 > SumLatentPool; // Sum of latent gains from pools
	Array1D< Real64 > QHTRadSysToPerson; // Sum of radiant gains to people from hi temp radiant heaters
	Array1D< Real64 > QHWBaseboardToPerson; // Sum of radiant gains to people from hot water baseboard heaters
	Array1D< Real64 > QSteamBaseboardToPerson; // Sum of radiant gains to people from steam baseboard heaters
	Array1D< Real64 > QElecBaseboardToPerson; // Sum of radiant gains to people from electric baseboard heaters
	//Zone air drybulb conditions variables
	Array1D< Real64 > ZTAV; // Zone Air Temperature Averaged over the Zone Time step
	Array1D< Real64 > MAT; // MEAN AIR TEMPARATURE (C)
	Array1D< Real64 > TempTstatAir; // temperature of air near the thermo stat
	Array1D< Real64 > ZT; // Zone Air Temperature Averaged over the System Time Increment
	Array1D< Real64 > XMAT; // TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE
	Array1D< Real64 > XM2T;
	Array1D< Real64 > XM3T;
	Array1D< Real64 > XM4T;
	Array1D< Real64 > DSXMAT; // Down Stepped MAT history storage
	Array1D< Real64 > DSXM2T; // Down Stepped MAT history storage
	Array1D< Real64 > DSXM3T; // Down Stepped MAT history storage
	Array1D< Real64 > DSXM4T; // Down Stepped MAT history storage
	Array1D< Real64 > XMPT; // Zone air temperature at previous time step

	Array1D< Real64 > ZTAVComf; // Zone Air Temperature Averaged over the Zone Time step used
	// in thermal comfort models (currently Fang model only)
	Array1D< Real64 > ZoneAirHumRatAvgComf; // AIR Humidity Ratio averaged over the zone time
	// step used in thermal comfort models (currently Fang model only)

	// Zone Air moisture conditions variables
	Array1D< Real64 > ZoneAirHumRatAvg; // AIR Humidity Ratio averaged over the zone time step
	Array1D< Real64 > ZoneAirHumRat; // AIR Humidity Ratio
	Array1D< Real64 > WZoneTimeMinus1; // Humidity ratio history terms for 3rd order derivative
	Array1D< Real64 > WZoneTimeMinus2; // Time Minus 2 Zone Time Steps Term
	Array1D< Real64 > WZoneTimeMinus3; // Time Minus 3 Zone Time Steps Term
	Array1D< Real64 > WZoneTimeMinus4; // Time Minus 4 Zone Time Steps Term
	Array1D< Real64 > DSWZoneTimeMinus1; // DownStepped Humidity ratio history terms for 3rd order derivative
	Array1D< Real64 > DSWZoneTimeMinus2; // DownStepped Time Minus 2 Zone Time Steps Term
	Array1D< Real64 > DSWZoneTimeMinus3; // DownStepped Time Minus 3 Zone Time Steps Term
	Array1D< Real64 > DSWZoneTimeMinus4; // DownStepped Time Minus 4 Zone Time Steps Term
	Array1D< Real64 > WZoneTimeMinusP; // Humidity ratio history terms at previous time step

	Array1D< Real64 > ZoneAirHumRatTemp; // Temp zone air humidity ratio at time plus 1
	Array1D< Real64 > WZoneTimeMinus1Temp; // Zone air humidity ratio at previous timestep
	Array1D< Real64 > WZoneTimeMinus2Temp; // Zone air humidity ratio at timestep T-2
	Array1D< Real64 > WZoneTimeMinus3Temp; // Zone air humidity ratio at timestep T-3
	Array1D< Real64 > ZoneAirHumRatOld; // Last Time Steps Zone AIR Humidity Ratio

	Array1D< Real64 > MCPI; // INFILTRATION MASS FLOW * AIR SPECIFIC HEAT
	Array1D< Real64 > MCPTI; // INFILTRATION MASS FLOW * AIR CP * AIR TEMPERATURE
	Array1D< Real64 > MCPV; // VENTILATION MASS FLOW * AIR SPECIFIC HEAT
	Array1D< Real64 > MCPTV; // VENTILATION MASS FLOW * AIR CP * AIR TEMPERATURE
	Array1D< Real64 > MCPM; // Mixing MASS FLOW * AIR SPECIFIC HEAT
	Array1D< Real64 > MCPTM; // Mixing MASS FLOW * AIR CP * AIR TEMPERATURE
	Array1D< Real64 > MCPE; // EARTHTUBE MASS FLOW * AIR SPECIFIC HEAT
	Array1D< Real64 > EAMFL; // OUTDOOR AIR MASS FLOW for EarthTube
	Array1D< Real64 > MCPTE; // EARTHTUBE MASS FLOW * AIR CP * AIR TEMPERATURE
	Array1D< Real64 > MCPC; // COOLTOWER MASS FLOW * AIR SPECIFIC HEAT
	Array1D< Real64 > CTMFL; // OUTDOOR AIR MASS FLOW for cooltower
	Array1D< Real64 > MCPTC; // COOLTOWER MASS FLOW * AIR CP * AIR TEMPERATURE
	Array1D< Real64 > ThermChimAMFL; // OUTDOOR AIR MASS FLOW for THERMALCHIMNEY
	Array1D< Real64 > MCPTThermChim; // THERMALCHIMNEY MASS FLOW * AIR SPECIFIC HEAT
	Array1D< Real64 > MCPThermChim; // THERMALCHIMNEY MASS FLOW * AIR CP * AIR TEMPERATURE
	Array1D< Real64 > ZoneLatentGain; // Latent Energy from each Zone (People, equipment)
	Array1D< Real64 > OAMFL; // OUTDOOR AIR MASS FLOW (M**3/SEC) for infiltration
	Array1D< Real64 > VAMFL; // OUTDOOR AIR MASS FLOW (M**3/SEC) for ventilation
	Array1D< Real64 > NonAirSystemResponse; // Convective heat addition rate from non forced air
	// equipment such as baseboards plus heat from lights to
	Array1D< Real64 > SysDepZoneLoads; // Convective heat addition or subtraction rate from sources that
	// depend on what is happening with the HVAC system. Such as:
	// heat gain from lights to return air when return flow = 0; heat gain
	// from air flow windows to return air when return air flow = 0;
	// and heat removed by return air from refrigeration cases when
	// return air flow = 0.
	Array1D< Real64 > SysDepZoneLoadsLagged; // SysDepZoneLoads saved to be added to zone heat balance next
	// HVAC time step
	Array1D< Real64 > MDotCPOA; // Airbalance MASS FLOW * AIR SPECIFIC HEAT
	Array1D< Real64 > MDotOA; // Airbalance MASS FLOW rate

	Array1D< Real64 > MixingMassFlowZone; // Mixing MASS FLOW
	Array1D< Real64 > MixingMassFlowXHumRat; // Mixing MASS FLOW * Humidity Ratio

	Array1D_bool ZoneMassBalanceFlag;  // zone mass flow balance flag
	Array1D_bool ZoneInfiltrationFlag; // Zone Infiltration flag
	Array1D_bool ZoneMassBalanceRepVarFlag; // zone mass flow balance reporting flag
	Array1D_int ZoneReOrder;           // zone number reordered for zone mass balance

	//REAL Variables for the Heat Balance Simulation

	Array1D< Real64 > QRadSysSource; // Current source/sink for a particular surface (radiant sys)
	Array1D< Real64 > TCondFDSourceNode; // Temperature of sourc/sink location in surface from CondFD algo
	Array1D< Real64 > QPVSysSource; // Current source/sink for a surface (integrated PV sys)

	Array1D< Real64 > CTFTsrcConstPart; // Constant Outside Portion of the CTF calculation of
	// temperature at source
	Array1D< Real64 > QHTRadSysSurf; // Current radiant heat flux at a surface due to the presence
	// of high temperature radiant heaters
	Array1D< Real64 > QHWBaseboardSurf; // Current radiant heat flux at a surface due to the presence
	// of hot water baseboard heaters
	Array1D< Real64 > QSteamBaseboardSurf; // Current radiant heat flux at a surface due to the presence
	// of steam baseboard heaters
	Array1D< Real64 > QElecBaseboardSurf; // Current radiant heat flux at a surface due to the presence
	// of electric baseboard heaters
	Array1D< Real64 > QPoolSurfNumerator; // Current pool heat flux impact at the surface (numerator of surface heat balance)
	Array1D< Real64 > PoolHeatTransCoefs; // Current pool heat transfer coefficients (denominator of surface heat balance)
	Array1D< Real64 > RadSysTiHBConstCoef; // Inside heat balance coefficient that is constant
	Array1D< Real64 > RadSysTiHBToutCoef; // Inside heat balance coefficient that modifies Toutside
	Array1D< Real64 > RadSysTiHBQsrcCoef; // Inside heat balance coefficient that modifies source/sink
	Array1D< Real64 > RadSysToHBConstCoef; // Outside heat balance coefficient that is constant
	Array1D< Real64 > RadSysToHBTinCoef; // Outside heat balance coefficient that modifies Toutside
	Array1D< Real64 > RadSysToHBQsrcCoef; // Outside heat balance coefficient that modifies source/sink

	//Moisture variables to carry info from HB to the Zone Temp Predictor-Corrector for Fan System
	Array1D< Real64 > SumHmAW; // SUM OF ZONE AREA*Moist CONVECTION COEFF*INSIDE Humidity Ratio
	Array1D< Real64 > SumHmARa; // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air
	Array1D< Real64 > SumHmARaW; // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air* Inside Humidity Ration

	Array1D< Real64 > TempZoneThermostatSetPoint;
	Array1D< Real64 > ZoneThermostatSetPointHi;
	Array1D< Real64 > ZoneThermostatSetPointLo;

	Array1D< Real64 > LoadCorrectionFactor; // PH 3/3/04

	Array1D_bool CrossMixingFlag; // TRUE when a zone is mixing

	Array1D< Real64 > AIRRAT; // "air power capacity"  PH 3/5/04
	Array1D< Real64 > ZTM1; // zone air temperature at previous timestep
	Array1D< Real64 > ZTM2; // zone air temperature at timestep T-2
	Array1D< Real64 > ZTM3; // zone air temperature at previous T-3
	// Exact and Euler solutions
	Array1D< Real64 > ZoneTMX; // TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE in Exact and Euler method
	Array1D< Real64 > ZoneTM2; // TEMPORARY ZONE TEMPERATURE at timestep t-2 in Exact and Euler method
	Array1D< Real64 > ZoneT1; // Zone temperature at the previous time step used in Exact and Euler method
	Array1D< Real64 > ZoneWMX; // TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE in Exact and Euler method
	Array1D< Real64 > ZoneWM2; // TEMPORARY ZONE TEMPERATURE at timestep t-2 in Exact and Euler method
	Array1D< Real64 > ZoneW1; // Zone temperature at the previous time step used in Exact and Euler method

	Real64 ZoneVolCapMultpSens; // This is a multiplier used on the zone volume to make the capacitance more realistic
	// for the calculation of the zone temp in the predictor and corrector step
	Real64 ZoneVolCapMultpMoist; // This is a multiplier used on the zone volume to make the capacitance more realistic
	// for the calculation of the zone humidity ratio in the predictor and corrector step
	Real64 ZoneVolCapMultpCO2; // This is a multiplier used on the zone volume to make the capacitance more realistic
	// for the calculation of the zone CO2 concentration in the predictor and corrector step
	Real64 ZoneVolCapMultpGenContam; // This is a multiplier used on the zone volume to make the capacitance more realistic
	// for the calculation of the zone generic contaminant concentration in the predictor
	// and corrector step

	Array1D_int TempControlType;
	Array1D_int ComfortControlType;

	// Object Data
	Array1D< ZoneComfortControlsFangerData > ZoneComfortControlsFanger;

	//     NOTICE
	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.
	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.
	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.
	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // DataHeatBalFanSys

} // EnergyPlus
