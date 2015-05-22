#ifndef DataHeatBalFanSys_hh_INCLUDED
#define DataHeatBalFanSys_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataHeatBalFanSys {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const UseSimpleAirFlow;
	extern Real64 const MaxRadHeatFlux; // [W/m2] max limit for radiant heat flux at a surface due to HVAC equipment

	// Controls for PredictorCorrector
	extern int const iGetZoneSetPoints;
	extern int const iPredictStep;
	extern int const iCorrectStep;
	extern int const iRevertZoneTimestepHistories;
	extern int const iPushZoneTimestepHistories;
	extern int const iPushSystemTimestepHistories;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D< Real64 > SumConvHTRadSys; // Sum of convection to zone air from hi temp radiant heaters
	extern Array1D< Real64 > SumLatentHTRadSys; // Sum of latent gains from hi temp radiant heaters
	extern Array1D< Real64 > SumConvPool; // Sum of convection to zone air from pools
	extern Array1D< Real64 > SumLatentPool; // Sum of latent gains from pools
	extern Array1D< Real64 > QHTRadSysToPerson; // Sum of radiant gains to people from hi temp radiant heaters
	extern Array1D< Real64 > QHWBaseboardToPerson; // Sum of radiant gains to people from hot water baseboard heaters
	extern Array1D< Real64 > QSteamBaseboardToPerson; // Sum of radiant gains to people from steam baseboard heaters
	extern Array1D< Real64 > QElecBaseboardToPerson; // Sum of radiant gains to people from electric baseboard heaters
	//Zone air drybulb conditions variables
	extern Array1D< Real64 > ZTAV; // Zone Air Temperature Averaged over the Zone Time step
	extern Array1D< Real64 > MAT; // MEAN AIR TEMPARATURE (C)
	extern Array1D< Real64 > TempTstatAir; // temperature of air near the thermo stat
	extern Array1D< Real64 > ZT; // Zone Air Temperature Averaged over the System Time Increment
	extern Array1D< Real64 > XMAT; // TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE
	extern Array1D< Real64 > XM2T;
	extern Array1D< Real64 > XM3T;
	extern Array1D< Real64 > XM4T;
	extern Array1D< Real64 > DSXMAT; // Down Stepped MAT history storage
	extern Array1D< Real64 > DSXM2T; // Down Stepped MAT history storage
	extern Array1D< Real64 > DSXM3T; // Down Stepped MAT history storage
	extern Array1D< Real64 > DSXM4T; // Down Stepped MAT history storage
	extern Array1D< Real64 > XMPT; // Zone air temperature at previous time step

	extern Array1D< Real64 > ZTAVComf; // Zone Air Temperature Averaged over the Zone Time step used
	// in thermal comfort models (currently Fang model only)
	extern Array1D< Real64 > ZoneAirHumRatAvgComf; // AIR Humidity Ratio averaged over the zone time
	// step used in thermal comfort models (currently Fang model only)

	// Zone Air moisture conditions variables
	extern Array1D< Real64 > ZoneAirHumRatAvg; // AIR Humidity Ratio averaged over the zone time step
	extern Array1D< Real64 > ZoneAirHumRat; // AIR Humidity Ratio
	extern Array1D< Real64 > WZoneTimeMinus1; // Humidity ratio history terms for 3rd order derivative
	extern Array1D< Real64 > WZoneTimeMinus2; // Time Minus 2 Zone Time Steps Term
	extern Array1D< Real64 > WZoneTimeMinus3; // Time Minus 3 Zone Time Steps Term
	extern Array1D< Real64 > WZoneTimeMinus4; // Time Minus 4 Zone Time Steps Term
	extern Array1D< Real64 > DSWZoneTimeMinus1; // DownStepped Humidity ratio history terms for 3rd order derivative
	extern Array1D< Real64 > DSWZoneTimeMinus2; // DownStepped Time Minus 2 Zone Time Steps Term
	extern Array1D< Real64 > DSWZoneTimeMinus3; // DownStepped Time Minus 3 Zone Time Steps Term
	extern Array1D< Real64 > DSWZoneTimeMinus4; // DownStepped Time Minus 4 Zone Time Steps Term
	extern Array1D< Real64 > WZoneTimeMinusP; // Humidity ratio history terms at previous time step

	extern Array1D< Real64 > ZoneAirHumRatTemp; // Temp zone air humidity ratio at time plus 1
	extern Array1D< Real64 > WZoneTimeMinus1Temp; // Zone air humidity ratio at previous timestep
	extern Array1D< Real64 > WZoneTimeMinus2Temp; // Zone air humidity ratio at timestep T-2
	extern Array1D< Real64 > WZoneTimeMinus3Temp; // Zone air humidity ratio at timestep T-3
	extern Array1D< Real64 > ZoneAirHumRatOld; // Last Time Steps Zone AIR Humidity Ratio

	extern Array1D< Real64 > MCPI; // INFILTRATION MASS FLOW * AIR SPECIFIC HEAT
	extern Array1D< Real64 > MCPTI; // INFILTRATION MASS FLOW * AIR CP * AIR TEMPERATURE
	extern Array1D< Real64 > MCPV; // VENTILATION MASS FLOW * AIR SPECIFIC HEAT
	extern Array1D< Real64 > MCPTV; // VENTILATION MASS FLOW * AIR CP * AIR TEMPERATURE
	extern Array1D< Real64 > MCPM; // Mixing MASS FLOW * AIR SPECIFIC HEAT
	extern Array1D< Real64 > MCPTM; // Mixing MASS FLOW * AIR CP * AIR TEMPERATURE
	extern Array1D< Real64 > MCPE; // EARTHTUBE MASS FLOW * AIR SPECIFIC HEAT
	extern Array1D< Real64 > EAMFL; // OUTDOOR AIR MASS FLOW for EarthTube
	extern Array1D< Real64 > MCPTE; // EARTHTUBE MASS FLOW * AIR CP * AIR TEMPERATURE
	extern Array1D< Real64 > MCPC; // COOLTOWER MASS FLOW * AIR SPECIFIC HEAT
	extern Array1D< Real64 > CTMFL; // OUTDOOR AIR MASS FLOW for cooltower
	extern Array1D< Real64 > MCPTC; // COOLTOWER MASS FLOW * AIR CP * AIR TEMPERATURE
	extern Array1D< Real64 > ThermChimAMFL; // OUTDOOR AIR MASS FLOW for THERMALCHIMNEY
	extern Array1D< Real64 > MCPTThermChim; // THERMALCHIMNEY MASS FLOW * AIR SPECIFIC HEAT
	extern Array1D< Real64 > MCPThermChim; // THERMALCHIMNEY MASS FLOW * AIR CP * AIR TEMPERATURE
	extern Array1D< Real64 > ZoneLatentGain; // Latent Energy from each Zone (People, equipment)
	extern Array1D< Real64 > OAMFL; // OUTDOOR AIR MASS FLOW (M**3/SEC) for infiltration
	extern Array1D< Real64 > VAMFL; // OUTDOOR AIR MASS FLOW (M**3/SEC) for ventilation
	extern Array1D< Real64 > NonAirSystemResponse; // Convective heat addition rate from non forced air
	// equipment such as baseboards plus heat from lights to
	extern Array1D< Real64 > SysDepZoneLoads; // Convective heat addition or subtraction rate from sources that
	// depend on what is happening with the HVAC system. Such as:
	// heat gain from lights to return air when return flow = 0; heat gain
	// from air flow windows to return air when return air flow = 0;
	// and heat removed by return air from refrigeration cases when
	// return air flow = 0.
	extern Array1D< Real64 > SysDepZoneLoadsLagged; // SysDepZoneLoads saved to be added to zone heat balance next
	// HVAC time step
	extern Array1D< Real64 > MDotCPOA; // Airbalance MASS FLOW * AIR SPECIFIC HEAT
	extern Array1D< Real64 > MDotOA; // Airbalance MASS FLOW rate

	extern Array1D< Real64 > MixingMassFlowZone; // Mixing MASS FLOW
	extern Array1D< Real64 > MixingMassFlowXHumRat; // Mixing MASS FLOW * Humidity Ratio

	extern Array1D_bool ZoneMassBalanceFlag;  // zone mass flow balance flag
	extern Array1D_bool ZoneInfiltrationFlag; // Zone Infiltration flag
	extern Array1D_bool ZoneMassBalanceRepVarFlag; // zone mass flow balance reporting flag
	extern Array1D_int ZoneReOrder;           // zone number reordered for zone mass balance

	//REAL Variables for the Heat Balance Simulation

	extern Array1D< Real64 > QRadSysSource; // Current source/sink for a particular surface (radiant sys)
	extern Array1D< Real64 > TCondFDSourceNode; // Temperature of sourc/sink location in surface from CondFD algo
	extern Array1D< Real64 > QPVSysSource; // Current source/sink for a surface (integrated PV sys)

	extern Array1D< Real64 > CTFTsrcConstPart; // Constant Outside Portion of the CTF calculation of
	// temperature at source
	extern Array1D< Real64 > QHTRadSysSurf; // Current radiant heat flux at a surface due to the presence
	// of high temperature radiant heaters
	extern Array1D< Real64 > QHWBaseboardSurf; // Current radiant heat flux at a surface due to the presence
	// of hot water baseboard heaters
	extern Array1D< Real64 > QSteamBaseboardSurf; // Current radiant heat flux at a surface due to the presence
	// of steam baseboard heaters
	extern Array1D< Real64 > QElecBaseboardSurf; // Current radiant heat flux at a surface due to the presence
	// of electric baseboard heaters
	extern Array1D< Real64 > QPoolSurfNumerator; // Current pool heat flux impact at the surface (numerator of surface heat balance)
	extern Array1D< Real64 > PoolHeatTransCoefs; // Current pool heat transfer coefficients (denominator of surface heat balance)
	extern Array1D< Real64 > RadSysTiHBConstCoef; // Inside heat balance coefficient that is constant
	extern Array1D< Real64 > RadSysTiHBToutCoef; // Inside heat balance coefficient that modifies Toutside
	extern Array1D< Real64 > RadSysTiHBQsrcCoef; // Inside heat balance coefficient that modifies source/sink
	extern Array1D< Real64 > RadSysToHBConstCoef; // Outside heat balance coefficient that is constant
	extern Array1D< Real64 > RadSysToHBTinCoef; // Outside heat balance coefficient that modifies Toutside
	extern Array1D< Real64 > RadSysToHBQsrcCoef; // Outside heat balance coefficient that modifies source/sink

	//Moisture variables to carry info from HB to the Zone Temp Predictor-Corrector for Fan System
	extern Array1D< Real64 > SumHmAW; // SUM OF ZONE AREA*Moist CONVECTION COEFF*INSIDE Humidity Ratio
	extern Array1D< Real64 > SumHmARa; // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air
	extern Array1D< Real64 > SumHmARaW; // SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air* Inside Humidity Ration

	extern Array1D< Real64 > TempZoneThermostatSetPoint;
	extern Array1D< Real64 > ZoneThermostatSetPointHi;
	extern Array1D< Real64 > ZoneThermostatSetPointLo;

	extern Array1D< Real64 > LoadCorrectionFactor; // PH 3/3/04

	extern Array1D_bool CrossMixingFlag; // TRUE when a zone is mixing

	extern Array1D< Real64 > AIRRAT; // "air power capacity"  PH 3/5/04
	extern Array1D< Real64 > ZTM1; // zone air temperature at previous timestep
	extern Array1D< Real64 > ZTM2; // zone air temperature at timestep T-2
	extern Array1D< Real64 > ZTM3; // zone air temperature at previous T-3
	// Exact and Euler solutions
	extern Array1D< Real64 > ZoneTMX; // TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE in Exact and Euler method
	extern Array1D< Real64 > ZoneTM2; // TEMPORARY ZONE TEMPERATURE at timestep t-2 in Exact and Euler method
	extern Array1D< Real64 > ZoneT1; // Zone temperature at the previous time step used in Exact and Euler method
	extern Array1D< Real64 > ZoneWMX; // TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE in Exact and Euler method
	extern Array1D< Real64 > ZoneWM2; // TEMPORARY ZONE TEMPERATURE at timestep t-2 in Exact and Euler method
	extern Array1D< Real64 > ZoneW1; // Zone temperature at the previous time step used in Exact and Euler method

	extern Real64 ZoneVolCapMultpSens; // This is a multiplier used on the zone volume to make the capacitance more realistic
	// for the calculation of the zone temp in the predictor and corrector step
	extern Real64 ZoneVolCapMultpMoist; // This is a multiplier used on the zone volume to make the capacitance more realistic
	// for the calculation of the zone humidity ratio in the predictor and corrector step
	extern Real64 ZoneVolCapMultpCO2; // This is a multiplier used on the zone volume to make the capacitance more realistic
	// for the calculation of the zone CO2 concentration in the predictor and corrector step
	extern Real64 ZoneVolCapMultpGenContam; // This is a multiplier used on the zone volume to make the capacitance more realistic
	// for the calculation of the zone generic contaminant concentration in the predictor
	// and corrector step

	extern Array1D_int TempControlType;
	extern Array1D_int ComfortControlType;

	// Types

	struct ZoneComfortControlsFangerData
	{
		// Members
		int FangerType; // Index for Fanger type
		Real64 LowPMV; // Low PMV value
		Real64 HighPMV; // High PMV Value
		int DualPMVErrCount; // Dual PMV setpoint error count
		int DualPMVErrIndex; // Dual PMV setpoint error index

		// Default Constructor
		ZoneComfortControlsFangerData() :
			FangerType( 0 ),
			LowPMV( 0.0 ),
			HighPMV( 0.0 ),
			DualPMVErrCount( 0 ),
			DualPMVErrIndex( 0 )
		{}

		// Member Constructor
		ZoneComfortControlsFangerData(
			int const FangerType, // Index for Fanger type
			Real64 const LowPMV, // Low PMV value
			Real64 const HighPMV, // High PMV Value
			int const DualPMVErrCount, // Dual PMV setpoint error count
			int const DualPMVErrIndex // Dual PMV setpoint error index
		) :
			FangerType( FangerType ),
			LowPMV( LowPMV ),
			HighPMV( HighPMV ),
			DualPMVErrCount( DualPMVErrCount ),
			DualPMVErrIndex( DualPMVErrIndex )
		{}

	};

	// Object Data
	extern Array1D< ZoneComfortControlsFangerData > ZoneComfortControlsFanger;

} // DataHeatBalFanSys

} // EnergyPlus

#endif
