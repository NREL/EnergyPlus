/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __geothermalModelDefinitions__
#define __geothermalModelDefinitions__

#include <math.h>
#include <vector>
#include "lib_weatherfile.h"
#include "lib_physics.h"
#include "lib_powerblock.h"

#ifndef __geothermalEnums__
#define __geothermalEnums__

enum calculationBasis { NO_CALCULATION_BASIS, POWER_SALES, NUMBER_OF_WELLS };
enum conversionTypes { NO_CONVERSION_TYPE, BINARY, FLASH }; //}
enum resourceTypes { NO_RESOURCE_TYPE, HYDROTHERMAL, EGS };
enum flashTypes { NO_FLASH_SUBTYPE, SINGLE_FLASH_NO_TEMP_CONSTRAINT, SINGLE_FLASH_WITH_TEMP_CONSTRAINT, DUAL_FLASH_NO_TEMP_CONSTRAINT, DUAL_FLASH_WITH_TEMP_CONSTRAINT };
enum tempDeclineMethod { NO_TEMPERATURE_DECLINE_METHOD, ENTER_RATE, CALCULATE_RATE };
enum makeupAlgorithmType { NO_MAKEUP_ALGORITHM, MA_BINARY, MA_FLASH, MA_EGS }; //}
enum condenserTypes { NO_CONDENSER_TYPE, SURFACE, DIRECT_CONTACT };
enum ncgRemovalTypes { NO_NCG_TYPE, JET, VAC_PUMP, HYBRID };
enum wellCostCurveChoices { NO_COST_CURVE, LOW, MED, HIGH };
enum depthCalculationForEGS { NOT_CHOSEN, DEPTH, TEMPERATURE };
enum reservoirPressureChangeCalculation { NO_PC_CHOICE, ENTER_PC, SIMPLE_FRACTURE, K_AREA };
#endif
struct SGeothermal_Inputs
{
	SGeothermal_Inputs()
	{
		me_cb = NO_CALCULATION_BASIS; me_ct = NO_CONVERSION_TYPE; me_ft = NO_FLASH_SUBTYPE; me_tdm = NO_TEMPERATURE_DECLINE_METHOD;
		me_rt = NO_RESOURCE_TYPE; me_dc = NOT_CHOSEN; me_pc = NO_PC_CHOICE;
		mi_ModelChoice = -1; mb_CalculatePumpWork = true;
		mi_ProjectLifeYears = mi_MakeupCalculationsPerYear = mi_TotalMakeupCalculations = 0;
		md_DesiredSalesCapacityKW = md_NumberOfWells = md_PlantEfficiency = md_TemperatureDeclineRate = md_MaxTempDeclineC = md_TemperatureWetBulbC = 0.0;
		md_PressureAmbientPSI = md_ProductionFlowRateKgPerS = md_GFPumpEfficiency = md_PressureChangeAcrossSurfaceEquipmentPSI = md_ExcessPressureBar = 0.0;
		md_DiameterProductionWellInches = md_DiameterPumpCasingInches = md_DiameterInjectionWellInches = md_UserSpecifiedPumpWorkKW = 0.0;
		md_PotentialResourceMW = md_ResourceDepthM = md_TemperatureResourceC = md_TemperaturePlantDesignC = md_EGSThermalConductivity = md_EGSSpecificHeatConstant = 0.0;
		md_EGSRockDensity = md_ReservoirDeltaPressure = md_ReservoirWidthM = md_ReservoirHeightM = md_ReservoirPermeability = md_DistanceBetweenProductionInjectionWellsM = 0.0;
		md_WaterLossPercent = md_EGSFractureAperature = md_EGSNumberOfFractures = md_EGSFractureWidthM = md_EGSFractureAngle = 0.0;
		md_TemperatureEGSAmbientC = md_RatioInjectionToProduction = 0.0;
		md_AdditionalPressure = 1.0;
	}

	calculationBasis me_cb;									// { NO_CALCULATION_BASIS, POWER_SALES, NUMBER_OF_WELLS };
	conversionTypes me_ct;
	flashTypes me_ft;
	tempDeclineMethod me_tdm;
	resourceTypes me_rt;									// 1=Hydrothermal (default), !1=EGS 
	depthCalculationForEGS me_dc;							// { NOT_CHOSEN, DEPTH, TEMPERATURE };
	reservoirPressureChangeCalculation me_pc;				// 1=user enter pressure change, 2=SAM calculates it using simple fracture flow (for EGS resourceTypes only), 3=SAM calculates it using k*A (permeability x area)

	int mi_ModelChoice;										// -1 on initialization; 0=GETEM, 1=Power Block monthly, 2=Power Block hourly
	bool mb_CalculatePumpWork;								// true (default) = getem calculates pump work

	size_t mi_ProjectLifeYears;
	size_t mi_MakeupCalculationsPerYear;					// 12 (monthly) or 8760 (hourly)
	size_t mi_TotalMakeupCalculations;						// mi_ProjectLifeYears * mi_MakeupCalculationsPerYear

	double md_DesiredSalesCapacityKW;						// entered or calculated, linked to 'cb'
	double md_NumberOfWells;								// entered or calculated, depending on 'cb'
	double md_PlantEfficiency;								// not in GETEM - essentially the ratio of plant brine effectiveness to max possible brine effectiveness
	double md_TemperatureDeclineRate;						// '% per year, 3% is default
	double md_MaxTempDeclineC;								// degrees C, default = 30
	double md_TemperatureWetBulbC;							// degrees celcius - used in Flash brine effectiveness
	double md_PressureAmbientPSI;							// psi, default=14.7, mostly for use in calculating flash brine effectiveness, but also pump work
	double md_ProductionFlowRateKgPerS;						// 70 kilograms per second in one well (default FlowRate in GETEM)
	double md_GFPumpEfficiency;								// default=0.6 or 60%
	double md_PressureChangeAcrossSurfaceEquipmentPSI;		// default 25 psi
	double md_ExcessPressureBar;							// default 3.5 bar, [2B.Resource&Well Input].D205
	double md_DiameterProductionWellInches;					// default 10 inches
	double md_DiameterPumpCasingInches;						// default 9.925 inches
	double md_DiameterInjectionWellInches;					// default 10 inches
	double md_UserSpecifiedPumpWorkKW;
	double md_PotentialResourceMW;							// MW, default = 200 MW, determines how many times reservoir can be replaced
	double md_ResourceDepthM;								// meters, default 2000
	double md_TemperatureResourceC;							// degrees C, default 200
	double md_TemperatureEGSAmbientC;						// Note in GETEM spreadsheet says that this is only used in calculating resource temp or depth.  However, if EGS calculations are based on depth, then resource temp is based on this number, so all power calcs are based on it as well
	double md_TemperaturePlantDesignC;						// degrees C, default 225, only used for EGS
	double md_EGSThermalConductivity;						// default 259,200 Joules per m-day-C, [2B.Resource&Well Input].D240
	double md_EGSSpecificHeatConstant;						// default 950 Joules per kg-C, [2B.Resource&Well Input].D241
	double md_EGSRockDensity;								// default 2600 kg per cubic meter, [2B.Resource&Well Input].D242
	double md_ReservoirDeltaPressure;						// default 0.35 psi-h per 1000lb, [2B.Resource&Well Input].D171
	double md_ReservoirWidthM;
	double md_ReservoirHeightM;
	double md_ReservoirPermeability;
	double md_DistanceBetweenProductionInjectionWellsM;		// default 1500 m [2B.Resource&Well Input].F185
	double md_WaterLossPercent;								// default 2%
	double md_EGSFractureAperature;							// default 0.0004 m
	double md_EGSNumberOfFractures;							// default 6
	double md_EGSFractureWidthM;							// default 175 m
	double md_EGSFractureAngle;								// default 15 degrees
	double md_RatioInjectionToProduction;					// used in non-cost equation, so it needs to be an input
	double md_AdditionalPressure;							// manually enter additional psi for injection pumps


	const char * mc_WeatherFileName;
	int * mia_tou;											// time of use array
};

struct SGeothermal_Outputs
{
	SGeothermal_Outputs()
	{
		md_PumpWorkKW = md_NumberOfWells = md_FlashBrineEffectiveness = md_PressureHPFlashPSI = md_PressureLPFlashPSI = 0.0;
		md_GrossPlantOutputMW = md_PlantBrineEffectiveness = md_PressureChangeAcrossReservoir = md_AverageReservoirTemperatureF = 0;
		md_PumpDepthFt = md_PumpHorsePower = md_BottomHolePressure = 0;
		maf_ReplacementsByYear = maf_monthly_resource_temp = maf_monthly_power = maf_monthly_energy = maf_timestep_resource_temp = NULL;
		maf_timestep_power = maf_timestep_test_values = maf_timestep_pressure = maf_timestep_dry_bulb = maf_timestep_wet_bulb = NULL;
		mb_BrineEffectivenessCalculated = mb_FlashPressuresCalculated = false;
		maf_hourly_power = NULL;

	}

	//Following list of variables used as inputs in cmod_geothermal_costs.cpp for calculating direct geothermal plant cost:
	double md_NumberOfWells;
	double md_PumpWorkKW;
	double eff_secondlaw;				//Overall Plant 2nd Law Efficiency 
	double qRejectedTotal;				//Used in calculating Cooling Tower Cost - Flash Plant Type
	double condenser_q;					//Condenser heat rejected - used in calculating Surface type condenser cost in cmod_geothermal_costs
	double v_stage_1;					//Vacuum Stage 1 Pump Power
	double v_stage_2;
	double v_stage_3;
	double GF_flowrate;					//GF Flow Rate Total
	double qRejectByStage_1;			//Used in NCG Condenser Cost Calculation 
	double qRejectByStage_2;
	double qRejectByStage_3;
	double ncg_condensate_pump;			//For calculating ncg pump cost
	double cw_pump_work;				//For calculating ncg pump cost
	double pressure_ratio_1;			//Suction steam ratio used in calculation of NCG Ejector Cost
	double pressure_ratio_2;
	double pressure_ratio_3;
	double condensate_pump_power;		//kW
	double cwflow;						// lb/h
	double cw_pump_head;				//ft
	double flash_temperature;			//Storing Value of HP Flash Temperature for Calculating Flash Vessel in cmod_geothermal_costs
	double flash_temperature_lp;		//Storing Value of LP Flash Temperature for Calculating Flash Vessel in cmod_geothermal_costs
	double spec_vol, spec_vol_lp;		//HP Specific Volume & LP Specific Volume used in Flash Vessel Cost Calculation
	double getX_hp, getX_lp;
	double flash_count;
	double max_secondlaw;				//Max 2nd Law efficiency


// single values used in calculations, some also used in UI
	bool mb_BrineEffectivenessCalculated;
	double md_FlashBrineEffectiveness;

	bool mb_FlashPressuresCalculated;
	double md_PressureHPFlashPSI; // D29, D64
	double md_PressureLPFlashPSI; // D30, D65

	// only for use in the interface to show 'calculated' values
	double md_PlantBrineEffectiveness;
	double md_GrossPlantOutputMW;	//double GetGrossPlantOutputMW(void) { return this->PlantOutputKW()/1000; }
	double md_PumpDepthFt;
	double md_PumpHorsePower;
	double md_PressureChangeAcrossReservoir; //double GetPressureChangeAcrossReservoir(void) { return moPPC.GetPressureChangeAcrossReservoir(); }
	double md_AverageReservoirTemperatureF; //double GetAverageReservoirTemperatureUsedF(void) { return moPPC.GetReservoirTemperatureF(); }
	double md_BottomHolePressure; //double GetBottomHolePressure(void) { return moPPC.GetBottomHolePressure(); }

	// output arrays
	double * maf_ReplacementsByYear;			// array of ones and zero's over time, ones representing years where reservoirs are replaced
	double * maf_monthly_resource_temp;
	double * maf_monthly_power;				// monthly values, even if timestep is hourly
	double * maf_monthly_energy;
	double * maf_timestep_resource_temp;
	double * maf_timestep_power;				// could be hourly or monthly, depending on timestep
	double * maf_timestep_test_values;
	double * maf_timestep_pressure;
	double * maf_timestep_dry_bulb;
	double * maf_timestep_wet_bulb;
	double * maf_hourly_power;				// hourly values even if the timestep is monthly
};

//******************************************************************************************************************************************************************************
//******************************************************************************************************************************************************************************
// Declaration of CGeothermalAnalyzer 
//******************************************************************************************************************************************************************************
//******************************************************************************************************************************************************************************
class CGeothermalAnalyzer
{
public:
	CGeothermalAnalyzer(const SGeothermal_Inputs& gti, SGeothermal_Outputs& gto);
	CGeothermalAnalyzer(const SPowerBlockParameters& pbp, SPowerBlockInputs& pbi, const SGeothermal_Inputs& gti, SGeothermal_Outputs& gto);
	~CGeothermalAnalyzer();


	bool RunAnalysis(bool(*update_function)(float, void*), void *user_data);
	bool InterfaceOutputsFilled(void);
	std::string error() { return ms_ErrorString; }


private:
	// objects
	SGeothermal_Outputs* mp_geo_out;
	SGeothermal_Inputs mo_geo_in;
	SPowerBlockParameters mo_pb_p;
	SPowerBlockInputs mo_pb_in;
	CPowerBlock_Type224 mo_PowerBlock;

	// variables
	std::string ms_ErrorString;
	float mf_LastIntervalDone; // used to display "% done" to user
	weatherfile m_wFile;
	weather_header m_hdr;
	weather_record m_wf;
	bool mb_WeatherFileOpen;
	long ml_ReadCount;  // resource file reads through the year, 1 to 8760
	long ml_HourCount;	// hour of analysis (zero to yearsX8760); used to tell the Power Block how many seconds have passed.
	makeupAlgorithmType me_makeup; // { NO_MAKEUP_ALGORITHM, MA_BINARY, MA_FLASH, MA_EGS }
	int mi_ReservoirReplacements;	// how many times the reservoir has been 'replaced' (holes redrilled)
	double md_WorkingTemperatureC; // current working temp of the fluid coming out of the ground
	double md_LastProductionTemperatureC; // store the last temperature before calculating new one
	double md_TimeOfLastReservoirReplacement; // for EGS calcs


	// functions
	void init(void); // code common to both constructors
	bool IsHourly(void);
	double PlantGrossPowerkW(void);
	double MaxSecondLawEfficiency(void);
	double FractionOfMaxEfficiency(void);
	bool CanReplaceReservoir(double dTimePassedInYears);
	void CalculateNewTemperature(double dElapsedTimeInYears);


	double GetPumpWorkKW(void);
	double NumberOfReservoirs(void);
	double CalculatePumpWorkInKW(double flowLbPerHr, double pumpHeadFt);
	double GetPumpWorkWattHrPerLb(void);
	double GetCalculatedPumpDepthInFeet(void); // only used in pumpHeadFt
	double pumpHeadFt(void);

	void ReplaceReservoir(double dElapsedTimeInYears);
	double GetTemperatureGradient(void);	// degrees C per km
	double GetResourceTemperatureC(void);	// degrees C
	double GetTemperaturePlantDesignC(void);
	double GetResourceDepthM(void);			// meters
	double GetAmbientTemperatureC(conversionTypes ct = NO_CONVERSION_TYPE);
	double InjectionTemperatureC(void); // calculate injection temperature in degrees C
	double InjectionTemperatureF(void);
	double InjectionDensity(void);

	double GetAEAtTemp(double tempC);
	double GetAEBinaryAtTemp(double tempC);
	double GetAEFlashAtTemp(double tempC);
	double GetAE(void);
	double GetAEBinary(void);
	double GetAEFlash(void);


	double EGSTimeStar(double tempC);
	double EGSAverageWaterTemperatureC2(void);
	double EGSThermalConductivity(void);
	double EGSFractureLength(void);
	double EGSFlowPerFracture(double tempC);
	double EGSAlpha(void);
	double EGSLengthOverVelocity(double tempC);
	double EGSAvailableEnergy(void);
	double EGSReservoirConstant(double avgWaterTempC, double timeDays);



	double GetPressureChangeAcrossReservoir(void);		// [7B.Reservoir Hydraulics].G70 (only used in GetCalculatedPumpDepthInFeet) 
	double pressureInjectionWellBottomHolePSI(void);	// [7B.Reservoir Hydraulics].G72, [7A.GF Pumps].G50
	double pressureWellHeadPSI(void);					// [7A.GF Pumps].G61
	double pressureHydrostaticPSI(void);				// [7B.Reservoir Hydraulics].G17
	double pZero(void);



	// production wells
	double productionTempF(void);
	double productionDensity(void);
	double productionFlowRate(void);// lbs per hr / lbs per cf = cf/hr
	double productionViscosity(void);
	double flowRatePerWell(void);		// take Kg/second input and translate to lbs/hour
	double flowRateTotal(void);			// flow rate per well * number of wells
	double GetNumberOfWells(void);
	double GetPlantBrineEffectiveness(void);

	// turbine output
	double calculateX(double enthalpyIn, double temperatureF);
	double enthalpyChangeTurbine(double dEnthalpyDeltaInitial, double dEnthalpyTurbineG); // I65-I80, I87-I102

	// Flash Turbine 1 - high pressure
	double turbine1dHInitial(void); // I65
	double turbine1TemperatureF(void); // D80
	double turbine1EnthalpyF(void); // D81
	double turbine1EnthalpyG(void); // D82
	double turbine1DH(void); // I80 - btu/lb
	double turbine1HEx(void); // I81 - btu/lb
	double turbine1X(void); // D83 - %
	double turbine1Steam(void); // D85 - lb/hr
	double turbine1NetSteam(void); // I82 lb/hr
	double turbine1OutputKWh(void); // I83 - kW/hr = (btu/lb) * (lb/hr) / (btu/kW)

	// Flash Turbine 2 - low pressure
	double turbine2dHInitial(void); // I87
	double turbine2TemperatureF(void); // D88
	double turbine2EnthalpyF(void); // D89
	double turbine2EnthalpyG(void); // D90
	double turbine2DH(void); // I102 - btu/lb
	double turbine2HEx(void); // I103 - btu/lb
	double turbine2X(void); // D91 %
	double turbine2Steam(void); // I104, D93 - lb/hr
	double turbine2OutputKWh(void); // I105 - kW/hr

	//Flash Vessel Cost Calculation 
	double GetSpecVol(double flash_temp);

	// NCG Removal
	//Note: Dabc -> D = Column D ; abc = corresponding row # ; 
	//Note: Add +4 to get to the correct cell 
	double pInter(int stage); // D156, D205, D253 - psi
	double pTotal(void); // calculated separately in spreadsheet, but mathematically equivalent to pressureCondenser					   D150,D74 - psi
	double pRatio(void); // D151
	double ncgFlowLbsPerHour(void); // D152 - lbs/hour
	double ncgFlowMolesPerHour(void); // D162... - moles/hr
	double pSuction(int stage); // D165, D214
	double prJet(int stage); // D157, D206, D254
	double h2oMolesPerHour(int st); // D163, D212, D260 - moles/hr
	double totalVentFlow(int st); // D161, D210, D258
	double moleWeightVent(int st); // D164, D213, D261
	double suctionSteamRatio(int st); // D167, D216, D264
	double AR(int stage); // D168, D217, D265
	double ERd(int stage); // D169, D218, D266
	double ER(int st); // D170, D219, D267
	double steamFlow(int st); // D171, D220, D268 - lb/hr



	int FlashCount(void);
	double calculateDH(double pressureIn);
	double TemperatureWetBulbF(void);
	double temperatureCondF(void); // D72 - deg F
	double pressureSaturation(void); // D72 - psi
	double pressureCondenser(void); // D74 - psi


	double FlashBrineEffectiveness(void);
	void calculateFlashPressures(void);

	// Main Pump Power
	double overAllSteam(void); // D96
	double qCondenser(void); // D100
	double cwFlow(void); // D115  (in lb/h ?)
	double overAllHEx(void); // I107

	// Pump Power
	double h2oVentFlow(int stage); // D160 - lb/hr
	double moleRatio(int st); // D184,
	double flowSteamMolesPerHr(int st); // D186,
	double flowSteamLbPerHr(int st); // D187,  - lb/hr
	double condensedSteamLbPerHour(int stage); // D188 = D171+D160-D187 = stage1CondensedSteam (lb/hr)
	double pumpWorkFromSteamFlow(double flow); // D189-D198, 
	double cwPumpWorkKWByStage(int st); // D199 - kW
	double cwPumpWorkKW(void); // D305 - kW, part of I116

	// Condensate Pump Power
	double condensatePumpHead(void); // D122 -> Condensate Pump Head
	double condensatePumpPowerKW(void); // D126->kw, part of I106
	double condensatePumpHeadByStage(int st); // (D205, D253, D301) -> Stages 1,2,3 of condensate pump head of ncg removal system
	double condensatePumpWorkByStage(int st); // D203, ... kW
	double totalCondensatePumpWorkKW(void); // D306 - kW	(This is actually the Total NCG Removal Condensate Pump Work)
	double condensatePumpingKW(void); // I117 - kW

	// Fan Power
	double qRejectByStage(int stage); // D190
	double qRejectTotal(void); // D303

	double qRejectedTower(void); // D101
	double fanPowerCoeffA(void); // O95
	double fanPowerCoeffB(void); // P95
	double fanPower(void); // D103 - hp per MMBtu/hr
	double fanPowerKW(void); // D105, I118

	// Vacuum Pump Power
	double deltaPressureByStage(int st); // D173, D222, D270 - psi
	double densityForVacuumPump(int st); // D166, D215, D263 - lb/ft^3
	double vaccumPumpHead(int st); // D175, D224, D272 - ft
	double vacuumPumpWorkByStage(int st); // D178, D227, D275 - kW
	double vacuumPumpingKW(void); // D307, I119

	// Condenser Injection Pump Power
	double injectionDeltaP(void); // D127 - psi (condensate injection delta pressure)
	double injectionPumpHead(void); // D128 - ft
	double injCoeffA(void); // R95
	double injCoeffB(void); // S95
	double injCoeffC(void); // T95
	double injCoeffD(void); // U95
	double evaporativeWaterLoss(void); // D129 - lb/hr (evaporative water loss)
	double drift(void); // D130
	double blowDown(void); // D132
	double waterLoss(void); // D133
	double steamCondensate(void); // D135
	double steamCondensateInjected(void); // D136 - lb/hr
	double condenserInjectionPumpingKW(); // D138, I120 - kW



	// Flash Pressures used mostly in calculateFlashPressures
	bool TempConstraint(void);
	double tempFlashLimitF(void); // D26 - deg F
	double pressureFlashAmorphousSilica(void); // D27 - psi
	double pressureSingleNoConstraint(void); // Q64
	double pressureSingleWithConstraint(void); // S64
	double pressureSingleToTest(void); // Q64 or S64
	double pressureSingle(void); // O64

	double pressureDualHighNoConstraint(void); // R64
	double pressureDualHighWithConstraint(void); // T64
	double pressureDualHigh(void); // P64
	double pressureDualLowUnconstrained(void); // R65
	double pressureDualLowConstrained(void);  // T65
	double pressureDualLowToTest(void); // R65 or T65
	double pressureDualLow(void); // P65


	// weather file opening, reading, checking inputs, etc.
	bool OpenWeatherFile(const char * fn);
	bool ReadWeatherForTimeStep(const bool bHourly, unsigned int timeStep);
	bool ReadNextLineInWeatherFile(void);
	bool determineMakeupAlgorithm(void);
	bool inputErrorsForUICalculations(void);
	bool inputErrorsForAnalysis(void);
	bool ReadyToAnalyze(void);
	bool TimeToUpdateInterface(float fPercentDone, float fNotificationIntervalInPercent);
};

int RunGeothermalAnalysis(bool(*update_function)(float, void*), void*user_data, std::string &err_msg,
	const SPowerBlockParameters &pbp, SPowerBlockInputs &pbInputs,
	const SGeothermal_Inputs &geo_inputs, SGeothermal_Outputs &geo_outputs);

int FillOutputsForUI(std::string &err_msg, const SGeothermal_Inputs &geo_inputs, SGeothermal_Outputs &geo_outputs);


#endif // __geothermalModelDefinitions__