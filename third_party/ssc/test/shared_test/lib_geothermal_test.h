#ifndef lib_geothermal_test_h_
#define lib_geothermal_test_h_
#include <gtest/gtest.h>
#include "lib_geothermal.h"
#include "core.h"
#include "lib_physics.h"

#include "../input_cases/geothermal_common_data.h"

static bool my_update_function(float percent, void *data)
{
	if (data != 0)
		return ((compute_module*)data)->update("working...", percent);
	else
		return true;
}

//Fixture to test CGeothermalAnalyzer class defined in 'lib_geothermal.h':
class GeothermalPlantAnalyzer : public ::testing::Test
{	
protected:

	//Inputs used for calculating values of all 4 struct members that are formal parameters for the 
	//CGeothermalAnalyzer class constructor:
	int	well_flow_rate;
	double num_wells_getem;
	int nameplate;
	int analysis_type;		
	int conversion_type;
	int conversion_subtype;
	double plant_efficiency_input;
	int decline_type;
	double temp_decline_rate;
	int temp_decline_max;
	int	wet_bulb_temp;
	double ambient_pressure;
	double pump_efficiency;
	int delta_pressure_equip;
	double excess_pressure_pump;
	double well_diameter;
	double casing_size;
	double inj_well_diam;
	int specify_pump_work;
	int specified_pump_work_amount;
	int	resource_type;
	int resource_depth;
	int resource_temp;
	int design_temp;
	int rock_thermal_conductivity;
	int rock_specific_heat;
	int rock_density;
	double reservoir_pressure_change;
	int reservoir_width;
	int	reservoir_pressure_change_type;
	double reservoir_height;
	double reservoir_permeability;
	int  inj_prod_well_distance;
	int subsurface_water_loss;
	double fracture_aperature;
	int num_fractures;
	int fracture_width;
	int fracture_angle;
	int geothermal_analysis_period;
	int resource_potential;
	int tou[8760];
			  
	//Initializing all 4 structs to defualt values in SAM 2018.11.11:
		SPowerBlockParameters SPBP;
		SPowerBlockInputs PBInputs;
		SGeothermal_Inputs geoPlant_inputs;
		SGeothermal_Outputs geoPlant_outputs;
	
	//Initializing CGeothermalAnalyzer class for testing:
	CGeothermalAnalyzer* geoTester; 
	
public:
	void SetUp() {
		
	//Current defaults in SAM 2018.11.11:
		well_flow_rate = 110;
		num_wells_getem = 3;
		nameplate = 30000;
		analysis_type = 0;
		resource_temp = 200;
		design_temp = 200;
		conversion_subtype = 3;
		plant_efficiency_input = 61.109;
		decline_type = 0;
		temp_decline_rate = 0.3;
		temp_decline_max = 30;
		wet_bulb_temp = 15;
		ambient_pressure = 14.7;
		pump_efficiency = 67.5;
		delta_pressure_equip = 25;
		excess_pressure_pump = 50;
		well_diameter = 12.25;
		casing_size = 9.625;
		inj_well_diam = 12.25;
		specify_pump_work = 0;
		specified_pump_work_amount = 0;
		resource_type = 0;
		resource_depth = 2000;																									
		rock_thermal_conductivity = 259200;
		rock_specific_heat = 950;
		rock_density = 2600;
		reservoir_pressure_change = 0.35;
		reservoir_width = 500;
		reservoir_pressure_change_type = 0;
		reservoir_height = 100;
		reservoir_permeability = 0.05;
		inj_prod_well_distance = 1500;
		subsurface_water_loss = 2;
		fracture_aperature = 0.0004;
		num_fractures = 6;
		fracture_width = 175;
		fracture_angle = 15;
		geothermal_analysis_period = 30;
		resource_potential = 210;
		
		//Following block intializes all 4 Structs (to default values in SAM 2018.11.11) that are used
		//as formal parameters in constructing the CGeothermalAnalyzer Class:
		//====================================================================================================================================================================
				//SPowerBlockParameters SPBP;
				SPBP.tech_type = 4;
				SPBP.T_htf_cold_ref = 90;						// design outlet fluid temp
				SPBP.T_htf_hot_ref = 175;						// design inlet fluid temp
				SPBP.HTF = 3;									// heat transfer fluid type - set in interface, but no user input
				SPBP.P_ref = nameplate / 1000;					// P_ref wants MW, 'nameplate' in kW
				SPBP.P_boil = 2;
				SPBP.eta_ref = 0.17;
				SPBP.q_sby_frac = 0.2;
				SPBP.startup_frac = 0.2;
				SPBP.startup_time = 1;
				SPBP.pb_bd_frac = 0.013;
				SPBP.T_amb_des = 27;
				SPBP.CT = 0;
				SPBP.dT_cw_ref = 10;
				SPBP.T_approach = 5;
				SPBP.T_ITD_des = 16;
				SPBP.P_cond_ratio = 1.0028;
				SPBP.P_cond_min = 1.25;
				SPBP.n_pl_inc = 8;
				SPBP.F_wc[0] = 0;
				SPBP.F_wc[1] = 0;
				SPBP.F_wc[2] = 0;
				SPBP.F_wc[3] = 0;
				SPBP.F_wc[4] = 0;
				SPBP.F_wc[5] = 0;
				SPBP.F_wc[6] = 0;
				SPBP.F_wc[7] = 0;
				SPBP.F_wc[8] = 0;
		
		//====================================================================================================================================================================
				//SPowerBlockInputs PBInputs;
				PBInputs.mode = 2;
				if (true) // used number of wells as calculated by GETEM
					PBInputs.m_dot_htf = well_flow_rate * 3600.0 * num_wells_getem;
				//Ignoring user defined number of wells for now.

				PBInputs.demand_var = PBInputs.m_dot_htf;
				PBInputs.standby_control = 1;
				PBInputs.rel_humidity = 0.7;

		//====================================================================================================================================================================
				//Initializing SGeothermal_Inputs:
				geoPlant_inputs.md_RatioInjectionToProduction = 0.5;
				geoPlant_inputs.md_DesiredSalesCapacityKW = nameplate;

				//geoPlant_inputs.md_NumberOfWells = as_double("num_wells");

				if (analysis_type == 0)
					geoPlant_inputs.me_cb = POWER_SALES;
				else
					geoPlant_inputs.me_cb = NUMBER_OF_WELLS;

				if (conversion_type == 0)
					geoPlant_inputs.me_ct = BINARY;
				else if (conversion_type == 1)
					geoPlant_inputs.me_ct = FLASH;

				switch (conversion_subtype)
				{
				case 0:	geoPlant_inputs.me_ft = SINGLE_FLASH_NO_TEMP_CONSTRAINT; break;
				case 1:	geoPlant_inputs.me_ft = SINGLE_FLASH_WITH_TEMP_CONSTRAINT; break;
				case 2:	geoPlant_inputs.me_ft = DUAL_FLASH_NO_TEMP_CONSTRAINT; break;
				case 3:	geoPlant_inputs.me_ft = DUAL_FLASH_WITH_TEMP_CONSTRAINT; break;
				}
				geoPlant_inputs.md_PlantEfficiency = plant_efficiency_input / 100;

				// temperature decline
				if (decline_type == 0)
					geoPlant_inputs.me_tdm = ENTER_RATE;
				else if (decline_type == 1)
					geoPlant_inputs.me_tdm = CALCULATE_RATE;
				geoPlant_inputs.md_TemperatureDeclineRate = temp_decline_rate / 100;
				geoPlant_inputs.md_MaxTempDeclineC = temp_decline_max;

				// flash inputs
				geoPlant_inputs.md_TemperatureWetBulbC = wet_bulb_temp;
				geoPlant_inputs.md_PressureAmbientPSI = ambient_pressure;

				//pumping parameters
				geoPlant_inputs.md_ProductionFlowRateKgPerS = well_flow_rate;
				geoPlant_inputs.md_GFPumpEfficiency = pump_efficiency / 100;
				geoPlant_inputs.md_PressureChangeAcrossSurfaceEquipmentPSI = delta_pressure_equip;
				geoPlant_inputs.md_ExcessPressureBar = physics::PsiToBar(excess_pressure_pump);
				geoPlant_inputs.md_DiameterProductionWellInches = well_diameter;
				geoPlant_inputs.md_DiameterPumpCasingInches = casing_size;
				geoPlant_inputs.md_DiameterInjectionWellInches = inj_well_diam;
				geoPlant_inputs.mb_CalculatePumpWork = (1 != specify_pump_work);
				geoPlant_inputs.md_UserSpecifiedPumpWorkKW = specified_pump_work_amount * 1000; // entered in MW

				//resource characterization
				if (resource_type == 0)
					geoPlant_inputs.me_rt = HYDROTHERMAL;
				else if (resource_type == 1)
					geoPlant_inputs.me_rt = EGS;
				geoPlant_inputs.md_ResourceDepthM = resource_depth;
				geoPlant_inputs.md_TemperatureResourceC = resource_temp;
				geoPlant_inputs.me_dc = TEMPERATURE;
				geoPlant_inputs.md_TemperaturePlantDesignC = design_temp;



				//reservoir properties
				geoPlant_inputs.md_TemperatureEGSAmbientC = 15.0;
				geoPlant_inputs.md_EGSThermalConductivity = rock_thermal_conductivity;
				geoPlant_inputs.md_EGSSpecificHeatConstant = rock_specific_heat;
				geoPlant_inputs.md_EGSRockDensity = rock_density;
				switch (reservoir_pressure_change_type)
				{
				case 0: geoPlant_inputs.me_pc = ENTER_PC; break;				// pressure change entered by user
				case 1: geoPlant_inputs.me_pc = SIMPLE_FRACTURE; break;		// use fracture flow (EGS only)
				case 2: geoPlant_inputs.me_pc = K_AREA; break;				// permeability * area
				}
				geoPlant_inputs.md_ReservoirDeltaPressure = reservoir_pressure_change;
				geoPlant_inputs.md_ReservoirWidthM = reservoir_width;
				geoPlant_inputs.md_ReservoirHeightM = reservoir_height;
				geoPlant_inputs.md_ReservoirPermeability = reservoir_permeability;
				geoPlant_inputs.md_DistanceBetweenProductionInjectionWellsM = inj_prod_well_distance;
				geoPlant_inputs.md_WaterLossPercent = subsurface_water_loss / 100;
				geoPlant_inputs.md_EGSFractureAperature = fracture_aperature;
				geoPlant_inputs.md_EGSNumberOfFractures = num_fractures;
				geoPlant_inputs.md_EGSFractureWidthM = fracture_width;
				geoPlant_inputs.md_EGSFractureAngle = fracture_angle;

				// calculate output array sizes
				geoPlant_inputs.mi_ModelChoice = 0;		 // 0=GETEM, 1=Power Block monthly, 2=Power Block hourly
				// set geothermal inputs RE how analysis is done and for how long
				geoPlant_inputs.mi_ProjectLifeYears = geothermal_analysis_period;
				//if (geoPlant_inputs.mi_ProjectLifeYears == 0)
				//	throw general_error("invalid analysis period specified in the geothermal hourly model");

				geoPlant_inputs.md_PotentialResourceMW = resource_potential;
				geoPlant_inputs.mc_WeatherFileName = geothermal_weather_path;
				geoPlant_inputs.mia_tou = tou;
				geoPlant_inputs.mi_MakeupCalculationsPerYear = (geoPlant_inputs.mi_ModelChoice == 2) ? 8760 : 12;
				geoPlant_inputs.mi_TotalMakeupCalculations = geoPlant_inputs.mi_ProjectLifeYears * geoPlant_inputs.mi_MakeupCalculationsPerYear;

		//====================================================================================================================================================================
				//Initializing SGeothermal_Outputs:
		
				geoPlant_outputs.md_NumberOfWells;
				geoPlant_outputs.md_PumpWorkKW;
				geoPlant_outputs.eff_secondlaw;
				geoPlant_outputs.qRejectedTotal;
				geoPlant_outputs.condenser_q;
				geoPlant_outputs.v_stage_1;
				geoPlant_outputs.v_stage_2;
				geoPlant_outputs.v_stage_3;
				geoPlant_outputs.GF_flowrate;
				geoPlant_outputs.qRejectByStage_1;
				geoPlant_outputs.qRejectByStage_2;
				geoPlant_outputs.qRejectByStage_3;
				geoPlant_outputs.ncg_condensate_pump;
				geoPlant_outputs.cw_pump_work;
				geoPlant_outputs.pressure_ratio_1;
				geoPlant_outputs.pressure_ratio_2;
				geoPlant_outputs.pressure_ratio_3;
				geoPlant_outputs.condensate_pump_power;
				geoPlant_outputs.cwflow;
				geoPlant_outputs.cw_pump_head;
				geoPlant_outputs.flash_temperature;
				geoPlant_outputs.flash_temperature_lp;
				geoPlant_outputs.spec_vol; 
				geoPlant_outputs.spec_vol_lp;
				geoPlant_outputs.getX_hp; 
				geoPlant_outputs.getX_lp;
				geoPlant_outputs.flash_count;
				geoPlant_outputs.max_secondlaw;
				geoPlant_outputs.mb_BrineEffectivenessCalculated;
				geoPlant_outputs.md_FlashBrineEffectiveness;

				geoPlant_outputs.mb_FlashPressuresCalculated;
				geoPlant_outputs.md_PressureHPFlashPSI; // d29, d64
				geoPlant_outputs.md_PressureLPFlashPSI; // d30, d65

				// only for use in the interface to show 'calculated' values
				geoPlant_outputs.md_PlantBrineEffectiveness;
				geoPlant_outputs.md_GrossPlantOutputMW;	//double getgrossplantoutputmw(void) { return this->plantoutputkw()/1000; }
				geoPlant_outputs.md_PumpDepthFt;
				geoPlant_outputs.md_PumpHorsePower;
				geoPlant_outputs.md_PressureChangeAcrossReservoir; //double getpressurechangeacrossreservoir(void) { return moppc.getpressurechangeacrossreservoir(); }
				geoPlant_outputs.md_AverageReservoirTemperatureF; //double getaveragereservoirtemperatureusedf(void) { return moppc.getreservoirtemperaturef(); }
				geoPlant_outputs.md_BottomHolePressure; //double getbottomholepressure(void) { return moppc.getbottomholepressure(); }



				geoPlant_outputs.maf_ReplacementsByYear = new double[geoPlant_inputs.mi_ProjectLifeYears];

				//ssc_number_t *annual_replacements = allocate( "annual_replacements", geoPlant_inputs.mi_ProjectLifeYears);
		
				// allocate lifetime monthly arrays (one element per month, over lifetime of project)
				geoPlant_outputs.maf_monthly_resource_temp = new double[12 * geoPlant_inputs.mi_ProjectLifeYears];
				geoPlant_outputs.maf_monthly_power = new double[12 * geoPlant_inputs.mi_ProjectLifeYears];
				geoPlant_outputs.maf_monthly_energy = new double[12 * geoPlant_inputs.mi_ProjectLifeYears];

				// allocate lifetime timestep arrays (one element per timestep, over lifetime of project)
				// if this is a monthly analysis, these are redundant with monthly arrays that track same outputs
		
				geoPlant_inputs.mi_MakeupCalculationsPerYear = (geoPlant_inputs.mi_ModelChoice == 2) ? 8760 : 12;
				geoPlant_inputs.mi_TotalMakeupCalculations = geoPlant_inputs.mi_ProjectLifeYears * geoPlant_inputs.mi_MakeupCalculationsPerYear;

				geoPlant_outputs.maf_timestep_resource_temp = new double[geoPlant_inputs.mi_TotalMakeupCalculations];
		
				geoPlant_outputs.maf_timestep_power = new double[geoPlant_inputs.mi_TotalMakeupCalculations];
				geoPlant_outputs.maf_timestep_test_values = new double[geoPlant_inputs.mi_TotalMakeupCalculations];

				geoPlant_outputs.maf_timestep_pressure = new double[geoPlant_inputs.mi_TotalMakeupCalculations];
				geoPlant_outputs.maf_timestep_dry_bulb = new double[geoPlant_inputs.mi_TotalMakeupCalculations];
				geoPlant_outputs.maf_timestep_wet_bulb = new double[geoPlant_inputs.mi_TotalMakeupCalculations];
		
				geoPlant_outputs.maf_hourly_power = new double[geoPlant_inputs.mi_ProjectLifeYears * 8760];
		
		//====================================================================================================================================================================
		void * user_data = nullptr;

		//Instantiating CGeothermalAnalyzer class:
		geoTester = new CGeothermalAnalyzer(SPBP, PBInputs, geoPlant_inputs, geoPlant_outputs);
		geoTester->RunAnalysis(my_update_function, user_data);
		geoTester->InterfaceOutputsFilled();
		
}	



	void TearDown() {
		
		if (geoTester != nullptr) {
			delete geoTester;
			geoTester = nullptr;
		}
		
		if (geoPlant_outputs.maf_hourly_power != nullptr) {
			delete[] geoPlant_outputs.maf_hourly_power;
			geoPlant_outputs.maf_hourly_power = nullptr;
		}
		
		if (geoPlant_outputs.maf_timestep_wet_bulb != nullptr) {
			delete[] geoPlant_outputs.maf_timestep_wet_bulb;
			geoPlant_outputs.maf_timestep_wet_bulb = nullptr;
		}
		
		if (geoPlant_outputs.maf_timestep_dry_bulb != nullptr) {
			delete[] geoPlant_outputs.maf_timestep_dry_bulb;
			geoPlant_outputs.maf_timestep_dry_bulb = nullptr;
		}
		
		if (geoPlant_outputs.maf_timestep_pressure != nullptr) {
			delete[] geoPlant_outputs.maf_timestep_pressure;
			geoPlant_outputs.maf_timestep_pressure = nullptr;
		}
		
		if (geoPlant_outputs.maf_timestep_test_values != nullptr) {
			delete[] geoPlant_outputs.maf_timestep_test_values;
			geoPlant_outputs.maf_timestep_test_values = nullptr;
		}
		
		if (geoPlant_outputs.maf_timestep_power != nullptr) {
			delete[] geoPlant_outputs.maf_timestep_power;
			geoPlant_outputs.maf_timestep_power = nullptr;
		}
		
		if (geoPlant_outputs.maf_timestep_resource_temp != nullptr) {
			delete[] geoPlant_outputs.maf_timestep_resource_temp;
			geoPlant_outputs.maf_timestep_resource_temp = nullptr;
		}
		
		if (geoPlant_outputs.maf_monthly_energy != nullptr) {
			delete[] geoPlant_outputs.maf_monthly_energy;
			geoPlant_outputs.maf_monthly_energy = nullptr;
		}
		
		if (geoPlant_outputs.maf_monthly_power != nullptr) {
			delete[] geoPlant_outputs.maf_monthly_power;
			geoPlant_outputs.maf_monthly_power = nullptr;
		}
		
		if (geoPlant_outputs.maf_monthly_resource_temp != nullptr) {
			delete[] geoPlant_outputs.maf_monthly_resource_temp;
			geoPlant_outputs.maf_monthly_resource_temp = nullptr;
		}
		
		if (geoPlant_outputs.maf_ReplacementsByYear != nullptr) {
			delete[] geoPlant_outputs.maf_ReplacementsByYear;
			geoPlant_outputs.maf_ReplacementsByYear = nullptr;
		}
			   		 
	}

};

#endif