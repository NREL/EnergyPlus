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

#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "sco2_power_cycle.h"
#include <vector>
#include "sco2_rec_util.h"
#include "sam_csp_util.h"
//#include "co2props.h"
//#include "co2props_nn.h"
#include "CO2_properties.h"

#include "heat_exchangers.h"

//#include "sco2_pc_core.h"
#include "sco2_recompression_cycle.h"

using namespace std;

enum{	//Parameters
	P_1,

	//Inputs
	I_1,

	//Outputs
	O_1,

	//N_MAX
	N_MAX
};

tcsvarinfo sco2_test_type401_variables[] = {
	//PARAMETERS
	{ TCS_PARAM, TCS_NUMBER, P_1, "eta", "Field efficiency", "-", "", "", "" },

	//INPUTS
	{ TCS_INPUT, TCS_NUMBER, I_1, "vwind", "Wind velocity", "m/s", "", "", "" },

	//OUTPUTS
	{ TCS_OUTPUT, TCS_NUMBER, O_1, "pparasi", "Parasitic tracking/startup power", "MWe", "", "", "" },

	//N_MAX
	{ TCS_INVALID, TCS_INVALID, N_MAX, 0, 0, 0, 0, 0, 0 }
};


class sco2_test_type401 : public tcstypeinterface
{
private:
	//Parameters

	double n_zen;

public:
	sco2_test_type401(tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface(cst, ti)
	{
		n_zen = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sco2_test_type401()
	{
	}

	virtual int init()
	{
		/*
		// ****************************************
		// ** Test update sCO2 power cycle code ***
		// ****************************************
		CO2_state co2_props_1;
		double T_in = 800.0;
		double P_in = 20000.0;
		CO2_TP(T_in, P_in, &co2_props_1);
		double ssnd_in = co2_props_1.ssnd;
		double P_out = 10000.0;
		double poly_eta = 0.75;
		int error_code = 0;
		double isen_eta = -999.9;

		double enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out, spec_work;
		enth_in = entr_in = dens_in = temp_out = enth_out = entr_out = dens_out = spec_work = std::numeric_limits<double>::quiet_NaN();

		isen_eta_from_poly_eta(T_in, P_in, P_out, poly_eta, false, error_code, isen_eta);
		calculate_turbomachinery_outlet_1(T_in, P_in, P_out, poly_eta, false, error_code, enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out, spec_work);

		C_turbine::S_design_parameters t_des_par;
		t_des_par.m_D_in = dens_in;
		t_des_par.m_h_in = enth_in;
		t_des_par.m_h_out = enth_out;
		t_des_par.m_m_dot = 35000.0 / spec_work;
		t_des_par.m_N_comp_design_if_linked = 123.456;
		t_des_par.m_N_design = 3600.0;
		t_des_par.m_P_in = P_in;
		t_des_par.m_P_out = P_out;
		t_des_par.m_s_in = entr_in;
		t_des_par.m_T_in = T_in;

		C_turbine turbine;

		turbine.turbine_sizing(t_des_par, error_code);
		
		double m_dot_t, T_t_out;
		m_dot_t = T_t_out = -999.9;
		turbine.off_design_turbine(T_in, 0.75*P_in, P_out, 3600.0, error_code, m_dot_t, T_t_out);

		// ************************************************
		// Test compressor
		T_in = 310.0;
		P_in = 10000.0;
		P_out = 20000.0;
		isen_eta = 0.75;

		calculate_turbomachinery_outlet_1(T_in, P_in, P_out, isen_eta, true, error_code, enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out, spec_work);

		CO2_TD(temp_out, dens_out, &co2_props_1);

		double f_recomp = 0.2;
		C_compressor::S_design_parameters mc_des_par;
		mc_des_par.m_D_in = dens_in;
		mc_des_par.m_D_out = dens_out;
		mc_des_par.m_h_in = enth_in;
		mc_des_par.m_h_out = enth_out;
		mc_des_par.m_m_dot = t_des_par.m_m_dot*(1.0-f_recomp);
		mc_des_par.m_P_out = P_out;
		mc_des_par.m_s_in = entr_in;
		mc_des_par.m_T_out = temp_out;

		C_compressor mc;

		mc.compressor_sizing(mc_des_par, error_code);

		double T_rc_in = 370.0;
		calculate_turbomachinery_outlet_1(T_rc_in, P_in, P_out, isen_eta, true, error_code, enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out, spec_work);
		C_recompressor::S_design_parameters rc_des_par;
		rc_des_par.m_P_in = P_in;
		rc_des_par.m_D_in = dens_in;
		rc_des_par.m_D_out = dens_out;
		rc_des_par.m_h_in = enth_in;
		rc_des_par.m_h_out = enth_out;
		rc_des_par.m_m_dot = t_des_par.m_m_dot*f_recomp;
		rc_des_par.m_P_out = P_out;
		rc_des_par.m_s_in = entr_in;
		rc_des_par.m_T_out = temp_out;
		
		C_recompressor rc;
		rc.recompressor_sizing(rc_des_par, error_code);

		double T_rc_od_out = -999.9;
		rc.off_design_recompressor(T_rc_in, 0.8*P_in, 0.85*rc_des_par.m_m_dot, P_out, error_code, T_rc_od_out);

		double T_c_od_out = std::numeric_limits<double>::quiet_NaN();
		double P_c_od_out = std::numeric_limits<double>::quiet_NaN();
		mc.off_design_compressor(T_in, P_in, 0.75*m_dot_t, mc.get_design_solved()->m_N_design, error_code, T_c_od_out, P_c_od_out);

		C_RecompCycle::S_design_parameters cycle_des_par;
			// Based on "dry" cycle in Dyreby recap
		cycle_des_par.m_W_dot_net = 10000.0;
		cycle_des_par.m_T_mc_in = 55.0 + 273.15;
		cycle_des_par.m_T_t_in = 700.0 + 273.15;
		cycle_des_par.m_P_mc_out = 20000.0;
		double pressure_ratio = 2.6;
		cycle_des_par.m_P_mc_in = cycle_des_par.m_P_mc_out / pressure_ratio;
		cycle_des_par.m_DP_LT[0] = 0.0;
		cycle_des_par.m_DP_LT[1] = 0.0;
		cycle_des_par.m_DP_HT[0] = 0.0;
		cycle_des_par.m_DP_HT[1] = 0.0;
		cycle_des_par.m_DP_PC[0] = 0.0;
		cycle_des_par.m_DP_PC[1] = 0.0;
		cycle_des_par.m_DP_PHX[0] = 0.0;
		cycle_des_par.m_DP_PHX[1] = 0.0;
		cycle_des_par.m_UA_LT = 250.0;
		cycle_des_par.m_UA_HT = 250.0;
		cycle_des_par.m_recomp_frac = 0.0;
		cycle_des_par.m_eta_mc = 0.89;
		cycle_des_par.m_eta_rc = 0.89;
		cycle_des_par.m_eta_t = 0.9;
		cycle_des_par.m_N_sub_hxrs = 10;
		cycle_des_par.m_P_high_limit = 20000.0;
		cycle_des_par.m_tol = 1.E-3;
		cycle_des_par.m_N_turbine = 3600.0;

		C_RecompCycle rc_cycle;

		//rc_cycle.design(cycle_des_par, error_code);

		C_RecompCycle::S_opt_design_parameters cycle_opt_des_par;
		cycle_opt_des_par.m_W_dot_net = 10000.0;
		cycle_opt_des_par.m_T_mc_in = 55.0 + 273.15;
		cycle_opt_des_par.m_T_t_in = 700.0 + 273.15;
		cycle_opt_des_par.m_DP_LT[0] = 0.0;
		cycle_opt_des_par.m_DP_LT[1] = 0.0;
		cycle_opt_des_par.m_DP_HT[0] = 0.0;
		cycle_opt_des_par.m_DP_HT[1] = 0.0;
		cycle_opt_des_par.m_DP_PC[0] = 0.0;
		cycle_opt_des_par.m_DP_PC[1] = 0.0;
		cycle_opt_des_par.m_DP_PHX[0] = 0.0;
		cycle_opt_des_par.m_DP_PHX[1] = 0.0;
		cycle_opt_des_par.m_UA_rec_total = 5000.0;
		cycle_opt_des_par.m_eta_mc = 0.89;
		cycle_opt_des_par.m_eta_rc = 0.89;
		cycle_opt_des_par.m_eta_t = 0.9;
		cycle_opt_des_par.m_N_sub_hxrs = 10;
		cycle_opt_des_par.m_P_high_limit = 20000.0;
		cycle_opt_des_par.m_tol = 1.E-3;
		cycle_opt_des_par.m_opt_tol = 1.E-3;
		cycle_opt_des_par.m_N_turbine = 3600.0;

		cycle_opt_des_par.m_P_mc_out_guess = 20000.0;
		cycle_opt_des_par.m_fixed_P_mc_out = true;

		cycle_opt_des_par.m_PR_HP_to_LP_guess = 2.6;
		cycle_opt_des_par.m_fixed_PR_HP_to_LP = true;

		cycle_opt_des_par.m_recomp_frac_guess = 0.0;
		cycle_opt_des_par.m_fixed_recomp_frac = false;

		cycle_opt_des_par.m_LT_frac_guess = 0.5;
		cycle_opt_des_par.m_fixed_LT_frac = false;

		//rc_cycle.opt_design(cycle_opt_des_par, error_code);

		C_RecompCycle::S_auto_opt_design_parameters cycle_auto_opt_des_par;
		cycle_auto_opt_des_par.m_W_dot_net = 10000.0;
		cycle_auto_opt_des_par.m_T_mc_in = 55.0 + 273.15;
		cycle_auto_opt_des_par.m_T_t_in = 700.0 + 273.15;
		cycle_auto_opt_des_par.m_DP_LT[0] = 0.0;
		cycle_auto_opt_des_par.m_DP_LT[1] = 0.0;
		cycle_auto_opt_des_par.m_DP_HT[0] = 0.0;
		cycle_auto_opt_des_par.m_DP_HT[1] = 0.0;
		cycle_auto_opt_des_par.m_DP_PC[0] = 0.0;
		cycle_auto_opt_des_par.m_DP_PC[1] = 0.0;
		cycle_auto_opt_des_par.m_DP_PHX[0] = 0.0;
		cycle_auto_opt_des_par.m_DP_PHX[1] = 0.0;
		cycle_auto_opt_des_par.m_UA_rec_total = 5000.0;
		cycle_auto_opt_des_par.m_eta_mc = 0.89;
		cycle_auto_opt_des_par.m_eta_rc = 0.89;
		cycle_auto_opt_des_par.m_eta_t = 0.9;
		cycle_auto_opt_des_par.m_N_sub_hxrs = 10;
		cycle_auto_opt_des_par.m_P_high_limit = 20000.0;
		cycle_auto_opt_des_par.m_tol = 1.E-3;
		cycle_auto_opt_des_par.m_opt_tol = 1.E-3;
		cycle_auto_opt_des_par.m_N_turbine = 3600.0;

		rc_cycle.auto_opt_design(cycle_auto_opt_des_par, error_code);

		C_RecompCycle::S_od_parameters cycle_od_par;
		cycle_od_par.m_T_mc_in = cycle_auto_opt_des_par.m_T_mc_in;
		cycle_od_par.m_T_t_in = cycle_auto_opt_des_par.m_T_t_in;
		cycle_od_par.m_P_mc_in = rc_cycle.get_design_solved()->m_pres[1-1];
			cycle_od_par.m_P_mc_in = 7698.8;
		cycle_od_par.m_recomp_frac = rc_cycle.get_design_solved()->m_recomp_frac;
			cycle_od_par.m_recomp_frac = 0.2452;
		cycle_od_par.m_N_mc = rc_cycle.get_design_solved()->m_N_mc;
			cycle_od_par.m_N_mc = 30181.0;
		cycle_od_par.m_N_t = rc_cycle.get_design_solved()->m_N_t;
		cycle_od_par.m_N_sub_hxrs = cycle_auto_opt_des_par.m_N_sub_hxrs;
		cycle_od_par.m_tol = cycle_auto_opt_des_par.m_tol;

		//rc_cycle.off_design(cycle_od_par, error_code);

		C_RecompCycle::S_target_od_parameters cycle_tar_od_par;
		cycle_tar_od_par.m_T_mc_in = cycle_auto_opt_des_par.m_T_mc_in;
		cycle_tar_od_par.m_T_t_in = cycle_auto_opt_des_par.m_T_t_in;
		cycle_tar_od_par.m_recomp_frac = rc_cycle.get_design_solved()->m_recomp_frac;
		cycle_tar_od_par.m_N_mc = rc_cycle.get_design_solved()->m_N_mc;
		cycle_tar_od_par.m_N_t = rc_cycle.get_design_solved()->m_N_t;
		cycle_tar_od_par.m_N_sub_hxrs = cycle_auto_opt_des_par.m_N_sub_hxrs;
		cycle_tar_od_par.m_tol = cycle_auto_opt_des_par.m_tol;

		cycle_tar_od_par.m_target = rc_cycle.get_design_solved()->m_W_dot_net / rc_cycle.get_design_solved()->m_eta_thermal * 0.5;
		cycle_tar_od_par.m_is_target_Q = true;

		cycle_tar_od_par.m_lowest_pressure = 3000.0;
		cycle_tar_od_par.m_highest_pressure = 25000.0;

		C_RecompCycle::S_opt_target_od_parameters cycle_opt_tar_od_par;
		cycle_opt_tar_od_par.m_T_mc_in = cycle_auto_opt_des_par.m_T_mc_in;
		cycle_opt_tar_od_par.m_T_t_in = cycle_auto_opt_des_par.m_T_t_in;
		cycle_opt_tar_od_par.m_target = cycle_tar_od_par.m_target;
		cycle_opt_tar_od_par.m_is_target_Q = true;
		cycle_opt_tar_od_par.m_N_sub_hxrs = cycle_auto_opt_des_par.m_N_sub_hxrs;
		cycle_opt_tar_od_par.m_lowest_pressure = cycle_tar_od_par.m_lowest_pressure;
		cycle_opt_tar_od_par.m_highest_pressure = cycle_tar_od_par.m_highest_pressure;
		cycle_opt_tar_od_par.m_recomp_frac_guess = rc_cycle.get_design_solved()->m_recomp_frac;
		cycle_opt_tar_od_par.m_fixed_recomp_frac = false;
		cycle_opt_tar_od_par.m_N_mc_guess = rc_cycle.get_design_solved()->m_N_mc;
		cycle_opt_tar_od_par.m_fixed_N_mc = false;
		cycle_opt_tar_od_par.m_N_t_guess = rc_cycle.get_design_solved()->m_N_t;
		cycle_opt_tar_od_par.m_fixed_N_t = true;
		cycle_opt_tar_od_par.m_tol = cycle_auto_opt_des_par.m_tol;
		cycle_opt_tar_od_par.m_opt_tol = cycle_auto_opt_des_par.m_opt_tol;

		rc_cycle.optimal_target_off_design(cycle_opt_tar_od_par, error_code);

		//rc_cycle.target_off_design(cycle_tar_od_par, error_code);
		//rc_cycle.target_off_design(error_code);

		C_RecompCycle::S_opt_od_parameters   cycle_opt_od_par;
		cycle_opt_od_par.m_T_mc_in = cycle_tar_od_par.m_T_mc_in;
		cycle_opt_od_par.m_T_t_in = cycle_tar_od_par.m_T_t_in;

		cycle_opt_od_par.m_is_max_W_dot = true;

		cycle_opt_od_par.m_N_sub_hxrs = cycle_tar_od_par.m_N_sub_hxrs;

		cycle_opt_od_par.m_P_mc_in_guess = cycle_od_par.m_P_mc_in;
		cycle_opt_od_par.m_fixed_P_mc_in = false;

		cycle_opt_od_par.m_recomp_frac_guess = rc_cycle.get_design_solved()->m_recomp_frac;
		cycle_opt_od_par.m_fixed_recomp_frac = false;

		cycle_opt_od_par.m_N_mc_guess = rc_cycle.get_design_solved()->m_N_mc;
		cycle_opt_od_par.m_fixed_N_mc = false;

		cycle_opt_od_par.m_N_t_guess = rc_cycle.get_design_solved()->m_N_t;
		cycle_opt_od_par.m_fixed_N_t = true;

		cycle_opt_od_par.m_tol = cycle_tar_od_par.m_tol;
		cycle_opt_od_par.m_opt_tol = cycle_tar_od_par.m_tol;

		//rc_cycle.optimal_off_design(cycle_opt_od_par, error_code);

		// ************************************************
		// Test HX

		int N_sub_hxrs = 10;
		double Q_dot = 30000.0;
		double m_dot_c = 57.0;
		double m_dot_h = 57.0;
		double T_c_in = 330.0;
		double T_h_in = 800.0;
		double P_c_in = 20000.0;
		double P_h_in = 10000.0;
		double P_c_out = P_c_in;
		double P_h_out = P_h_in;

		double UA_des = -999.9;
		double min_DT_des = -999.9;

		calculate_hxr_UA_1(N_sub_hxrs, Q_dot, m_dot_c, m_dot_h, T_c_in, T_h_in, P_c_in, P_c_out, P_h_in, P_h_out, error_code, UA_des, min_DT_des);

		C_HeatExchanger::S_design_parameters hx_des_par;
		hx_des_par.m_m_dot_design[0] = m_dot_c;
		hx_des_par.m_m_dot_design[1] = m_dot_h;
		hx_des_par.m_UA_design = UA_des;
		hx_des_par.m_DP_design[0] = 0.0;
		hx_des_par.m_DP_design[1] = 0.0;

		C_HeatExchanger hx;
		hx.initialize(hx_des_par);

		std::vector<double> m_dots_od(2);
		m_dots_od[0] = m_dot_c*0.75;
		m_dots_od[1] = m_dot_h*0.75;

		double UA_od = -999.9;
		hx.hxr_conductance(m_dots_od, UA_od);

		std::vector<double> deltaP_od;
		hx.hxr_pressure_drops(m_dots_od, deltaP_od);
		*/

		// ****************************************
		// ** Test Air-Cooler Model **
		// ****************************************
		C_CO2_to_air_cooler air_cooler;
		C_CO2_to_air_cooler::S_des_par_ind ac_des_par_ind;
		ac_des_par_ind.m_T_amb_des = 32.0+273.15;			//[K]
		ac_des_par_ind.m_elev = 300.0;						//[m]
		C_CO2_to_air_cooler::S_des_par_cycle_dep ac_des_par_cycle_dep;
		ac_des_par_cycle_dep.m_T_hot_in_des = 100.0+273.15;	//[K]
		ac_des_par_cycle_dep.m_P_hot_in_des = 8000.0;		//[kPa]
		ac_des_par_cycle_dep.m_m_dot_total = 938.9;			//[kg/s]
		ac_des_par_cycle_dep.m_delta_P_des = 62.5;		//[kPa]
		ac_des_par_cycle_dep.m_T_hot_in_des = ac_des_par_ind.m_T_amb_des+15.0;	//[K]
		ac_des_par_cycle_dep.m_W_dot_fan_des = 0.35;				//[MW] Cooler air fan power at design


		//bool air_cooler_success = air_cooler.design_hx(ac_des_par_ind,ac_des_par_cycle_dep);

		double W_dot_od = 0.0;
		
		int air_cooler_error_code = 0;

		//air_cooler.off_design_hx(32.0 + 273.15, 101325.0, 100.0 + 273.15, 8000.0,
		//	938.9, 48.0 + 273.15, W_dot_od, air_cooler_error_code);

		//double adfadf = 1.23;

		// ************************************
		// ** Test procedure to find Haynes Allowable Cycles for Fatige Cycles and Creept Life ***
		// ************************************


		//N_sco2_rec::C_rec_des_props     tube_mat(N_sco2_rec::C_rec_des_props::Haynes_230);

		//double epsilon_equiv = 0.6;		//[-]
		//double T_cycles = 600.0;		//[C]

		//double N_allowable = tube_mat.cycles_to_failure(epsilon_equiv, T_cycles);

		//double sigma_MPa = 133.6;		//[MPa]
		//double T_creep = 650.0;			//[C]

		//double time_to_creep = tube_mat.creep_life(sigma_MPa, T_creep);

		//double T_cond = 600.0;		//[C]
		//double k = tube_mat.cond(T_cond);

		//double T_E = 600.0;			//[C]
		//double E = tube_mat.modE(T_E);

		//double T_alpha = 600.0;		//[C]
		//double alpha = tube_mat.alpha_inst(T_alpha);

		//N_sco2_rec::C_tube_slice      tube_slice(N_sco2_rec::C_rec_des_props::Haynes_230);

		//N_sco2_rec::C_tube_slice::S_ID_OD_perf_and_lifetime_inputs     tube_inputs;
		//tube_inputs.m_P_internal = 25.0;
		//tube_inputs.m_T_fluid = 470.0;
		//tube_inputs.m_d_out = 0.012;		//[m]
		//tube_inputs.m_d_in = 0.0052;		//[m]
		//tube_inputs.m_flux = 900000.0;		//[W/m2]
		//tube_inputs.m_h_conv = 20232.66;		//[W/m2-K]

		//N_sco2_rec::C_tube_slice::S_ID_OD_perf_and_lifetime_outputs       tube_outputs;
		//
		//tube_slice.calc_ID_OD_perf_and_lifetime(tube_inputs, tube_outputs);

		//N_sco2_rec::C_tube_slice::S_ID_OD_stress_and_lifetime_inputs  no_flux_inputs;
		//no_flux_inputs.m_P_internal = 25.0;
		//no_flux_inputs.m_T_fluid = 470.0;
		//no_flux_inputs.m_d_out = 0.012;		//[m]
		//no_flux_inputs.m_d_in = 0.0052;		//[m]
		//no_flux_inputs.m_T_surf_in = 470.0;
		//no_flux_inputs.m_T_surf_out = 470.0;
		//N_sco2_rec::C_tube_slice::S_ID_OD_stress_and_lifetime_outputs no_flux_outputs;

		//tube_slice.calc_ID_OD_stress_and_lifetime(no_flux_inputs, no_flux_outputs);

		//double check = 12.34235;

		// **********************************************************
		// **********************************************************
		// **********************************************************
		// **********************************************************

		// Design Inputs //
		/*
		- Flux profile
		- T_fluid_in
		- P_fluid_in
		- T_fluid_out +/ m_dot?
		- d_out
		- L
		- material
		- roughness
		*/		

		// Objective:
		/* Find minimum thickness that results in all axial sections having a Total Damage < 1 */
		// Procedure:
		/* 1) Step in to axial node i = 0. Find minimum thickness.
	       2) i++, Test th_min from previous step.
		         if th_min results in Total Damage > 1, then find new min thickness and GOTO 1, else GOTO 2
		   3) End either with a min thickness for the tube or all possible thicknesses exhausted. The latter 
		         resulting in a solid tube. Could instead enforce some pressure drop that, when exceeded, signals
				 that there is no feasible solution for the given inputs
	    */
	
		// Values that will be inputs in the future
		double tube_length = 4.1;		//[m]
		util::matrix_t<double>  tube_flux_map;
		int n_tube_nodes = 5;
		double q_abs_total_input = 150000/(CSP::pi/2.0);
		tube_flux_map.resize(n_tube_nodes,1);
		int n_axial = (int)tube_flux_map.nrows();
		int n_circ = (int)tube_flux_map.ncols();
		for( int i = 0; i < n_axial; i++ )
		{
			for( int j = 0; j < n_circ; j++ )
				tube_flux_map(i,j) = q_abs_total_input;		//[W/m2]
		}

		// Sample case
		n_tube_nodes = 10;
		q_abs_total_input = 300000.0;
		tube_flux_map.resize(n_tube_nodes, 1);
		n_axial = (int)tube_flux_map.nrows();
		n_circ = (int)tube_flux_map.ncols();
		for( int i = 0; i < n_axial; i++ )
		{
			for( int j = 0; j < n_circ; j++ )
				tube_flux_map(i, j) = q_abs_total_input - 0.1*q_abs_total_input*(i);		//[W/m2]
		}

	

		//int 
			n_tube_nodes = 10;
		double d_out = 0.012;			//[m]
		double T_fluid_in = 470.0;		//[C]
		double T_fluid_out = 650.0;		//[C]
		double P_fluid_in = 25.0;		//[MPa]
		double e_roughness = 4.5E-5;	//[m] Absolute tube roughness
		double L_tube = 4.1;			//[m] Length of tube
		
		N_sco2_rec::C_calc_tube_min_th      calc_min_th;

		//double 
			q_abs_total_input = 300000.0;
		vector<double> max_flux_in(n_tube_nodes);
		for( int i = 0; i < n_tube_nodes; i++ )
			max_flux_in[i] = q_abs_total_input - 0.1*q_abs_total_input*(i);		//[W/m2]
		
        //MJW 2015.6.9 -- Commenting out because call structure changed and this type is not maintained.
		//bool is_tube_feasible = calc_min_th.calc_th_1Dmaxflux_Tout(
		//	                    max_flux_in, L_tube, d_out, T_fluid_in, T_fluid_out, P_fluid_in);

		//double d_in_min = calc_min_th.get_min_d_in();
		//double m_dot_class = calc_min_th.get_m_dot_tube_kgsec();
		//double deltaP = calc_min_th.get_deltaP_kPa();

		for( int i = 0; i < n_tube_nodes; i++ )
			max_flux_in[i] *= 0.9;

		//MJW 2015.6.9 -- Commenting out because call structure changed and this type is not maintained.
        //bool is_tube2_feasible = calc_min_th.calc_th_1Dmaxflux_mdot(
		//						max_flux_in, L_tube, d_out, T_fluid_in, P_fluid_in, m_dot_class);

		//double d_in_min2 = calc_min_th.get_min_d_in();
		//double T_out_C = calc_min_th.get_T_out_C();
		//double deltaP2 = calc_min_th.get_deltaP_kPa();
		
		// Know flux and tube surface area, so calculate total absorbed flux
		double A_surf_total = d_out*CSP::pi*tube_length;			//[m^2] Total tube surface area
		double A_surf_per_node = A_surf_total/(n_axial*n_circ);		//[m^2] Total surface area per axial/circ control area

		double q_abs_total = 0.0;
		vector<double> q_abs_1D(n_axial);
		for( int i = 0; i < n_axial; i++ )
		{
			q_abs_1D[i] = 0.0;
			for( int j = 0; j < n_circ; j++ )
			{
				q_abs_total += tube_flux_map(i, j)*A_surf_per_node;		//[W]
				q_abs_1D[i] += tube_flux_map(i, j)*A_surf_per_node;		//[W]
			}
		}

		int n_temps = n_axial + 1;
		vector<double> Temp(n_temps);
		vector<double> Pres(n_temps);
		vector<double> Enth(n_temps);

		vector<double> h_conv_ave(n_axial);
		vector<double> L_node(n_axial);
		L_node.assign(n_axial, L_tube / (double)n_axial);

		CO2_state co2_props;
		Temp[0] = T_fluid_in;
		Pres[0] = P_fluid_in*1000.0;		//[kPa]
		CO2_TP(Temp[0]+273.15,Pres[0],&co2_props);
		Enth[0] = co2_props.enth*1000.0;				//[J/kg], convert from [kJ/kg]

		// Set up Class and Structures for creep-fatigue lifetime model
		N_sco2_rec::C_tube_slice      tube_slice(N_sco2_rec::C_rec_des_props::Haynes_230);
		N_sco2_rec::C_tube_slice::S_ID_OD_perf_and_lifetime_inputs     tube_inputs;
		N_sco2_rec::C_tube_slice::S_ID_OD_perf_and_lifetime_outputs      tube_outputs;

		bool search_min_th = true;
		double th_min_guess = 0.001;	// Smallest possible thickness = 1 mm
		double th_step = 0.0002;
		double P_tube_out_prev = Pres[0];
		double m_dot_tube = std::numeric_limits<double>::quiet_NaN();
		double d_in = std::numeric_limits<double>::quiet_NaN();

		double P_tube_out_min = 0.8*Pres[0];	// At max, allow 20% pressure drop
		bool is_deltaP_too_large = false;

		int iter_d_in = -1;

		do
		{
			iter_d_in++;

			d_in = d_out - 2.0*(th_min_guess+th_step*iter_d_in);
			//******************
			// Just for testing
			//d_in = 0.0052;
			//
			// ****************
			double A_cs = 0.25*CSP::pi*pow(d_in,2);

			double relRough = e_roughness / d_in;

			// Guess tube outlet pressure
			double P_tube_out_guess = 0.95*P_tube_out_prev;
			double P_tube_out_tolerance = 0.001;
			double P_tube_out_diff = 2.0*P_tube_out_tolerance;

			double P_tube_guess_high = P_tube_out_prev;
			double P_tube_guess_low = -999.9;

			int iter_P_tube = 0;			

			do		// Solve for correct mass flow rate given pressure drops through tube
			{
				iter_P_tube++;

				if(iter_P_tube > 1)
				{
					if(P_tube_out_diff > 0.0)	// Calculated P_tube_Out > Guessed P_tube_out
					{
						P_tube_guess_low = P_tube_out_guess;
						P_tube_out_guess = 0.5*(P_tube_guess_low+P_tube_guess_high);
					}
					else						// Calculated P_tube_out < Guessed P_tube_out
					{
						P_tube_guess_high = P_tube_out_guess;
						if( P_tube_guess_low < 0.0 )
							P_tube_out_guess = 0.95*Pres[n_temps-1];
						else
							P_tube_out_guess = 0.5*(P_tube_guess_low + P_tube_guess_high);
					}		
					if(P_tube_guess_high <= P_tube_out_min)
					{
						is_deltaP_too_large = true;
						break;
					}
				}

				// If guess is less than minimum, set to minimum
				if( P_tube_out_guess < P_tube_out_min )
					P_tube_out_guess = P_tube_out_min;

				CO2_TP(T_fluid_out + 273.15, P_tube_out_guess, &co2_props);
				double h_tube_out = co2_props.enth*1000.0;
				m_dot_tube = q_abs_total/(h_tube_out-Enth[0]);

				// Set up iteration constants for converging on local pressure
				double P_node_out_tolerance = P_tube_out_tolerance;
				double P_node_out_diff = 2.0*P_node_out_tolerance;
				bool is_P_out_too_low = false;

				for( int i = 1; i < n_temps; i++ )		// Step through loop
				{
					double P_node_out_guess = -999.9;
					if(i==1)
						P_node_out_guess = Pres[0] - (double)i/n_temps*(Pres[0]-P_tube_out_guess);
					else
						P_node_out_guess = Pres[i-1] - 1.25*(Pres[i-1]-Pres[i-2]);
					
					double P_guess_high = Pres[i - 1];	//[kPa] Upper guess is always pressure at previous node
					double P_guess_low = -999.9;		//[kPa] If negative then it's a flag that lower guess hasn't been calculated

					int iter_P_local = 0;		// Track iterations					

					do		// Converge local pressure
					{
						iter_P_local++;

						if(iter_P_local > 1)		// Reguess P_node_out_guess until convergence
						{
							if(P_node_out_diff > 0.0)	// Calculated P_out > Guessed P_out
							{
								P_guess_low = Pres[i];
								P_node_out_guess = 0.5*(P_guess_high + P_guess_low);
							}
							else						// Calculated P_out < Guessed P_out
							{
								P_guess_high = Pres[i];
								if( P_guess_low < 0.0 )		// Lower bound hasn't been reached
									P_node_out_guess = 0.95*Pres[i];
								else
									P_node_out_guess = 0.5*(P_guess_high + P_guess_low);
							}						

							if( P_guess_high <= P_tube_out_min )
							{
								is_P_out_too_low = true;
								break;
							}	
						}

						// If guess is less than minimum, set to minimum
						if( P_node_out_guess < P_tube_out_min )
							P_node_out_guess = P_tube_out_min;

						// Calculate outlet enthalpy
						//h_out = h_in + q_abs/m_dot
						Enth[i] = Enth[i-1] + q_abs_1D[i-1]/m_dot_tube;
						
						// Know enthalpy and guessed pressure, so get props
						CO2_PH(P_node_out_guess, Enth[i]/1000.0, &co2_props);
						Temp[i] = co2_props.temp-273.15;		//[C], convert from K
						
						// Calculate friction factor, Reynolds number, nusselt number and h_conv at average nodal P and h
						double P_ave = 0.5*(P_node_out_guess + Pres[i-1]);
						double h_ave = 0.5*(Enth[i] + Enth[i-1]);

						CO2_PH(P_ave, h_ave/1000.0, &co2_props);

						// double visc_dyn = co2_props.visc*1.E-6;
						double visc_dyn = CO2_visc(co2_props.dens, co2_props.temp)*1.E-6;
						double Re = m_dot_tube*d_in/(A_cs*visc_dyn);

						double rho = co2_props.dens;
						double visc_kin = visc_dyn / rho;
						//double cond = co2_props.cond;
						double cond = CO2_cond(co2_props.dens, co2_props.temp);
						double specheat = co2_props.cp*1000.0;
						double alpha = cond/(specheat*rho);
						double Pr = visc_kin/alpha;

						double Nusselt = -999.9;
						double f = -999.9;

						// Specifying the length over diameter = 1000 sets the problem as Fully Developed Flow
						CSP::PipeFlow(Re, Pr, 1000.0, relRough, Nusselt, f);

						h_conv_ave[i-1] = Nusselt*cond / d_in;
						
						double u_m = m_dot_tube/(rho*A_cs);
						Pres[i] = Pres[i-1] - f*L_node[i-1]*rho*pow(u_m,2)/(2.0*d_in)/1000.0;

						P_node_out_diff = (Pres[i] - P_node_out_guess)/P_node_out_guess;
					
					} while( fabs(P_node_out_diff)>P_node_out_tolerance );
					// End iteration on local node pressure

					// Pressure drops in one of the nodes was too large, so set outlet pressure to minimum and break out of loop iteration
					if( is_P_out_too_low )
					{
						Pres[n_temps-1] = 0.9*P_tube_out_guess;		// Ensures P_tube_out_diff is negative
						break;
					}

				}	
				// End local node pressure iterations for series of nodes in tube

				P_tube_out_diff = (Pres[n_temps-1] - P_tube_out_guess)/P_tube_out_guess;
				P_tube_out_prev = Pres[n_temps-1];

			} while(fabs(P_tube_out_diff)>P_tube_out_tolerance);
			// End iteration on tube outlet pressure and dependent mass flow rate calculation

			// If pressure drop is too large, then increasing thickness will further increase pressure drop
			// So get out of tube thickness iteration
			if( is_deltaP_too_large )
				break;

			// Have solved for correct mass flow rate given tube thickness, outlet temp, and flux
			// Now solve for the creep-fatigue lifetime at this thickness

			double total_damage = 0.0;

			for( int i = 1; i < n_temps; i++ )
			{
				// Define input structure for performance and lifetime model
				tube_inputs.m_P_internal = Pres[0]/1.E3;	//[MPa] Constant: always max pressure
				tube_inputs.m_T_fluid = Temp[i];
				tube_inputs.m_d_out = d_out;				// Constant
				tube_inputs.m_d_in = d_in;					// Constant
				tube_inputs.m_flux = tube_flux_map(i-1,0);
				tube_inputs.m_h_conv = h_conv_ave[i-1];

				tube_slice.calc_ID_OD_perf_and_lifetime(tube_inputs, tube_outputs);
				
				double inner_total_damage = tube_outputs.s_ID_lifetime_outputs.m_total_damage;
				double outer_total_damage = tube_outputs.s_OD_lifetime_outputs.m_total_damage;
				total_damage = max(total_damage, max(inner_total_damage, outer_total_damage));
			}

			if( total_damage <= 1.0 )
				search_min_th = false;

		} while(search_min_th);

		//// Report results
		// Should have either:
		//     * Minimum allowable thickness w/ corresponding lifetimes & performance
		//            OR
		//     * is_deltaP_too_large = true, and there is no case that satisfies constraints



		// **************************************************************
		// **************************************************************
		// **************************************************************

		/*
		cycle_design_parameters rc_des_par;

		rc_des_par.m_mc_type = 1;
		rc_des_par.m_rc_type = 1;

		rc_des_par.m_W_dot_net = 10.0 * 1000.0;				//[kW]
		rc_des_par.m_T_mc_in = 32.0 + 273.15;				//[K]
		rc_des_par.m_T_t_in = 550.0 + 273.15;				//[K]
		
		//(cold, hot) positive values are absolute [kPa], negative values are relative (-)
		rc_des_par.m_DP_LT[0] = 0.0;
		rc_des_par.m_DP_LT[1] = 0.0;
		rc_des_par.m_DP_HT[0] = 0.0;
		rc_des_par.m_DP_HT[1] = 0.0;
		rc_des_par.m_DP_PC[0] = 0.0;
		rc_des_par.m_DP_PC[1] = 0.0;
		rc_des_par.m_DP_PHX[0] = 0.0;
		rc_des_par.m_DP_PHX[1] = 0.0;

		rc_des_par.m_N_t = -1.0;
		rc_des_par.m_eta_mc = 0.89;
		rc_des_par.m_eta_rc = 0.89;
		rc_des_par.m_eta_t = 0.9;
		rc_des_par.m_N_sub_hxrs = 20;
		rc_des_par.m_tol = 1.E-6;
		rc_des_par.m_opt_tol = 1.E-6;

		double UA_LT = 500.0;
		double UA_HT = 500.0;

		rc_des_par.m_fixed_LT_frac = true;
		rc_des_par.m_UA_rec_total = UA_LT + UA_HT;
		rc_des_par.m_LT_frac = UA_LT / rc_des_par.m_UA_rec_total;
		rc_des_par.m_LT_frac_guess = 0.5;

		double P_mc_in = 7.69*1000.0;
		double P_mc_out = 20.0*1000.0;		
		
		rc_des_par.m_fixed_P_mc_out = true;
		rc_des_par.m_P_mc_out = P_mc_out;
		rc_des_par.m_P_high_limit = 25.0*1000.0;
		rc_des_par.m_P_mc_out_guess = rc_des_par.m_P_mc_out;

		rc_des_par.m_fixed_PR_HP_to_LP = true;
		rc_des_par.m_PR_mc = P_mc_out / P_mc_in;
		rc_des_par.m_PR_HP_to_LP_guess = rc_des_par.m_PR_mc;

		rc_des_par.m_fixed_recomp_frac = false;
		rc_des_par.m_recomp_frac = 0.5;
		rc_des_par.m_recomp_frac_guess = 0.5;

		RecompCycle rc_cycle(rc_des_par);

		//bool cycle_success = rc_cycle.optimal_design();

		bool auto_cycle_success = rc_cycle.auto_optimal_design();

		cycle_opt_off_des_inputs rc_opt_off_des_in;
		rc_opt_off_des_in.m_T_mc_in = 40.0 + 273.15;
		rc_opt_off_des_in.m_T_t_in = rc_cycle.get_cycle_design_parameters()->m_T_t_in;
		rc_opt_off_des_in.m_W_dot_net_target = rc_cycle.get_cycle_design_parameters()->m_W_dot_net;
		rc_opt_off_des_in.m_N_sub_hxrs = rc_cycle.get_cycle_design_parameters()->m_N_sub_hxrs;
			// Optimize recompression fraction?
		rc_opt_off_des_in.m_fixed_recomp_frac = false;
		rc_opt_off_des_in.m_recomp_frac_guess = rc_cycle.get_cycle_design_parameters()->m_recomp_frac;
			// Optimize main compressor speed?
		rc_opt_off_des_in.m_fixed_N_mc = false;
		rc_opt_off_des_in.m_N_mc_guess = rc_cycle.get_cycle_design_metrics()->m_N_mc;
			// Optimize turbine speed?
		rc_opt_off_des_in.m_fixed_N_t = true;
		rc_opt_off_des_in.m_N_t = rc_cycle.get_cycle_design_parameters()->m_N_t;
			// Tolerances
		rc_opt_off_des_in.m_tol = rc_cycle.get_cycle_design_parameters()->m_tol;
		rc_opt_off_des_in.m_opt_tol = rc_cycle.get_cycle_design_parameters()->m_opt_tol;
		bool od_opt_cycle_success = rc_cycle.optimal_off_design(rc_opt_off_des_in);
		*/

		return 0;
	}

	virtual int call(double /*time*/, double /*step*/, int /*ncall*/)
	{
		return 0;
	}

	virtual int converged(double /*time*/)
	{

		return 0;
	}

};

TCS_IMPLEMENT_TYPE(sco2_test_type401, "Basic heliostat field", "Ty Neises", 1, sco2_test_type401_variables, NULL, 1)

