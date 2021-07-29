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

#include "sco2_partialcooling_cycle.h"

#include <algorithm>
#include <string>

#include "nlopt.hpp"
#include "fmin.h"

int C_PartialCooling_Cycle::design(S_des_params & des_par_in)
{
	ms_des_par = des_par_in;

	return design_core();
}

int C_PartialCooling_Cycle::design_core()
{
	// Apply scaling to the turbomachinery here
	mc_mc.m_r_W_dot_scale = ms_des_par.m_W_dot_net / 10.E3;	//[-]
	mc_rc.m_r_W_dot_scale = mc_mc.m_r_W_dot_scale;			//[-]
	mc_pc.m_r_W_dot_scale = mc_mc.m_r_W_dot_scale;			//[-]
	mc_t.m_r_W_dot_scale = mc_mc.m_r_W_dot_scale;			//[-]

	// Check that the recompression fraction is not too close to 0
	if (ms_des_par.m_recomp_frac < 0.01)
	{
		ms_des_par.m_recomp_frac = 0.0;
		double UA_tot = ms_des_par.m_LTR_UA + ms_des_par.m_HTR_UA;		//[kW/K]
		ms_des_par.m_LTR_UA = UA_tot;		//[kW/K]
		ms_des_par.m_HTR_UA = 0.0;			//[kW/K]
	}

	// Initialize Recuperators
    mc_LTR.initialize(ms_des_par.m_LTR_N_sub_hxrs, ms_des_par.m_LTR_od_UA_target_type); 
	mc_HTR.initialize(ms_des_par.m_HTR_N_sub_hxrs, ms_des_par.m_HTR_od_UA_target_type);

	// Initialize known temps and pressures from design parameters
	m_temp_last[MC_IN] = ms_des_par.m_T_mc_in;	//[K]
	m_pres_last[MC_IN] = ms_des_par.m_P_mc_in;	//[kPa]
	m_temp_last[PC_IN] = ms_des_par.m_T_pc_in;	//[K]
	m_pres_last[PC_IN] = ms_des_par.m_P_pc_in;	//[kPa]
	m_temp_last[TURB_IN] = ms_des_par.m_T_t_in;	//[K]
	m_pres_last[MC_OUT] = ms_des_par.m_P_mc_out;//[kPa]

	// Apply design pressure drops to heat exchangers to fully define pressures at all states
	if (ms_des_par.m_DP_LTR[0] < 0.0)
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT] * (1.0 - fabs(ms_des_par.m_DP_LTR[0]));	//[kPa]
	else
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT] - ms_des_par.m_DP_LTR[0];		//[kPa]

	if ((ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA && ms_des_par.m_LTR_UA < 1.0E-12)
		|| (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_UA && ms_des_par.m_LTR_UA < 1.0E-12)
		|| (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_MIN_DT && ms_des_par.m_LTR_min_dT < 1.0E-12)
		|| (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_EFFECTIVENESS && ms_des_par.m_LTR_eff_target < 1.0E-12))
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT];	//[kPa] If no LTR then no pressure drop

	m_pres_last[MIXER_OUT] = m_pres_last[LTR_HP_OUT];	//[kPa] assume no pressure drop in mixer
	m_pres_last[RC_OUT] = m_pres_last[LTR_HP_OUT];		//[kPa] assume no pressure drop in mixer

	if (ms_des_par.m_DP_HTR[0] < 0.0)
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT] * (1.0 - fabs(ms_des_par.m_DP_HTR[0]));	//[kPa]
	else
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT] - ms_des_par.m_DP_HTR[0];	//[kPa]

	if ((ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA && ms_des_par.m_HTR_UA < 1.0E-12)
		|| (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_UA && ms_des_par.m_HTR_UA < 1.0E-12)
		|| (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_MIN_DT && ms_des_par.m_HTR_min_dT < 1.0E-12)
		|| (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_EFFECTIVENESS && ms_des_par.m_HTR_eff_target < 1.0E-12))
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT];	//[kPa] If no HTR then no pressure drop

	if (ms_des_par.m_DP_PHX[0] < 0.0)
		m_pres_last[TURB_IN] = m_pres_last[HTR_HP_OUT] * (1.0 - fabs(ms_des_par.m_DP_PHX[0]));	//[kPa]
	else
		m_pres_last[TURB_IN] = m_pres_last[HTR_HP_OUT] - ms_des_par.m_DP_PHX[0];	//[kPa]

	if (ms_des_par.m_DP_PC_IP[1] < 0.0)
		m_pres_last[PC_OUT] = m_pres_last[MC_IN] / (1.0 - fabs(ms_des_par.m_DP_PC_IP[1]));	//[kPa]
	else
		m_pres_last[PC_OUT] = m_pres_last[MC_IN] + ms_des_par.m_DP_PC_IP[1];	//[kPa]

	if (ms_des_par.m_DP_PC_LP[1] < 0.0)
		m_pres_last[LTR_LP_OUT] = m_pres_last[PC_IN] / (1.0 - fabs(ms_des_par.m_DP_PC_LP[1]));	//[kPa]
	else
		m_pres_last[LTR_LP_OUT] = m_pres_last[PC_IN] + ms_des_par.m_DP_PC_LP[1];	//[kPa]

	if (ms_des_par.m_DP_LTR[1] < 0.0)
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT] / (1.0 - fabs(ms_des_par.m_DP_LTR[1]));	//[kPa]
	else
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT] + ms_des_par.m_DP_LTR[1];		//[kPa]

	if ((ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA && ms_des_par.m_LTR_UA < 1.0E-12)
		|| (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_UA && ms_des_par.m_LTR_UA < 1.0E-12)
		|| (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_MIN_DT && ms_des_par.m_LTR_min_dT < 1.0E-12)
		|| (ms_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::TARGET_EFFECTIVENESS && ms_des_par.m_LTR_eff_target < 1.0E-12))
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT];	//[kPa] if no LTR then no pressure drop

	if (ms_des_par.m_DP_HTR[1] < 0.0)
		m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT] / (1.0 - fabs(ms_des_par.m_DP_HTR[1]));	//[kPa]
	else
		m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT] + ms_des_par.m_DP_HTR[1];	//[kPa]

	if ((ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA && ms_des_par.m_HTR_UA < 1.0E-12)
		|| (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_UA && ms_des_par.m_HTR_UA < 1.0E-12)
		|| (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_MIN_DT && ms_des_par.m_HTR_min_dT < 1.0E-12)
		|| (ms_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::TARGET_EFFECTIVENESS && ms_des_par.m_HTR_eff_target < 1.0E-12))
		m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT];

	// Calculate equivalent isentropic efficiencies for turbomachinery, if necessary
	double eta_mc_isen = ms_des_par.m_eta_mc;		//[-]
	if (ms_des_par.m_eta_mc < 0.0)
	{
		int poly_error_code = 0;

		isen_eta_from_poly_eta(m_temp_last[MC_IN], m_pres_last[MC_IN], m_pres_last[MC_OUT], fabs(ms_des_par.m_eta_mc),
			true, poly_error_code, eta_mc_isen);

		if (poly_error_code != 0)
			return poly_error_code;
	}

	double eta_rc_isen = ms_des_par.m_eta_rc;		//[-]
	if (ms_des_par.m_eta_rc < 0.0)
	{
		int poly_error_code = 0;

		isen_eta_from_poly_eta(m_temp_last[PC_OUT], m_pres_last[PC_OUT], m_pres_last[RC_OUT], fabs(ms_des_par.m_eta_rc),
			true, poly_error_code, eta_rc_isen);

		if (poly_error_code != 0)
			return poly_error_code;
	}

	double eta_pc_isen = ms_des_par.m_eta_pc;		//[-]
	if (ms_des_par.m_eta_pc < 0.0)
	{
		int poly_error_code = 0;

		isen_eta_from_poly_eta(m_temp_last[PC_IN], m_pres_last[PC_IN], m_pres_last[PC_OUT], fabs(ms_des_par.m_eta_pc),
			true, poly_error_code, eta_pc_isen);

		if (poly_error_code != 0)
			return poly_error_code;
	}

	double eta_t_isen = ms_des_par.m_eta_t;		//[-]
	if (ms_des_par.m_eta_t < 0.0)
	{
		int poly_error_code = 0;

		isen_eta_from_poly_eta(m_temp_last[TURB_IN], m_pres_last[TURB_IN], m_pres_last[TURB_OUT], fabs(ms_des_par.m_eta_t),
			false, poly_error_code, eta_t_isen);

		if (poly_error_code != 0)
			return poly_error_code;
	}

	// Determine the outlet state and specific work for the turbomachinery
	int comp_error_code = 0;
	double w_mc = std::numeric_limits<double>::quiet_NaN();
	calculate_turbomachinery_outlet_1(m_temp_last[MC_IN], m_pres_last[MC_IN], m_pres_last[MC_OUT], eta_mc_isen, true,
		comp_error_code, m_enth_last[MC_IN], m_entr_last[MC_IN], m_dens_last[MC_IN], m_temp_last[MC_OUT],
		m_enth_last[MC_OUT], m_entr_last[MC_OUT], m_dens_last[MC_OUT], w_mc);

	if (comp_error_code != 0)
		return comp_error_code;

	double w_pc = std::numeric_limits<double>::quiet_NaN();
	calculate_turbomachinery_outlet_1(m_temp_last[PC_IN], m_pres_last[PC_IN], m_pres_last[PC_OUT], eta_pc_isen, true,
		comp_error_code, m_enth_last[PC_IN], m_entr_last[PC_IN], m_dens_last[PC_IN], m_temp_last[PC_OUT],
		m_enth_last[PC_OUT], m_entr_last[PC_OUT], m_dens_last[PC_OUT], w_pc);

	if (comp_error_code != 0)
		return comp_error_code;

	double w_rc = 0.0;
	if (ms_des_par.m_recomp_frac >= 1.E-12)
	{
		calculate_turbomachinery_outlet_1(m_temp_last[PC_OUT], m_pres_last[PC_OUT], m_pres_last[RC_OUT], eta_rc_isen, true,
			comp_error_code, m_enth_last[PC_OUT], m_entr_last[PC_OUT], m_dens_last[PC_OUT], m_temp_last[RC_OUT],
			m_enth_last[RC_OUT], m_entr_last[RC_OUT], m_dens_last[RC_OUT], w_rc);

		if (comp_error_code != 0)
			return comp_error_code;
	}

	double w_t = std::numeric_limits<double>::quiet_NaN();
	calculate_turbomachinery_outlet_1(m_temp_last[TURB_IN], m_pres_last[TURB_IN], m_pres_last[TURB_OUT], eta_t_isen, false,
		comp_error_code, m_enth_last[TURB_IN], m_entr_last[TURB_IN], m_dens_last[TURB_IN], m_temp_last[TURB_OUT],
		m_enth_last[TURB_OUT], m_entr_last[TURB_OUT], m_dens_last[TURB_OUT], w_t);

	if (comp_error_code != 0)
		return comp_error_code;

	// know all turbomachinery specific work, so can calculate mass flow rate required to hit target power
	m_m_dot_t = ms_des_par.m_W_dot_net / (w_t + w_pc + ms_des_par.m_recomp_frac*w_rc + (1.0 - ms_des_par.m_recomp_frac)*w_mc);	//[kg/s]
	
	if (m_m_dot_t <= 0.0 || !std::isfinite(m_m_dot_t))	// positive net power is impossible; return an error
		return 25;

	m_m_dot_pc = m_m_dot_t;	//[kg/s]
	m_m_dot_rc = m_m_dot_t * ms_des_par.m_recomp_frac;	//[kg/s]
	m_m_dot_mc = m_m_dot_t *(1.0 - ms_des_par.m_recomp_frac);	//[kg/s]

	// Solve the recuperator performance
	double T_HTR_LP_out_lower = m_temp_last[MC_OUT];		//[K] Coldest possible temperature
	double T_HTR_LP_out_upper = m_temp_last[TURB_OUT];		//[K] Hottest possible temperature

	double T_HTR_LP_out_guess_lower = std::min(T_HTR_LP_out_upper - 2.0, std::max(T_HTR_LP_out_lower+15.0, 220.0 + 273.15));	//[K] There is nothing magic about 15
	double T_HTR_LP_out_guess_upper = std::min(T_HTR_LP_out_guess_lower + 20.0, T_HTR_LP_out_upper - 1.0);		//[K] There is nothing magic about 20

	C_MEQ_HTR_des HTR_des_eq(this);
	C_monotonic_eq_solver HTR_des_solver(HTR_des_eq);

	HTR_des_solver.settings(ms_des_par.m_des_tol*m_temp_last[MC_IN], 1000, T_HTR_LP_out_lower, T_HTR_LP_out_upper, false);

	double T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved;
	T_HTR_LP_out_solved = tol_T_HTR_LP_out_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_HTR_LP_out = -1;

	int T_HTR_LP_out_code = HTR_des_solver.solve(T_HTR_LP_out_guess_lower, T_HTR_LP_out_guess_upper, 0,
								T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved, iter_T_HTR_LP_out);

	if (T_HTR_LP_out_code != C_monotonic_eq_solver::CONVERGED)
	{
		return 35;
	}

	double Q_dot_HTR = HTR_des_eq.m_Q_dot_HTR;		//[kWt]

	// Can now define HTR HP outlet state
	m_enth_last[HTR_HP_OUT] = m_enth_last[MIXER_OUT] + Q_dot_HTR / m_m_dot_t;		//[kJ/kg]
	int prop_error_code = CO2_PH(m_pres_last[HTR_HP_OUT], m_enth_last[HTR_HP_OUT], &mc_co2_props);
	if (prop_error_code != 0)
	{
		return prop_error_code;
	}
	m_temp_last[HTR_HP_OUT] = mc_co2_props.temp;	//[K]
	m_entr_last[HTR_HP_OUT] = mc_co2_props.entr;	//[kJ/kg-K]
	m_dens_last[HTR_HP_OUT] = mc_co2_props.dens;	//[kg/m^3]

	// Set design values for PHX and coolers
	C_HeatExchanger::S_design_parameters PHX_des_par;
	PHX_des_par.m_DP_design[0] = m_pres_last[HTR_HP_OUT] - m_pres_last[TURB_IN];		//[kPa]
	PHX_des_par.m_DP_design[1] = 0.0;
	PHX_des_par.m_m_dot_design[0] = m_m_dot_t;		//[kg/s]
	PHX_des_par.m_m_dot_design[1] = 0.0;			//[kg/s]
	PHX_des_par.m_Q_dot_design = m_m_dot_t*(m_enth_last[TURB_IN] - m_enth_last[HTR_HP_OUT]);	//[kWt]
	mc_PHX.initialize(PHX_des_par);

	C_HeatExchanger::S_design_parameters PC_full_des_par;
	PC_full_des_par.m_DP_design[0] = 0.0;
	PC_full_des_par.m_DP_design[1] = m_pres_last[LTR_LP_OUT] - m_pres_last[PC_IN];	//[kPa]
	PC_full_des_par.m_m_dot_design[0] = 0.0;		//[kg/s]
	PC_full_des_par.m_m_dot_design[1] = m_m_dot_pc;	//[kg/s]
	PC_full_des_par.m_Q_dot_design = m_m_dot_pc*(m_enth_last[LTR_LP_OUT] - m_enth_last[PC_IN]);	//[kWt]
	mc_cooler_pc.initialize(PC_full_des_par);

	C_HeatExchanger::S_design_parameters PC_partial_des_par;
	PC_partial_des_par.m_DP_design[0] = 0.0;
	PC_partial_des_par.m_DP_design[1] = m_pres_last[PC_OUT] - m_pres_last[MC_IN];	//[kPa]
	PC_partial_des_par.m_m_dot_design[0] = 0.0;
	PC_partial_des_par.m_m_dot_design[1] = m_m_dot_mc;	//[kg/s]
	PC_partial_des_par.m_Q_dot_design = m_m_dot_mc*(m_enth_last[PC_OUT] - m_enth_last[MC_IN]);	//[kWt]
	mc_cooler_mc.initialize(PC_partial_des_par);

	// Calculate and set cycle performance metrics
	m_W_dot_t = m_m_dot_t*w_t;		//[kWe]
	m_W_dot_pc = m_m_dot_pc*w_pc;	//[kWe]
	m_W_dot_rc = m_m_dot_rc*w_rc;	//[kWe]
	m_W_dot_mc = m_m_dot_mc*w_mc;	//[kWe]
	m_W_dot_net_last = m_m_dot_t*w_t + m_m_dot_pc*w_pc + m_m_dot_rc*w_rc + m_m_dot_mc*w_mc;		//[kWe]
	m_eta_thermal_calc_last = m_W_dot_net_last / PHX_des_par.m_Q_dot_design;	//[-]
	m_energy_bal_last = (PHX_des_par.m_Q_dot_design - m_W_dot_net_last - PC_partial_des_par.m_Q_dot_design - PC_full_des_par.m_Q_dot_design) / PHX_des_par.m_Q_dot_design;	//[-]

	if (ms_des_par.m_des_objective_type == 2)
	{
		double phx_deltaT = m_temp_last[TURB_IN] - m_temp_last[HTR_HP_OUT];
		double under_min_deltaT = std::max(0.0, ms_des_par.m_min_phx_deltaT - phx_deltaT);
		double eta_deltaT_scale = std::exp(-under_min_deltaT);
		m_objective_metric_last = m_eta_thermal_calc_last * eta_deltaT_scale;
	}
	else
	{
		m_objective_metric_last = m_eta_thermal_calc_last;
	}

	return 0;
}

int C_PartialCooling_Cycle::C_MEQ_HTR_des::operator()(double T_HTR_LP_out /*K*/, double *diff_T_HTR_LP_out /*K*/)
{
	m_Q_dot_LTR = m_Q_dot_HTR = std::numeric_limits<double>::quiet_NaN();

	mpc_pc_cycle->m_temp_last[HTR_LP_OUT] = T_HTR_LP_out;	//[K]

	int prop_error_code = CO2_TP(mpc_pc_cycle->m_temp_last[HTR_LP_OUT], mpc_pc_cycle->m_pres_last[HTR_LP_OUT], &mpc_pc_cycle->mc_co2_props);
	if (prop_error_code != 0)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_pc_cycle->m_enth_last[HTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.enth;	//[kJ/kg]
	mpc_pc_cycle->m_entr_last[HTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_pc_cycle->m_dens_last[HTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.dens;	//[kg/m^3]

	try
	{
		mpc_pc_cycle->mc_LTR.design_for_target__calc_outlet(mpc_pc_cycle->ms_des_par.m_LTR_target_code,
            mpc_pc_cycle->ms_des_par.m_LTR_UA, mpc_pc_cycle->ms_des_par.m_LTR_min_dT, mpc_pc_cycle->ms_des_par.m_LTR_eff_target,
            mpc_pc_cycle->ms_des_par.m_LTR_eff_max,
			mpc_pc_cycle->m_temp_last[MC_OUT], mpc_pc_cycle->m_pres_last[MC_OUT], mpc_pc_cycle->m_m_dot_mc, mpc_pc_cycle->m_pres_last[LTR_HP_OUT],
			mpc_pc_cycle->m_temp_last[HTR_LP_OUT], mpc_pc_cycle->m_pres_last[HTR_LP_OUT], mpc_pc_cycle->m_m_dot_t, mpc_pc_cycle->m_pres_last[LTR_LP_OUT],
            mpc_pc_cycle->ms_des_par.m_des_tol,
            m_Q_dot_LTR, mpc_pc_cycle->m_temp_last[LTR_HP_OUT], mpc_pc_cycle->m_temp_last[LTR_LP_OUT]);
	}
	catch (C_csp_exception &)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return -2;
	}

	prop_error_code = CO2_TP(mpc_pc_cycle->m_temp_last[LTR_LP_OUT], mpc_pc_cycle->m_pres_last[LTR_LP_OUT], &mpc_pc_cycle->mc_co2_props);
	if (prop_error_code)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_pc_cycle->m_enth_last[LTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.enth;	//[kJ/kg]
	mpc_pc_cycle->m_entr_last[LTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_pc_cycle->m_dens_last[LTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.dens;	//[kg/m^3]

	// *****************************************************************************
		// Energy balance on the LTR HP stream
	mpc_pc_cycle->m_enth_last[LTR_HP_OUT] = mpc_pc_cycle->m_enth_last[MC_OUT] + m_Q_dot_LTR / mpc_pc_cycle->m_m_dot_mc;	//[kJ/kg]
	prop_error_code = CO2_PH(mpc_pc_cycle->m_pres_last[LTR_HP_OUT], mpc_pc_cycle->m_enth_last[LTR_HP_OUT], &mpc_pc_cycle->mc_co2_props);
	if (prop_error_code != 0)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_pc_cycle->m_temp_last[LTR_HP_OUT] = mpc_pc_cycle->mc_co2_props.temp;	//[K]
	mpc_pc_cycle->m_entr_last[LTR_HP_OUT] = mpc_pc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_pc_cycle->m_dens_last[LTR_HP_OUT] = mpc_pc_cycle->mc_co2_props.dens;	//[kg/m^3]

	// Solve enthalpy balance for the mixer
	if (mpc_pc_cycle->ms_des_par.m_recomp_frac >= 1.E-12)
	{
		mpc_pc_cycle->m_enth_last[MIXER_OUT] = (1.0 - mpc_pc_cycle->ms_des_par.m_recomp_frac)*mpc_pc_cycle->m_enth_last[LTR_HP_OUT] + mpc_pc_cycle->ms_des_par.m_recomp_frac*mpc_pc_cycle->m_enth_last[RC_OUT];	//[kJ/kg]
		prop_error_code = CO2_PH(mpc_pc_cycle->m_pres_last[MIXER_OUT], mpc_pc_cycle->m_enth_last[MIXER_OUT], &mpc_pc_cycle->mc_co2_props);
		if (prop_error_code != 0)
		{
			*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return prop_error_code;
		}
		mpc_pc_cycle->m_temp_last[MIXER_OUT] = mpc_pc_cycle->mc_co2_props.temp;		//[K]
		mpc_pc_cycle->m_entr_last[MIXER_OUT] = mpc_pc_cycle->mc_co2_props.entr;		//[kJ/kg-K]
		mpc_pc_cycle->m_dens_last[MIXER_OUT] = mpc_pc_cycle->mc_co2_props.dens;		//[kg/m^3]
	}
	else
	{
		mpc_pc_cycle->m_temp_last[MIXER_OUT] = mpc_pc_cycle->m_temp_last[LTR_HP_OUT];	//[K]
		mpc_pc_cycle->m_enth_last[MIXER_OUT] = mpc_pc_cycle->m_enth_last[LTR_HP_OUT];	//[kJ/kg]
		mpc_pc_cycle->m_entr_last[MIXER_OUT] = mpc_pc_cycle->m_entr_last[LTR_HP_OUT];	//[kJ/kg-K]
		mpc_pc_cycle->m_dens_last[MIXER_OUT] = mpc_pc_cycle->m_dens_last[LTR_HP_OUT];	//[kg/m^3]
	}

	// Calculate the HTR design performance
	double T_HTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();

	try
	{
		mpc_pc_cycle->mc_HTR.design_for_target__calc_outlet(mpc_pc_cycle->ms_des_par.m_HTR_target_code,
            mpc_pc_cycle->ms_des_par.m_HTR_UA, mpc_pc_cycle->ms_des_par.m_HTR_min_dT, mpc_pc_cycle->ms_des_par.m_HTR_eff_target,
            mpc_pc_cycle->ms_des_par.m_HTR_eff_max,
			mpc_pc_cycle->m_temp_last[MIXER_OUT], mpc_pc_cycle->m_pres_last[MIXER_OUT], mpc_pc_cycle->m_m_dot_t, mpc_pc_cycle->m_pres_last[HTR_HP_OUT],
			mpc_pc_cycle->m_temp_last[TURB_OUT], mpc_pc_cycle->m_pres_last[TURB_OUT], mpc_pc_cycle->m_m_dot_t, mpc_pc_cycle->m_pres_last[HTR_LP_OUT],
            mpc_pc_cycle->ms_des_par.m_des_tol,
            m_Q_dot_HTR, mpc_pc_cycle->m_temp_last[HTR_HP_OUT], T_HTR_LP_out_calc);
	}
	catch (C_csp_exception &)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}

	*diff_T_HTR_LP_out = T_HTR_LP_out_calc - mpc_pc_cycle->m_temp_last[HTR_LP_OUT];		//[K]

	return 0;

}

int C_PartialCooling_Cycle::C_MEQ_LTR_des::operator()(double T_LTR_LP_out /*K*/, double *diff_T_LTR_LP_out /*K*/)
{
	m_Q_dot_LTR = std::numeric_limits<double>::quiet_NaN();		//[kWt]
	
	mpc_pc_cycle->m_temp_last[LTR_LP_OUT] = T_LTR_LP_out;		//[K]

	int prop_error_code = CO2_TP(mpc_pc_cycle->m_temp_last[LTR_LP_OUT], mpc_pc_cycle->m_pres_last[LTR_LP_OUT], &mpc_pc_cycle->mc_co2_props);
	if (prop_error_code)
	{
		*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_pc_cycle->m_enth_last[LTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.enth;	//[kJ/kg]
	mpc_pc_cycle->m_entr_last[LTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_pc_cycle->m_dens_last[LTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.dens;	//[kg/m^3]

	double T_LTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();

	try
	{
		mpc_pc_cycle->mc_LTR.design_for_target__calc_outlet(mpc_pc_cycle->ms_des_par.m_LTR_target_code,
            mpc_pc_cycle->ms_des_par.m_LTR_UA, mpc_pc_cycle->ms_des_par.m_LTR_min_dT, mpc_pc_cycle->ms_des_par.m_LTR_eff_target,
            mpc_pc_cycle->ms_des_par.m_LTR_eff_max,
			mpc_pc_cycle->m_temp_last[MC_OUT], mpc_pc_cycle->m_pres_last[MC_OUT], mpc_pc_cycle->m_m_dot_mc, mpc_pc_cycle->m_pres_last[LTR_HP_OUT],
			mpc_pc_cycle->m_temp_last[HTR_LP_OUT], mpc_pc_cycle->m_pres_last[HTR_LP_OUT], mpc_pc_cycle->m_m_dot_t, mpc_pc_cycle->m_pres_last[LTR_LP_OUT],
            mpc_pc_cycle->ms_des_par.m_des_tol,
            m_Q_dot_LTR, mpc_pc_cycle->m_temp_last[LTR_HP_OUT], T_LTR_LP_out_calc);
	}
	catch (C_csp_exception &)
	{
		*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();

		return -1;
	}

	*diff_T_LTR_LP_out = T_LTR_LP_out_calc - mpc_pc_cycle->m_temp_last[LTR_LP_OUT];

	return 0;
}

double C_PartialCooling_Cycle::opt_eta_fixed_P_high(double P_high_opt /*kPa*/)
{
	// Complete 'ms_opt_des_par'
	ms_opt_des_par.m_P_mc_out_guess = P_high_opt;	//[kPa]
	ms_opt_des_par.m_fixed_P_mc_out = true;

	ms_opt_des_par.m_fixed_PR_total = false;
	ms_opt_des_par.m_PR_total_guess = 25. / 6.5;	//[-] Guess could be improved...

    if (ms_auto_opt_des_par.m_fixed_f_PR_HP_to_IP)
    {
        ms_opt_des_par.m_fixed_f_PR_mc = true;
        ms_opt_des_par.m_f_PR_mc_guess = ms_auto_opt_des_par.m_fixed_f_PR_HP_to_IP; //[-]
    }
    else
    {
        ms_opt_des_par.m_fixed_f_PR_mc = false;
        ms_opt_des_par.m_f_PR_mc_guess = (25. - 8.5) / (25. - 6.5);		//[-] Guess could be improved...
    }

    // Is the recompression fraction fixed or optimized?
    if (ms_auto_opt_des_par.m_is_recomp_ok < 0.0)
    {
        ms_opt_des_par.m_recomp_frac_guess = fabs(ms_auto_opt_des_par.m_is_recomp_ok);  //[-]
        ms_opt_des_par.m_fixed_recomp_frac = true;
    }
    else
    {
        ms_opt_des_par.m_recomp_frac_guess = 0.25;	//[-]
        ms_opt_des_par.m_fixed_recomp_frac = false;
    }	

	ms_opt_des_par.m_LTR_frac_guess = 0.5;		//[-]
	ms_opt_des_par.m_fixed_LTR_frac = false;

    if (ms_opt_des_par.m_LTR_target_code != NS_HX_counterflow_eqs::OPTIMIZE_UA || ms_opt_des_par.m_HTR_target_code != NS_HX_counterflow_eqs::OPTIMIZE_UA)
    {
        ms_opt_des_par.m_fixed_LTR_frac = true;
    }

	int pc_error_code = opt_design_core();

	double local_objective_metric = 0.0;
	if (pc_error_code == 0)
		local_objective_metric = m_objective_metric_opt;

	if (pc_error_code == 0 && m_objective_metric_opt > m_objective_metric_auto_opt)
	{
		ms_des_par_auto_opt = ms_des_par_optimal;
		m_objective_metric_auto_opt = m_objective_metric_opt;
	}

	return -local_objective_metric;
}

int C_PartialCooling_Cycle::finalize_design()
{
	int mc_des_err = mc_mc.design_given_outlet_state(ms_des_par.m_mc_comp_model_code, m_temp_last[MC_IN],
										m_pres_last[MC_IN],
										m_m_dot_mc,
										m_temp_last[MC_OUT],
										m_pres_last[MC_OUT], ms_des_par.m_des_tol);

	if (mc_des_err != 0)
	{
		return 71;
	}

	int pc_des_err = mc_pc.design_given_outlet_state(ms_des_par.m_pc_comp_model_code, m_temp_last[PC_IN],
										m_pres_last[PC_IN],
										m_m_dot_pc,
										m_temp_last[PC_OUT],
										m_pres_last[PC_OUT], ms_des_par.m_des_tol);

	if (pc_des_err != 0)
	{
		return 72;
	}

	if (ms_des_par.m_recomp_frac > 0.01)
	{
		int rc_des_err = mc_rc.design_given_outlet_state(ms_des_par.m_rc_comp_model_code, m_temp_last[PC_OUT],
										m_pres_last[PC_OUT],
										m_m_dot_rc,
										m_temp_last[RC_OUT],
										m_pres_last[RC_OUT], ms_des_par.m_des_tol);

		if (rc_des_err != 0)
		{
			return 73;
		}

		ms_des_solved.m_is_rc = true;
	}
	else
		ms_des_solved.m_is_rc = false;

	C_turbine::S_design_parameters t_des_par;
		// Set turbine shaft speed
	t_des_par.m_N_design = ms_des_par.m_N_turbine;		//[rpm]
	t_des_par.m_N_comp_design_if_linked = mc_mc.get_design_solved()->m_N_design;	//[rpm]
		// Turbine inlet state
	t_des_par.m_P_in = m_pres_last[TURB_IN];	//[kPa]
	t_des_par.m_T_in = m_temp_last[TURB_IN];	//[K]
	t_des_par.m_D_in = m_dens_last[TURB_IN];	//[kg/m^3]
	t_des_par.m_h_in = m_enth_last[TURB_IN];	//[kJ/kg]
	t_des_par.m_s_in = m_entr_last[TURB_IN];	//[kJ/kg-K]
		// Turbine outlet state
	t_des_par.m_P_out = m_pres_last[TURB_OUT];	//[kPa]
	t_des_par.m_h_out = m_enth_last[TURB_OUT];	//[kJ/kg]
		// Mass flow
	t_des_par.m_m_dot = m_m_dot_t;		//[kg/s]

	int turb_size_err = 0;
	mc_t.turbine_sizing(t_des_par, turb_size_err);
	if (turb_size_err != 0)
	{
		return 74;
	}

	// Design air coolers
        // Calculate cooler duties
    double q_dot_IP_local = m_m_dot_mc*(m_enth_last[PC_OUT] - m_enth_last[MC_IN]);      //[kWt]
    double q_dot_LP_local = m_m_dot_t*(m_enth_last[LTR_LP_OUT] - m_enth_last[PC_IN]);   //[kWt]
    double f_W_dot_fan_to_IP = q_dot_IP_local / (q_dot_IP_local + q_dot_LP_local);     //[-] Fraction of total fan power allocated to the IP cooler fan
	
        // Low Pressure
		// Structure for design parameters that are dependent on cycle design solution
	C_CO2_to_air_cooler::S_des_par_cycle_dep s_LP_air_cooler_des_par_dep;
		// Set air cooler design parameters that are dependent on the cycle design solution
	s_LP_air_cooler_des_par_dep.m_T_hot_in_des = m_temp_last[C_sco2_cycle_core::LTR_LP_OUT];		//[K]
	s_LP_air_cooler_des_par_dep.m_P_hot_in_des = m_pres_last[C_sco2_cycle_core::LTR_LP_OUT];		//[kPa]
	s_LP_air_cooler_des_par_dep.m_m_dot_total = m_m_dot_pc;		//[kg/s]

	double LP_cooler_deltaP = m_pres_last[C_sco2_cycle_core::LTR_LP_OUT] - m_pres_last[C_sco2_cycle_core::PC_IN];		//[kPa]
	if (LP_cooler_deltaP == 0.0)
		s_LP_air_cooler_des_par_dep.m_delta_P_des = ms_des_par.m_deltaP_cooler_frac*m_pres_last[C_sco2_cycle_core::LTR_LP_OUT];	//[kPa]
	else
		s_LP_air_cooler_des_par_dep.m_delta_P_des = LP_cooler_deltaP;		//[kPa]

	s_LP_air_cooler_des_par_dep.m_T_hot_out_des = m_temp_last[C_sco2_cycle_core::PC_IN];			//[K]
		// Use half the rated fan power on each cooler fan
	s_LP_air_cooler_des_par_dep.m_W_dot_fan_des = ms_des_par.m_frac_fan_power*(1.0 - f_W_dot_fan_to_IP)*ms_des_par.m_W_dot_net / 1000.0;		//[MWe]
		// Structure for design parameters that are independent of cycle design solution
	C_CO2_to_air_cooler::S_des_par_ind s_LP_air_cooler_des_par_ind;
	s_LP_air_cooler_des_par_ind.m_T_amb_des = ms_des_par.m_T_amb_des;		//[K]
	s_LP_air_cooler_des_par_ind.m_elev = ms_des_par.m_elevation;			//[m]
    s_LP_air_cooler_des_par_ind.m_eta_fan = ms_des_par.m_eta_fan;           //[-]
    s_LP_air_cooler_des_par_ind.m_N_nodes_pass = ms_des_par.m_N_nodes_pass; //[-]

	if (ms_des_par.m_is_des_air_cooler && std::isfinite(ms_des_par.m_deltaP_cooler_frac) && std::isfinite(ms_des_par.m_frac_fan_power)
		&& std::isfinite(ms_des_par.m_T_amb_des) && std::isfinite(ms_des_par.m_elevation) && std::isfinite(ms_des_par.m_eta_fan) && ms_des_par.m_N_nodes_pass > 0)
	{
		mc_pc_air_cooler.design_hx(s_LP_air_cooler_des_par_ind, s_LP_air_cooler_des_par_dep, ms_des_par.m_des_tol);
	}

	// High Pressure
		// Structure for design parameters that are dependent on cycle design solution
	C_CO2_to_air_cooler::S_des_par_cycle_dep s_IP_air_cooler_des_par_dep;
		// Set air cooler design parameters that are dependent on the cycle design solution
	s_IP_air_cooler_des_par_dep.m_T_hot_in_des = m_temp_last[C_sco2_cycle_core::PC_OUT];		//[K]
	s_IP_air_cooler_des_par_dep.m_P_hot_in_des = m_pres_last[C_sco2_cycle_core::PC_OUT];		//[kPa]
	s_IP_air_cooler_des_par_dep.m_m_dot_total = m_m_dot_mc;		//[kg/s]
		
	double HP_cooler_deltaP = m_pres_last[C_sco2_cycle_core::PC_OUT] - m_pres_last[C_sco2_cycle_core::MC_IN];	//[kPa]
	if (HP_cooler_deltaP == 0.0)
		s_IP_air_cooler_des_par_dep.m_delta_P_des = ms_des_par.m_deltaP_cooler_frac*m_pres_last[C_sco2_cycle_core::PC_OUT];	//[kPa]
	else
		s_IP_air_cooler_des_par_dep.m_delta_P_des = HP_cooler_deltaP;		//[kPa]

	s_IP_air_cooler_des_par_dep.m_T_hot_out_des = m_temp_last[C_sco2_cycle_core::MC_IN];			//[K]
		// Use half the rated fan power on each cooler fan
	s_IP_air_cooler_des_par_dep.m_W_dot_fan_des = ms_des_par.m_frac_fan_power*f_W_dot_fan_to_IP*ms_des_par.m_W_dot_net / 1000.0;		//[MWe]
		// Structure for design parameters that are independent of cycle design solution
	C_CO2_to_air_cooler::S_des_par_ind s_IP_air_cooler_des_par_ind;
	s_IP_air_cooler_des_par_ind.m_T_amb_des = ms_des_par.m_T_amb_des;		//[K]
	s_IP_air_cooler_des_par_ind.m_elev = ms_des_par.m_elevation;			//[m]
    s_IP_air_cooler_des_par_ind.m_eta_fan = ms_des_par.m_eta_fan;           //[-]
    s_IP_air_cooler_des_par_ind.m_N_nodes_pass = ms_des_par.m_N_nodes_pass; //[-]

	if (ms_des_par.m_is_des_air_cooler && std::isfinite(ms_des_par.m_deltaP_cooler_frac) && std::isfinite(ms_des_par.m_frac_fan_power)
		&& std::isfinite(ms_des_par.m_T_amb_des) && std::isfinite(ms_des_par.m_elevation) && std::isfinite(ms_des_par.m_eta_fan) && ms_des_par.m_N_nodes_pass > 0)
	{
		mc_mc_air_cooler.design_hx(s_IP_air_cooler_des_par_ind, s_IP_air_cooler_des_par_dep, ms_des_par.m_des_tol);
	}


	// Get 'design_solved' structures from component classes
	ms_des_solved.ms_mc_ms_des_solved = *mc_mc.get_design_solved();
	ms_des_solved.ms_rc_ms_des_solved = *mc_rc.get_design_solved();
	ms_des_solved.ms_pc_ms_des_solved = *mc_pc.get_design_solved();
	ms_des_solved.ms_t_des_solved = *mc_t.get_design_solved();
	ms_des_solved.ms_LTR_des_solved = mc_LTR.ms_des_solved;
	ms_des_solved.ms_HTR_des_solved = mc_HTR.ms_des_solved;
	ms_des_solved.ms_pc_air_cooler = *mc_pc_air_cooler.get_design_solved();
	ms_des_solved.ms_mc_air_cooler = *mc_mc_air_cooler.get_design_solved();

	// Set solved design point metrics
	ms_des_solved.m_temp = m_temp_last;
	ms_des_solved.m_pres = m_pres_last;
	ms_des_solved.m_enth = m_enth_last;
	ms_des_solved.m_entr = m_entr_last;
	ms_des_solved.m_dens = m_dens_last;

	ms_des_solved.m_eta_thermal = m_eta_thermal_calc_last;
	ms_des_solved.m_W_dot_net = m_W_dot_net_last;
	ms_des_solved.m_m_dot_mc = m_m_dot_mc;
	ms_des_solved.m_m_dot_rc = m_m_dot_rc;
	ms_des_solved.m_m_dot_pc = m_m_dot_pc;
	ms_des_solved.m_m_dot_t = m_m_dot_t;
	ms_des_solved.m_recomp_frac = m_m_dot_rc / m_m_dot_t;

	ms_des_solved.m_UA_LTR = ms_des_par.m_LTR_UA;
	ms_des_solved.m_UA_HTR = ms_des_par.m_HTR_UA;

	ms_des_solved.m_W_dot_t = m_W_dot_t;     //[kWe]
	ms_des_solved.m_W_dot_mc = m_W_dot_mc;	 //[kWe]
	ms_des_solved.m_W_dot_rc = m_W_dot_rc;	 //[kWe]
	ms_des_solved.m_W_dot_pc = m_W_dot_pc;	 //[kWe]

	ms_des_solved.m_W_dot_cooler_tot = (mc_mc_air_cooler.get_design_solved()->m_W_dot_fan
		+ mc_pc_air_cooler.get_design_solved()->m_W_dot_fan) * 1.E3;	//[kWe] convert from MWe
	return 0;
}

double C_PartialCooling_Cycle::design_cycle_return_objective_metric(const std::vector<double> &x)
{
	int index = 0;

	// Main compressor outlet pressure
	if (!ms_opt_des_par.m_fixed_P_mc_out)
	{
		ms_des_par.m_P_mc_out = x[index];
		if (ms_des_par.m_P_mc_out > ms_opt_des_par.m_P_high_limit)
			return 0.0;
		index++;
	}
	else
		ms_des_par.m_P_mc_out = ms_opt_des_par.m_P_mc_out_guess;	//[kPa]

	// Total pressure ratio
	double PR_total_local = -999.9;
	double P_pc_in = -999.9;
	if (!ms_opt_des_par.m_fixed_PR_total)
	{
		PR_total_local = x[index];
		if (PR_total_local > 50.0)
			return 0.0;
		index++;
		P_pc_in = ms_des_par.m_P_mc_out / PR_total_local;		//[kPa]
	}
	else
	{
		if (ms_opt_des_par.m_PR_total_guess >= 0.0)
		{
			PR_total_local = ms_opt_des_par.m_PR_total_guess;
			P_pc_in = ms_des_par.m_P_mc_out / PR_total_local;	//[kPa]
		}
		else
		{
			P_pc_in = fabs(ms_opt_des_par.m_PR_total_guess);	//[kPa]
		}
	}

	if (P_pc_in >= ms_des_par.m_P_mc_out)
		return 0.0;
	if (P_pc_in <= 100.0)
		return 0.0;
	ms_des_par.m_P_pc_in = P_pc_in;		//[kPa]

	// Main compressor inlet pressure
	double P_mc_in = -999.9;
	if (!ms_opt_des_par.m_fixed_f_PR_mc)
	{
		P_mc_in = ms_des_par.m_P_mc_out - x[index] * (ms_des_par.m_P_mc_out - ms_des_par.m_P_pc_in);	//[kPa]
		index++;
	}
	else
	{
		P_mc_in = ms_des_par.m_P_mc_out - ms_opt_des_par.m_f_PR_mc_guess*(ms_des_par.m_P_mc_out - ms_des_par.m_P_pc_in);	//[kPa]
	}
	ms_des_par.m_P_mc_in = P_mc_in;		//[kPa]

	// Recompression fraction
	if (!ms_opt_des_par.m_fixed_recomp_frac)
	{
		ms_des_par.m_recomp_frac = x[index];		//[-]
		if (ms_des_par.m_recomp_frac < 0.0)
			return 0.0;
		index++;
	}
	else
		ms_des_par.m_recomp_frac = ms_opt_des_par.m_recomp_frac_guess;	//[-]

	// Recuperator split fraction
	double LTR_frac_local = -999.9;
	if (!ms_opt_des_par.m_fixed_LTR_frac)
	{
		LTR_frac_local = x[index];								//[-]
		if (LTR_frac_local > 1.0 || LTR_frac_local < 0.0)
			return 0.0;
		index++;
	}
	else
		LTR_frac_local = ms_opt_des_par.m_LTR_frac_guess;		//[-]

    if (ms_opt_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA || ms_opt_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA)
    {
        ms_des_par.m_LTR_UA = ms_opt_des_par.m_UA_rec_total*LTR_frac_local;			//[kW/K]
        ms_des_par.m_HTR_UA = ms_opt_des_par.m_UA_rec_total*(1.0 - LTR_frac_local);	//[kW/K]
    }
    else
    {
        ms_des_par.m_LTR_UA = ms_opt_des_par.m_LTR_UA;      //[kW/K]
        ms_des_par.m_HTR_UA = ms_opt_des_par.m_HTR_UA;      //[kW/K]
    }
	

	int des_err_code = design_core();

	double objective_metric = 0.0;
	if (des_err_code == 0)
	{
		objective_metric = m_objective_metric_last;

		if (m_objective_metric_last > m_objective_metric_opt)
		{
			ms_des_par_optimal = ms_des_par;
			m_objective_metric_opt = m_objective_metric_last;
		}
	}

	return objective_metric;
}

int C_PartialCooling_Cycle::opt_design_core()
{
	// Map ms_opt_des_par to ms_des_par
	ms_des_par.m_W_dot_net = ms_opt_des_par.m_W_dot_net;	//[kWe]
	ms_des_par.m_T_mc_in = ms_opt_des_par.m_T_mc_in;		//[K]
	ms_des_par.m_T_pc_in = ms_opt_des_par.m_T_pc_in;		//[K]
	ms_des_par.m_T_t_in = ms_opt_des_par.m_T_t_in;			//[K]
	ms_des_par.m_DP_LTR = ms_opt_des_par.m_DP_LTR;			//
	ms_des_par.m_DP_HTR = ms_opt_des_par.m_DP_HTR;			//
	ms_des_par.m_DP_PC_LP = ms_opt_des_par.m_DP_PC_LP;	//
	ms_des_par.m_DP_PC_IP = ms_opt_des_par.m_DP_PC_IP;	//
	ms_des_par.m_DP_PHX = ms_opt_des_par.m_DP_PHX;			//
        // LTR thermal design
    ms_des_par.m_LTR_target_code = ms_opt_des_par.m_LTR_target_code;    //[-]
    ms_des_par.m_LTR_min_dT = ms_opt_des_par.m_LTR_min_dT;      //[K]
    ms_des_par.m_LTR_eff_target = ms_opt_des_par.m_LTR_eff_target;  //[-]
    ms_des_par.m_LTR_eff_max = ms_opt_des_par.m_LTR_eff_max;    //[-]
    ms_des_par.m_LTR_N_sub_hxrs = ms_opt_des_par.m_LTR_N_sub_hxrs;  //[-]
    ms_des_par.m_LTR_od_UA_target_type = ms_opt_des_par.m_LTR_od_UA_target_type;
        // HTR thermal design
    ms_des_par.m_HTR_target_code = ms_opt_des_par.m_HTR_target_code;    //[-]
    ms_des_par.m_HTR_min_dT = ms_opt_des_par.m_HTR_min_dT;      //[K]
    ms_des_par.m_HTR_eff_target = ms_opt_des_par.m_HTR_eff_target;  //[-]
    ms_des_par.m_HTR_eff_max = ms_opt_des_par.m_HTR_eff_max;    //[-]
    ms_des_par.m_HTR_N_sub_hxrs = ms_opt_des_par.m_HTR_N_sub_hxrs;  //[-]
    ms_des_par.m_HTR_od_UA_target_type = ms_opt_des_par.m_HTR_od_UA_target_type;
        //
	ms_des_par.m_eta_mc = ms_opt_des_par.m_eta_mc;			//[-]
	ms_des_par.m_eta_rc = ms_opt_des_par.m_eta_rc;			//[-]
	ms_des_par.m_eta_pc = ms_opt_des_par.m_eta_pc;			//[-]
	ms_des_par.m_eta_t = ms_opt_des_par.m_eta_t;			//[-]
	ms_des_par.m_P_high_limit = ms_opt_des_par.m_P_high_limit;	//[kPa]
	ms_des_par.m_des_tol = ms_opt_des_par.m_des_tol;				//[-]
	ms_des_par.m_N_turbine = ms_opt_des_par.m_N_turbine;	//[rpm]

	ms_des_par.m_is_des_air_cooler = ms_opt_des_par.m_is_des_air_cooler;	//[-]
	ms_des_par.m_frac_fan_power = ms_opt_des_par.m_frac_fan_power;			//[-]
	ms_des_par.m_deltaP_cooler_frac = ms_opt_des_par.m_deltaP_cooler_frac;	//[-]
	ms_des_par.m_T_amb_des = ms_opt_des_par.m_T_amb_des;					//[K]
	ms_des_par.m_elevation = ms_opt_des_par.m_elevation;					//[m]
    ms_des_par.m_eta_fan = ms_opt_des_par.m_eta_fan;                        //[-]
    ms_des_par.m_N_nodes_pass = ms_opt_des_par.m_N_nodes_pass;              //[-]

	ms_des_par.m_des_objective_type = ms_opt_des_par.m_des_objective_type;	//[-]
	ms_des_par.m_min_phx_deltaT = ms_opt_des_par.m_min_phx_deltaT;			//[K]

	int index = 0;

	std::vector<double> x(0);
	std::vector<double> lb(0);
	std::vector<double> ub(0);
	std::vector<double> scale(0);

	if (!ms_opt_des_par.m_fixed_P_mc_out)
	{
		x.push_back(ms_opt_des_par.m_P_mc_out_guess);	//[kPa]
		lb.push_back(1.E3);								//[kPa]
		ub.push_back(ms_opt_des_par.m_P_high_limit);	//[kPa]
		scale.push_back(500.0);							//[kPa]

		index++;
	}

	if (!ms_opt_des_par.m_fixed_PR_total)
	{
		x.push_back(ms_opt_des_par.m_PR_total_guess);	//[-]
		lb.push_back(1.E-3);			//[-]
		ub.push_back(50);				//[-]
		scale.push_back(0.45);			//[-]

		index++;
	}

	if (!ms_opt_des_par.m_fixed_f_PR_mc)
	{
		x.push_back(ms_opt_des_par.m_f_PR_mc_guess);	//[-]
		lb.push_back(1.E-3);			//[-]
		ub.push_back(0.999);			//[-]
		scale.push_back(0.2);			//[-]

		index++;
	}

	if (!ms_opt_des_par.m_fixed_recomp_frac)
	{
		x.push_back(ms_opt_des_par.m_recomp_frac_guess);	//[-]
		lb.push_back(0.0);			//[-]
		ub.push_back(1.0);			//[-]
		scale.push_back(0.05);		//[-]

		index++;
	}

	if (!ms_opt_des_par.m_fixed_LTR_frac)
	{
		x.push_back(ms_opt_des_par.m_LTR_frac_guess);		//[-]
		lb.push_back(0.0);			//[-]
		ub.push_back(1.0);			//[-]
		scale.push_back(0.1);		//[-]

		index++;
	}

	int no_opt_err_code = 0;
	if (index > 0)
	{
		m_objective_metric_opt = 0.0;

		// Set up instance of nlopt class and set optimization parameters
		nlopt::opt     opt_des_cycle(nlopt::LN_SBPLX, index);
		opt_des_cycle.set_lower_bounds(lb);
		opt_des_cycle.set_upper_bounds(ub);
		opt_des_cycle.set_initial_step(scale);
		opt_des_cycle.set_xtol_rel(ms_opt_des_par.m_des_opt_tol);

		// set max objective function
		opt_des_cycle.set_max_objective(nlopt_cb_opt_partialcooling_des, this);
		double max_f = std::numeric_limits<double>::quiet_NaN();
		nlopt::result  result_des_cycle = opt_des_cycle.optimize(x, max_f);

		ms_des_par = ms_des_par_optimal;

		no_opt_err_code = design_core();

	}
	else
	{
		ms_des_par.m_P_mc_out = ms_opt_des_par.m_P_mc_out_guess;							//[kPa]
		ms_des_par.m_P_pc_in = ms_des_par.m_P_mc_out / ms_opt_des_par.m_PR_total_guess;		//[kPa]
		ms_des_par.m_P_mc_in = ms_des_par.m_P_mc_out - ms_opt_des_par.m_f_PR_mc_guess*(ms_des_par.m_P_mc_out - ms_des_par.m_P_pc_in);	//[kPa]
		ms_des_par.m_recomp_frac = ms_opt_des_par.m_recomp_frac_guess;	//[-]

        if (ms_opt_des_par.m_LTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA || ms_opt_des_par.m_HTR_target_code == NS_HX_counterflow_eqs::OPTIMIZE_UA)
        {
            ms_des_par.m_LTR_UA = ms_opt_des_par.m_UA_rec_total*ms_opt_des_par.m_LTR_frac_guess;	//[kW/K]
            ms_des_par.m_HTR_UA = ms_opt_des_par.m_UA_rec_total*(1.0 - ms_opt_des_par.m_LTR_frac_guess);	//[kW/K]
        }
        else
        {
            ms_des_par.m_LTR_UA = ms_opt_des_par.m_LTR_UA;      //[kW/K]
            ms_des_par.m_HTR_UA = ms_opt_des_par.m_HTR_UA;      //[kW/K]
        }

        // Ensure thermal efficiency is initialized to 0
        m_objective_metric_opt = 0.0;
        double eta_local = design_cycle_return_objective_metric(x);

        if (eta_local == 0.0)
        {
            return -1;
        }

		ms_des_par_optimal = ms_des_par;
	}

	return no_opt_err_code;
}

int C_PartialCooling_Cycle::opt_design(S_opt_des_params & opt_des_par_in)
{
	ms_opt_des_par = opt_des_par_in;

	int opt_des_err_code = opt_design_core();

	if (opt_des_err_code != 0)
		return opt_des_err_code;

	return finalize_design();
}

int C_PartialCooling_Cycle::auto_opt_design(S_auto_opt_design_parameters & auto_opt_des_par_in)
{
	ms_auto_opt_des_par = auto_opt_des_par_in;

	return auto_opt_design_core();
}

int C_PartialCooling_Cycle::auto_opt_design_core()
{
    // Check that simple/recomp flag is set
    if (ms_auto_opt_des_par.m_is_recomp_ok < -1.0 || (ms_auto_opt_des_par.m_is_recomp_ok > 0 && ms_auto_opt_des_par.m_is_recomp_ok != 1.0))
    {
        throw(C_csp_exception("C_PartialCooling_Cycle::auto_opt_design_core(...) requires that ms_auto_opt_des_par.m_is_recomp_ok"
            "is either between -1 and 0 (fixed recompression fraction) or equal to 1 (recomp allowed)\n"));
    }

	// map 'auto_opt_des_par_in' to 'ms_auto_opt_des_par'
	ms_opt_des_par.m_W_dot_net = ms_auto_opt_des_par.m_W_dot_net;	//[kWe]
	ms_opt_des_par.m_T_mc_in = ms_auto_opt_des_par.m_T_mc_in;		//[K]
	ms_opt_des_par.m_T_pc_in = ms_auto_opt_des_par.m_T_pc_in;		//[K]
	ms_opt_des_par.m_T_t_in = ms_auto_opt_des_par.m_T_t_in;			//[K]
	ms_opt_des_par.m_DP_LTR = ms_auto_opt_des_par.m_DP_LTR;			        //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_opt_des_par.m_DP_HTR = ms_auto_opt_des_par.m_DP_HTR;				    //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_opt_des_par.m_DP_PC_LP = ms_auto_opt_des_par.m_DP_PC_pre;		    //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_opt_des_par.m_DP_PC_IP = ms_auto_opt_des_par.m_DP_PC_main;   //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_opt_des_par.m_DP_PHX = ms_auto_opt_des_par.m_DP_PHX;				    //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_opt_des_par.m_UA_rec_total = ms_auto_opt_des_par.m_UA_rec_total;		//[kW/K]
        // LTR thermal design
    ms_opt_des_par.m_LTR_target_code = ms_auto_opt_des_par.m_LTR_target_code;   //[-]
    ms_opt_des_par.m_LTR_UA = ms_auto_opt_des_par.m_LTR_UA;                 //[kW/K]
    ms_opt_des_par.m_LTR_min_dT = ms_auto_opt_des_par.m_LTR_min_dT;         //[K]
    ms_opt_des_par.m_LTR_eff_target = ms_auto_opt_des_par.m_LTR_eff_target; //[-]
    ms_opt_des_par.m_LTR_eff_max = ms_auto_opt_des_par.m_LTR_eff_max;    //[-]
    ms_opt_des_par.m_LTR_N_sub_hxrs = ms_auto_opt_des_par.m_LTR_N_sub_hxrs; //[-]
    ms_opt_des_par.m_LTR_od_UA_target_type = ms_auto_opt_des_par.m_LTR_od_UA_target_type;
        // HTR thermal design
    ms_opt_des_par.m_HTR_target_code = ms_auto_opt_des_par.m_HTR_target_code;   //[-]
    ms_opt_des_par.m_HTR_UA = ms_auto_opt_des_par.m_HTR_UA;             //[kW/K]
    ms_opt_des_par.m_HTR_min_dT = ms_auto_opt_des_par.m_HTR_min_dT;     //[K]
    ms_opt_des_par.m_HTR_eff_target = ms_auto_opt_des_par.m_HTR_eff_target; //[-]
    ms_opt_des_par.m_HTR_eff_max = ms_auto_opt_des_par.m_HTR_eff_max;
    ms_opt_des_par.m_HTR_N_sub_hxrs = ms_auto_opt_des_par.m_HTR_N_sub_hxrs; //[-]
    ms_opt_des_par.m_HTR_od_UA_target_type = ms_auto_opt_des_par.m_HTR_od_UA_target_type;
        //
	ms_opt_des_par.m_eta_mc = ms_auto_opt_des_par.m_eta_mc;					//[-]
	ms_opt_des_par.m_eta_rc = ms_auto_opt_des_par.m_eta_rc;					//[-]
	ms_opt_des_par.m_eta_pc = ms_auto_opt_des_par.m_eta_pc;					//[-]
	ms_opt_des_par.m_eta_t = ms_auto_opt_des_par.m_eta_t;					//[-]
	ms_opt_des_par.m_P_high_limit = ms_auto_opt_des_par.m_P_high_limit;		//[kPa]
	ms_opt_des_par.m_des_tol = ms_auto_opt_des_par.m_des_tol;						//[-]
	ms_opt_des_par.m_des_opt_tol = ms_auto_opt_des_par.m_des_opt_tol;				//[-]
	ms_opt_des_par.m_N_turbine = ms_auto_opt_des_par.m_N_turbine;			//[rpm] Turbine shaft speed (negative values link turbine to compressor)

	ms_opt_des_par.m_is_des_air_cooler = ms_auto_opt_des_par.m_is_des_air_cooler;	//[-]
	ms_opt_des_par.m_frac_fan_power = ms_auto_opt_des_par.m_frac_fan_power;			//[-]
	ms_opt_des_par.m_deltaP_cooler_frac = ms_auto_opt_des_par.m_deltaP_cooler_frac;	//[-]
	ms_opt_des_par.m_T_amb_des = ms_auto_opt_des_par.m_T_amb_des;					//[K]
	ms_opt_des_par.m_elevation = ms_auto_opt_des_par.m_elevation;					//[m]
    ms_opt_des_par.m_eta_fan = ms_auto_opt_des_par.m_eta_fan;                       //[-]
    ms_opt_des_par.m_N_nodes_pass = ms_auto_opt_des_par.m_N_nodes_pass;             //[-]

	ms_opt_des_par.m_des_objective_type = ms_auto_opt_des_par.m_des_objective_type;	//[-]
	ms_opt_des_par.m_min_phx_deltaT = ms_auto_opt_des_par.m_min_phx_deltaT;			//[C]

	ms_opt_des_par.m_fixed_P_mc_out = ms_auto_opt_des_par.m_fixed_P_mc_out;		//[-]
	
	ms_opt_des_par.m_fixed_PR_total = ms_auto_opt_des_par.m_fixed_PR_HP_to_LP;		//[-]

	// Outer optimization loop
	m_objective_metric_auto_opt = 0.0;

	double best_P_high = ms_auto_opt_des_par.m_P_high_limit;
	if (!ms_opt_des_par.m_fixed_P_mc_out)
	{
		double P_low_limit = std::min(ms_auto_opt_des_par.m_P_high_limit, std::max(10.E3, ms_auto_opt_des_par.m_P_high_limit*0.2));		//[kPa]
		best_P_high = fminbr(
			P_low_limit, ms_auto_opt_des_par.m_P_high_limit, &fmin_cb_opt_partialcooling_des_fixed_P_high, this, 1.0);
	}

	// fminb_cb_opt_partialcooling_des_fixed_P_high should calculate:
		// ms_des_par_optimal;
		// m_eta_thermal_opt;

		// Complete 'ms_opt_des_par'
	ms_opt_des_par.m_P_mc_out_guess = ms_auto_opt_des_par.m_P_high_limit;	//[kPa]
	ms_opt_des_par.m_fixed_P_mc_out = true;

	if (ms_opt_des_par.m_fixed_PR_total)
	{
		ms_opt_des_par.m_PR_total_guess = ms_auto_opt_des_par.m_PR_HP_to_LP_guess;	//[-]
	}
	else
	{
		ms_opt_des_par.m_PR_total_guess = 25. / 6.5;	//[-] Guess could be improved...
	}

    if (ms_auto_opt_des_par.m_fixed_f_PR_HP_to_IP)
    {
        ms_opt_des_par.m_fixed_f_PR_mc = true;
        ms_opt_des_par.m_f_PR_mc_guess = ms_auto_opt_des_par.m_f_PR_HP_to_IP_guess; //[-]
    }
    else
    {
        ms_opt_des_par.m_fixed_f_PR_mc = false;
        ms_opt_des_par.m_f_PR_mc_guess = (25. - 8.5) / (25. - 6.5);		//[-] Guess could be improved...
    }
	

    // Is recompression fraction fixed or optimized?
    if (ms_auto_opt_des_par.m_is_recomp_ok < 0.0)
    {
        ms_opt_des_par.m_recomp_frac_guess = fabs(ms_auto_opt_des_par.m_is_recomp_ok);  //[-]
        ms_opt_des_par.m_fixed_recomp_frac = true;  
    }
    else
    {
        ms_opt_des_par.m_recomp_frac_guess = 0.25;	//[-]
        ms_opt_des_par.m_fixed_recomp_frac = false;
    }	

	ms_opt_des_par.m_LTR_frac_guess = 0.5;		//[-]
	ms_opt_des_par.m_fixed_LTR_frac = false;

    if (ms_opt_des_par.m_LTR_target_code != NS_HX_counterflow_eqs::OPTIMIZE_UA || ms_opt_des_par.m_HTR_target_code != NS_HX_counterflow_eqs::OPTIMIZE_UA)
    {
        ms_opt_des_par.m_fixed_LTR_frac = true;
    }

	int pc_error_code = opt_design_core();

	if (pc_error_code == 0 && m_objective_metric_opt > m_objective_metric_auto_opt)
	{
		ms_des_par_auto_opt = ms_des_par_optimal;
		m_objective_metric_auto_opt = m_objective_metric_opt;
	}

	ms_des_par = ms_des_par_auto_opt;

	int pc_opt_des_error_code = design_core();

	if (pc_opt_des_error_code != 0)
	{
		return pc_opt_des_error_code;
	}

	pc_opt_des_error_code = finalize_design();

	return pc_opt_des_error_code;
}

int C_PartialCooling_Cycle::auto_opt_design_hit_eta(S_auto_opt_design_hit_eta_parameters & auto_opt_des_hit_eta_in, std::string & error_msg)
{
	ms_auto_opt_des_par.m_W_dot_net = auto_opt_des_hit_eta_in.m_W_dot_net;	//[kWe]
	ms_auto_opt_des_par.m_T_mc_in = auto_opt_des_hit_eta_in.m_T_mc_in;		//[K]
	ms_auto_opt_des_par.m_T_pc_in = auto_opt_des_hit_eta_in.m_T_pc_in;		//[K]
	ms_auto_opt_des_par.m_T_t_in = auto_opt_des_hit_eta_in.m_T_t_in;			//[K]
	ms_auto_opt_des_par.m_DP_LTR = auto_opt_des_hit_eta_in.m_DP_LT;			        //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_HTR = auto_opt_des_hit_eta_in.m_DP_HT;				    //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_PC_pre = auto_opt_des_hit_eta_in.m_DP_PC_pre;		    //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_PC_main = auto_opt_des_hit_eta_in.m_DP_PC_main;   //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_DP_PHX = auto_opt_des_hit_eta_in.m_DP_PHX;				    //(cold, hot) positive values are absolute [kPa], negative values are relative (-)
	ms_auto_opt_des_par.m_UA_rec_total = std::numeric_limits<double>::quiet_NaN();		//[kW/K]  ***** This method finds the UA required to hit the input efficiency! *****
     // LTR thermal design
    ms_auto_opt_des_par.m_LTR_target_code = auto_opt_des_hit_eta_in.m_LTR_target_code;  //[-]
    ms_auto_opt_des_par.m_LTR_UA = auto_opt_des_hit_eta_in.m_LTR_UA;                    //[kW/K]
    ms_auto_opt_des_par.m_LTR_min_dT = auto_opt_des_hit_eta_in.m_LTR_min_dT;            //[K]
    ms_auto_opt_des_par.m_LTR_eff_target = auto_opt_des_hit_eta_in.m_LTR_eff_target;    //[-]
    ms_auto_opt_des_par.m_LTR_eff_max = auto_opt_des_hit_eta_in.m_LTR_eff_max;          //[-]
    ms_auto_opt_des_par.m_LTR_N_sub_hxrs = auto_opt_des_hit_eta_in.m_LTR_N_sub_hxrs;    //[-]
    ms_auto_opt_des_par.m_LTR_od_UA_target_type = auto_opt_des_hit_eta_in.m_LTR_od_UA_target_type;
        // HTR thermal design
    ms_auto_opt_des_par.m_HTR_target_code = auto_opt_des_hit_eta_in.m_HTR_target_code;  //[-]
    ms_auto_opt_des_par.m_HTR_UA = auto_opt_des_hit_eta_in.m_HTR_UA;                    //[kW/K]
    ms_auto_opt_des_par.m_HTR_min_dT = auto_opt_des_hit_eta_in.m_HTR_min_dT;            //[K]
    ms_auto_opt_des_par.m_HTR_eff_target = auto_opt_des_hit_eta_in.m_HTR_eff_target;    //[-]
    ms_auto_opt_des_par.m_HTR_eff_max = auto_opt_des_hit_eta_in.m_HTR_eff_max;    //[-]
    ms_auto_opt_des_par.m_HTR_N_sub_hxrs = auto_opt_des_hit_eta_in.m_HTR_N_sub_hxrs;    //[-]
    ms_auto_opt_des_par.m_HTR_od_UA_target_type = auto_opt_des_hit_eta_in.m_HTR_od_UA_target_type;
        //
	ms_auto_opt_des_par.m_eta_mc = auto_opt_des_hit_eta_in.m_eta_mc;					//[-]
	ms_auto_opt_des_par.m_eta_rc = auto_opt_des_hit_eta_in.m_eta_rc;					//[-]
	ms_auto_opt_des_par.m_eta_pc = auto_opt_des_hit_eta_in.m_eta_pc;					//[-]
	ms_auto_opt_des_par.m_eta_t = auto_opt_des_hit_eta_in.m_eta_t;					//[-]
	ms_auto_opt_des_par.m_P_high_limit = auto_opt_des_hit_eta_in.m_P_high_limit;		//[kPa]
	ms_auto_opt_des_par.m_des_tol = auto_opt_des_hit_eta_in.m_des_tol;						//[-]
	ms_auto_opt_des_par.m_des_opt_tol = auto_opt_des_hit_eta_in.m_des_opt_tol;				//[-]
	ms_auto_opt_des_par.m_N_turbine = auto_opt_des_hit_eta_in.m_N_turbine;			//[rpm] Turbine shaft speed (negative values link turbine to compressor)
	ms_auto_opt_des_par.m_is_recomp_ok = auto_opt_des_hit_eta_in.m_is_recomp_ok;		//[-] 1 = yes, 0 = no, other = invalid

	ms_auto_opt_des_par.m_is_des_air_cooler = auto_opt_des_hit_eta_in.m_is_des_air_cooler;		//[-]
	ms_auto_opt_des_par.m_frac_fan_power = auto_opt_des_hit_eta_in.m_frac_fan_power;			//[-]
	ms_auto_opt_des_par.m_deltaP_cooler_frac = auto_opt_des_hit_eta_in.m_deltaP_cooler_frac;	//[-]
	ms_auto_opt_des_par.m_T_amb_des = auto_opt_des_hit_eta_in.m_T_amb_des;					//[K]
	ms_auto_opt_des_par.m_elevation = auto_opt_des_hit_eta_in.m_elevation;					//[m]
    ms_auto_opt_des_par.m_eta_fan = auto_opt_des_hit_eta_in.m_eta_fan;                      //[-]
    ms_auto_opt_des_par.m_N_nodes_pass = auto_opt_des_hit_eta_in.m_N_nodes_pass;            //[-]

	ms_auto_opt_des_par.m_des_objective_type = auto_opt_des_hit_eta_in.m_des_objective_type;	//[-]
	ms_auto_opt_des_par.m_min_phx_deltaT = auto_opt_des_hit_eta_in.m_min_phx_deltaT;			//[C]

	ms_auto_opt_des_par.mf_callback_log = auto_opt_des_hit_eta_in.mf_callback_log;
	ms_auto_opt_des_par.mp_mf_active = auto_opt_des_hit_eta_in.mp_mf_active;

	ms_auto_opt_des_par.m_fixed_P_mc_out = auto_opt_des_hit_eta_in.m_fixed_P_mc_out;		//[-]
	
	ms_auto_opt_des_par.m_fixed_PR_HP_to_LP = auto_opt_des_hit_eta_in.m_fixed_PR_HP_to_LP;  //[-]
	ms_auto_opt_des_par.m_PR_HP_to_LP_guess = auto_opt_des_hit_eta_in.m_fixed_PR_HP_to_LP;  //[-]

    ms_auto_opt_des_par.m_f_PR_HP_to_IP_guess = auto_opt_des_hit_eta_in.m_f_PR_HP_to_IP_guess;  //[-]
    ms_auto_opt_des_par.m_fixed_f_PR_HP_to_IP = auto_opt_des_hit_eta_in.m_fixed_f_PR_HP_to_IP;  //[-]

	// At this point, 'auto_opt_des_hit_eta_in' should only be used to access the targer thermal efficiency: 'm_eta_thermal'

	double Q_dot_rec_des = ms_auto_opt_des_par.m_W_dot_net / auto_opt_des_hit_eta_in.m_eta_thermal;		//[kWt] Receiver thermal input at design

	error_msg = "";

	// Check cycle parameter values are reasonable
    if (ms_auto_opt_des_par.m_is_recomp_ok < -1.0 || (ms_auto_opt_des_par.m_is_recomp_ok > 0 && ms_auto_opt_des_par.m_is_recomp_ok != 1.0))
        {
		throw(C_csp_exception("C_PartialCooling_Cyclee::auto_opt_design_core(...) requires that ms_auto_opt_des_par.m_is_recomp_ok"
            "is either between -1 and 0 (fixed recompression fraction) or equal to 1 (recomp allowed)\n"));
	}
	// Can't operate compressore in 2-phase region
	if (ms_auto_opt_des_par.m_T_mc_in <= N_co2_props::T_crit)
	{
		error_msg.append(util::format("Only single phase cycle operation is allowed in this model."
			"The compressor inlet temperature (%lg [C]) must be great than the critical temperature: %lg [C]",
			ms_auto_opt_des_par.m_T_mc_in - 273.15, ((N_co2_props::T_crit) - 273.15)));

		return -1;
	}

	// "Reasonable" ceiling on main compressor inlet temp
	double T_mc_in_max = 70.0 + 273.15;		//[K] Arbitrary value for max compressor inlet temperature
	if (ms_auto_opt_des_par.m_T_mc_in > T_mc_in_max)
	{
		error_msg.append(util::format("Them main compressor inlet temperature input was %lg [C]. This value was reset internally to the max allowable inlet temperature: %lg [C]\n",
			ms_auto_opt_des_par.m_T_mc_in - 273.15, T_mc_in_max - 273.15));

		ms_auto_opt_des_par.m_T_mc_in = T_mc_in_max;
	}

	// "Reasonable" ceiling on main compressor inlet temp
	double T_pc_in_max = 70.0 + 273.15;		//[K] Arbitrary value for max compressor inlet temperature
	if (ms_auto_opt_des_par.m_T_pc_in > T_pc_in_max)
	{
		error_msg.append(util::format("The pre compressor inlet temperature input was %lg [C]. This value was reset internally to the max allowable inlet temperature: %lg [C]\n",
			ms_auto_opt_des_par.m_T_pc_in - 273.15, T_pc_in_max - 273.15));

		ms_auto_opt_des_par.m_T_pc_in = T_pc_in_max;
	}

	// "Reasonable" floor on turbine inlet temp
	double T_t_in_min = 300.0 + 273.15;		//[K] Arbitrary value for min turbine inlet temperature
	if (ms_auto_opt_des_par.m_T_t_in < T_t_in_min)
	{
		error_msg.append(util::format("The turbine inlet temperature input was %lg [C]. This value was reset internally to the min allowable inlet temperature: %lg [C]\n",
			ms_auto_opt_des_par.m_T_t_in - 273.15, T_t_in_min - 273.15));

		ms_auto_opt_des_par.m_T_t_in = T_t_in_min;
	}

	// Turbine inlet temperature must be hotter than main compressor outlet temperature
	if (ms_auto_opt_des_par.m_T_t_in <= ms_auto_opt_des_par.m_T_mc_in)
	{
		error_msg.append(util::format("The turbine inlet temperature, %lg [C], is colder than the specified main compressor inlet temperature %lg [C]",
			ms_auto_opt_des_par.m_T_t_in - 273.15, ms_auto_opt_des_par.m_T_mc_in - 273.15));

		return -1;
	}

	// Turbine inlet temperature must be hotter than pre compressor outlet temperature
	if (ms_auto_opt_des_par.m_T_t_in <= ms_auto_opt_des_par.m_T_pc_in)
	{
		error_msg.append(util::format("The turbine inlet temperature, %lg [C], is colder than the specified pre compressor inlet temperature %lg [C]",
			ms_auto_opt_des_par.m_T_t_in - 273.15, ms_auto_opt_des_par.m_T_pc_in - 273.15));

		return -1;
	}

	// Turbine inlet temperature must be colder than property limits
	if (ms_auto_opt_des_par.m_T_t_in >= N_co2_props::T_upper_limit)
	{
		error_msg.append(util::format("The turbine inlet temperature, %lg [C], is hotter than the maximum allow temperature in the CO2 property code %lg [C]",
			ms_auto_opt_des_par.m_T_t_in - 273.15, N_co2_props::T_upper_limit - 273.15));

		return -1;
	}

	// Check for realistic isentropic efficiencies
	if (ms_auto_opt_des_par.m_eta_mc > 1.0)
	{
		error_msg.append(util::format("The main compressor isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n",
			ms_auto_opt_des_par.m_eta_mc));

		ms_auto_opt_des_par.m_eta_mc = 1.0;
	}
	if (ms_auto_opt_des_par.m_eta_rc > 1.0)
	{
		error_msg.append(util::format("The re-compressor isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n",
			ms_auto_opt_des_par.m_eta_rc));

		ms_auto_opt_des_par.m_eta_rc = 1.0;
	}
	if (ms_auto_opt_des_par.m_eta_pc > 1.0)
	{
		error_msg.append(util::format("The pre-compressor isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n",
			ms_auto_opt_des_par.m_eta_pc));

		ms_auto_opt_des_par.m_eta_pc = 1.0;
	}
	if (ms_auto_opt_des_par.m_eta_t > 1.0)
	{
		error_msg.append(util::format("The turbine isentropic efficiency, %lg, was reset to theoretical maximum 1.0\n",
			ms_auto_opt_des_par.m_eta_t));

		ms_auto_opt_des_par.m_eta_t = 1.0;
	}
	if (ms_auto_opt_des_par.m_eta_mc < 0.1)
	{
		error_msg.append(util::format("The main compressor isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n",
			ms_auto_opt_des_par.m_eta_mc));

		ms_auto_opt_des_par.m_eta_mc = 0.1;
	}
	if (ms_auto_opt_des_par.m_eta_rc < 0.1)
	{
		error_msg.append(util::format("The re-compressor isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n",
			ms_auto_opt_des_par.m_eta_rc));

		ms_auto_opt_des_par.m_eta_rc = 0.1;
	}
	if (ms_auto_opt_des_par.m_eta_pc < 0.1)
	{
		error_msg.append(util::format("The pre-compressor isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n",
			ms_auto_opt_des_par.m_eta_pc));

		ms_auto_opt_des_par.m_eta_pc = 0.1;
	}
	if (ms_auto_opt_des_par.m_eta_t < 0.1)
	{
		error_msg.append(util::format("The turbine isentropic efficiency, %lg, was increased to the internal limit of 0.1 to improve solution stability\n",
			ms_auto_opt_des_par.m_eta_t));

		ms_auto_opt_des_par.m_eta_t = 0.1;
	}

	if (ms_auto_opt_des_par.m_LTR_eff_max > 1.0)
	{
		error_msg.append(util::format("The LT recuperator max effectiveness, %lg, was decreased to the limit of 1.0\n", ms_auto_opt_des_par.m_LTR_eff_max));

		ms_auto_opt_des_par.m_LTR_eff_max = 1.0;
	}

	if (ms_auto_opt_des_par.m_LTR_eff_max < 0.70)
	{
		error_msg.append(util::format("The LT recuperator max effectiveness, %lg, was increased to the internal limit of 0.70 improve convergence\n", ms_auto_opt_des_par.m_LTR_eff_max));

		ms_auto_opt_des_par.m_LTR_eff_max = 0.7;
	}

	if (ms_auto_opt_des_par.m_HTR_eff_max > 1.0)
	{
		error_msg.append(util::format("The HT recuperator max effectiveness, %lg, was decreased to the limit of 1.0\n", ms_auto_opt_des_par.m_HTR_eff_max));

		ms_auto_opt_des_par.m_HTR_eff_max = 1.0;
	}

	if (ms_auto_opt_des_par.m_HTR_eff_max < 0.70)
	{
		error_msg.append(util::format("The LT recuperator max effectiveness, %lg, was increased to the internal limit of 0.70 improve convergence\n", ms_auto_opt_des_par.m_HTR_eff_max));

		ms_auto_opt_des_par.m_HTR_eff_max = 0.7;
	}
	// Limits on high pressure limit
	if (ms_auto_opt_des_par.m_P_high_limit >= N_co2_props::P_upper_limit)
	{
		error_msg.append(util::format("The upper pressure limit, %lg [MPa], was set to the internal limit in the CO2 properties code %lg [MPa]\n",
			ms_auto_opt_des_par.m_P_high_limit, N_co2_props::P_upper_limit));

		ms_auto_opt_des_par.m_P_high_limit = N_co2_props::P_upper_limit;
	}
	double P_high_limit_min = 10.0*1.E3;	//[kPa]
	if (ms_auto_opt_des_par.m_P_high_limit <= P_high_limit_min)
	{
		error_msg.append(util::format("The upper pressure limit, %lg [MPa], must be greater than %lg [MPa] to ensure solution stability",
			ms_auto_opt_des_par.m_P_high_limit, P_high_limit_min));

		return -1;
	}
	// Finally, check thermal efficiency
	if (auto_opt_des_hit_eta_in.m_eta_thermal <= 0.0)
	{
		error_msg.append(util::format("The design cycle thermal efficiency, %lg, must be at least greater than 0 ",
			auto_opt_des_hit_eta_in.m_eta_thermal));

		return -1;
	}
	double eta_carnot = 1.0 - ms_auto_opt_des_par.m_T_mc_in / ms_auto_opt_des_par.m_T_t_in;
	if (auto_opt_des_hit_eta_in.m_eta_thermal >= eta_carnot)
	{
		error_msg.append(util::format("To solve the cycle within the allowable recuperator conductance, the design cycle thermal efficiency, %lg, must be at least less than the Carnot efficiency: %lg ",
			auto_opt_des_hit_eta_in.m_eta_thermal, eta_carnot));

		return -1;
	}
	// Send log update upstream
	if (ms_auto_opt_des_par.mf_callback_log && ms_auto_opt_des_par.mp_mf_active)
	{
		std::string msg_log = util::format("Iterate on total recuperator conductance to hit target cycle efficiency: %lg [-]", auto_opt_des_hit_eta_in.m_eta_thermal);
		std::string msg_progress = "Designing cycle...";
		if (!ms_auto_opt_des_par.mf_callback_log(msg_log, msg_progress, ms_auto_opt_des_par.mp_mf_active, 0.0, 2))
		{
			std::string error_msg = "User terminated simulation...";
			std::string loc_msg = "C_MEQ_sco2_design_hit_eta__UA_total";
			throw(C_csp_exception(error_msg, loc_msg, 1));
		}
	}

	// Set up monotonic equation solver to find the total recuperator UA that results in the target efficiency
	C_MEQ_sco2_design_hit_eta__UA_total c_eq(this);
	C_monotonic_eq_solver c_solver(c_eq);

	// Generate min and max values
	double UA_recup_total_max = ms_des_limits.m_UA_net_power_ratio_max*ms_auto_opt_des_par.m_W_dot_net;		//[kW/K]
	double UA_recup_total_min = ms_des_limits.m_UA_net_power_ratio_min*ms_auto_opt_des_par.m_W_dot_net;		//[kW/K]
																											// Set solver settings
	c_solver.settings(ms_auto_opt_des_par.m_des_tol, 50, UA_recup_total_min, UA_recup_total_max, true);

	// Generate guess values
	double UA_recups_guess = 0.1*ms_auto_opt_des_par.m_W_dot_net;

	double UA_recup_total_solved, tol_solved;
	UA_recup_total_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int solver_code = 0;
	try
	{
		solver_code = c_solver.solve(UA_recups_guess, 1.1*UA_recups_guess, auto_opt_des_hit_eta_in.m_eta_thermal,
			UA_recup_total_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception &csp_except)
	{
		if (csp_except.m_error_code == 1)
		{
			throw(C_csp_exception(csp_except));
		}
		else
		{
			throw(C_csp_exception("PartialCooling_Cycle::C_MEQ_sco2_design_hit_eta__UA_total received an exception from the solver"));
		}
	}

	if (solver_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (solver_code < C_monotonic_eq_solver::CONVERGED)
		{
			error_msg.append("Can't find a value of total recuperator conductance that achieves"
				" the target cycle efficiency. Check design parameters.");
		}
		else if (!c_solver.did_solver_find_negative_error(solver_code))
		{
			error_msg.append("Can't find a value of total recuperator conductance that results"
				" in an efficiency smaller than the target efficiency.");
		}
		else if (!c_solver.did_solver_find_positive_error(solver_code))
		{
			error_msg.append("Can't find a value of total recuperator conductance that results"
				" in an efficiency larger than the target efficiency.");
		}
		else
		{
			error_msg.append("Can't find a value of total recuperator conductance that achieves"
				" the target cycle efficiency. Check design parameters.");
		}

		return -1;
	}

	return 0;
}

int C_PartialCooling_Cycle::C_MEQ_sco2_design_hit_eta__UA_total::operator()(double UA_recup_total /*kW/K*/, double *eta /*-*/)
{
	mpc_pc_cycle->ms_auto_opt_des_par.m_UA_rec_total = UA_recup_total;	//[kW/K]

	int error_code = mpc_pc_cycle->auto_opt_design_core();
	if (error_code != 0)
	{
		*eta = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}

	*eta = mpc_pc_cycle->get_design_solved()->m_eta_thermal;	//[-]

	if (mpc_pc_cycle->ms_auto_opt_des_par.mf_callback_log && mpc_pc_cycle->ms_auto_opt_des_par.mp_mf_active)
	{
		msg_log = util::format(" Total recuperator conductance = %lg [kW/K per MWe]. Optimized cycle efficiency = %lg [-].  ",
			UA_recup_total / (mpc_pc_cycle->ms_auto_opt_des_par.m_W_dot_net * 1.E-3), *eta);
		if (!mpc_pc_cycle->ms_auto_opt_des_par.mf_callback_log(msg_log, msg_progress, mpc_pc_cycle->ms_auto_opt_des_par.mp_mf_active, 0.0, 2))
		{
			std::string error_msg = "User terminated simulation...";
			std::string loc_msg = "C_MEQ_sco2_design_hit_eta__UA_total";
			throw(C_csp_exception(error_msg, loc_msg, 1));
		}
	}

	return 0;
}

int C_PartialCooling_Cycle::off_design_fix_shaft_speeds_core(double od_tol /*-*/)
{
	// Need to reset 'ms_od_solved' here
	clear_ms_od_solved();

	// Initialize known cycle state point properties
	mv_temp_od[MC_IN] = ms_od_par.m_T_mc_in;		//[K]
	
	mv_temp_od[PC_IN] = ms_od_par.m_T_pc_in;		//[K]
	mv_pres_od[PC_IN] = ms_od_par.m_P_LP_comp_in;	//[kPa]

	mv_temp_od[TURB_IN] = ms_od_par.m_T_t_in;		//[K]

    double N_rc_des = mc_rc.get_design_solved()->m_N_design;    //[rpm]
    double N_rc_od = N_rc_des;      //[rpm]
    if (!ms_od_par.m_is_rc_N_od_at_design) {
        N_rc_od = ms_od_par.m_rc_N_od_f_des * N_rc_des;     //[rpm]
    }

	// Solve for recompression fraction that results in all turbomachinery operating at design/target speeds
	C_MEQ__f_recomp__y_N_rc c_rc_shaft_speed(this, ms_od_par.m_T_pc_in,
													ms_od_par.m_P_LP_comp_in,
													ms_od_par.m_T_mc_in,
													ms_od_par.m_T_t_in,
													ms_od_par.m_f_mc_pc_bypass, od_tol,
                                                    N_rc_od);

	C_monotonic_eq_solver c_rc_shaft_speed_solver(c_rc_shaft_speed);

	if (ms_des_solved.m_is_rc)
	{
		// Define bounds on the recompression fraction
		double f_recomp_lower = 0.0;
		double f_recomp_upper = 1.0;

		c_rc_shaft_speed_solver.settings(od_tol, 50, f_recomp_lower, f_recomp_upper, false);

		C_monotonic_eq_solver::S_xy_pair f_recomp_pair_1st;
		C_monotonic_eq_solver::S_xy_pair f_recomp_pair_2nd;

		double f_recomp_guess = ms_des_solved.m_recomp_frac;
		double y_f_recomp_guess = std::numeric_limits<double>::quiet_NaN();

		// Send the guessed recompression fraction to method; see if it returns a calculated N_rc or fails
		int rc_shaft_speed_err_code = c_rc_shaft_speed_solver.call_mono_eq(f_recomp_guess, &y_f_recomp_guess);

		// If guessed recompression fraction fails, then try to find a recompression fraction that works
		if (rc_shaft_speed_err_code != 0)
		{
			double delta = 0.02;
			bool is_iter = true;
			for (int i = 1; is_iter; i++)
			{
				for (int j = -1; j <= 1; j += 2)
				{
					f_recomp_guess = std::min(1.0, std::max(0.0, ms_des_solved.m_recomp_frac + j * i*delta));
					rc_shaft_speed_err_code = c_rc_shaft_speed_solver.call_mono_eq(f_recomp_guess, &y_f_recomp_guess);
					if (rc_shaft_speed_err_code == 0)
					{
						is_iter = false;
						break;
					}
					if (f_recomp_guess == 0.0)
					{
						// Have tried a fairly fine grid of recompression fraction values; have not found one that works
						return -40;
					}
				}
			}
		}

		f_recomp_pair_1st.x = f_recomp_guess;
		f_recomp_pair_1st.y = y_f_recomp_guess;

		f_recomp_guess = 1.02*f_recomp_pair_1st.x;
		rc_shaft_speed_err_code = c_rc_shaft_speed_solver.call_mono_eq(f_recomp_guess, &y_f_recomp_guess);

		if (rc_shaft_speed_err_code == 0)
		{
			f_recomp_pair_2nd.x = f_recomp_guess;
			f_recomp_pair_2nd.y = y_f_recomp_guess;
		}
		else
		{
			f_recomp_guess = 0.98*f_recomp_pair_1st.x;
			rc_shaft_speed_err_code = c_rc_shaft_speed_solver.call_mono_eq(f_recomp_guess, &y_f_recomp_guess);

			if (rc_shaft_speed_err_code == 0)
			{
				f_recomp_pair_2nd.x = f_recomp_guess;
				f_recomp_pair_2nd.y = y_f_recomp_guess;
			}
			else
			{
				return -41;
			}
		}

		// Now, using the two solved guess values, solve for the recompression fraction that results in:
		// ... balanced turbomachinery at their design/target shaft speed
		double f_recomp_solved, tol_solved;
		f_recomp_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		int iter_solved = -1;

		int f_recomp_code = 0;
		try
		{
			f_recomp_code = c_rc_shaft_speed_solver.solve(f_recomp_pair_1st, f_recomp_pair_2nd, 0.0,
													f_recomp_solved, tol_solved, iter_solved);
		}
		catch (C_csp_exception)
		{
			return -42;
		}

		if (f_recomp_code != C_monotonic_eq_solver::CONVERGED)
		{
			int n_call_history = (int)c_rc_shaft_speed_solver.get_solver_call_history()->size();

			int error_code = 0;
			if(n_call_history > 0)
				error_code = -(*(c_rc_shaft_speed_solver.get_solver_call_history()))[n_call_history - 1].err_code;

			if (error_code == 0)
				error_code = f_recomp_code;

			return error_code;
		}

	}
	else
	{
		double y_eq = std::numeric_limits<double>::quiet_NaN();
		int rc_shaft_speed_err_code = c_rc_shaft_speed_solver.call_mono_eq(0.0, &y_eq);
		if (rc_shaft_speed_err_code != 0)
		{
			throw(C_csp_exception("C_PartialCooling::off_design_fix_shaft_speeds_core does not yet have ability to solve for cycles with recompression"));
		}
	}

	// Get mass flow rates from solver
	double m_dot_mc = c_rc_shaft_speed.m_m_dot_mc;	//[kg/s]
	double m_dot_rc = c_rc_shaft_speed.m_m_dot_rc;	//[kg/s]
	double m_dot_pc = c_rc_shaft_speed.m_m_dot_pc;	//[kg/s]
	double m_dot_t = c_rc_shaft_speed.m_m_dot_t;	//[kg/s]
	double m_dot_LTR_HP = c_rc_shaft_speed.m_m_dot_LTR_HP;	//[kg/s]

	// Fully define known states
	std::vector<int> known_states;
	known_states.push_back(PC_IN);
	known_states.push_back(PC_OUT);
	known_states.push_back(RC_OUT);
	known_states.push_back(MC_IN);
	known_states.push_back(MC_OUT);
	known_states.push_back(TURB_IN);
	known_states.push_back(TURB_OUT);	

	int n_states = known_states.size();
	int prop_error_code = 0;
	for (int i = 0; i < n_states; i++)
	{
		int j = known_states[i];
		prop_error_code = CO2_TP(mv_temp_od[j], mv_pres_od[j], &mc_co2_props);
		if (prop_error_code != 0)
		{
			return prop_error_code;
		}
		mv_enth_od[j] = mc_co2_props.enth;
		mv_entr_od[j] = mc_co2_props.entr;
		mv_dens_od[j] = mc_co2_props.dens;
	}

	// Solve recuperators here...
	double T_HTR_LP_out_lower = mv_temp_od[MC_OUT];		//[K] Coldest possible temperature
	double T_HTR_LP_out_upper = mv_temp_od[TURB_OUT];	//[K] Hottest possible temperature

	double T_HTR_LP_out_guess_lower = std::min(T_HTR_LP_out_upper - 2.0, std::max(T_HTR_LP_out_lower + 15.0, mv_temp_od[RC_OUT]));
	double T_HTR_LP_out_guess_upper = std::min(T_HTR_LP_out_guess_lower + 20.0, T_HTR_LP_out_upper - 1.0);

	C_MEQ_recup_od recup_od_eq(this, m_dot_LTR_HP, m_dot_t, m_dot_rc, od_tol);
	C_monotonic_eq_solver recup_od_solver(recup_od_eq);

	recup_od_solver.settings(ms_des_par.m_des_tol*mv_temp_od[MC_IN], 1000, T_HTR_LP_out_lower, T_HTR_LP_out_upper, false);

	double T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved;
	T_HTR_LP_out_solved = tol_T_HTR_LP_out_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_HTR_LP_out = -1;

	int T_HTR_LP_out_code = recup_od_solver.solve(T_HTR_LP_out_guess_lower, T_HTR_LP_out_guess_upper, 0,
		T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved, iter_T_HTR_LP_out);

	if (T_HTR_LP_out_code != C_monotonic_eq_solver::CONVERGED)
	{
		int n_call_history = (int)recup_od_solver.get_solver_call_history()->size();
		int error_code = 0;
		if (n_call_history > 0)
			error_code = (*(recup_od_solver.get_solver_call_history()))[n_call_history - 1].err_code;
		if (error_code == 0)
		{
			error_code = T_HTR_LP_out_code;
		}
		return error_code;
	}

	// Fully define remaining states
	std::vector<int> remaining_states;
	remaining_states.push_back(HTR_LP_OUT);
	remaining_states.push_back(LTR_LP_OUT);
	remaining_states.push_back(HTR_HP_OUT);

	n_states = remaining_states.size();
	prop_error_code = 0;
	for (int i = 0; i < n_states; i++)
	{
		int j = remaining_states[i];
		prop_error_code = CO2_TP(mv_temp_od[j], mv_pres_od[j], &mc_co2_props);
		if (prop_error_code != 0)
		{
			return prop_error_code;
		}
		mv_enth_od[j] = mc_co2_props.enth;
		mv_entr_od[j] = mc_co2_props.entr;
		mv_dens_od[j] = mc_co2_props.dens;
	}	

	// Calculate cycle performance metrics
	double w_pc = mv_enth_od[PC_IN] - mv_enth_od[PC_OUT];		//[kJ/kg] (negative) specific work of pre-compressor
	double w_mc = mv_enth_od[MC_IN] - mv_enth_od[MC_OUT];		//[kJ/kg] (negative) specific work of main compressor
	double w_t = mv_enth_od[TURB_IN] - mv_enth_od[TURB_OUT];	//[kJ/kg] (positive) specific work of turbine
	
	double w_rc = 0.0;
	if (m_dot_rc > 0.0)
		w_rc = mv_enth_od[PC_OUT] - mv_enth_od[RC_OUT];		//[kJ/kg] (negative) specific work of recompressor

	m_Q_dot_PHX_od = m_dot_t * (mv_enth_od[TURB_IN] - mv_enth_od[HTR_HP_OUT]);
	m_W_dot_net_od = w_pc*m_dot_pc + w_mc*m_dot_mc + w_rc*m_dot_rc + w_t*m_dot_t;
	m_eta_thermal_od = m_W_dot_net_od / m_Q_dot_PHX_od;
    m_Q_dot_mc_cooler_od = m_dot_mc * (mv_enth_od[PC_OUT] - mv_enth_od[MC_IN])*1.E-3;       //[MWt] convert from kwt
    m_Q_dot_pc_cooler_od = m_dot_pc * (mv_enth_od[LTR_LP_OUT] - mv_enth_od[PC_IN])*1.E-3;   //[MWt] convert from kwt

	// Get 'od_solved' structures from component classes
	//ms_od_solved.ms_mc_od_solved = *m_mc.get_od_solved();
	ms_od_solved.ms_mc_ms_od_solved = *mc_mc.get_od_solved();
	ms_od_solved.ms_rc_ms_od_solved = *mc_rc.get_od_solved();
	ms_od_solved.ms_pc_ms_od_solved = *mc_pc.get_od_solved();
	ms_od_solved.ms_t_od_solved = *mc_t.get_od_solved();
	ms_od_solved.ms_LT_recup_od_solved = mc_LTR.ms_od_solved;
	ms_od_solved.ms_HT_recup_od_solved = mc_HTR.ms_od_solved;

	// Set ms_od_solved
	ms_od_solved.m_eta_thermal = m_eta_thermal_od;
	ms_od_solved.m_W_dot_net = m_W_dot_net_od;
	ms_od_solved.m_Q_dot = m_Q_dot_PHX_od;
    ms_od_solved.m_Q_dot_mc_cooler = m_Q_dot_mc_cooler_od;  //[MWt]
    ms_od_solved.m_Q_dot_pc_cooler = m_Q_dot_pc_cooler_od;  //[MWt]
	ms_od_solved.m_m_dot_mc = m_dot_mc;
	ms_od_solved.m_m_dot_rc = m_dot_rc;
	ms_od_solved.m_m_dot_pc = m_dot_pc;
	ms_od_solved.m_m_dot_t = m_dot_t;
	ms_od_solved.m_recomp_frac = m_dot_rc / m_dot_t;

	ms_od_solved.m_mc_f_bypass = 1.0 - m_dot_LTR_HP / m_dot_mc;	//[kg/s]
	ms_od_solved.m_pc_f_bypass = 1.0 - m_dot_t / m_dot_pc;		//[kg/s]

	ms_od_solved.m_temp = mv_temp_od;
	ms_od_solved.m_pres = mv_pres_od;
	ms_od_solved.m_enth = mv_enth_od;
	ms_od_solved.m_entr = mv_entr_od;
	ms_od_solved.m_dens = mv_dens_od;

	return 0;
}

int C_PartialCooling_Cycle::solve_OD_all_coolers_fan_power(double T_amb /*K*/, double od_tol /*-*/, double & W_dot_fan_total /*MWe*/)
{
	double W_dot_LP_cooler = std::numeric_limits<double>::quiet_NaN();
	double W_dot_IP_cooler = std::numeric_limits<double>::quiet_NaN();

    double LP_P_out = std::numeric_limits<double>::quiet_NaN();
    int LP_err_code = solve_OD_pc_cooler_fan_power(T_amb, od_tol, W_dot_LP_cooler, LP_P_out);

	if (LP_err_code != 0)
		return LP_err_code;

	ms_od_solved.ms_pc_air_cooler_od_solved = mc_pc_air_cooler.get_od_solved();

    double IP_P_out = std::numeric_limits<double>::quiet_NaN();
    int IP_err_code = solve_OD_mc_cooler_fan_power(T_amb, od_tol, W_dot_IP_cooler, IP_P_out);

	W_dot_fan_total = W_dot_LP_cooler + W_dot_IP_cooler;	//[MWe]

	ms_od_solved.ms_mc_air_cooler_od_solved = mc_mc_air_cooler.get_od_solved();

	return IP_err_code;
}

int C_PartialCooling_Cycle::solve_OD_mc_cooler_fan_power(double T_amb /*K*/, double od_tol /*-*/,
                                                double & W_dot_mc_cooler_fan /*MWe*/, double & P_co2_out /*kPa*/)
{
    double tol_acc = od_tol / 10.0;

    int IP_err_code = mc_mc_air_cooler.off_design_given_T_out(T_amb, mv_temp_od[PC_OUT], mv_pres_od[PC_OUT],
        ms_od_solved.m_m_dot_mc, mv_temp_od[MC_IN], tol_acc, od_tol, W_dot_mc_cooler_fan, P_co2_out);

    ms_od_solved.ms_mc_air_cooler_od_solved = mc_mc_air_cooler.get_od_solved();

    return IP_err_code;
}

int C_PartialCooling_Cycle::solve_OD_pc_cooler_fan_power(double T_amb /*K*/, double od_tol /*-*/,
                                                double & W_dot_pc_cooler_fan /*MWe*/, double & P_co2_out /*kPa*/)
{
    double tol_acc = od_tol / 10.0;

    int LP_err_code = mc_pc_air_cooler.off_design_given_T_out(T_amb, mv_temp_od[LTR_LP_OUT], mv_pres_od[LTR_LP_OUT],
        ms_od_solved.m_m_dot_pc, mv_temp_od[PC_IN], tol_acc, od_tol, W_dot_pc_cooler_fan, P_co2_out);

    ms_od_solved.ms_pc_air_cooler_od_solved = mc_pc_air_cooler.get_od_solved();

    return LP_err_code;
}

int C_PartialCooling_Cycle::C_MEQ_recup_od::operator()(double T_HTR_LP_out_guess /*K*/, double *diff_T_HTR_LP_out /*K*/)
{
	mpc_pc_cycle->mv_temp_od[HTR_LP_OUT] = T_HTR_LP_out_guess;		//[K]

		// Low temperature recuperator
	double Q_dot_LTR = std::numeric_limits<double>::quiet_NaN();
	try
	{
		mpc_pc_cycle->mc_LTR.off_design_solution_fixed_dP(mpc_pc_cycle->mv_temp_od[MC_OUT], mpc_pc_cycle->mv_pres_od[MC_OUT], m_m_dot_LTR_HP, mpc_pc_cycle->mv_pres_od[LTR_HP_OUT],
			mpc_pc_cycle->mv_temp_od[HTR_LP_OUT], mpc_pc_cycle->mv_pres_od[HTR_LP_OUT], m_m_dot_t, mpc_pc_cycle->mv_pres_od[LTR_LP_OUT],
            m_od_tol,
            Q_dot_LTR, mpc_pc_cycle->mv_temp_od[LTR_HP_OUT], mpc_pc_cycle->mv_temp_od[LTR_LP_OUT]);
	}
	catch (C_csp_exception &)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();

		return -1;
	}

		// Mixer
			// Get LTR HP outlet state
	int prop_error_code = CO2_TP(mpc_pc_cycle->mv_temp_od[LTR_HP_OUT], mpc_pc_cycle->mv_pres_od[LTR_HP_OUT], &mpc_pc_cycle->mc_co2_props);
	if (prop_error_code != 0)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_pc_cycle->mv_enth_od[LTR_HP_OUT] = mpc_pc_cycle->mc_co2_props.enth;	//[kJ/kg]
	mpc_pc_cycle->mv_entr_od[LTR_HP_OUT] = mpc_pc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_pc_cycle->mv_dens_od[LTR_HP_OUT] = mpc_pc_cycle->mc_co2_props.dens;	//[kg/m^3]

			// Enthalpy balance at mixer
	if (mpc_pc_cycle->m_m_dot_rc > 1.E-12)
	{
		double f_recomp = m_m_dot_rc / m_m_dot_t;	//[-]
			// Conservation of energy
		mpc_pc_cycle->mv_enth_od[MIXER_OUT] = (1.0 - f_recomp)*mpc_pc_cycle->mv_enth_od[LTR_HP_OUT] +
												f_recomp*mpc_pc_cycle->mv_enth_od[RC_OUT];
		prop_error_code = CO2_PH(mpc_pc_cycle->mv_pres_od[MIXER_OUT], mpc_pc_cycle->mv_enth_od[MIXER_OUT], &mpc_pc_cycle->mc_co2_props);
		if (prop_error_code != 0)
		{
			*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return prop_error_code;
		}
		mpc_pc_cycle->mv_temp_od[MIXER_OUT] = mpc_pc_cycle->mc_co2_props.temp;	//[K]
		mpc_pc_cycle->mv_entr_od[MIXER_OUT] = mpc_pc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
		mpc_pc_cycle->mv_dens_od[MIXER_OUT] = mpc_pc_cycle->mc_co2_props.dens;	//[kg/m^3]
	}
	else
	{
		mpc_pc_cycle->mv_enth_od[MIXER_OUT] = mpc_pc_cycle->mv_enth_od[LTR_HP_OUT];		//[kJ/kg]
		mpc_pc_cycle->mv_temp_od[MIXER_OUT] = mpc_pc_cycle->mv_temp_od[LTR_HP_OUT];		//[K]
		mpc_pc_cycle->mv_entr_od[MIXER_OUT] = mpc_pc_cycle->mv_entr_od[LTR_HP_OUT];		//[kJ/kg-K]
		mpc_pc_cycle->mv_dens_od[MIXER_OUT] = mpc_pc_cycle->mv_dens_od[LTR_HP_OUT];		//[kg/m^3]
	}

		// High temperature recuperator
	double T_HTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();
	double Q_dot_HTR = std::numeric_limits<double>::quiet_NaN();
	try
	{
		mpc_pc_cycle->mc_HTR.off_design_solution_fixed_dP(mpc_pc_cycle->mv_temp_od[MIXER_OUT], mpc_pc_cycle->mv_pres_od[MIXER_OUT],
			m_m_dot_t, mpc_pc_cycle->mv_pres_od[HTR_HP_OUT],
			mpc_pc_cycle->mv_temp_od[TURB_OUT], mpc_pc_cycle->mv_pres_od[TURB_OUT], 
			m_m_dot_t, mpc_pc_cycle->mv_pres_od[HTR_LP_OUT],
            m_od_tol,
			Q_dot_HTR, mpc_pc_cycle->mv_temp_od[HTR_HP_OUT], T_HTR_LP_out_calc);
	}
	catch (C_csp_exception & csp_except)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		if (csp_except.m_error_code == 0)
			return -1;
		else
			return csp_except.m_error_code;
	}

	*diff_T_HTR_LP_out = T_HTR_LP_out_calc - mpc_pc_cycle->mv_temp_od[HTR_LP_OUT];	//[K]

	return 0;
}

int C_PartialCooling_Cycle::C_MEQ__f_recomp__y_N_rc::operator()(double f_recomp /*-*/, double *diff_N_rc /*-*/)
{
	// Solve for turbine mass flow rate
	C_MEQ__t_m_dot__bal_turbomachinery c_turbo_bal(mpc_pc_cycle, m_T_pc_in, 
													m_P_pc_in, 
													m_T_mc_in,
													f_recomp,
													m_T_t_in,
													m_f_mc_pc_bypass);

	C_monotonic_eq_solver c_turbo_bal_solver(c_turbo_bal);

	// Set lower bound on mass flow rate
	double m_dot_lower = mpc_pc_cycle->ms_des_solved.m_m_dot_t * 1.E-3;		//[kg/s]
	double m_dot_upper = std::numeric_limits<double>::quiet_NaN();

	// Set solver settings
	// Because this application of the solver is trying to get the calculated mass flow rate to match the guess
	//  ... then need to calculate the error in the operator() method
	// So the error is already relative, and the solver is comparing against absolute value of 0
	c_turbo_bal_solver.settings(m_od_tol, 100, m_dot_lower, m_dot_upper, false);

	// Generate two guess values
	double m_dot_guess_upper = mpc_pc_cycle->ms_des_solved.m_m_dot_t;		//[kg/s]
	double y_m_dot_guess_upper = std::numeric_limits<double>::quiet_NaN();	//[-]

	int m_dot_guess_iter = 0;
	int m_dot_err_code = 1;
	while (m_dot_err_code != 0)
	{
		if (m_dot_guess_iter > 0)
			m_dot_guess_upper *= 0.9;
		if (m_dot_guess_iter > 10)
		{
			*diff_N_rc = std::numeric_limits<double>::quiet_NaN();
			return -71;
		}
		m_dot_guess_iter++;
		m_dot_err_code = c_turbo_bal_solver.call_mono_eq(m_dot_guess_upper, &y_m_dot_guess_upper);
	}

	double m_dot_guess_lower = 0.7*m_dot_guess_upper;		//[kg/s]

															// Now, solve for the turbine mass flow rate
	double m_dot_t_solved, tol_solved;
	m_dot_t_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int m_dot_t_code = c_turbo_bal_solver.solve(m_dot_guess_lower, m_dot_guess_upper, 0.0,
		m_dot_t_solved, tol_solved, iter_solved);

	if (m_dot_t_code != C_monotonic_eq_solver::CONVERGED)
	{
		*diff_N_rc = std::numeric_limits<double>::quiet_NaN();
		return m_dot_t_code;
	}

	m_m_dot_mc = c_turbo_bal.m_m_dot_mc;		//[kg/s]
	m_m_dot_pc = c_turbo_bal.m_m_dot_pc;		//[kg/s]
	m_m_dot_LTR_HP = c_turbo_bal.m_m_dot_LTR_HP;	//[kg/s]

	m_m_dot_t = m_dot_t_solved;	//[kg/s]
	m_m_dot_rc = m_m_dot_t * f_recomp;	//[kg/s]

	// Now we know the required recompressor performance, so we can solve the recompressor
	//     model for shaft speed and report design/target
	if (m_m_dot_rc > 1.E-12)
	{
		int rc_error_code = 0;
		mpc_pc_cycle->mc_rc.off_design_given_P_out(mpc_pc_cycle->mv_temp_od[PC_OUT], mpc_pc_cycle->mv_pres_od[PC_OUT], m_m_dot_rc,
			mpc_pc_cycle->mv_pres_od[RC_OUT], m_od_tol, rc_error_code, mpc_pc_cycle->mv_temp_od[RC_OUT]);

		if (rc_error_code != 0)
		{
			*diff_N_rc = std::numeric_limits<double>::quiet_NaN();
			return rc_error_code;
		}

		// Get recompressor shaft speed
		double N_rc = mpc_pc_cycle->mc_rc.get_od_solved()->m_N;

		// Get difference between solved and design shaft speed
		*diff_N_rc = (N_rc - m_N_rc_od_target) / m_N_rc_od_target;
	}
	else
	{
		*diff_N_rc = 0.0;
	}

	return 0;
}

int C_PartialCooling_Cycle::C_MEQ__t_m_dot__bal_turbomachinery::operator()(double m_dot_t_in /*kg/s*/, double *diff_m_dot_t /*-*/)
{
	// Calculate the main compressor mass flow rate
	m_m_dot_LTR_HP = (1.0 - m_f_recomp)*m_dot_t_in;		//[kg/s]
	m_m_dot_mc = m_m_dot_LTR_HP/(1.0 - 3.0*m_f_mc_pc_bypass);	//[kg/s]
	m_m_dot_pc = m_dot_t_in / (1.0 - m_f_mc_pc_bypass);		//[kg/s]

	// Calculate Pre-compressor performance
	int pc_error_code = 0;
	double T_pc_out, P_pc_out;
	T_pc_out = P_pc_out = std::numeric_limits<double>::quiet_NaN();

    if (mpc_pc_cycle->ms_od_par.m_is_pc_N_od_at_design) {
        mpc_pc_cycle->mc_pc.off_design_at_N_des(m_T_pc_in, m_P_pc_in, m_m_dot_pc, pc_error_code,
            T_pc_out, P_pc_out);
    }
    else {
        double N_pc_od = mpc_pc_cycle->ms_od_par.m_pc_N_od_f_des * mpc_pc_cycle->ms_des_solved.ms_pc_ms_des_solved.m_N_design;  //[rpm]
        mpc_pc_cycle->mc_pc.off_design_given_N(m_T_pc_in, m_P_pc_in, m_m_dot_pc, N_pc_od, pc_error_code, T_pc_out, P_pc_out);
    }

	// Check that pre-compressor performance solved
	if (pc_error_code != 0)
	{
		*diff_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		return pc_error_code;
	}

	mpc_pc_cycle->mv_pres_od[PC_OUT] = P_pc_out;	//[kPa]
	mpc_pc_cycle->mv_temp_od[PC_OUT] = T_pc_out;	//[K]

	// Calculate scaled pressure drop through main compressor cooler
	std::vector<double> DP_cooler_main;
	std::vector<double> m_dot_cooler_main;
	m_dot_cooler_main.push_back(0.0);
	m_dot_cooler_main.push_back(m_m_dot_mc);		//[kg/s]
	mpc_pc_cycle->mc_cooler_mc.hxr_pressure_drops(m_dot_cooler_main, DP_cooler_main);

    // Calculating HX *outlet* from dP, so adding 'calc less guess' here because a + value means a smaller pressure drop
    double P_mc_in_est = mpc_pc_cycle->mv_pres_od[PC_OUT] - DP_cooler_main[1];	//[kPa]
    mpc_pc_cycle->mv_pres_od[MC_IN] = P_mc_in_est + 0.75*mpc_pc_cycle->ms_od_deltaP.m_od_diff_P_mc_cooler_out_calc_less_guess;

	// Calculate main compressor performance
	int mc_error_code = 0;
	double T_mc_out, P_mc_out;
	T_mc_out = P_mc_out = std::numeric_limits<double>::quiet_NaN();

	// Solve main compressor performance at design/target shaft speed
    if (mpc_pc_cycle->ms_od_par.m_is_mc_N_od_at_design) {
        mpc_pc_cycle->mc_mc.off_design_at_N_des(m_T_mc_in, mpc_pc_cycle->mv_pres_od[MC_IN], m_m_dot_mc,
            mc_error_code, T_mc_out, P_mc_out);
    }
    else {
        double N_mc_od = mpc_pc_cycle->ms_od_par.m_mc_N_od_f_des * mpc_pc_cycle->ms_des_solved.ms_mc_ms_des_solved.m_N_design;  //[rpm]
        mpc_pc_cycle->mc_mc.off_design_given_N(m_T_mc_in, mpc_pc_cycle->mv_pres_od[MC_IN], m_m_dot_mc, N_mc_od,
            mc_error_code, T_mc_out, P_mc_out);
    }

	// Check that main compressor performance solved
	if (mc_error_code != 0)
	{
		*diff_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		return mc_error_code;
	}

	mpc_pc_cycle->mv_pres_od[MC_OUT] = P_mc_out;	//[kPa]
	mpc_pc_cycle->mv_temp_od[MC_OUT] = T_mc_out;	//[K]

	// Calculate scaled pressure drops through remaining heat exchangers
		// Low temp recuperator
	std::vector<double> DP_LTR;
	DP_LTR.resize(2);
	DP_LTR[0] = mpc_pc_cycle->mc_LTR.od_delta_p_cold(m_m_dot_LTR_HP);
	DP_LTR[1] = mpc_pc_cycle->mc_LTR.od_delta_p_hot(m_dot_t_in);
		// High temp recuperator
	std::vector<double> DP_HTR;
	DP_HTR.resize(2);
	DP_HTR[0] = mpc_pc_cycle->mc_HTR.od_delta_p_cold(m_dot_t_in);
	DP_HTR[1] = mpc_pc_cycle->mc_HTR.od_delta_p_hot(m_dot_t_in);
		// Primary heat exchanger
	std::vector<double> DP_PHX;
	std::vector<double> m_dot_PHX;
	m_dot_PHX.push_back(m_dot_t_in);
	m_dot_PHX.push_back(0.0);
	mpc_pc_cycle->mc_PHX.hxr_pressure_drops(m_dot_PHX, DP_PHX);
		// Pre-compressor cooler
	std::vector<double> DP_cooler_pre;
	std::vector<double> m_dot_cooler_pre;
	m_dot_cooler_pre.push_back(0.0);
	m_dot_cooler_pre.push_back(m_m_dot_pc);
	mpc_pc_cycle->mc_cooler_pc.hxr_pressure_drops(m_dot_cooler_pre, DP_cooler_pre);
		
	// Apply pressure drops to heat exchangers, fully defining the pressure at all states
    // Calculating HX *outlet* from dP, so adding 'calc less guess' here because a + value means a smaller pressure drop
    double P_LTR_hp_out_est = mpc_pc_cycle->mv_pres_od[MC_OUT] - DP_LTR[0];
    mpc_pc_cycle->mv_pres_od[LTR_HP_OUT] = P_LTR_hp_out_est + 0.75*mpc_pc_cycle->ms_od_deltaP.m_od_diff_P_LTR_HP_out_calc_less_guess;
	
    mpc_pc_cycle->mv_pres_od[MIXER_OUT] = mpc_pc_cycle->mv_pres_od[LTR_HP_OUT];
	mpc_pc_cycle->mv_pres_od[RC_OUT] = mpc_pc_cycle->mv_pres_od[LTR_HP_OUT];

    // Calculating HX *outlet* from dP, so adding 'calc less guess' here because a + value means a smaller pressure drop
    double P_HTR_hp_out_est = mpc_pc_cycle->mv_pres_od[MIXER_OUT] - DP_HTR[0];
    mpc_pc_cycle->mv_pres_od[HTR_HP_OUT] = P_HTR_hp_out_est + 0.75*mpc_pc_cycle->ms_od_deltaP.m_od_diff_P_HTR_HP_out_calc_less_guess;

    // Calculating HX *outlet* from dP, so adding 'calc less guess' here because a + value means a smaller pressure drop
    double P_t_in_est = mpc_pc_cycle->mv_pres_od[HTR_HP_OUT] - DP_PHX[0];
    double P_t_in = mpc_pc_cycle->mv_pres_od[TURB_IN] = P_t_in_est + 0.75*mpc_pc_cycle->ms_od_deltaP.m_od_diff_P_PHX_out_calc_less_guess;

    // Calculating HX *inlet* from dP, so subtracting 'calc less guess' here because a + value means a smaller pressure drop
    double P_LTR_LP_out_est = mpc_pc_cycle->mv_pres_od[PC_IN] + DP_cooler_pre[1];
    mpc_pc_cycle->mv_pres_od[LTR_LP_OUT] = P_LTR_LP_out_est - 0.75*mpc_pc_cycle->ms_od_deltaP.m_od_diff_P_pc_cooler_out_calc_less_guess;

    // Calculating HX *inlet* from dP, so subtracting 'calc less guess' here because a + value means a smaller pressure drop
    double P_HTR_LP_out_est = mpc_pc_cycle->mv_pres_od[LTR_LP_OUT] + DP_LTR[1];
    mpc_pc_cycle->mv_pres_od[HTR_LP_OUT] = P_HTR_LP_out_est - 0.75*mpc_pc_cycle->ms_od_deltaP.m_od_diff_P_LTR_LP_out_calc_less_guess;
	
    // Calculating HX *inlet* from dP, so subtracting 'calc less guess' here because a + value means a smaller pressure drop
    double P_t_out_est = mpc_pc_cycle->mv_pres_od[HTR_LP_OUT] + DP_HTR[1];
    double P_t_out = mpc_pc_cycle->mv_pres_od[TURB_OUT] = P_t_out_est - 0.75*mpc_pc_cycle->ms_od_deltaP.m_od_diff_P_HTR_LP_out_calc_less_guess;

	// Calculate turbine performance
	int t_err_code = 0;
	double m_dot_t_calc, T_t_out;
	m_dot_t_calc = T_t_out = std::numeric_limits<double>::quiet_NaN();
	mpc_pc_cycle->mc_t.od_turbine_at_N_des(m_T_t_in, P_t_in, P_t_out, t_err_code, m_dot_t_calc, T_t_out);

	// Check that turbine performance solved
	if (t_err_code != 0)
	{
		*diff_m_dot_t = std::numeric_limits<double>::quiet_NaN();
		return t_err_code;
	}

	mpc_pc_cycle->mv_temp_od[TURB_OUT] = T_t_out;	//[K]

	// Calculate difference between calculated and guessed mass flow rates
	*diff_m_dot_t = (m_dot_t_calc - m_dot_t_in) / m_dot_t_in;

	return 0;
}

int C_PartialCooling_Cycle::off_design_fix_shaft_speeds(S_od_par & od_phi_par_in, double od_tol)
{
	ms_od_par = od_phi_par_in;

	return off_design_fix_shaft_speeds_core(od_tol);
}

void C_PartialCooling_Cycle::check_od_solution(double & diff_m_dot, double & diff_E_cycle,
    double & diff_Q_LTR, double & diff_Q_HTR)
{
    CO2_state c_co2_props;

    double m_dot_pc = mc_pc.get_od_solved()->m_m_dot; //[kg/s]   
    double m_dot_mc = mc_mc.get_od_solved()->m_m_dot;  //m_m_dot_mc; // ms_od_solved.m_m_dot_mc;  //[kg/s]
    double m_dot_rc = mc_rc.get_od_solved()->m_m_dot; //m_m_dot_rc; // ms_od_solved.m_m_dot_rc;  //[kg/s]
    if (!std::isfinite(m_dot_rc))
    {
        m_dot_rc = 0.0;
    }
    double m_dot_t = mc_t.get_od_solved()->m_m_dot;           //[kg/s]

    double diff_m_dot_pc_t = (m_dot_pc - m_dot_t) / m_dot_t;    //[-]
    double diff_m_dot_mc_rc_t = ((m_dot_mc + m_dot_rc) - m_dot_t) / m_dot_t;    //[-]

    if (fabs(diff_m_dot_mc_rc_t) > fabs(diff_m_dot_pc_t))
    {
        diff_m_dot = diff_m_dot_mc_rc_t;    //[-]
    }
    else
    {
        diff_m_dot = diff_m_dot_pc_t;       //[-]
    }
     
    double P_co2_phx_in = mv_pres_od[HTR_HP_OUT];    //[kPa]
    double T_co2_phx_in = mv_temp_od[HTR_HP_OUT];    //[K]
    int co2_err = CO2_TP(T_co2_phx_in, P_co2_phx_in, &c_co2_props);
    double h_co2_phx_in = c_co2_props.enth;         //[kJ/kg]

    double P_t_in = mv_pres_od[TURB_IN];             //[kPa]
    double T_t_in = mv_temp_od[TURB_IN];             //[K]
    co2_err = CO2_TP(T_t_in, P_t_in, &c_co2_props);
    double h_co2_t_in = c_co2_props.enth;           //[kJ/kg]

    double Q_dot_phx = m_dot_t * (h_co2_t_in - h_co2_phx_in); //[kWt]

    double P_t_out = mv_pres_od[TURB_OUT];           //[kPa]
    double T_t_out = mv_temp_od[TURB_OUT];           //[K]
    co2_err = CO2_TP(T_t_out, P_t_out, &c_co2_props);
    double h_t_out = c_co2_props.enth;      //[kJ/kg]

    double W_dot_t = m_dot_t * (h_co2_t_in - h_t_out);    //[kWe]

    double P_pc_cool_in = mv_pres_od[LTR_LP_OUT];   //[kPa]
    double T_pc_cool_in = mv_temp_od[LTR_LP_OUT];   //[K]
    co2_err = CO2_TP(T_pc_cool_in, P_pc_cool_in, &c_co2_props);
    double h_pc_cool_in = c_co2_props.enth;        //[kJ/kg]

    double P_pc_in = mv_pres_od[PC_IN];         //[kPa]
    double T_pc_in = mv_temp_od[PC_IN];         //[K]
    co2_err = CO2_TP(T_pc_in, P_pc_in, &c_co2_props);
    double h_pc_in = c_co2_props.enth;          //[kJ/kg]

    double Q_dot_pc_cool = m_dot_pc*(h_pc_cool_in - h_pc_in);   //[kWt]

    double P_mc_cool_in = mv_pres_od[PC_OUT];   //[kPa]
    double T_mc_cool_in = mv_temp_od[PC_OUT];   //[K]
    co2_err = CO2_TP(T_mc_cool_in, P_mc_cool_in, &c_co2_props);
    double h_mc_cool_in = c_co2_props.enth;     //[kJ/kg]

    double W_dot_pc = m_dot_pc * (h_mc_cool_in - h_pc_in);      //[kWe]

    double P_mc_in = mv_pres_od[MC_IN];          //[kPa]
    double T_mc_in = mv_temp_od[MC_IN];          //[K]
    co2_err = CO2_TP(T_mc_in, P_mc_in, &c_co2_props);
    double h_mc_in = c_co2_props.enth;          //[kJ/kg]

    double Q_dot_mc_cool = m_dot_mc * (h_mc_cool_in - h_mc_in); //[kWt]

    double P_rc_out = mv_pres_od[RC_OUT];        //[kPa]
    double T_rc_out = mv_temp_od[RC_OUT];        //[K]
    co2_err = CO2_TP(T_rc_out, P_rc_out, &c_co2_props);
    double h_rc_out = c_co2_props.enth;         //[kJ/kg]

    double W_dot_rc = m_dot_rc * (h_rc_out - h_mc_cool_in);  //[kWe]

    double P_mc_out = mv_pres_od[MC_OUT];        //[kPa]
    double T_mc_out = mv_temp_od[MC_OUT];        //[K]
    co2_err = CO2_TP(T_mc_out, P_mc_out, &c_co2_props);
    double h_mc_out = c_co2_props.enth;         //[kJ/kg]

    double W_dot_mc = m_dot_mc * (h_mc_out - h_mc_in);    //[kWe]

    diff_E_cycle = (Q_dot_phx - Q_dot_mc_cool - Q_dot_pc_cool - (W_dot_t - W_dot_mc - W_dot_rc - W_dot_pc)) / Q_dot_phx;

    double Q_dot_cool_from_eta = Q_dot_phx * (1.0 - ms_od_solved.m_eta_thermal);
    double diff_E_reject = (Q_dot_cool_from_eta - Q_dot_mc_cool - Q_dot_pc_cool) / (Q_dot_pc_cool + Q_dot_mc_cool);

    double P_LTR_HP_out = mv_pres_od[LTR_HP_OUT];    //[kPa]
    double T_LTR_HP_out = mv_temp_od[LTR_HP_OUT];    //[K]
    co2_err = CO2_TP(T_LTR_HP_out, P_LTR_HP_out, &c_co2_props);
    double h_LTR_HP_out = c_co2_props.enth;         //[kJ/kg]

    double Q_dot_LTR_HP = m_dot_mc * (h_LTR_HP_out - h_mc_out); //[kWt]

    double P_HTR_LP_out = mv_pres_od[HTR_LP_OUT];    //[kPa]
    double T_HTR_LP_out = mv_temp_od[HTR_LP_OUT];    //[K]
    co2_err = CO2_TP(T_HTR_LP_out, P_HTR_LP_out, &c_co2_props);
    double h_HTR_LP_out = c_co2_props.enth;         //[kJ/kg]

    double Q_dot_LTR_LP = m_dot_t * (h_HTR_LP_out - h_pc_cool_in);   //[kWt]
    diff_Q_LTR = (Q_dot_LTR_HP - Q_dot_LTR_LP) / Q_dot_LTR_LP;
    double Q_dot_HTR_LP = m_dot_t * (h_t_out - h_HTR_LP_out);     //[kWt]

    double P_HTR_HP_in = mv_pres_od[MIXER_OUT];      //[kPa]
    double T_HTR_HP_in = mv_temp_od[MIXER_OUT];      //[K]
    co2_err = CO2_TP(T_HTR_HP_in, P_HTR_HP_in, &c_co2_props);
    double h_HTR_HP_in = c_co2_props.enth;          //[kJ/kg]

    double Q_dot_HTR_HP = m_dot_t * (h_co2_phx_in - h_HTR_HP_in); //[kWt]
    diff_Q_HTR = (Q_dot_HTR_HP - Q_dot_HTR_LP) / Q_dot_HTR_LP;

}

double nlopt_cb_opt_partialcooling_des(const std::vector<double> &x, std::vector<double> &grad, void *data)
{
	C_PartialCooling_Cycle *frame = static_cast<C_PartialCooling_Cycle*>(data);
	if (frame != NULL)
		return frame->design_cycle_return_objective_metric(x);
	else
		return 0.0;
}

double fmin_cb_opt_partialcooling_des_fixed_P_high(double P_high /*kPa*/, void *data)
{
	C_PartialCooling_Cycle *frame = static_cast<C_PartialCooling_Cycle*>(data);

	return frame->opt_eta_fixed_P_high(P_high);
}
