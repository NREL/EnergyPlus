#include "sco2_cycle_components.h"
#include "CO2_properties.h"
#include <limits>
#include <algorithm>

#include "numeric_solvers.h"
#include "csp_solver_core.h"

#include "sco2_cycle_templates.h"

const double C_turbine::m_nu_design = 0.7476;

void calculate_turbomachinery_outlet_1(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, 
	double eta_isen /*-*/, bool is_comp, int & error_code, double & spec_work /*kJ/kg*/)
{
	double enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out;

	calculate_turbomachinery_outlet_1(T_in, P_in, P_out, eta_isen, is_comp, error_code, enth_in, entr_in, dens_in, temp_out, enth_out, entr_out, dens_out, spec_work);
}

void calculate_turbomachinery_outlet_1(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, 
	double eta_isen /*-*/, bool is_comp, int & error_code, 
	double & enth_in /*kJ/kg*/, double & entr_in /*kJ/kg-K*/, double & dens_in /*kg/m3*/, double & temp_out /*K*/, 
	double & enth_out /*kJ/kg*/, double & entr_out /*kJ/kg-K*/, double & dens_out /*kg/m3*/, double & spec_work /*kJ/kg*/)
{
	/*Calculates the outlet state of a compressor or turbine using its isentropic efficiency.
	is_comp = .true.means the turbomachine is a compressor(w = w_s / eta)
	is_comp = .false.means the turbomachine is a turbine(w = w_s * eta) */

	CO2_state co2_props;

	error_code = 0;

	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);		// properties at the inlet conditions
	if (prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;
	dens_in = co2_props.dens;

	prop_error_code = CO2_PS(P_out, s_in, &co2_props);			// outlet enthalpy if compression/expansion is isentropic
	if (prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	double w_s = h_in - h_s_out;			// specific work if process is isentropic (negative for compression, positive for expansion)

	double w = 0.0;
	if (is_comp)
		w = w_s / eta_isen;						// actual specific work of compressor (negative)
	else
		w = w_s * eta_isen;						// actual specific work of turbine (positive)

	double h_out = h_in - w;

	prop_error_code = CO2_PH(P_out, h_out, &co2_props);
	if (prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}

	enth_in = h_in;
	entr_in = s_in;
	temp_out = co2_props.temp;
	enth_out = h_out;
	entr_out = co2_props.entr;
	dens_out = co2_props.dens;
	spec_work = w;

	return;
};

void isen_eta_from_poly_eta(double T_in /*K*/, double P_in /*kPa*/, double P_out /*kPa*/, double poly_eta /*-*/, bool is_comp, int & error_code, double & isen_eta)
{
	/* 9.3.14: code written by John Dyreby, translated to C++ by Ty Neises
	! Calculate the isentropic efficiency that corresponds to a given polytropic efficiency
	! for the expansion or compression from T_in and P_in to P_out.
	!
	! Inputs:
	!   T_in -- inlet temperature (K)
	!   P_in -- inlet pressure (kPa)
	!   P_out -- outlet pressure (kPa)
	!   poly_eta -- polytropic efficiency (-)
	!   is_comp -- if .true., model a compressor (w = w_s / eta); if .false., model a turbine (w = w_s * eta)
	!
	! Outputs:
	!   error_trace -- an ErrorTrace object
	!   isen_eta -- the equivalent isentropic efficiency (-)
	!
	! Notes:
	!   1) Integration of small DP is approximated numerically by using 200 stages.
	!   2) No error checking is performed on the inlet and outlet pressures; valid pressure ratios are assumed. */

	CO2_state co2_props;

	// Properties at the inlet conditions
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if (prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;

	// Outlet enthalpy if compression/expansion is isentropic
	prop_error_code = CO2_PS(P_out, s_in, &co2_props);
	if (prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	double stage_P_in = P_in;		// Initialize first stage inlet pressure
	double stage_h_in = h_in;		// Initialize first stage inlet enthalpy
	double stage_s_in = s_in;		// Initialize first stage inlet entropy

	int N_stages = 200;

	double stage_DP = (P_out - P_in) / (double)N_stages;

	double stage_P_out = -999.9;
	double stage_h_out = -999.9;

	for (int i = 1; i <= N_stages; i++)
	{
		stage_P_out = stage_P_in + stage_DP;

		// Outlet enthalpy if compression/expansion is isentropic
		prop_error_code = CO2_PS(stage_P_out, stage_s_in, &co2_props);
		if (prop_error_code != 0)
		{
			error_code = prop_error_code;
			return;
		}
		double stage_h_s_out = co2_props.enth;

		double w_s = stage_h_in - stage_h_s_out;		// specific work if process is isentropic
		double w = std::numeric_limits<double>::quiet_NaN();
		if (is_comp)
			w = w_s / poly_eta;
		else
			w = w_s * poly_eta;
		stage_h_out = stage_h_in - w;

		// Reset next stage inlet values
		stage_P_in = stage_P_out;
		stage_h_in = stage_h_out;

        prop_error_code = CO2_PH(stage_P_in, stage_h_in, &co2_props);
        if (prop_error_code != 0)
        {
	        error_code = prop_error_code;
	        return;
        }
        stage_s_in = co2_props.entr;
	}

	// Note: last stage outlet enthalpy is equivalent to turbomachinery outlet enthalpy
	if (is_comp)
		isen_eta = (h_s_out - h_in) / (stage_h_out - h_in);
	else
		isen_eta = (stage_h_out - h_in) / (h_s_out - h_in);
}

int calc_turbomachinery_eta_isen(double T_in /*K*/, double P_in /*kPa*/,
	double T_out /*K*/, double P_out /*kPa*/, double & eta_isen)
{
	CO2_state co2_props;

	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);		// properties at the inlet conditions
	if (prop_error_code != 0)
	{
		return prop_error_code;
	}
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;

	prop_error_code = CO2_TP(T_out, P_out, &co2_props);		// properties at the inlet conditions
	if (prop_error_code != 0)
	{
		return prop_error_code;
	}
	double h_out = co2_props.enth;

	prop_error_code = CO2_PS(P_out, s_in, &co2_props);			// outlet enthalpy if compression/expansion is isentropic
	if (prop_error_code != 0)
	{
		return prop_error_code;
	}
	double h_s_out = co2_props.enth;
	
	if (P_out > P_in)
		eta_isen = (h_s_out - h_in) / (h_out - h_in);
	else
		eta_isen = (h_out - h_in) / (h_s_out - h_in);

	if (eta_isen > 1.00001)     // leave some room for tolerance in property routines
		return -2;

	return 0;
}

int Ts_data_over_linear_dP_ds(double P_in /*kPa*/, double s_in /*kJ/kg-K*/, double P_out /*kPa*/, double s_out /*kJ/kg-K*/,
	std::vector<double> & T_data /*C*/, std::vector<double> & s_data /*kJ/kg-K*/, int N_points /*=30*/)
{
	CO2_state co2_props;

	int err_code = 0;

	double deltaP = (P_in - P_out) / double(N_points - 1);	//[kPa]
	double deltas = (s_in - s_out) / double(N_points - 1);	//[kJ/kg-K]

	T_data.resize(N_points);	//[C]
	s_data.resize(N_points);	//[kJ/kg-K]

	double P_local = std::numeric_limits<double>::quiet_NaN();	//[kPa]
	double s_local = std::numeric_limits<double>::quiet_NaN();	//[kJ/kg-K]

	for (int i = 0; i < N_points; i++)
	{
		s_local = s_in - deltas * i;	//[kJ/kg-K]
		P_local = P_in - deltaP * i;	//[kPa]

		err_code = CO2_PS(P_local, s_local, &co2_props);
		if (err_code != 0)
			return err_code;

		T_data[i] = co2_props.temp - 273.15;		//[C]
		s_data[i] = co2_props.entr;		//[kJ/kg-K]
	}

	return 0;
}

int Ph_data_over_turbomachinery(double T_in /*K*/, double P_in /*kPa*/,
	double T_out /*K*/, double P_out /*kPa*/,
	std::vector<double> & P_data /*MPa*/, std::vector<double> & h_data /*kJ/kg*/, int N_points /*30*/)
{
	CO2_state co2_props;

	int err_code = 0;

	double eta_isen = std::numeric_limits<double>::quiet_NaN();
	err_code = calc_turbomachinery_eta_isen(T_in, P_in, T_out, P_out, eta_isen);
	if (err_code != 0)
	{
		return err_code;
	}

	P_data.resize(N_points);
	h_data.resize(N_points);

	err_code = CO2_TP(T_in, P_in, &co2_props);
	if (err_code != 0)
	{
		return err_code;
	}

	P_data[0] = P_in / 1.E3;	//[MPa]
	h_data[0] = co2_props.enth;	//[kJ/kg]

	double deltaP = (P_in - P_out) / double(N_points - 1);	//[kPa]

	bool is_comp = false;
	if (deltaP < 0.0)
		is_comp = true;

	double P_local = std::numeric_limits<double>::quiet_NaN();	//[kPa]
	double h_in_local = std::numeric_limits<double>::quiet_NaN();
	double s_in_local = std::numeric_limits<double>::quiet_NaN();
	double rho_in_local = std::numeric_limits<double>::quiet_NaN();
	double T_out_local = std::numeric_limits<double>::quiet_NaN();
	double h_out_local = std::numeric_limits<double>::quiet_NaN();	//[kJ/kg]
	double s_out_local = std::numeric_limits<double>::quiet_NaN();
	double rho_out_local = std::numeric_limits<double>::quiet_NaN();
	double spec_work_local = std::numeric_limits<double>::quiet_NaN();

	for (int i = 1; i < N_points; i++)
	{
		P_local = P_in - deltaP * i;	//[kPa]

		calculate_turbomachinery_outlet_1(T_in, P_in, P_local,
			eta_isen, is_comp, err_code,
			h_in_local, s_in_local, rho_in_local, T_out_local,
			h_out_local, s_out_local, rho_out_local, spec_work_local);

		if (err_code != 0)
		{
			return err_code;
		}

		P_data[i] = P_local / 1.E3;	//[MPa]
		h_data[i] = h_out_local;	//[kJ/kg]
	}

	return 0;
}

int sco2_cycle_plot_data_TS(int cycle_config,
	const std::vector<double> pres /*kPa*/,
	const std::vector<double> entr /*kJ/kg-K*/,
	std::vector<double> & T_LTR_HP /*C*/,
	std::vector<double> & s_LTR_HP /*kJ/kg-K*/,
	std::vector<double> & T_HTR_HP /*C*/,
	std::vector<double> & s_HTR_HP /*kJ/kg-K*/,
	std::vector<double> & T_PHX	   /*C*/,
	std::vector<double> & s_PHX    /*kJ/kg-K*/,
	std::vector<double> & T_HTR_LP /*C*/,
	std::vector<double> & s_HTR_LP /*kJ/kg-K*/,
	std::vector<double> & T_LTR_LP /*C*/,
	std::vector<double> & s_LTR_LP /*kJ/kg-K*/,
	std::vector<double> & T_main_cooler /*C*/,
	std::vector<double> & s_main_cooler /*kJ/kg-K*/,
	std::vector<double> & T_pre_cooler /*C*/,
	std::vector<double> & s_pre_cooler /*kJ/kg-K*/)
{
	int n_pres = pres.size();
	int n_entr = entr.size();

	// Get LTR HP data
	int err_code = Ts_data_over_linear_dP_ds(pres[C_sco2_cycle_core::MC_OUT], entr[C_sco2_cycle_core::MC_OUT],
		pres[C_sco2_cycle_core::LTR_HP_OUT], entr[C_sco2_cycle_core::LTR_HP_OUT],
		T_LTR_HP, s_LTR_HP, 25);
	if (err_code != 0)
		return err_code;

	// Get HTR HP data
	err_code = Ts_data_over_linear_dP_ds(pres[C_sco2_cycle_core::MIXER_OUT], entr[C_sco2_cycle_core::MIXER_OUT],
		pres[C_sco2_cycle_core::HTR_HP_OUT], entr[C_sco2_cycle_core::HTR_HP_OUT],
		T_HTR_HP, s_HTR_HP, 25);
	if (err_code != 0)
		return err_code;

	// Get PHX data
	err_code = Ts_data_over_linear_dP_ds(pres[C_sco2_cycle_core::HTR_HP_OUT], entr[C_sco2_cycle_core::HTR_HP_OUT],
		pres[C_sco2_cycle_core::TURB_IN], entr[C_sco2_cycle_core::TURB_IN],
		T_PHX, s_PHX, 25);
	if (err_code != 0)
		return err_code;

	// Get HTR HP data
	err_code = Ts_data_over_linear_dP_ds(pres[C_sco2_cycle_core::TURB_OUT], entr[C_sco2_cycle_core::TURB_OUT],
		pres[C_sco2_cycle_core::HTR_LP_OUT], entr[C_sco2_cycle_core::HTR_LP_OUT],
		T_HTR_LP, s_HTR_LP, 25);
	if (err_code != 0)
		return err_code;

	// Get LTR HP data
	err_code = Ts_data_over_linear_dP_ds(pres[C_sco2_cycle_core::HTR_LP_OUT], entr[C_sco2_cycle_core::HTR_LP_OUT],
		pres[C_sco2_cycle_core::LTR_LP_OUT], entr[C_sco2_cycle_core::LTR_LP_OUT],
		T_LTR_LP, s_LTR_LP, 25);
	if (err_code != 0)
		return err_code;

	if (cycle_config != 2)		// Recompression Cycle
	{
		if (n_pres < C_sco2_cycle_core::RC_OUT + 1 || n_entr != n_pres)
			return -1;

		// Get main cooler data
		err_code = Ts_data_over_linear_dP_ds(pres[C_sco2_cycle_core::LTR_LP_OUT], entr[C_sco2_cycle_core::LTR_LP_OUT],
			pres[C_sco2_cycle_core::MC_IN], entr[C_sco2_cycle_core::MC_IN],
			T_main_cooler, s_main_cooler, 25);
		if (err_code != 0)
			return err_code;

		// Set IP data
		T_pre_cooler.resize(1);
		T_pre_cooler[0] = T_main_cooler[0];
		s_pre_cooler.resize(1);
		s_pre_cooler[0] = s_main_cooler[0];
	}
	else		// Partial Cooling Cycle
	{
		if (n_pres < C_sco2_cycle_core::PC_OUT + 1 || n_entr != n_pres)
			return -1;

		// Get pre cooler data
		err_code = Ts_data_over_linear_dP_ds(pres[C_sco2_cycle_core::LTR_LP_OUT], entr[C_sco2_cycle_core::LTR_LP_OUT],
			pres[C_sco2_cycle_core::PC_IN], entr[C_sco2_cycle_core::PC_IN],
			T_pre_cooler, s_pre_cooler, 25);
		if (err_code != 0)
			return err_code;

		// Get main cooler data
		err_code = Ts_data_over_linear_dP_ds(pres[C_sco2_cycle_core::PC_OUT], entr[C_sco2_cycle_core::PC_OUT],
			pres[C_sco2_cycle_core::MC_IN], entr[C_sco2_cycle_core::MC_IN],
			T_main_cooler, s_main_cooler, 25);
		if (err_code != 0)
			return err_code;
	}

	return 0;
}

int sco2_cycle_plot_data_PH(int cycle_config,
	const std::vector<double> temp /*K*/,
	const std::vector<double> pres /*kPa*/,
	std::vector<double> & P_t /*MPa*/,
	std::vector<double> & h_t /*kJ/kg*/,
	std::vector<double> & P_mc /*MPa*/,
	std::vector<double> & h_mc /*kJ/kg*/,
	std::vector<double> & P_rc /*MPa*/,
	std::vector<double> & h_rc /*kJ/kg*/,
	std::vector<double> & P_pc /*MPa*/,
	std::vector<double> & h_pc /*kJ/kg*/)
{
	int n_pres = pres.size();
	int n_temp = temp.size();

	int err_code = Ph_data_over_turbomachinery(temp[C_sco2_cycle_core::TURB_IN], pres[C_sco2_cycle_core::TURB_IN],
		temp[C_sco2_cycle_core::TURB_OUT], pres[C_sco2_cycle_core::TURB_OUT],
		P_t, h_t, 25);

	if (err_code != 0)
		return err_code;

	err_code = Ph_data_over_turbomachinery(temp[C_sco2_cycle_core::MC_IN], pres[C_sco2_cycle_core::MC_IN],
		temp[C_sco2_cycle_core::MC_OUT], pres[C_sco2_cycle_core::MC_OUT],
		P_mc, h_mc, 25);

	if (err_code != 0)
		return err_code;

	if (cycle_config != 2)		// Recompression Cycle
	{
		if (n_pres < C_sco2_cycle_core::RC_OUT + 1 || n_temp != n_pres)
			return -1;

		// Recompressor
		err_code = Ph_data_over_turbomachinery(temp[C_sco2_cycle_core::LTR_LP_OUT], pres[C_sco2_cycle_core::LTR_LP_OUT],
			temp[C_sco2_cycle_core::RC_OUT], pres[C_sco2_cycle_core::RC_OUT],
			P_rc, h_rc, 25);

		if (err_code != 0)
			return err_code;

		// Precompressor
		P_pc.resize(1);
		P_pc[0] = P_mc[0];
		h_pc.resize(1);
		h_pc[0] = h_mc[0];
	}
	else		// Partial Cooling Cycle
	{
		if (n_pres < C_sco2_cycle_core::PC_OUT + 1 || n_temp != n_pres)
			return -1;

		// Recompressor
		err_code = Ph_data_over_turbomachinery(temp[C_sco2_cycle_core::PC_OUT], pres[C_sco2_cycle_core::PC_OUT],
			temp[C_sco2_cycle_core::RC_OUT], pres[C_sco2_cycle_core::RC_OUT],
			P_rc, h_rc, 25);

		if (err_code != 0)
			return err_code;

		// Precompressor
		err_code = Ph_data_over_turbomachinery(temp[C_sco2_cycle_core::PC_IN], pres[C_sco2_cycle_core::PC_IN],
			temp[C_sco2_cycle_core::PC_OUT], pres[C_sco2_cycle_core::PC_OUT],
			P_pc, h_pc, 25);

		if (err_code != 0)
			return err_code;
	}

	return 0;
}

int Ph_arrays_over_constT(double P_low /*MPa*/, double P_high /*MPa*/, std::vector<double> T_consts /*C*/,
	std::vector<std::vector<double>> & P_data /*MPa*/, std::vector<std::vector<double>> & h_data)
{
	CO2_state t_co2_props;
	int n_points = 200;
	P_low = P_low * 1.E3;		//[MPa] convert from kPa
	P_high = P_high * 1.E3;	//[MPa] convert from kPa
	double deltaP = (P_high - P_low) / (double)(n_points - 1);

	int n_T = T_consts.size();

	P_data.resize(n_T);
	h_data.resize(n_T);

	double P_i = std::numeric_limits<double>::quiet_NaN();

	int prop_err_code = 0;

	bool is_2phase_calc = false;

	double P_x1 = std::numeric_limits<double>::quiet_NaN();
	double h_x1 = std::numeric_limits<double>::quiet_NaN();

	for (int j = 0; j < n_T; j++)
	{
		P_data[j].resize(n_points);
		h_data[j].resize(n_points);

		for (int i = 0; i < n_points; i++)
		{
			P_i = P_low + deltaP * i;

			prop_err_code = CO2_TP(T_consts[j] + 273.13, P_i, &t_co2_props);
			if (prop_err_code != 0)
			{
				if (prop_err_code == 205)
				{
					prop_err_code = CO2_TQ(T_consts[j] + 273.15, 0.0, &t_co2_props);
					if (prop_err_code != 0)
					{
						return -1;
					}
					else if(!is_2phase_calc)
					{
						P_data[j][i] = t_co2_props.pres / 1.E3;		//[MPa]
						h_data[j][i] = t_co2_props.enth;			//[kJ/kg]

						prop_err_code = CO2_TQ(T_consts[j] + 273.15, 1.0, &t_co2_props);

						i++;

						P_x1 = t_co2_props.pres / 1.E3;		//[MPa]
						h_x1 = t_co2_props.enth;			//[kJ/kg]

						P_data[j][i] = P_x1;
						h_data[j][i] = h_x1;

						is_2phase_calc = true;
					}
					else
					{
						P_data[j][i] = P_x1;
						h_data[j][i] = h_x1;
					}
				}
				else
				{
					return -1;
				}
			}
			else
			{
				P_data[j][i] = t_co2_props.pres / 1.E3;		//[MPa]
				h_data[j][i] = t_co2_props.enth;			//[kJ/kg]
			}
		}
	}

	//double P_low = 5.0;		//[MPa]
	//double P_high = 30.0;	//[MPa]

	//std::vector<double> T_consts;
	//double T_low = 0.0;
	//double T_high = 800.0;
	//for (double T = T_low; T <= T_high; T += 50.0)
	//{
	//	T_consts.push_back(T);	//[C]
	//}

	//std::vector<std::vector<double>> P_data;
	//std::vector<std::vector<double>> h_data;

	//int ph_err = Ph_arrays_over_constT(P_low, P_high, T_consts,
	//	P_data, h_data);

	//ofstream myfile;
	//myfile.open("File.txt");

	//int n_T = T_consts.size();

	//for (int i = 0; i < n_T; i++)
	//{
	//	myfile << "P_" << to_string((int)(T_consts[i])) << ",";
	//	myfile << "h_" << to_string((int)(T_consts[i]));
	//	if (i < n_T - 1)
	//		myfile << ",";
	//	else
	//		myfile << "\n";
	//}

	//int n_points = P_data[0].size();
	//for (int j = 0; j < n_points; j++)
	//{
	//	for (int i = 0; i < n_T; i++)
	//	{
	//		myfile << P_data[i][j] << ",";
	//		myfile << h_data[i][j];
	//		if (i < n_T - 1)
	//			myfile << ",";
	//		else
	//			myfile << "\n";
	//	}
	//}

	//myfile.close();
	
	return 0;
}

int Ts_arrays_over_constP(double T_cold /*C*/, double T_hot /*C*/, std::vector<double> P_consts /*kPa*/,
	std::vector<std::vector<double>> & T_data /*C*/, std::vector<std::vector<double>> & s_data)
{
	CO2_state t_co2_props;
	int n_points = 200;
	T_cold = T_cold + 273.15;	//[K] convert from C
	T_hot = T_hot + 273.15;	//[K] convert from C

	int n_P = P_consts.size();

	T_data.resize(n_P);
	s_data.resize(n_P);

	for (int i = 0; i < n_P; i++)
	{
		int co2_err = CO2_TP(T_cold, P_consts[i], &t_co2_props);
		if (co2_err != 0)
			return co2_err;
		double s_cold = t_co2_props.entr;		//[kJ/kg-K]

		co2_err = CO2_TP(T_hot, P_consts[i], &t_co2_props);
		if (co2_err != 0)
			return co2_err;
		double s_hot = t_co2_props.entr;		//[kJ/kg-K]

		Ts_data_over_linear_dP_ds(P_consts[i], s_cold, P_consts[i], s_hot, T_data[i], s_data[i], n_points);
	}

	//double T_cold = 0.0;	//[C]
	//double T_hot = 750.0;	//[C]

	//std::vector<double> P_consts;
	//P_consts.push_back(35.E3);		//[kPa]
	//P_consts.push_back(25.E3);		//[kPa]
	//P_consts.push_back(15.E3);		//[kPa]
	//P_consts.push_back(5.E3);		//[kPa]

	//std::vector<std::vector<double>> T_data;
	//std::vector<std::vector<double>> s_data;

	//int errrrr = Ts_arrays_over_constP(T_cold, T_hot, P_consts,
	//	T_data, s_data);

	//ofstream myfile;
	//myfile.open("directory:/File.txt");

	//int n_P = P_consts.size();

	//for (int i = 0; i < n_P; i++)
	//{
	//	myfile << "T_" << to_string((int)(P_consts[i] / 1.E3)) << ",";
	//	myfile << "s_" << to_string((int)(P_consts[i] / 1.E3));
	//	if (i < n_P - 1)
	//		myfile << ",";
	//	else
	//		myfile << "\n";
	//}

	//int n_points = T_data[0].size();
	//for (int j = 0; j < n_points; j++)
	//{
	//	for (int i = 0; i < n_P; i++)
	//	{
	//		myfile << T_data[i][j] << ",";		//[C]
	//		myfile << s_data[i][j];
	//		if (i < n_P - 1)
	//			myfile << ",";
	//		else
	//			myfile << "\n";
	//	}
	//}

	//myfile.close();

	return 0;
}

int Ts_dome(double T_cold /*C*/, std::vector<double> & T_data /*C*/, std::vector<double> & s_data)
{
	std::vector<double> P_data;
	std::vector<double> h_data;
	return Ts_full_dome(T_cold, T_data, s_data, P_data, h_data);
}

int Ts_full_dome(double T_cold /*C*/, std::vector<double> & T_data /*C*/, std::vector<double> & s_data /*kJ/kg-K*/,
	std::vector<double> & P_data /*MPa*/, std::vector<double> & h_data /*kJ/kg*/)
{
	CO2_state t_co2_props;
	int n_x0 = 50;
	int n_x1 = 50;

	CO2_info t_co2_info;
	get_CO2_info(&t_co2_info);
	double T_crit = 0.999*t_co2_info.T_critical;		//[K]

	T_data.resize(n_x0 + n_x1);
	s_data.resize(n_x0 + n_x1);
	P_data.resize(n_x0 + n_x1);
	h_data.resize(n_x0 + n_x1);

	T_cold = T_cold + 273.15;		//[K]

	double deltaT_x0 = (T_crit - T_cold) / (n_x0 - 1);		//[K]
	int prop_err = 0;
	double T_i = std::numeric_limits<double>::quiet_NaN();

	for (int i = 0; i < n_x0; i++)
	{
		T_i = T_cold + deltaT_x0 * i;			//[K]
		prop_err = CO2_TQ(T_i, 0.0, &t_co2_props);
		if (prop_err != 0)
			return -1;

		T_data[i] = t_co2_props.temp - 273.15;		//[C]
		s_data[i] = t_co2_props.entr;				//[kJ/kg-K]
		P_data[i] = t_co2_props.pres / 1.E3;		//[MPa]
		h_data[i] = t_co2_props.enth;				//[kJ/kg]
	}

	double deltaT_x1 = (T_cold - T_crit) / (n_x1 - 1);		//[K]

	for (int i = 0; i < n_x1; i++)
	{
		T_i = T_crit + deltaT_x1 * i;			//[K]
		prop_err = CO2_TQ(T_i, 1.0, &t_co2_props);
		if (prop_err != 0)
			return -1;

		T_data[n_x0 + i] = t_co2_props.temp - 273.15;	//[C]
		s_data[n_x0 + i] = t_co2_props.entr;			//[kJ/kg-K]
		P_data[n_x0 + i] = t_co2_props.pres / 1.E3;		//[MPa]
		h_data[n_x0 + i] = t_co2_props.enth;				//[kJ/kg]
	}
}

int Ph_dome(double P_low /*MPa*/, std::vector<double> & P_data /*MPa*/, std::vector<double> & h_data)
{
	CO2_info t_co2_info;
	get_CO2_info(&t_co2_info);
	double P_crit = 0.999*t_co2_info.P_critical;	//[kPa]
	double T_crit = 0.999*t_co2_info.T_critical;	//[K]
	double T_low_limit = 1.001*t_co2_info.temp_lower_limit;	//[K]

	C_MEQ_CO2_props_at_2phase_P P_x0_eq;
	C_monotonic_eq_solver P_x0_solver(P_x0_eq);

	P_x0_solver.settings(1.E-3, 100, T_low_limit, T_crit, true);

	double T_P_target_solved = std::numeric_limits<double>::quiet_NaN();
	double tol_T_P_target_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_P_target = -1;

	P_low *= 1.005;

	int T_P_target_code = P_x0_solver.solve(T_crit - 10.0, T_crit - 20.0, P_low*1.E3, T_P_target_solved, tol_T_P_target_solved, iter_T_P_target);

	if (T_P_target_code != C_monotonic_eq_solver::CONVERGED)
	{
		return T_P_target_code;
	}

	std::vector<double> T_data;
	std::vector<double> s_data;

	return Ts_full_dome(T_P_target_solved - 273.15, T_data, s_data, P_data, h_data);
}

int C_MEQ_CO2_props_at_2phase_P::operator()(double T_co2 /*K*/, double *P_calc /*kPa*/)
{
	int prop_err_code = CO2_TQ(T_co2, 0.0, &mc_co2_props);
	if (prop_err_code != 0)
	{
		return prop_err_code;
		*P_calc = std::numeric_limits<double>::quiet_NaN();
	}
	
	*P_calc = mc_co2_props.pres;		//[kPa]

	return 0;
}

void C_HeatExchanger::initialize(const S_design_parameters & des_par_in)
{
	ms_des_par = des_par_in;
	return;
}

void C_HeatExchanger::hxr_pressure_drops(const std::vector<double> & m_dots, std::vector<double> & hxr_deltaP)
{
	int N = (int)m_dots.size();
	hxr_deltaP.resize(N);
	for (int i = 0; i < N; i++)
		hxr_deltaP[i] = ms_des_par.m_DP_design[i] * pow((m_dots[i] / ms_des_par.m_m_dot_design[i]), 1.75);
}

void C_HeatExchanger::hxr_conductance(const std::vector<double> & m_dots, double & hxr_UA)
{
	double m_dot_ratio = 0.5*(m_dots[0] / ms_des_par.m_m_dot_design[0] + m_dots[1] / ms_des_par.m_m_dot_design[1]);
	hxr_UA = ms_des_par.m_UA_design*pow(m_dot_ratio, 0.8);
}

double C_turbine::calculate_cost(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
	double T_out /*K*/, double P_out /*kPa*/, double W_dot /*kWe*/)
{
	switch (m_cost_model)
	{
	case C_turbine::E_CARLSON_17:
		return 7.79*1.E-3*std::pow(W_dot, 0.6842);		//[M$] needs power in kWe
	default:
		return std::numeric_limits<double>::quiet_NaN();
	}
}

void C_turbine::turbine_sizing(const S_design_parameters & des_par_in, int & error_code)
{
	/* 9.4.14: code from John Dyreby, converted to C++ by Ty Neises
	! Determine the turbine rotor diameter, effective nozzle area, and design-point shaft
	! speed and store values in recomp_cycle%t.
	!
	! Arguments:
	!   recomp_cycle -- a RecompCycle object that defines the simple/recompression cycle at the design point
	!   error_trace -- an ErrorTrace object
	!
	! Notes:
	!   1) The value for recomp_cycle%t%N_design is required to be set.  If it is <= 0.0 then
	!      the value for recomp_cycle%mc%N_design is used (i.e., link the compressor and turbine
	!      shafts).  For this reason, turbine_sizing must be called after compressor_sizing if
	!      the shafts are to be linked. */

	CO2_state co2_props;

	ms_des_par = des_par_in;

	// Check that a design-point shaft speed is available
	if (ms_des_par.m_N_design <= 0.0)	// Link shafts
	{
		ms_des_solved.m_N_design = ms_des_par.m_N_comp_design_if_linked;
		if (ms_des_par.m_N_design <= 0.0)
		{
			error_code = 7;
			return;
		}
	}
	else
		ms_des_solved.m_N_design = ms_des_par.m_N_design;

	// Get speed of sound at inlet
	int prop_error_code = CO2_TD(ms_des_par.m_T_in, ms_des_par.m_D_in, &co2_props);
	if (prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double ssnd_in = co2_props.ssnd;

	// Outlet specific enthalpy after isentropic expansion
	prop_error_code = CO2_PS(ms_des_par.m_P_out, ms_des_par.m_s_in, &co2_props);
	if (prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;		//[kJ/kg]
	double T_out = co2_props.temp;			//[K]

	// Determine necessary turbine parameters
	ms_des_solved.m_nu_design = m_nu_design;
	double w_i = ms_des_par.m_h_in - h_s_out;			//[kJ/kg] Isentropic specific work of turbine
	double C_s = sqrt(2.0*w_i*1000.0);					//[m/s] Spouting velocity
	double U_tip = ms_des_solved.m_nu_design*C_s;		//[m/s] Tip speed
	ms_des_solved.m_D_rotor = U_tip / (0.5*ms_des_solved.m_N_design*0.104719755);	//[m]
	ms_des_solved.m_A_nozzle = (ms_des_par.m_m_dot / m_r_W_dot_scale) / (C_s*ms_des_par.m_D_in);		//[m^2]
	ms_des_solved.m_rho_in = ms_des_par.m_D_in;	//[kg/m3]
	ms_des_solved.m_delta_h_isen = w_i;		//[kJ/kg]

	// Set other turbine variables
	ms_des_solved.m_w_tip_ratio = U_tip / ssnd_in;				//[-]
	ms_des_solved.m_eta = (ms_des_par.m_h_in - ms_des_par.m_h_out) / w_i;	//[-] Isentropic efficiency

	ms_des_solved.m_W_dot = ms_des_par.m_m_dot*(ms_des_par.m_h_in - ms_des_par.m_h_out);

	ms_des_solved.m_cost = calculate_cost(ms_des_par.m_T_in, ms_des_par.m_P_in, ms_des_par.m_m_dot,
							T_out, ms_des_par.m_P_out, ms_des_solved.m_W_dot);
}

void C_turbine::off_design_turbine(double T_in, double P_in, double P_out, double N, int & error_code, double & m_dot_cycle, double & T_out)
{
	/* 9.4.14: code from John Dyreby, converted to C++ by Ty Neises
	! Solve for the outlet state of 'turb' given its inlet conditions, outlet pressure, and shaft speed.
	!
	! Inputs:
	!   turb -- a Turbine object, with design-point values and sizing set
	!   T_in -- turbine inlet temperature (K)
	!   P_in -- turbine inlet pressure (kPa)
	!   P_out -- turbine outlet pressure (kPa)
	!   N -- shaft speed of turbine (rpm)
	!
	! Outputs:
	!   error_trace -- an ErrorTrace object
	!   m_dot -- allowable mass flow rate through the turbine (kg/s)
	!   T_out -- turbine outlet temperature (K)
	!
	! Notes:
	!   1) This subroutine also sets the following values in 'turb': nu, eta, m_dot, w, w_tip_ratio */

	CO2_state co2_props;

	// Get properties at turbine inlet
	int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
	if (prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double D_in = co2_props.dens;
	double h_in = co2_props.enth;
	double s_in = co2_props.entr;
	double ssnd_in = co2_props.ssnd;

	prop_error_code = CO2_PS(P_out, s_in, &co2_props);
	if (prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	double h_s_out = co2_props.enth;

	// Apply the radial turbine equations for efficiency
	double C_s = sqrt(2.0*(h_in - h_s_out)*1000.0);				//[m/s] spouting velocity
	double U_tip = ms_des_solved.m_D_rotor*0.5*N*0.104719755;	//[m/s] tip speed
	ms_od_solved.m_nu = U_tip / C_s;							//[-] ratio of tip speed to spouting velocity

	double eta_0 = (((1.0626*ms_od_solved.m_nu - 3.0874)*ms_od_solved.m_nu + 1.3668)*ms_od_solved.m_nu + 1.3567)*ms_od_solved.m_nu + 0.179921180;
	eta_0 = std::max(eta_0, 0.0);
	eta_0 = std::min(eta_0, 1.0);
	ms_od_solved.m_eta = eta_0*ms_des_solved.m_eta;		//[-] Actual turbine efficiency

	// Calculate the outlet state and allowable mass flow rate
	double h_out = h_in - ms_od_solved.m_eta*(h_in - h_s_out);		//[kJ/kg] Enthalpy at turbine outlet
	prop_error_code = CO2_PH(P_out, h_out, &co2_props);
	if (prop_error_code != 0)
	{
		error_code = prop_error_code;
		return;
	}
	T_out = co2_props.temp;

	double m_dot_basis = C_s*ms_des_solved.m_A_nozzle*D_in;			//[kg/s] Mass flow rate through turbine
	ms_od_solved.m_w_tip_ratio = U_tip / ssnd_in;		//[-] Ratio of the tip speed to the local (turbine inlet) speed of sound
	ms_od_solved.m_N = N;
	m_dot_cycle = m_dot_basis * m_r_W_dot_scale;				//[kg/s]
	ms_od_solved.m_W_dot_out = m_dot_cycle*(h_in - h_out);		//[kW] Turbine power output
    ms_od_solved.m_m_dot = m_dot_cycle;     //[kg/s]

	ms_od_solved.m_delta_h_isen = h_in - h_s_out;		//[kJ/kg]
	ms_od_solved.m_rho_in = D_in;						//[kg/m3]
}

void C_turbine::od_turbine_at_N_des(double T_in, double P_in, double P_out, int & error_code, double & m_dot_cycle, double & T_out)
{
	double N = ms_des_solved.m_N_design;		//[rpm]

	off_design_turbine(T_in, P_in, P_out, N, error_code, m_dot_cycle, T_out);

	return;
}

int C_comp__psi_eta_vs_phi::design_given_shaft_speed(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
    double N_rpm /*rpm*/, double eta_isen /*-*/, double & P_out /*kPa*/, double & T_out /*K*/, double & tip_ratio /*-*/)
{
    CO2_state co2_props;

    // Get inlet state
    int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
    if (prop_error_code != 0)
    {
        return prop_error_code;
    }
    double h_in = co2_props.enth;	//[kJ/kg]
    double s_in = co2_props.entr;	//[kJ/kg-K]
    double rho_in = co2_props.dens;	//[kg/m^3]

    // Convert shaft speed to rad/s
    double N_rad_s = N_rpm / 9.549296590;		//[rad/s]

    // Solve for the diameter that gives the design flow coefficient
    double phi_design = calc_phi_design(T_in, P_in);
    double D_rotor = std::pow(m_dot / (phi_design * rho_in * 0.5 * N_rad_s), 1.0 / 3.0);		//[m]

    // Calculate psi at the design-point phi using Horner's method
    double psi_design = calc_psi_isen_design(T_in, P_in);

    // Solve for idea head
    double U_tip = 0.5 * D_rotor * N_rad_s;		//[m/s]
    double w_i = psi_design * std::pow(U_tip, 2) * 0.001;		//[kJ/kg]

    // Solve for isentropic outlet enthalpy
    double h_out_isen = h_in + w_i;		//[kJ/kg]

    // Get isentropic outlet state
    prop_error_code = CO2_HS(h_out_isen, s_in, &co2_props);
    if (prop_error_code != 0)
    {
        return prop_error_code;
    }
    P_out = co2_props.pres;		//[kPa]

    // Get actual outlet state
    double h_out = h_in + w_i / eta_isen;	//[kJ/kg]
    prop_error_code = CO2_PH(P_out, h_out, &co2_props);
    if (prop_error_code != 0)
    {
        return prop_error_code;
    }
    T_out = co2_props.temp;		//[K]
    double ssnd_out = co2_props.ssnd;	//[m/s]

    // Solve for tip ratio
    tip_ratio = U_tip / ssnd_out;

    ms_des_solved.m_T_in = T_in;	//[K]
    ms_des_solved.m_P_in = P_in;	//[kPa]
    ms_des_solved.m_D_in = rho_in;	//[kg/m^3]
    ms_des_solved.m_h_in = h_in;	//[kJ/kg]
    ms_des_solved.m_s_in = s_in;	//[kJ/kg-K]

    ms_des_solved.m_T_out = T_out;	//[K]
    ms_des_solved.m_P_out = P_out;	//[kPa]
    ms_des_solved.m_h_out = h_out;	//[kJ/kg]
    ms_des_solved.m_D_out = co2_props.dens;	//[kg/m^3]

    ms_des_solved.m_m_dot = m_dot;	//[kg/s]

    ms_des_solved.m_D_rotor = D_rotor;	//[m]
    ms_des_solved.m_N_design = N_rpm;	//[rpm]
    ms_des_solved.m_tip_ratio = tip_ratio;	//[-]
    ms_des_solved.m_eta_design = eta_isen;		//[-]

    ms_des_solved.m_phi_des = phi_design;
    ms_des_solved.m_phi_surge = calc_phi_min(T_in, P_in);
    ms_des_solved.m_phi_max = calc_phi_max(T_in, P_in);
    set_design_solution(phi_design, T_in, P_in);

    ms_des_solved.m_psi_des = psi_design;   //[-] ideal head coefficient
    ms_des_solved.m_psi_max_at_N_des = calc_psi_isen(ms_des_solved.m_phi_surge, 1.0, T_in, P_in);  //[-] max ideal head coefficient at design shaft speed

    return 0;
}

int C_comp__psi_eta_vs_phi::design_given_performance(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
    double T_out /*K*/, double P_out /*K*/)
{
    CO2_state in_props;
    int prop_err_code = CO2_TP(T_in, P_in, &in_props);
    if (prop_err_code != 0)
    {
        return -1;
    }
    double s_in = in_props.entr;	//[kJ/kg-K]
    double h_in = in_props.enth;	//[kJ/kg]
    double rho_in = in_props.dens;	//[kg/m^3]

    CO2_state isen_out_props;
    prop_err_code = CO2_PS(P_out, s_in, &isen_out_props);
    if (prop_err_code != 0)
    {
        return -1;
    }
    double h_isen_out = isen_out_props.enth;	//[kJ/kg]

    CO2_state out_props;
    prop_err_code = CO2_TP(T_out, P_out, &out_props);
    if (prop_err_code != 0)
    {
        return -1;
    }
    double h_out = out_props.enth;

    // Calculate psi at the design-point phi using Horner's method
    double phi_design = calc_phi_design(T_in, P_in);
    double psi_design = calc_psi_isen_design(T_in, P_in);	//[-]

    // Determine required size and speed of compressor
    double w_i = h_isen_out - h_in;						//[kJ/kg] positive isentropic specific work of compressor
    double U_tip = sqrt(1000.0*w_i / psi_design);		//[m/s]
    double D_rotor = sqrt(m_dot / (phi_design*rho_in*U_tip));
    double N_rad_s = U_tip * 2.0 / D_rotor;				//[rad/s] shaft speed

    double ssnd_out = out_props.ssnd;	//[m/s]

    // Solve for tip ratio
    double tip_ratio = U_tip / ssnd_out;

    ms_des_solved.m_T_in = T_in;	//[K]
    ms_des_solved.m_P_in = P_in;	//[kPa]
    ms_des_solved.m_D_in = rho_in;	//[kg/m^3]
    ms_des_solved.m_h_in = h_in;	//[kJ/kg]
    ms_des_solved.m_s_in = s_in;	//[kJ/kg-K]

    ms_des_solved.m_T_out = T_out;	//[K]
    ms_des_solved.m_P_out = P_out;	//[kPa]
    ms_des_solved.m_h_out = h_out;	//[kJ/kg]
    ms_des_solved.m_D_out = out_props.dens;	//[kg/m^3]

    ms_des_solved.m_m_dot = m_dot;	//[kg/s]

    ms_des_solved.m_D_rotor = D_rotor;	//[m]
    ms_des_solved.m_N_design = N_rad_s * 9.549296590;			//[rpm] shaft speed
    ms_des_solved.m_tip_ratio = tip_ratio;	//[-]
    ms_des_solved.m_eta_design = (h_isen_out - h_in) / (h_out - h_in);		//[-]

    ms_des_solved.m_phi_des = calc_phi_design(T_in, P_in);
    ms_des_solved.m_phi_surge = calc_phi_min(T_in, P_in);
    ms_des_solved.m_phi_max = calc_phi_max(T_in, P_in);
    set_design_solution(phi_design, T_in, P_in);

    ms_des_solved.m_psi_des = psi_design;   //[-] ideal head coefficient
    ms_des_solved.m_psi_max_at_N_des = calc_psi_isen(ms_des_solved.m_phi_surge, 1.0, T_in, P_in);  //[-] max ideal head coefficient at design shaft speed

    return 0;
}

int C_comp__psi_eta_vs_phi::off_design_given_N(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/, double N_rpm /*rpm*/,
    double & T_out /*K*/, double & P_out /*kPa*/)
{
    CO2_state co2_props;

    ms_od_solved.m_N = N_rpm;		//[rpm]

    // Fully define the inlet state of the compressor
    int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
    if (prop_error_code != 0)
    {
        return prop_error_code;
    }
    double rho_in = co2_props.dens;	//[kg/m^3]
    double h_in = co2_props.enth;	//[kJ/kg]
    double s_in = co2_props.entr;	//[kJ/kg-K]

    // Calculate the modified flow and head coefficients and efficiency
    double U_tip = ms_des_solved.m_D_rotor*0.5*ms_od_solved.m_N*0.104719755;				//[m/s]
    double phi = m_dot / (rho_in*U_tip*pow(ms_des_solved.m_D_rotor, 2));	//[-]
    double phi_min = calc_phi_min(T_in, P_in);
    if (phi < phi_min)
    {
        ms_od_solved.m_surge = true;
    }
    else
    {
        ms_od_solved.m_surge = false;
    }

    double N_des_over_N_od = ms_des_solved.m_N_design / N_rpm;      //[-]

    double psi = calc_psi_isen(phi, N_des_over_N_od, T_in, P_in);		//[-]

    double eta_ND_od = calc_eta_OD_normalized(phi, N_des_over_N_od, T_in, P_in);

    ms_od_solved.m_eta = std::max(eta_ND_od*ms_des_solved.m_eta_design, 0.0);		//[-] Actual compressor efficiency, not allowed to go negative

    // Check that the specified mass flow rate is possible with the compressor's current shaft speed
    if (psi <= 0.0)
    {
        return 1;
    }

    // Calculate the compressor outlet state
    double dh_s = psi * pow(U_tip, 2.0) * 0.001;			//[kJ/kg] Ideal enthalpy rise in compressor, from definition of head coefficient
    double dh = dh_s / ms_od_solved.m_eta;					//[kJ/kg] Actual enthalpy rise in compressor
    double h_s_out = h_in + dh_s;							//[kJ/kg] Ideal enthalpy at compressor outlet
    double h_out = h_in + dh;								//[kJ/kg] Actual enthalpy at compressor outlet

    // Get the compressor outlet pressure
    prop_error_code = CO2_HS(h_s_out, s_in, &co2_props);
    if (prop_error_code != 0)
    {
        return 2;
    }
    P_out = co2_props.pres;

    // Determine compressor outlet temperature and speed of sound
    prop_error_code = CO2_PH(P_out, h_out, &co2_props);
    if (prop_error_code != 0)
    {
        return 2;
    }
    T_out = co2_props.temp;
    double ssnd_out = co2_props.ssnd;

    // Set a few compressor variables
    ms_od_solved.m_P_in = P_in;		//[kPa]
    ms_od_solved.m_h_in = h_in;		//[kJ/kg]
    ms_od_solved.m_T_in = T_in;		//[K]
    ms_od_solved.m_s_in = s_in;		//[kJ/kg-K]

    ms_od_solved.m_P_out = P_out;	//[kPa]
    ms_od_solved.m_h_out = h_out;	//[kJ/kg]
    ms_od_solved.m_T_out = T_out;	//[K]
    ms_od_solved.m_s_out = co2_props.entr;	//[kJ/kg-K]

    ms_od_solved.m_phi = phi;   //[-]
    ms_od_solved.m_psi = psi;   //[-]
    ms_od_solved.m_surge_safety = phi / phi_min;	//[-] If > 1, then not in surge
    ms_od_solved.m_w_tip_ratio = U_tip / ssnd_out;
    ms_od_solved.m_W_dot_in = m_dot * (h_out - h_in);	//[kWe]

    return 0;
}

int C_comp__psi_eta_vs_phi::calc_N_from_phi(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/, double phi_in /*-*/, double & N_rpm /*rpm*/)
{
    CO2_state co2_props;

    // Fully define the inlet state of the compressor
    int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
    if (prop_error_code != 0)
    {
        return prop_error_code;
    }
    double rho_in = co2_props.dens;	//[kg/m^3]
    double U_tip = m_dot / (phi_in*rho_in*std::pow(ms_des_solved.m_D_rotor, 2));		//[m/s]
    N_rpm = (U_tip*2.0 / ms_des_solved.m_D_rotor)*9.549296590;		//[rpm]

    return 0;
}

int C_comp__psi_eta_vs_phi::calc_m_dot__phi_des(double T_in /*K*/, double P_in /*kPa*/, double N_rpm /*rpm*/, double & m_dot /*kg/s*/)
{
    CO2_state co2_props;

    // Fully define the inlet state of the compressor
    int prop_error_code = CO2_TP(T_in, P_in, &co2_props);
    if (prop_error_code != 0)
    {
        return prop_error_code;
    }
    double rho_in = co2_props.dens;	//[kg/m^3]
    double h_in = co2_props.enth;	//[kJ/kg]
    double s_in = co2_props.entr;	//[kJ/kg-K]

    // Calculate the modified flow and head coefficients and efficiency
    double U_tip = ms_des_solved.m_D_rotor*0.5*N_rpm*0.104719755;	//[m/s]
    m_dot = ms_des_solved.m_phi_des * U_tip * rho_in * pow(ms_des_solved.m_D_rotor, 2);					//[kg/s]

    return 0;
}

std::unique_ptr<C_comp__psi_eta_vs_phi> C_comp__psi_eta_vs_phi::construct_derived_C_comp__psi_eta_vs_phi(int comp_model_code)
{
    if (comp_model_code == E_snl_radial_via_Dyreby)
    {
        return std::unique_ptr<C_comp__snl_radial_via_Dyreby>(new C_comp__snl_radial_via_Dyreby());
    }
    else
    {
        throw(C_csp_exception("C_comp__psi_eta_vs_phi::construct_derived_C_comp__psi_eta_vs_phi unrecognized compressor model code"));
    }
}

void C_comp__snl_radial_via_Dyreby::set_design_solution(double phi /*-*/, double T_comp_in_des /*K*/, double P_comp_in /*kPa*/)
{}

void C_comp__snl_radial_via_Dyreby::report_phi_psi_eta_vectors(std::vector<double> & phi, 
    std::vector<double> & psi, std::vector<double> & eta, double & eta_norm_design)
{
    double T_comp_dummy = std::numeric_limits<double>::quiet_NaN();
    double P_comp_dummy = std::numeric_limits<double>::quiet_NaN();

    double phi_min = calc_phi_min(T_comp_dummy, P_comp_dummy);
    double phi_max = calc_phi_max(T_comp_dummy, P_comp_dummy);

    size_t n_phi = 20;
    phi.resize(n_phi, 0.0);
    psi.resize(n_phi, 0.0);
    eta.resize(n_phi, 0.0);

    double delta_phi = (phi_max - phi_min) / (double)(n_phi - 1);
    double i_phi = 0.0;
    for (size_t i = 0; i < n_phi; i++)
    {
        i_phi = phi_min + i * delta_phi;
        phi[i] = i_phi;
        psi[i] = calc_psi_isen(i_phi, 1.0, T_comp_dummy, P_comp_dummy);
        eta[i] = calc_eta_OD_normalized(i_phi, 1.0, T_comp_dummy, P_comp_dummy);
    }

    eta_norm_design = 1.0;
}

double C_comp__snl_radial_via_Dyreby::adjust_phi_for_N(double phi /*-*/, double N_des_over_N_od /*-*/)
{
    return phi*pow(1.0 / N_des_over_N_od, 0.2);		//[-] modified flow coefficient
}

double C_comp__snl_radial_via_Dyreby::calc_phi_min(double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    return m_phi_min;
}

double C_comp__snl_radial_via_Dyreby::calc_phi_design(double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    return m_phi_design;
}

double C_comp__snl_radial_via_Dyreby::calc_phi_max(double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    return m_phi_max;
}

double C_comp__snl_radial_via_Dyreby::calc_psi_isen_design(double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    double phi_design = calc_phi_design(T_comp_in, P_comp_in);

    return calc_psi_isen(phi_design, 1.0, T_comp_in, P_comp_in);
}

double C_comp__snl_radial_via_Dyreby::calc_psi_isen(double phi_in /*-*/, double N_des_over_N_od /*-*/, double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    double phi = adjust_phi_for_N(phi_in, N_des_over_N_od);
    
    double psi = std::numeric_limits<double>::quiet_NaN();

    if (phi >= m_phi_min)
    {
        psi = ((((-498626.0*phi) + 53224.0) * phi - 2505.0) * phi + 54.6)*phi + 0.04049;  // from dimensionless modified head curve(at design - point, psi and modified psi are equal)
    }
    else
    {
        double psi_at_surge = ((((-498626.0*m_phi_min) + 53224.0)*m_phi_min - 2505.0)*m_phi_min + 54.6)*m_phi_min + 0.04049;  // from dimensionless modified head curve(at design - point, psi and modified psi are equal)
        psi = (1 + 0.5*(m_phi_min - phi) / m_phi_min)*psi_at_surge;		//[-] Check for surge after model converges	
    }

    return psi / pow(N_des_over_N_od, pow(20.0*phi, 3.0));
}

double C_comp__snl_radial_via_Dyreby::calc_eta_OD_normalized(double phi_in /*-*/, double N_des_over_N_od /*-*/, double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    double phi = adjust_phi_for_N(phi_in, N_des_over_N_od);
    
    double eta_star = ((((-1.638e6*phi) + 182725.0)*phi - 8089.0)*phi + 168.6)*phi - 0.7069;	// from dimensionless modified efficiency curve

    return eta_star * 1.47528 / pow(N_des_over_N_od, pow(20.0*phi, 5.0));
}

void C_comp__compA__PT_map_template::set_design_solution(double phi /*-*/, double T_comp_in_des /*K*/, double P_comp_in /*kPa*/)
{}

void C_comp__compA__PT_map_template::report_phi_psi_eta_vectors(std::vector<double> & phi, 
    std::vector<double> & psi, std::vector<double> & eta, double & eta_norm_design)
{
    std::vector<double> phi_temp = mc_data_at_PT.get_column_data(0);
    int n_pts = phi_temp.size() - 1;        //[-] Don't want first value, which only helps converge during surge conditions
    phi.resize(n_pts);
    psi.resize(n_pts);
    eta.resize(n_pts);
    std::copy_n(phi_temp.begin() + 1, n_pts, phi.begin());
    copy_n(mc_data_at_PT.get_column_data(1).begin() + 1, n_pts, psi.begin());
    copy_n(mc_data_at_PT.get_column_data(2).begin() + 1, n_pts, eta.begin());

    eta_norm_design = m_eta_isen_norm;
}

double C_comp__compA__PT_map_template::calc_phi_min(double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    return m_phi_min;
}

double C_comp__compA__PT_map_template::calc_phi_design(double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    return m_phi_design;
}

double C_comp__compA__PT_map_template::calc_phi_max(double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    return m_phi_max;
}

double C_comp__compA__PT_map_template::calc_psi_isen_design(double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    double phi_design = calc_phi_design(T_comp_in, P_comp_in);

    return calc_psi_isen(phi_design, 1.0, T_comp_in, P_comp_in);
}

double C_comp__compA__PT_map_template::calc_psi_isen(double phi /*-*/, double N_des_over_N_od /*-*/, double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    return mc_data_at_PT.linear_1D_interp(0, 1, phi);
}

double C_comp__compA__PT_map_template::calc_eta_OD_normalized(double phi /*-*/, double N_des_over_N_od /*-*/, double T_comp_in /*K*/, double P_comp_in /*kPa*/)
{
    return mc_data_at_PT.linear_1D_interp(0, 2, phi);
}

int C_comp_multi_stage::C_MEQ_eta_isen__h_out::operator()(double eta_isen /*-*/, double *h_comp_out /*kJ/kg*/)
{
	C_MEQ_N_rpm__P_out c_stages(mpc_multi_stage, m_T_in, m_P_in, m_m_dot_basis, eta_isen);
	C_monotonic_eq_solver c_solver(c_stages);

	// Set lowr bound
	double N_rpm_lower = 1.E-4;
	double N_rpm_upper = std::numeric_limits<double>::quiet_NaN();

	// Generate guess values
	double N_rpm_guess_1 = 3000.0;
	double N_rpm_guess_2 = 30000.0;

    double tol = m_tol_in / 10.0;
	c_solver.settings(tol, 50, N_rpm_lower, N_rpm_upper, true);

	// Now solve for the shaft speed
	double N_rpm_solved = std::numeric_limits<double>::quiet_NaN();
	double tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int N_rpm_code = 0;
	try
	{
		N_rpm_code = c_solver.solve(N_rpm_guess_1, N_rpm_guess_2, m_P_out, N_rpm_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		throw(C_csp_exception("C_comp_multi_stage::C_MEQ_eta_isen__h_out threw an exception"));
	}

	if (N_rpm_code != C_monotonic_eq_solver::CONVERGED)
	{
		if (!(N_rpm_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.01))
		{
			throw(C_csp_exception("C_comp_multi_stage::C_MEQ_eta_isen__h_out failed to converge within a reasonable tolerance"));
		}
	}

    int n_stages = mpc_multi_stage->mv_c_stages.size();     //[-] 

    *h_comp_out = mpc_multi_stage->mv_c_stages[n_stages - 1]->ms_des_solved.m_h_out;    //[kJ/kg] 

	return 0;
}

int C_comp_multi_stage::C_MEQ_N_rpm__P_out::operator()(double N_rpm /*rpm*/, double *P_comp_out /*kPa*/)
{
    int n_stages = mpc_multi_stage->mv_c_stages.size(); //[-] 

	double T_in = m_T_in;	//[K]
	double P_in = m_P_in;	//[kPa]

	double P_out = std::numeric_limits<double>::quiet_NaN();
	double T_out = std::numeric_limits<double>::quiet_NaN();
	double tip_ratio = std::numeric_limits<double>::quiet_NaN();

	int comp_err_code = 0;

	for (int i = 0; i < n_stages; i++)
	{
		if (i > 0)
		{
			T_in = T_out;	//[K]
			P_in = P_out;	//[kPa]
		}

        mpc_multi_stage->mv_c_stages[i] = C_comp__psi_eta_vs_phi::construct_derived_C_comp__psi_eta_vs_phi(mpc_multi_stage->m_compressor_model);
        comp_err_code = mpc_multi_stage->mv_c_stages[i]->design_given_shaft_speed(T_in, P_in, m_m_dot_basis, N_rpm, m_eta_isen, P_out, T_out, tip_ratio);

		if (comp_err_code != 0)
		{
			*P_comp_out = std::numeric_limits<double>::quiet_NaN();
			return -1;
		}
	}

	*P_comp_out = P_out;	//[kPa]

	return 0;
}

double C_comp_multi_stage::calculate_cost(double T_in /*K*/, double P_in /*kPa*/, double m_dot /*kg/s*/,
	double T_out /*K*/, double P_out /*kPa*/, double W_dot /*kWe*/)
{
	switch (m_cost_model)
	{
	case C_comp_multi_stage::E_CARLSON_17:
		return 6.898*1.E-3*std::pow(W_dot, 0.7865);		//[M$] needs power in kWe
	default:
		return std::numeric_limits<double>::quiet_NaN();
	}
}

int C_comp_multi_stage::design_given_outlet_state(int comp_model_code, double T_in /*K*/, double P_in /*kPa*/, double m_dot_cycle /*kg/s*/,
	double T_out /*K*/, double P_out /*K*/, double tol /*-*/)
{
    m_compressor_model = comp_model_code;   //[-]

    double m_dot_basis = m_dot_cycle / m_r_W_dot_scale;		//[kg/s]

    mv_c_stages.resize(1);
    mv_c_stages[0] = C_comp__psi_eta_vs_phi::construct_derived_C_comp__psi_eta_vs_phi(m_compressor_model);    
    mv_c_stages[0]->design_given_performance(T_in, P_in, m_dot_basis, T_out, P_out);

    double max_calc_tip_speed = mv_c_stages[0]->ms_des_solved.m_tip_ratio;      //[-]

	double tip_speed_limit = 0.85;

	CO2_state co2_props;

	double h_in = mv_c_stages[0]->ms_des_solved.m_h_in; //[kJ/kg] 
	double s_in = mv_c_stages[0]->ms_des_solved.m_s_in; //[kJ/kg-K] 

	int prop_err_code = CO2_PS(P_out, s_in, &co2_props);
	if (prop_err_code != 0)
	{
		return -1;
	}
	double h_out_isen = co2_props.enth;		//[kJ/kg]

	if(mv_c_stages[0]->ms_des_solved.m_tip_ratio > tip_speed_limit)
	{		
        double h_out = mv_c_stages[0]->ms_des_solved.m_h_out;   //[kJ/kg]

		double eta_isen_total = (h_out_isen - h_in) / (h_out - h_in);

		bool is_add_stages = true;
		int n_stages = 1;

		while (is_add_stages)
		{
			tip_speed_limit = 0.9;		//[-] If multi-stage, increase tip speed limit to 0.9
			
			n_stages++;

            mv_c_stages.clear();
            mv_c_stages.resize(n_stages);

			//mv_stages.resize(n_stages);

			C_MEQ_eta_isen__h_out c_stages(this, T_in, P_in, P_out, m_dot_basis, tol);
			C_monotonic_eq_solver c_solver(c_stages);

			// Set bounds on isentropic efficiency
			double eta_isen_lower = 0.1;
			double eta_isen_upper = 1.0;

			// Generate guess values
			double eta_isen_guess_1 = eta_isen_total;
			double eta_isen_guess_2 = 0.95*eta_isen_total;

			c_solver.settings(tol/10.0, 50, eta_isen_lower, eta_isen_upper, true);

			// Now solve for the isentropic efficiency for each stage that results in the total compressor design isentropic efficiency
			double eta_isen_solved = std::numeric_limits<double>::quiet_NaN();
			double tol_solved = std::numeric_limits<double>::quiet_NaN();
			int iter_solved = -1;

			int eta_isen_code = 0;

			try
			{
				eta_isen_code = c_solver.solve(eta_isen_guess_1, eta_isen_guess_2, h_out, eta_isen_solved, tol_solved, iter_solved);
			}
			catch (C_csp_exception)
			{
				throw(C_csp_exception("C_comp_multi_stage::design_given_outlet_state threw an exception"));
			}

			if (eta_isen_code != C_monotonic_eq_solver::CONVERGED)
			{
				if (!(eta_isen_code > C_monotonic_eq_solver::CONVERGED && fabs(tol_solved) < 0.01))
				{
					throw(C_csp_exception("C_comp_multi_stage::design_given_outlet_state failed to converge within a reasonable tolerance"));
				}
			}

			max_calc_tip_speed = 0.0;
			for (int i = 0; i < n_stages; i++)
			{
                max_calc_tip_speed = std::max(max_calc_tip_speed, mv_c_stages[i]->ms_des_solved.m_tip_ratio);
			}

			if (max_calc_tip_speed < tip_speed_limit)
			{
				is_add_stages = false;
			}

			if (n_stages > 20)
			{
				return -1;
			}
		}
	}

    int n_stages = mv_c_stages.size();  // mv_stages.size();

	ms_des_solved.m_T_in = T_in;	//[K]
	ms_des_solved.m_P_in = P_in;	//[kPa]
	ms_des_solved.m_D_in = mv_c_stages[0]->ms_des_solved.m_D_in;	//[kg/m^3]
	ms_des_solved.m_s_in = mv_c_stages[0]->ms_des_solved.m_s_in;	//[kJ/kg-K]
	ms_des_solved.m_h_in = mv_c_stages[0]->ms_des_solved.m_h_in;	//[kJ/kg]

	ms_des_solved.m_T_out = mv_c_stages[n_stages - 1]->ms_des_solved.m_T_out;	//[K]
	ms_des_solved.m_P_out = mv_c_stages[n_stages - 1]->ms_des_solved.m_P_out;	//[kPa]
	ms_des_solved.m_h_out = mv_c_stages[n_stages - 1]->ms_des_solved.m_h_out;	//[kJ/kg]
	ms_des_solved.m_D_out = mv_c_stages[n_stages - 1]->ms_des_solved.m_D_out;	//[kg/m^3]

	ms_des_solved.m_isen_spec_work = h_out_isen - h_in;	//[kJ/kg]

	ms_des_solved.m_m_dot = m_dot_cycle;					//[kg/s]
	ms_des_solved.m_W_dot = ms_des_solved.m_m_dot*(ms_des_solved.m_h_out - ms_des_solved.m_h_in);	//[kWe]

	ms_des_solved.m_N_design = mv_c_stages[n_stages - 1]->ms_des_solved.m_N_design;		//[rpm]
	ms_des_solved.m_phi_des = mv_c_stages[0]->ms_des_solved.m_phi_des;		//[-]
    ms_des_solved.m_psi_des = mv_c_stages[0]->ms_des_solved.m_psi_des;     //[-]
	ms_des_solved.m_tip_ratio_max = max_calc_tip_speed;					//[-]
	ms_des_solved.m_n_stages = n_stages;								//[-]
    ms_des_solved.m_phi_surge = mv_c_stages[0]->ms_des_solved.m_phi_surge;				//[-]
    ms_des_solved.m_psi_max_at_N_des = mv_c_stages[0]->ms_des_solved.m_psi_max_at_N_des; //[-] Max ideal head coefficient at design shaft speed

	ms_des_solved.mv_D.resize(n_stages);					//[m]
	ms_des_solved.mv_tip_speed_ratio.resize(n_stages);		//[-]
	ms_des_solved.mv_eta_stages.resize(n_stages);			//[-]
	for (int i = 0; i < n_stages; i++)
	{
		ms_des_solved.mv_D[i] = mv_c_stages[i]->ms_des_solved.m_D_rotor;	//[m]
		ms_des_solved.mv_tip_speed_ratio[i] = mv_c_stages[i]->ms_des_solved.m_tip_ratio;	//[-]
		ms_des_solved.mv_eta_stages[i] = mv_c_stages[i]->ms_des_solved.m_eta_design;	//[-]
	}

	ms_des_solved.m_cost = calculate_cost(ms_des_solved.m_T_in, ms_des_solved.m_P_in, ms_des_solved.m_m_dot,
							ms_des_solved.m_T_out, ms_des_solved.m_P_out, ms_des_solved.m_W_dot);

	// Also need to size OD vectors here
	ms_od_solved.mv_eta.resize(n_stages);
	ms_od_solved.mv_phi.resize(n_stages);
    ms_od_solved.mv_psi.resize(n_stages);
	ms_od_solved.mv_tip_speed_ratio.resize(n_stages);

	return 0;
}

void C_comp_multi_stage::off_design_at_N_des(double T_in /*K*/, double P_in /*kPa*/, double m_dot_cycle /*kg/s*/,
	int & error_code, double & T_out /*K*/, double & P_out /*kPa*/)
{
	double N = ms_des_solved.m_N_design;	//[rpm]

	// passing m_dot cycle not m_dot basis
	off_design_given_N(T_in, P_in, m_dot_cycle, N, error_code, T_out, P_out);
}

int C_comp_multi_stage::calc_m_dot__N_des__phi_des_first_stage(double T_in /*K*/, double P_in /*kPa*/, double & m_dot_cycle /*kg/s*/)
{
	double N_des = mv_c_stages[0]->ms_des_solved.m_N_design;		//[rpm]

	double m_dot_basis = std::numeric_limits<double>::quiet_NaN();
	int stage_err_code = mv_c_stages[0]->calc_m_dot__phi_des(T_in, P_in, N_des, m_dot_basis);

	m_dot_cycle = m_dot_basis * m_r_W_dot_scale;		//[kg/s]

	return stage_err_code;
}

void C_comp_multi_stage::off_design_given_N(double T_in /*K*/, double P_in /*kPa*/, double m_dot_in /*kg/s*/, double N_rpm /*rpm*/,
	int & error_code, double & T_out /*K*/, double & P_out /*kPa*/)
{
	double m_dot = m_dot_in / m_r_W_dot_scale;		//[kg/s]

	int n_stages = mv_c_stages.size();

	double T_stage_in = T_in;	//[K]
	double P_stage_in = P_in;	//[kPa]

	double T_stage_out = std::numeric_limits<double>::quiet_NaN();
	double P_stage_out = std::numeric_limits<double>::quiet_NaN();

	double tip_ratio_max = 0.0;
	bool is_surge = false;
	double surge_safety_min = 10.0;
	double phi_min = 10.0;

	for (int i = 0; i < n_stages; i++)
	{
		if (i > 0)
		{
			T_stage_in = T_stage_out;
			P_stage_in = P_stage_out;
		}

		error_code = mv_c_stages[i]->off_design_given_N(T_stage_in, P_stage_in, m_dot, N_rpm, T_stage_out, P_stage_out);
		if (error_code != 0)
		{
			return;
		}

		if (mv_c_stages[i]->ms_od_solved.m_w_tip_ratio > tip_ratio_max)
		{
			tip_ratio_max = mv_c_stages[i]->ms_od_solved.m_w_tip_ratio;
		}

		if (mv_c_stages[i]->ms_od_solved.m_surge)
		{
			is_surge = true;
		}

		if (mv_c_stages[i]->ms_od_solved.m_surge_safety < surge_safety_min)
		{
			surge_safety_min = mv_c_stages[i]->ms_od_solved.m_surge;
		}

		phi_min = std::min(phi_min, mv_c_stages[i]->ms_od_solved.m_phi);
	}

	P_out = mv_c_stages[n_stages - 1]->ms_od_solved.m_P_out;		//[kPa]
	T_out = mv_c_stages[n_stages - 1]->ms_od_solved.m_T_out;	//[K]

	double h_in = mv_c_stages[0]->ms_od_solved.m_h_in;					//[kJ/kg]
	double s_in = mv_c_stages[0]->ms_od_solved.m_s_in;					//[kJ/kg-K]

	CO2_state co2_props;
	int prop_err_code = CO2_PS(P_out, s_in, &co2_props);
	if (prop_err_code != 0)
	{
		error_code = prop_err_code;
		return;
	}
	double h_out_isen = co2_props.enth;

	double h_out = mv_c_stages[n_stages - 1]->ms_od_solved.m_h_out;	//[kJ/kg]

	ms_od_solved.m_P_in = P_in;
	ms_od_solved.m_T_in = T_in;

	ms_od_solved.m_P_out = P_out;
	ms_od_solved.m_T_out = T_out;

	ms_od_solved.m_m_dot = m_dot_in;		//[kg/s] (cycle, not basis)

	ms_od_solved.m_isen_spec_work = h_out_isen - h_in;	//[kJ/kg]

	ms_od_solved.m_surge = is_surge;
	ms_od_solved.m_eta = (h_out_isen - h_in) / (h_out - h_in);		//[-] Overall compressor efficiency
	
	ms_od_solved.m_phi_min = phi_min;								//[-] Min (all stages) flow coefficient

	ms_od_solved.m_tip_ratio_max = tip_ratio_max;					//[-] Max (all stages) tip ratio overall

	ms_od_solved.m_N = N_rpm;

	ms_od_solved.m_W_dot_in = m_dot_in*(h_out - h_in);	//[kg/s] (scaled to cycle, not basis)
	ms_od_solved.m_surge_safety = surge_safety_min;

	for (int i = 0; i < n_stages; i++)
	{
		ms_od_solved.mv_tip_speed_ratio[i] = mv_c_stages[i]->ms_od_solved.m_w_tip_ratio;	//[-]
		ms_od_solved.mv_phi[i] = mv_c_stages[i]->ms_od_solved.m_phi;		//[-]
        ms_od_solved.mv_psi[i] = mv_c_stages[i]->ms_od_solved.m_psi;       //[-]
		ms_od_solved.mv_eta[i] = mv_c_stages[i]->ms_od_solved.m_eta;		//[-]
	}

}

int C_comp_multi_stage::C_MEQ_phi_od__P_out::operator()(double phi_od /*-*/, double *P_comp_out /*kPa*/)
{
	double m_dot_basis = m_m_dot_cycle / mpc_multi_stage->m_r_W_dot_scale;	//[kg/s]

	int error_code = 0;
	double N_rpm = std::numeric_limits<double>::quiet_NaN();
	error_code = mpc_multi_stage->mv_c_stages[0]->calc_N_from_phi(m_T_in, m_P_in, m_dot_basis, phi_od, N_rpm);
	if (error_code != 0)
	{
		*P_comp_out = std::numeric_limits<double>::quiet_NaN();
		return error_code;
	}

	double T_out = std::numeric_limits<double>::quiet_NaN();
	error_code = 0;
	mpc_multi_stage->off_design_given_N(m_T_in, m_P_in, m_m_dot_cycle, N_rpm, error_code, T_out, *P_comp_out);

	if (error_code != 0)
	{
		*P_comp_out = std::numeric_limits<double>::quiet_NaN();
		return error_code;
	}

	return 0;
}

void C_comp_multi_stage::off_design_given_P_out(double T_in /*K*/, double P_in /*kPa*/, double m_dot_cycle /*kg/s*/,
	double P_out /*kPa*/, double tol /*-*/, int & error_code, double & T_out /*K*/)
{
	// Apply 1 var solver to find the phi that results in a converged recompressor
	C_MEQ_phi_od__P_out c_rc_od(this, T_in, P_in, m_dot_cycle);
	C_monotonic_eq_solver c_rd_od_solver(c_rc_od);

	// Set upper and lower bounds
	double phi_upper = mv_c_stages[0]->ms_des_solved.m_phi_max;
	double phi_lower = 0.001;		

	// Generate first x-y pair
	double phi_guess_lower = ms_des_solved.m_phi_des;
	double P_solved_phi_guess_lower = std::numeric_limits<double>::quiet_NaN();
	int test_code = c_rd_od_solver.test_member_function(phi_guess_lower, &P_solved_phi_guess_lower);
	if (test_code != 0)
	{
		for (int i = 1; i < 9; i++)
		{
			phi_guess_lower = ms_des_solved.m_phi_des*(10 - i) / 10.0 + mv_c_stages[0]->ms_des_solved.m_phi_max*i / 10.0;
			test_code = c_rd_od_solver.test_member_function(phi_guess_lower, &P_solved_phi_guess_lower);
			if (test_code == 0)
				break;
		}
	}
	if (test_code != 0)
	{
		// Can't find a RC phi guess value that returns an outlet pressure
		error_code = -20;
		return;
	}
	C_monotonic_eq_solver::S_xy_pair phi_pair_lower;
	phi_pair_lower.x = phi_guess_lower;
	phi_pair_lower.y = P_solved_phi_guess_lower;

	// Generate second x-y pair
	double phi_guess_upper = phi_guess_lower*0.5 + mv_c_stages[0]->ms_des_solved.m_phi_max*0.5;
	double P_solved_phi_guess_upper = std::numeric_limits<double>::quiet_NaN();
	test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
	if (test_code != 0)
	{
		for (int i = 6; i < 10; i++)
		{
			phi_guess_upper = phi_guess_lower*i / 10.0 + mv_c_stages[0]->ms_des_solved.m_phi_max*(10 - i) / 10.0;
			test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
			if (test_code == 0)
				break;
		}
		if (test_code != 0 && phi_guess_lower == ms_des_solved.m_phi_des)
		{
			for (int i = 6; i < 10; i++)
			{
				phi_guess_upper = phi_guess_lower*i / 10.0 + ms_des_solved.m_phi_surge*(10 - i) / 10.0;
				//phi_guess_upper = phi_guess_lower * i / 10.0 + phi_lower*(10 - i) / 10.0;
				test_code = c_rd_od_solver.test_member_function(phi_guess_upper, &P_solved_phi_guess_upper);
				if (test_code == 0)
					break;
			}
		}
	}
	if (test_code != 0)
	{
		// Can't find a RC 2nd guess value (which, if we've found a first, means the solution space is really small?)
		error_code = -20;
		return;
	}
	C_monotonic_eq_solver::S_xy_pair phi_pair_upper;
	phi_pair_upper.x = phi_guess_upper;
	phi_pair_upper.y = P_solved_phi_guess_upper;

	// Set solver settings
	c_rd_od_solver.settings(tol, 50, phi_lower, phi_upper, true);

	// Now, solve for the flow coefficient
	double phi_solved, tol_solved;
	phi_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;

	int phi_code = 0;
	try
	{
		phi_code = c_rd_od_solver.solve(phi_pair_lower, phi_pair_upper, P_out, phi_solved, tol_solved, iter_solved);
	}
	catch (C_csp_exception)
	{
		error_code = -1;
		return;
	}

	if (phi_code != C_monotonic_eq_solver::CONVERGED)
	{
		int n_call_history = (int)c_rd_od_solver.get_solver_call_history()->size();

		if (n_call_history > 0)
			error_code = -(*(c_rd_od_solver.get_solver_call_history()))[n_call_history - 1].err_code;

		if (error_code == 0)
		{
			error_code = phi_code;
		}

		return;
	}

	T_out = ms_od_solved.m_T_out;		//[K]
}
