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

#include "ud_power_cycle.h"
#include "csp_solver_util.h"

void C_ud_power_cycle::init(const util::matrix_t<double> & T_htf_ind, double T_htf_ref /*C*/, double T_htf_low /*C*/, double T_htf_high /*C*/,
	const util::matrix_t<double> & T_amb_ind, double T_amb_ref /*C*/, double T_amb_low /*C*/, double T_amb_high /*C*/,
	const util::matrix_t<double> & m_dot_htf_ind, double m_dot_htf_ref /*-*/, double m_dot_htf_low /*-*/, double m_dot_htf_high /*-*/)
{

	// Set up Linear Interp class
	int error_index = -2;
	int column_index_array[1] = {0};
	if( !mc_T_htf_ind.Set_1D_Lookup_Table( T_htf_ind, column_index_array, 1, error_index) )
	{
		if(error_index == -1)
		{
			throw(C_csp_exception("Table representing Hot HTF Temperature parametric results must have"
							"at least 3 rows", "User defined power cycle initialization"));
		}
		else
		{
			throw(C_csp_exception("The Hot HTF Temperature must monotonically increase in the table",
							"User defined power cycle initialization"));
		}
	}

	if( !mc_T_amb_ind.Set_1D_Lookup_Table(T_amb_ind, column_index_array, 1, error_index) )
	{
		if( error_index == -1 )
		{
			throw(C_csp_exception("Table representing Ambient Temperature parametric results must have"
				"at least 3 rows", "User defined power cycle initialization"));
		}
		else
		{
			throw(C_csp_exception("The Ambient Temperature must monotonically increase in the table",
				"User defined power cycle initialization"));
		}
	}

	if( !mc_m_dot_htf_ind.Set_1D_Lookup_Table(m_dot_htf_ind, column_index_array, 1, error_index) )
	{
		if( error_index == -1 )
		{
			throw(C_csp_exception("Table representing HTF mass flow rate parametric results must have"
				"at least 3 rows", "User defined power cycle initialization"));
		}
		else
		{
			throw(C_csp_exception("The HTF mass flow rate must monotonically increase in the table",
				"User defined power cycle initialization"));
		}
	}

	// Set member data for reference and upper and lower bounds of independent variables
	m_T_htf_ref = T_htf_ref;
	m_T_htf_low = T_htf_low;
	m_T_htf_high = T_htf_high;

	m_T_amb_ref = T_amb_ref;
	m_T_amb_low = T_amb_low;
	m_T_amb_high = T_amb_high;

	m_m_dot_htf_ref = m_dot_htf_ref;
	m_m_dot_htf_low = m_dot_htf_low;
	m_m_dot_htf_high = m_dot_htf_high;

	// Check that the reference (design) value and upper and lower levels for each independent variable are contained within the x-range of the corresponding table
		// T_HTF
	if( !mc_T_htf_ind.check_x_value_x_col_0(m_T_htf_ref) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the hot HTF temperature"
		" must contain the design HTF temperature %lg [C]. %s [C]", m_T_htf_ref, mc_T_htf_ind.get_error_msg().c_str());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_T_htf_ind.check_x_value_x_col_0(m_T_htf_low) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the hot HTF temperature"
			" must contain the lower level HTF temperature %lg [C]. %s [C]", m_T_htf_low, mc_T_htf_ind.get_error_msg().c_str());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_T_htf_ind.check_x_value_x_col_0(m_T_htf_high) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the hot HTF temperature"
			" must contain the upper level HTF temperature %lg [C]. %s [C]", m_T_htf_high, mc_T_htf_ind.get_error_msg().c_str());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}

		// T_amb
	if( !mc_T_amb_ind.check_x_value_x_col_0(m_T_amb_ref) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the ambient temperature"
		" must contain the design ambient temperature %lg [C]. %s [C]", m_T_amb_ref, mc_T_amb_ind.get_error_msg().c_str());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_T_amb_ind.check_x_value_x_col_0(m_T_amb_low) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the ambient temperature"
		" must contain the lower level ambient temperature %lg [C]. %s [C]", m_T_amb_low, mc_T_amb_ind.get_error_msg().c_str());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_T_amb_ind.check_x_value_x_col_0(m_T_amb_high) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the ambient temperature"
		" must contain the upper level ambient temperature %lg [C]. %s [C]", m_T_amb_high, mc_T_amb_ind.get_error_msg().c_str());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}

		// m_dot_HTF
	if( !mc_m_dot_htf_ind.check_x_value_x_col_0(m_m_dot_htf_ref) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the normalized HTF mass flow rate"
		" must contain the design normalized HTF mass flow rate %lg [-]. %s [-]", m_m_dot_htf_ref, mc_m_dot_htf_ind.get_error_msg().c_str());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_m_dot_htf_ind.check_x_value_x_col_0(m_m_dot_htf_low) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the normalized HTF mass flow rate"
			" must contain the lower level normalized HTF mass flow rate %lg [-]. %s [-]", m_m_dot_htf_low, mc_m_dot_htf_ind.get_error_msg().c_str());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_m_dot_htf_ind.check_x_value_x_col_0(m_m_dot_htf_high) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the normalized HTF mass flow rate"
			" must contain the upper level normalized HTF mass flow rate %lg [-]. %s [-]", m_m_dot_htf_high, mc_m_dot_htf_ind.get_error_msg().c_str());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}

	// ************************************************************************
	// ************************************************************************

	// Calculate main effects of each independent variable at its upper and lower levels
	m_ME_T_htf_low.resize(4);
	m_ME_T_htf_high.resize(4);

	m_ME_T_amb_low.resize(4);
	m_ME_T_amb_high.resize(4);

	m_ME_m_dot_htf_low.resize(4);
	m_ME_m_dot_htf_high.resize(4);

	for(int i = 0; i < 4; i++)
	{
		int i_col = i*3+2;

		m_ME_T_htf_low[i] = mc_T_htf_ind.interpolate_x_col_0(i_col, m_T_htf_low) - 1.0;
		m_ME_T_htf_high[i] = mc_T_htf_ind.interpolate_x_col_0(i_col, m_T_htf_high) - 1.0;

		m_ME_T_amb_low[i] = mc_T_amb_ind.interpolate_x_col_0(i_col, m_T_amb_low) - 1.0;
		m_ME_T_amb_high[i] = mc_T_amb_ind.interpolate_x_col_0(i_col, m_T_amb_high) - 1.0;

		m_ME_m_dot_htf_low[i] = mc_m_dot_htf_ind.interpolate_x_col_0(i_col, m_m_dot_htf_low) - 1.0;
		m_ME_m_dot_htf_high[i] = mc_m_dot_htf_ind.interpolate_x_col_0(i_col, m_m_dot_htf_high) - 1.0;
	}

	// Set up 2D tables to store calculated Interactions	
	int n_T_htf_runs = mc_T_htf_ind.get_number_of_rows();
	int n_T_amb_runs = mc_T_amb_ind.get_number_of_rows();
	int n_m_dot_htf_runs = mc_m_dot_htf_ind.get_number_of_rows();

	// 2 interaction effects (upper and lower) for each output
	util::matrix_t<double> T_htf_int_on_T_amb(n_T_amb_runs, 9);
	util::matrix_t<double> T_amb_int_on_m_dot_htf(n_m_dot_htf_runs, 9);
	util::matrix_t<double> m_dot_htf_int_on_T_htf(n_T_htf_runs, 9);
	
	// Calculate interaction effects
	for(int i = 0; i < 4; i++)
	{
		// T_HTF interaction on ambient temperature
		for(int j = 0; j < n_T_amb_runs; j++)
		{
			if( i == 0 )
			{
				T_htf_int_on_T_amb(j,0) = mc_T_amb_ind.get_x_value_x_col_0(j);
			}
				// lower level interaction effect
			double aa = mc_T_amb_ind.Get_Value(i*3+1,j);
			double bb = m_ME_T_htf_low[i];
			double cc = mc_T_amb_ind.Get_Value(i*3+2,j);
			T_htf_int_on_T_amb(j,i*2+1) = -(mc_T_amb_ind.Get_Value(i*3+1,j)-1.0-m_ME_T_htf_low[i]-(mc_T_amb_ind.Get_Value(i*3+2,j)-1.0));
				// upper level interaction
			aa = mc_T_amb_ind.Get_Value(i*3+3,j);
			bb = m_ME_T_htf_high[i];
			cc = mc_T_amb_ind.Get_Value(i*3+2,j);
			T_htf_int_on_T_amb(j,i*2+2) = -(mc_T_amb_ind.Get_Value(i*3+3,j)-1.0-m_ME_T_htf_high[i]-(mc_T_amb_ind.Get_Value(i*3+2,j)-1.0));
		}

		// Ambient temperature interaction on HTF mass flow rate
		for(int j = 0; j < n_m_dot_htf_runs; j++)
		{
			if( i == 0 )
			{
				T_amb_int_on_m_dot_htf(j,0) = mc_m_dot_htf_ind.get_x_value_x_col_0(j);
			}
				// lower level interaction effect
			double aa = mc_m_dot_htf_ind.Get_Value(i*3+1,j);
			double bb = m_ME_T_amb_low[i];
			double cc = mc_m_dot_htf_ind.Get_Value(i*3+2,j);
			T_amb_int_on_m_dot_htf(j,i*2+1) = -(mc_m_dot_htf_ind.Get_Value(i*3+1,j)-1.0-m_ME_T_amb_low[i]-(mc_m_dot_htf_ind.Get_Value(i*3+2,j)-1.0));
				// upper level interaction effect
			aa = mc_m_dot_htf_ind.Get_Value(i*3+3,j);
			bb = m_ME_T_amb_high[i];
			cc = mc_m_dot_htf_ind.Get_Value(i*3+2,j);
			T_amb_int_on_m_dot_htf(j,i*2+2) = -(mc_m_dot_htf_ind.Get_Value(i*3+3,j)-1.0-m_ME_T_amb_high[i]-(mc_m_dot_htf_ind.Get_Value(i*3+2,j)-1.0));
		}

		// HTF mass flow
		for(int j = 0; j < n_T_htf_runs; j++)
		{
			if( i == 0 )
			{
				m_dot_htf_int_on_T_htf(j,0) = mc_T_htf_ind.get_x_value_x_col_0(j);
			}
				// lower level interaction effect
			double aa = mc_T_htf_ind.Get_Value(i*3+1,j);
			double bb = m_ME_m_dot_htf_low[i];
			double cc = mc_T_htf_ind.Get_Value(i*3+2,j);
			m_dot_htf_int_on_T_htf(j,i*2+1) = -(mc_T_htf_ind.Get_Value(i*3+1,j)-1.0-m_ME_m_dot_htf_low[i]-(mc_T_htf_ind.Get_Value(i*3+2,j)-1.0));
				// upper level interaction effect
			aa = mc_T_htf_ind.Get_Value(i*3+3,j);
			bb = m_ME_m_dot_htf_high[i];
			cc = mc_T_htf_ind.Get_Value(i*3+2,j);
			m_dot_htf_int_on_T_htf(j,i*2+2) = -(mc_T_htf_ind.Get_Value(i*3+3,j)-1.0-m_ME_m_dot_htf_high[i]-(mc_T_htf_ind.Get_Value(i*3+2,j)-1.0));
		}
	}

	// Initialize Linear_Interp classes for interaction effects
	if( !mc_T_htf_on_T_amb.Set_1D_Lookup_Table(T_htf_int_on_T_amb, column_index_array, 1, error_index) )
	{
		throw(C_csp_exception("Initialization of interpolation table for the interaction effect of T_HTF levels"
		"on the ambient temperature failed", "User defined power cycle initialization"));
	}
	if( !mc_T_amb_on_m_dot_htf.Set_1D_Lookup_Table(T_amb_int_on_m_dot_htf, column_index_array, 1, error_index) )
	{
		throw(C_csp_exception("Initialization of interpolation table for the interaction effect of T_amb levels"
			"on HTF mass flow rate failed", "User defined power cycle initialization"));
	}
	if( !mc_m_dot_htf_on_T_htf.Set_1D_Lookup_Table(m_dot_htf_int_on_T_htf, column_index_array, 1, error_index) )
	{
		throw(C_csp_exception("Initialization of interpolation table for the interaction effect of m_dot_HTF levels"
			"on the HTF temperature failed", "User defined power cycle initialization"));
	}
	
}

double C_ud_power_cycle::get_W_dot_gross_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect
	
	return get_interpolated_ND_output(i_W_dot_gross, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle::get_Q_dot_HTF_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_Q_dot_HTF, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle::get_W_dot_cooling_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_W_dot_cooling, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle::get_m_dot_water_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_m_dot_water, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle::get_interpolated_ND_output(int i_ME /*M.E. table index*/, 
							double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	
	double ME_T_htf = mc_T_htf_ind.interpolate_x_col_0(i_ME*3+2, T_htf_hot) - 1.0;
	double ME_T_amb = mc_T_amb_ind.interpolate_x_col_0(i_ME*3+2, T_amb) - 1.0;
	double ME_m_dot_htf = mc_m_dot_htf_ind.interpolate_x_col_0(i_ME*3+2, m_dot_htf_ND) - 1.0;

	double INT_T_htf_on_T_amb = 0.0;
	if( T_htf_hot < m_T_htf_ref )
	{
		INT_T_htf_on_T_amb = mc_T_htf_on_T_amb.interpolate_x_col_0(i_ME*2+1,T_amb)*(T_htf_hot-m_T_htf_ref)/(m_T_htf_ref-m_T_htf_low);
	}
	if( T_htf_hot > m_T_htf_ref )
	{
		INT_T_htf_on_T_amb = mc_T_htf_on_T_amb.interpolate_x_col_0(i_ME*2+2,T_amb)*(T_htf_hot-m_T_htf_ref)/(m_T_htf_ref-m_T_htf_high);
	}

	double INT_T_amb_on_m_dot_htf = 0.0;
	if( T_amb < m_T_amb_ref )
	{
		INT_T_amb_on_m_dot_htf = mc_T_amb_on_m_dot_htf.interpolate_x_col_0(i_ME*2+1,m_dot_htf_ND)*(T_amb-m_T_amb_ref)/(m_T_amb_ref-m_T_amb_low);
	}
	if( T_amb > m_T_amb_ref )
	{
		INT_T_amb_on_m_dot_htf = mc_T_amb_on_m_dot_htf.interpolate_x_col_0(i_ME*2+2,m_dot_htf_ND)*(T_amb-m_T_amb_ref)/(m_T_amb_ref-m_T_amb_high);
	}

	double INT_m_dot_htf_on_T_htf = 0.0;
	if( m_dot_htf_ND < m_m_dot_htf_ref )
	{
		INT_m_dot_htf_on_T_htf = mc_m_dot_htf_on_T_htf.interpolate_x_col_0(i_ME*2+1,T_htf_hot)*(m_dot_htf_ND-m_m_dot_htf_ref)/(m_m_dot_htf_ref-m_m_dot_htf_low);
	}
	if( m_dot_htf_ND > m_m_dot_htf_ref )
	{
		INT_T_amb_on_m_dot_htf = mc_m_dot_htf_on_T_htf.interpolate_x_col_0(i_ME*2+2,T_htf_hot)*(m_dot_htf_ND-m_m_dot_htf_ref)/(m_m_dot_htf_ref-m_m_dot_htf_high);
	}

	return 1.0 + ME_T_htf + ME_T_amb + ME_m_dot_htf + INT_T_htf_on_T_amb + INT_T_amb_on_m_dot_htf + INT_m_dot_htf_on_T_htf;
}



C_ud_pc_table_generator::C_ud_pc_table_generator(C_od_pc_function & f_pc_eq) : mf_pc_eq(f_pc_eq)
{
	mf_callback = 0;		// = NULL
	mp_mf_active = 0;			// = NULL
	m_progress_msg = "Power cycle preprocessing...";
	m_log_msg = "Log message";

	return;
}

void C_ud_pc_table_generator::send_callback(bool is_od_model_error, int run_number, int n_runs_total,
	double T_htf_hot, double m_dot_htf_ND, double T_amb,
	double W_dot_gross_ND, double Q_dot_in_ND,
	double W_dot_cooling_ND, double m_dot_water_ND)
{
	if (mf_callback && mp_mf_active)
	{
		std::string od_err_msg = "";
		if (is_od_model_error)
		{
			od_err_msg = "***************\nWarning: off design model failed\n"
				"Using generic off design for this point\n"
				"Check if values are appropriate before running annual simulation\n"
				"***************\n";
		}

		m_log_msg = od_err_msg + util::format("[%d/%d] At T_htf = %lg [C],"
			" normalized m_dot = %lg,"
			" and T_amb = %lg [C]. The normalized outputs are: gross power = %lg,"
			" thermal input = %lg, cooling power = %lg, and water use = %lg",
			run_number, n_runs_total,
			T_htf_hot, m_dot_htf_ND, T_amb,
			W_dot_gross_ND, Q_dot_in_ND,
			W_dot_cooling_ND, m_dot_water_ND);
		if (!mf_callback(m_log_msg, m_progress_msg, mp_mf_active, 100.0*run_number / n_runs_total, 2))
		{
			std::string error_msg = "User terminated simulation...";
			std::string loc_msg = "C_ud_pc_table_generator";
			throw(C_csp_exception(error_msg, loc_msg, 1));
		}
	}
}

int C_ud_pc_table_generator::generate_tables(double T_htf_ref /*C*/, double T_htf_low /*C*/, double T_htf_high /*C*/, int n_T_htf /*-*/,
	double T_amb_ref /*C*/, double T_amb_low /*C*/, double T_amb_high /*C*/, int n_T_amb /*-*/,
	double m_dot_htf_ND_ref /*-*/, double m_dot_htf_ND_low /*-*/, double m_dot_htf_ND_high /*-*/, int n_m_dot_htf_ND,
	util::matrix_t<double> & T_htf_ind, util::matrix_t<double> & T_amb_ind, util::matrix_t<double> & m_dot_htf_ind)
{
	// Check levels against design values
	if(T_htf_low >= T_htf_ref)
	{
		std::string msg = util::format("The lower level of HTF temperature %lg [C] must be colder than the design temperature %lg [C].",
							T_htf_low, T_htf_ref);
		throw(C_csp_exception(msg, "User defined power cycle, generate tables"));
	}
	if(T_htf_high <= T_htf_ref)
	{
		std::string msg = util::format("The upper level of HTF temperature %lg [C] must be hotter than the design temperature %lg [C].",
							T_htf_high, T_htf_ref);
		throw(C_csp_exception(msg, "User defined power cycle, generate tables"));
	}
	if(T_amb_low >= T_amb_ref)
	{
		std::string msg = util::format("The lower level of ambient temperature %lg [C] must be colder than the design temperatuare %lg [C].",
							T_amb_low, T_amb_ref);
		throw(C_csp_exception(msg, "User defined power cycle, generate tables"));
	}
	if(T_amb_high <= T_amb_ref)
	{
		std::string msg = util::format("The upper level of ambient temperature %lg [C] must be warmer than the design temperature %lg [C].",
							T_amb_high, T_amb_ref);
		throw(C_csp_exception(msg, "User defined power cycle, generate tables"));
	}
	if(m_dot_htf_ND_low >= m_dot_htf_ND_ref)
	{
		std::string msg = util::format("The lower level of the normalized HTF mass flow rate %lg must be less than the design value %lg.",
							m_dot_htf_ND_low, m_dot_htf_ND_ref);
		throw(C_csp_exception(msg, "User defined power cycle, generate tables"));
	}
	if(m_dot_htf_ND_high <= m_dot_htf_ND_ref)
	{
		std::string msg = util::format("The upper level of the normalized HTF mass flow rate %lg must be greater than the design value %lg.",
							m_dot_htf_ND_high, m_dot_htf_ND_ref);
		throw(C_csp_exception(msg, "User defined power cycle, generate tables"));
	}

	C_od_pc_function::S_f_inputs pc_inputs;
	C_od_pc_function::S_f_outputs pc_outputs;

	// ******************************************
	// Setup T_HTF parameteric runs
	if(n_T_htf < 3)
	{
		std::string msg = util::format("The input argument for number of indepedent HTF temperatures is %d."
						" It was reset to the minimum value of 3.", n_T_htf);
		mc_messages.add_notice(msg);
		n_T_htf = 3;
	}

	T_htf_ind.clear();
	T_htf_ind.resize(n_T_htf, 13);		// Set matrix size
	double delta_T_htf = (T_htf_high - T_htf_low)/double(n_T_htf-1);

	double n_runs_total = 3.0*(n_T_htf + n_T_amb + n_m_dot_htf_ND);

	// Set ambient temperature because it is constant for the HTF temperature parametrics
	pc_inputs.m_T_amb = T_amb_ref;	//[C]
	for(int i = 0; i < n_T_htf; i++)
	{
		T_htf_ind(i,0) = T_htf_low + delta_T_htf*i;	//[C]
		pc_inputs.m_T_htf_hot = T_htf_ind(i,0);

		// Call at low, ref, and high ND mass flow rate levels
		std::vector<double> m_dot_htf_ND_levels(3);
		m_dot_htf_ND_levels[0] = m_dot_htf_ND_low;
		m_dot_htf_ND_levels[1] = m_dot_htf_ND_ref;
		m_dot_htf_ND_levels[2] = m_dot_htf_ND_high;
		for(int j = 0; j < 3; j++)
		{
			bool is_od_model_error = false;

			pc_inputs.m_m_dot_htf_ND = m_dot_htf_ND_levels[j];
			int off_design_code = mf_pc_eq(pc_inputs,pc_outputs);

			if( off_design_code == 0 )
			{
				// Save outputs
				T_htf_ind(i,1+j) = pc_outputs.m_W_dot_gross_ND;		//[-]
				T_htf_ind(i,4+j) = pc_outputs.m_Q_dot_in_ND;		//[-]
				T_htf_ind(i,7+j) = pc_outputs.m_W_dot_cooling_ND;	//[-]
				T_htf_ind(i,10+j) = pc_outputs.m_m_dot_water_ND;	//[-]
			}
			else if (off_design_code == -1)
			{
				// Save 'generic' off design model response
				T_htf_ind(i, 1 + j) = pc_inputs.m_m_dot_htf_ND;		//[-]
				T_htf_ind(i, 4 + j) = pc_inputs.m_m_dot_htf_ND;		//[-]
				T_htf_ind(i, 7 + j) = pc_inputs.m_m_dot_htf_ND;		//[-]
				T_htf_ind(i, 10 + j) = pc_inputs.m_m_dot_htf_ND;	//[-]

				is_od_model_error = true;
			}
			else
			{
				std::string err_msg = util::format("The 1st UDPC table (primary: T_htf, interaction: m_dot_htf_ND) generation failed at T_htf = %lg [C] and m_dot_htf = %lg [-]", pc_inputs.m_T_htf_hot, pc_inputs.m_m_dot_htf_ND);
				throw(C_csp_exception(err_msg, "UDPC"));
			}

			double run_number = i*3 + j;

			send_callback(is_od_model_error, (int)run_number + 1, (int)n_runs_total,
				pc_inputs.m_T_htf_hot, pc_inputs.m_m_dot_htf_ND, pc_inputs.m_T_amb,
				T_htf_ind(i, 1 + j), T_htf_ind(i, 4 + j),
				T_htf_ind(i, 7 + j), T_htf_ind(i, 10 + j));

		}

	}
	// ******************************************

	// ******************************************
	// Setup T_amb parametric runs
	if(n_T_amb < 3)
	{
		std::string msg = util::format("The input argument for number of independent ambient temperatures"
						" is %d. It was reset to the minimum value of 3.", n_T_amb);
		mc_messages.add_notice(msg);
		n_T_amb = 3;
	}

	T_amb_ind.clear();
	T_amb_ind.resize(n_T_amb, 13);		// Set matrix size
	double delta_T_amb = (T_amb_high - T_amb_low)/double(n_T_amb-1);

	// Set ND htf mass flow rate because it is constant for the ambient temperature parametrics
	pc_inputs.m_m_dot_htf_ND = m_dot_htf_ND_ref;
	for(int i = 0; i < n_T_amb; i++)
	{
		T_amb_ind(i,0) = T_amb_low + delta_T_amb*i;		//[C]
		pc_inputs.m_T_amb = T_amb_ind(i,0);				//[C]

		// Call at low, ref, and high HTF temperature levels
		std::vector<double> T_htf_levels(3);
		T_htf_levels[0] = T_htf_low;   //[C]
		T_htf_levels[1] = T_htf_ref;   //[C]
		T_htf_levels[2] = T_htf_high;  //[C]
		for(int j = 0; j < 3; j++)
		{
			bool is_od_model_error = false;

			pc_inputs.m_T_htf_hot = T_htf_levels[j];
			int off_design_code = mf_pc_eq(pc_inputs,pc_outputs);

			if( off_design_code == 0 )
			{
				// Save outputs
				T_amb_ind(i,1+j) = pc_outputs.m_W_dot_gross_ND;		//[-]
				T_amb_ind(i,4+j) = pc_outputs.m_Q_dot_in_ND;		//[-]
				T_amb_ind(i,7+j) = pc_outputs.m_W_dot_cooling_ND;	//[-]
				T_amb_ind(i,10+j) = pc_outputs.m_m_dot_water_ND;	//[-]
			}
			else if (off_design_code == -1)
			{
				// Save 'generic' off design model response
				T_amb_ind(i, 1 + j) = pc_inputs.m_m_dot_htf_ND;		//[-]
				T_amb_ind(i, 4 + j) = pc_inputs.m_m_dot_htf_ND;		//[-]
				T_amb_ind(i, 7 + j) = pc_inputs.m_m_dot_htf_ND;		//[-]
				T_amb_ind(i, 10 + j) = pc_inputs.m_m_dot_htf_ND;	//[-]

				is_od_model_error = true;
			}
			else
			{
				std::string err_msg = util::format("The 2nd UDPC table (primary: T_amb, interaction: T_htf) generation failed at T_amb = %lg [C] and T_htf = %lg [C]", pc_inputs.m_T_amb, pc_inputs.m_T_htf_hot);
				throw(C_csp_exception(err_msg, "UDPC"));
			}

			double run_number = 3.0*n_T_htf + i*3 + j;

			send_callback(is_od_model_error, (int)run_number + 1, (int)n_runs_total,
				pc_inputs.m_T_htf_hot, pc_inputs.m_m_dot_htf_ND, pc_inputs.m_T_amb,
				T_amb_ind(i, 1 + j), T_amb_ind(i, 4 + j),
				T_amb_ind(i, 7 + j), T_amb_ind(i, 10 + j));
		}
	}
	// ******************************************

	// ******************************************
	// Setup ND m_dot parametric runs
	if(n_m_dot_htf_ND < 3)
	{
		std::string msg = util::format("The input argument for number of independent normalized HTF mass flow rates"
						" is %d. It was reset to the minimum value of 3.", n_m_dot_htf_ND);
		mc_messages.add_notice(msg);
		n_m_dot_htf_ND = 3;
	}

	m_dot_htf_ind.clear();
	m_dot_htf_ind.resize(n_m_dot_htf_ND,13);		// Set matrix size
	double delta_m_dot = (m_dot_htf_ND_high-m_dot_htf_ND_low)/double(n_m_dot_htf_ND-1);

	// Set HTF temperature because it is constant for the ambient temperature parametrics
	pc_inputs.m_T_htf_hot = T_htf_ref;
	for(int i = 0; i < n_m_dot_htf_ND; i++)
	{
		m_dot_htf_ind(i,0) = m_dot_htf_ND_low + delta_m_dot*i;		//[-]
		pc_inputs.m_m_dot_htf_ND = m_dot_htf_ind(i,0);				//[-]

		// Call at low, ref, and high ambient temperatures
		std::vector<double> T_amb_levels(3);
		T_amb_levels[0] = T_amb_low;    //[C]
		T_amb_levels[1] = T_amb_ref;	//[C]
		T_amb_levels[2] = T_amb_high;	//[C]
		for(int j = 0; j < 3; j++)
		{
			bool is_od_model_error = false;

			pc_inputs.m_T_amb = T_amb_levels[j];
			int off_design_code = mf_pc_eq(pc_inputs, pc_outputs);
		
			if( off_design_code == 0 )
			{
				// Save outputs
				m_dot_htf_ind(i,1+j) = pc_outputs.m_W_dot_gross_ND;		//[-]
				m_dot_htf_ind(i,4+j) = pc_outputs.m_Q_dot_in_ND;		//[-]
				m_dot_htf_ind(i,7+j) = pc_outputs.m_W_dot_cooling_ND;	//[-]
				m_dot_htf_ind(i,10+j) = pc_outputs.m_m_dot_water_ND;	//[-]
			}
			else if (off_design_code == -1)
			{
				// Save 'generic' off design model response
				m_dot_htf_ind(i, 1 + j) = pc_inputs.m_m_dot_htf_ND;		//[-]
				m_dot_htf_ind(i, 4 + j) = pc_inputs.m_m_dot_htf_ND;		//[-]
				m_dot_htf_ind(i, 7 + j) = pc_inputs.m_m_dot_htf_ND;		//[-]
				m_dot_htf_ind(i, 10 + j) = pc_inputs.m_m_dot_htf_ND;	//[-]

				is_od_model_error = true;
			}
			else
			{
				std::string err_msg = util::format("The 3rd UDPC table (primary: m_dot_htf_ND, interaction: T_amb) generation failed at T_amb = %lg [C] and m_dot_htf = %lg [-]", pc_inputs.m_T_amb, pc_inputs.m_m_dot_htf_ND);
				throw(C_csp_exception(err_msg, "UDPC"));
			}

			double run_number = 3.0*n_T_htf + 3.0*n_T_amb  + i*3 + j;

			send_callback(is_od_model_error, (int)run_number + 1, (int)n_runs_total,
				pc_inputs.m_T_htf_hot, pc_inputs.m_m_dot_htf_ND, pc_inputs.m_T_amb,
				m_dot_htf_ind(i, 1 + j), m_dot_htf_ind(i, 4 + j),
				m_dot_htf_ind(i, 7 + j), m_dot_htf_ind(i, 10 + j));
		}
	}
	// ******************************************
	
	return 0;
}