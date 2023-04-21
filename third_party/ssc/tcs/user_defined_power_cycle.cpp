#include "user_defined_power_cycle.h"
#include "csp_solver_util.h"

void C_user_defined_pc::init(const util::matrix_t<double> & T_htf_ind,
	const util::matrix_t<double> & T_amb_ind,
	const util::matrix_t<double> & m_dot_htf_ind)
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
}

double C_user_defined_pc::get_W_dot_gross_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect
	
	return get_interpolated_ND_output(i_W_dot_gross, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_user_defined_pc::get_Q_dot_HTF_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_Q_dot_HTF, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_user_defined_pc::get_W_dot_cooling_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_W_dot_cooling, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_user_defined_pc::get_m_dot_water_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_m_dot_water, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_user_defined_pc::get_interpolated_ND_output(int i_ME /*M.E. table index*/, 
							double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{

	// Y_ND = (Y_ME(T_htf)-1)*Y_INT(T_amb) + 1
	//              *
	//        (Y_ME(T_amb)-1)*Y_INT(m_dot_htf) + 1
	//              *
	//        (Y_ME(m_dot_htf)-1)*Y_INT(T_htf) + 1

	double Y_ME_T_htf = mc_T_htf_ind.interpolate_x_col_0(i_ME, T_htf_hot);
	double Y_INT_on_T_htf = mc_T_amb_ind.interpolate_x_col_0(i_ME+1, T_amb);

	double Y_ME_T_amb = mc_T_amb_ind.interpolate_x_col_0(i_ME, T_amb);
	double Y_INT_on_T_amb = mc_m_dot_htf_ind.interpolate_x_col_0(i_ME+1, m_dot_htf_ND);

	double Y_ME_m_dot = mc_m_dot_htf_ind.interpolate_x_col_0(i_ME, m_dot_htf_ND);
	double Y_INT_on_m_dot = mc_T_htf_ind.interpolate_x_col_0(i_ME+1, T_htf_hot);

	return ( (Y_ME_T_htf-1)*Y_INT_on_T_htf + 1.0) *
		   ( (Y_ME_T_amb-1)*Y_INT_on_T_amb + 1.0) *
		   ( (Y_ME_m_dot-1)*Y_INT_on_m_dot + 1.0);
}