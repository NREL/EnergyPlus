#ifndef __USER_DEFINED_POWER_CYCLE_
#define __USER_DEFINED_POWER_CYCLE_

#include "interpolation_routines.h"
#include "csp_solver_util.h"

class C_user_defined_pc
{

private:
	
	// Each Linear_Interp Table in C_user_defined_pc shares the following column structure:

	//    Independent |    Gross Power Output   |   HTF Thermal Power	|   Cooling Parasitics  |	 Water Use 
	// 0)  Variable   | 1) M.E.   |  2) INT w/  | 3) M.E.  | 4) INT w/  | 5) M.E.  | 6) INT w/  | 7) M.E.  | 8) INT w/

	enum E_output_ME_index
	{
		i_W_dot_gross = 1,
		i_Q_dot_HTF = 3,
		i_W_dot_cooling = 5,
		i_m_dot_water = 7
	};

	// Lookup table with dependent variables corresponding to parametric on independent variable T_htf_hot [C] (first column)
	Linear_Interp mc_T_htf_ind;		// Interaction w/ m_dot_htf

	// Lookup table with dependent variables corresponding to parametric on independent variable T_amb [C] (first column)
	Linear_Interp mc_T_amb_ind;		// Interaction w/ T_htf

	// Lookup table with dependent variables corresponding to parametric on independent variable m_dot_htf [ND] (first column)
	Linear_Interp mc_m_dot_htf_ind;	// Interaction w/ T_amb

	// member string for exception messages
	std::string m_error_msg;

	double get_interpolated_ND_output(int i_ME /*M.E. table index*/, double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/);

public:

	C_user_defined_pc(){};

	~C_user_defined_pc(){};

	void init( const util::matrix_t<double> & T_htf_ind,
				const util::matrix_t<double> & T_amb_ind,
				const util::matrix_t<double> & m_dot_htf_ind );

	double get_W_dot_gross_ND( double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/);

	double get_Q_dot_HTF_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/);

	double get_W_dot_cooling_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/);

	double get_m_dot_water_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/);

};





#endif