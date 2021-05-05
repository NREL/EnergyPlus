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

// HTF_props.h -- function prototypes for HTF property routines
#ifndef __THERMOCLINE_TES_
#define __THERMOCLINE_TES_

#include "sam_csp_util.h"
#include "htf_props.h"
#include <vector>

class TC_Fill_Props
{
public:
	TC_Fill_Props(){};

	bool Set_TC_Material( int fill_material )
	{ 
		m_fill_material = fill_material;
		if( m_fill_material > Quartzite || m_fill_material < Taconite )
			return false;
		else
			return true;
	};

	~TC_Fill_Props(){};
	
	enum {
		Taconite = 1,
		Calcium_Carbonate,
		Gravel,
		Marbel,
		Limestone,
		Carbon_Steel,
		Sand,
		Quartzite,
	};
	
	double dens_bed()
	{
		switch (m_fill_material)
		{
		case Taconite:
			return 3800.0;
		case Calcium_Carbonate:
			return 2710.0;
		case Gravel:
			return 2643.0;
		case Marbel:
			return 2680.0;
		case Limestone:
			return 2320.0;
		case Carbon_Steel:
			return 7854.0;
		case Sand:
			return 1515.0;
		case Quartzite:
			return 2640.0;
		default:
			return -999.0;
		};
	};

	double cp_bed()		//[kJ/kg-K]
	{
		switch (m_fill_material)
		{
		case Taconite:
			return 0.651;
		case Calcium_Carbonate:
			return 0.835;
		case Gravel:
			return 1.065;
		case Marbel:
			return 0.83;
		case Limestone:
			return 0.81;
		case Carbon_Steel:
			return 0.567;
		case Sand:
			return 0.8;
		case Quartzite:
			return 1.105;
		default:
			return -999.0;
		};
	};

	double k_bed()		//[W/m-K]
	{
		switch (m_fill_material)
		{
		case Taconite:
			return 2.1;
		case Calcium_Carbonate:
			return 2.7;
		case Gravel:
			return 1.8;
		case Marbel:
			return 2.8;
		case Limestone:
			return 2.15;
		case Carbon_Steel:
			return 48.0;
		case Sand:
			return 0.27;
		case Quartzite:
			return 5.38;
		default:
			return -999.0;
		};
	};


private:

	int m_fill_material;

};

class Thermocline_TES
{
public:
	Thermocline_TES()
	{
		m_Q_dot_htr_kJ = std::numeric_limits<double>::quiet_NaN();
		m_Q_dot_losses = std::numeric_limits<double>::quiet_NaN();
		m_T_hot_node = std::numeric_limits<double>::quiet_NaN();
		m_T_cold_node = std::numeric_limits<double>::quiet_NaN();
		m_T_max = std::numeric_limits<double>::quiet_NaN();
		m_f_hot = std::numeric_limits<double>::quiet_NaN();
		m_f_cold = std::numeric_limits<double>::quiet_NaN();
	};
	
	~Thermocline_TES(){};

	bool Initialize_TC( double H_m, double A_m2, int Fill, double U_kJ_hrm2K, double Utop_kJ_hrm2K, double Ubot_kJ_hrm2K,
							double f_void, double capfac, double Thmin_C, double Tcmax_C, int nodes, double T_hot_init_C,
							double T_cold_init_C, double TC_break, double T_htr_set_C, double tank_max_heat_MW, int tank_pairs, 
							HTFProperties & htf_fluid_props );

	bool Solve_TC( double T_hot_in_C, double flow_h_kghr, double T_cold_in_C, double flow_c_kghr, double T_env_C, int mode_in,
		              double Q_dis_target_W, double Q_cha_target_W, double f_storage_in, double time_hr,
					  double & m_dis_avail_tot, double & T_dis_avail_C, double & m_ch_avail_tot, double & T_ch_avail_C,
					  double & Q_dot_out_W, double & Q_dot_losses, double & T_hot_bed_C, double & T_cold_bed_C, double & T_max_bed_C,
					  double & f_hot, double & f_cold, double & Q_dot_htr_kJ);

	void Converged( double /*time*/ )
	{
		m_T_prev = m_T_end;
		m_T_final_ave_prev = m_T_final_ave;
		return;
	};

	double GetHeaterLoad_kJ()
	{ return m_Q_dot_htr_kJ; };

	double GetHeatLosses()
	{ return m_Q_dot_losses; };

	void GetFinalOutputs( double & T_hot_node, double & T_cold_node, double & T_max, double & f_hot, double & f_cold )
	{
		T_hot_node = m_T_hot_node;
		T_cold_node = m_T_cold_node;
		T_max = m_T_max;
		f_hot = m_f_hot;
		f_cold = m_f_cold;
		return;
	}
	

private:

	HTFProperties htfProps;		// Instance of HTFProperties class for Storage HTF
	TC_Fill_Props fillProps;

	// From calling method during "Initialize_TC" 
	double m_H;
	double m_A;
	double m_U;
	double m_U_top;
	double m_U_bot;
	double m_void;
	double m_capfac;
	double m_Thmin;
	double m_Tcmax;
	int m_nodes;	
	double m_T_hot_init;
	double m_T_cold_init;
	double m_TC_break;
	double m_T_htr_set;
	double m_tank_max_heat;
	int m_tank_pairs;

	// Calculated values
	double m_Th_avail_min;
	double m_Tc_avail_max;
	int m_num_TC_max;	
	double m_P;
	double m_vol;
	double m_UA;
	double m_UA_top;
	double m_UA_bot;
	double m_ef_cond;
	double m_cap;
	double m_e_tes;
	double m_cap_node;
	double m_tol_TC;
	double m_T_hot_in_min; 
	double m_T_cold_in_max;

	vector<double> m_T_prev, m_T_start, m_T_ave, m_T_end, m_T_ts_ave, m_Q_losses, m_Q_htr, m_T_cout_ave, m_T_hout_ave;

	//util::matrix_t<double> m_T_prev;
	//util::matrix_t<double> m_T_start;
	//util::matrix_t<double> m_T_ave;
	//util::matrix_t<double> m_T_end;
	//util::matrix_t<double> m_T_ts_ave;
	//util::matrix_t<double> m_Q_losses;
	//util::matrix_t<double> m_Q_htr;
	//util::matrix_t<double> m_T_cout_ave;
	//util::matrix_t<double> m_T_hout_ave;


	// Values from previous timestep
	double m_T_final_ave_prev;
	double m_T_final_ave;	

	// Ouputs accessible through class methods
	double m_Q_dot_htr_kJ;
	double m_Q_dot_losses;
	double m_T_hot_node;
	double m_T_cold_node;
	double m_T_max;
	double m_f_hot;
	double m_f_cold;

	// Constant Properties
	double m_cond;
	double m_cp_a;
	double m_rho_a;
	double m_cp_r;
	double m_rho_r;
										
};

#endif
