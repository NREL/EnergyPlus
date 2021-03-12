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

#ifndef __SCO2_REC_UTIL_
#define __SCO2_REC_UTIL_

#include <vector>
#include <stdexcept>

//#include "co2props.h"
//#include "co2props_nn.h"
#include "CO2_properties.h"
#include "sam_csp_util.h"

using namespace std;


namespace N_sco2_rec
{
    
    class sco2_exception : public std::runtime_error
    {
    public:
        sco2_exception(const std::string& message) 
		    : std::runtime_error(message.c_str()){};
	    sco2_exception(const char *msg)
		    : std::runtime_error(msg){};
    };

	class C_rec_des_props
	{
	public:
		C_rec_des_props(int material);

		enum 
		{
			Haynes_230 = 1		
		};
	
		double cond(double T_C);
		double modE(double T_C);
		double alpha_inst(double T_C);
		double poisson();
		double creep_life(double sigma_MPa, double T_C);
		double cycles_to_failure(double epsilon_equiv, double T_C);
	
	private:
		int m_material;

		enum E_haynes230_creep_temps
		{
			T_1050F = 1,
			T_1100F,
			T_1200F,
			T_1300F,
			T_1400F,
			T_1500F,
			T_1600F,
			T_1700F,
			T_1800F
		};

		enum E_haynes230_cycle_temps
		{
			T_427C = 1,
			T_538C,
			T_649C,
			T_760C,
			T_871C,
			T_982C		
		};

		double haynes230_enum_creep_temps(int enum_T_F);
		double haynes230_creep_life(int enum_T_F, double sigma_ksi);
		double interpolate_creep_life(int enum_T_low, int enum_T_high, double T_F, double sigma_ksi);

		double haynes230_enum_cycle_temps(int enum_T_C);
		double haynes230_eps_min(int enum_T_C);
		double haynes230_cycles_to_failure(int enum_T_C, double eps_equiv);
		double interpolate_cycles_to_failure(int enum_T_low, int enum_T_high, double T_C, double eps_equiv);

	};

	class C_tube_slice
	{	

	private:
		struct S_creep_fatigue_outputs
		{
			double m_eps_a_perc_inel;
			double m_eps_r_perc_inel;
			double m_eps_t_perc_inel;

			double m_eps_equiv_perc_SF;
			double m_N_cycles;
			double m_fatigue_damage;

			double m_max_stress_SF;
			double m_creep_life;
			double m_creep_damage;

			double m_total_damage;

			S_creep_fatigue_outputs();
		};
		
		struct S_principal_stresses
		{
			double m_sigma_r;			//[MPa] stresses in radial direction
			double m_sigma_t;			//[MPa] stresses in tangential direction
			double m_sigma_a;			//[MPa] stresses in axial direction

			S_principal_stresses();
		};

		struct S_thermal_stress_rad_profile_outputs
		{
			// Outputs for 'tube_thermal_stress_rad_profile'
			S_principal_stresses s_thermal_stresses;
			S_principal_stresses s_pressure_stresses;
			S_principal_stresses s_total_stresses;
		};

	public:
		struct S_ID_OD_perf_and_lifetime_inputs
		{
			double m_P_internal;	//[MPa] Internal pressure
			double m_T_fluid;		//[C] Fluid temperature

			double m_d_out;		//[m] Tube O.D.
			double m_d_in;		//[m] Tube I.D.

			double m_flux;		//[W/m2] Flux on surface (assumed uniform over circumference)

			double m_h_conv;	//[W/m2-K] Heat transfer coefficient for sCO2 flow through tube

			S_ID_OD_perf_and_lifetime_inputs();
		};

		struct S_ID_OD_perf_and_lifetime_outputs
		{
			double m_T_surf_in;		//[C] Tube inner surface temperature
			double m_T_surf_out;	//[C] Tube outer surface temperature

			S_thermal_stress_rad_profile_outputs s_ID_stress_outputs;
			S_thermal_stress_rad_profile_outputs s_OD_stress_outputs;

			S_creep_fatigue_outputs s_ID_lifetime_outputs;
			S_creep_fatigue_outputs s_OD_lifetime_outputs;

			S_ID_OD_perf_and_lifetime_outputs();
		};

		struct S_ID_OD_stress_and_lifetime_inputs
		{
			double m_P_internal;	//[MPa] Internal pressure
			double m_T_fluid;		//[C] Fluid temperature

			double m_d_out;			//[m] Tube O.D.
			double m_d_in;			//[m] Tube I.D.

			double m_T_surf_in;		//[C] Inner surface temp
			double m_T_surf_out;	//[C] Outer surface temp

			S_ID_OD_stress_and_lifetime_inputs();
		};

		struct S_ID_OD_stress_and_lifetime_outputs
		{
			S_thermal_stress_rad_profile_outputs s_ID_stress_outputs;
			S_thermal_stress_rad_profile_outputs s_OD_stress_outputs;

			S_creep_fatigue_outputs s_ID_lifetime_outputs;
			S_creep_fatigue_outputs s_OD_lifetime_outputs;
		};

		C_tube_slice(int enum_tube_mat);
		C_tube_slice(int enum_tube_mat, double F_avg, double SF_fatigue, double F_inelastic, double N_design_cycles, double t_hours_design);

		~C_tube_slice();

		// Resets safety factors, inelastic multiplier, and receiver design cycles and creep duration to hardcoded values
		void reset_SFs_and_design_targets();

		// Resets value if parameter is positive, otherwise sets to hardcoded/reset value
		void specify_SFs_and_design_targets(double F_avg, double SF_fatigue, double F_inelastic, double N_design_cycles, double t_hours_design);

		// Calculate tube energy balance, stresses, and lifetime. Return all useful metrics in a structure
		void calc_ID_OD_perf_and_lifetime(const S_ID_OD_perf_and_lifetime_inputs & s_inputs, S_ID_OD_perf_and_lifetime_outputs & s_outputs);

		void calc_ID_OD_stress_and_lifetime(const S_ID_OD_stress_and_lifetime_inputs & s_inputs, S_ID_OD_stress_and_lifetime_outputs & s_outputs);

	private:
		C_rec_des_props * p_tube_mat;		// Material Class

		S_ID_OD_perf_and_lifetime_inputs s_ID_OD_perf_and_lifetime_inputs;

		// Store safety factors and inelastic multipliers for creep-fatigue lifetime calcs here
		double m_F_avg;
		double m_SF_fatigue;
		double m_F_inelastic;

		// Receiver lifetime # of design cycles and design creep duration
		double m_N_design_cycles;
		double m_t_hours_design;

		// Calculated member data
		double m_T_surf_in;
		double m_T_surf_out;
		
		double m_nu_poisson;
		double m_E;					//[MPa]
		double m_alpha;
		double m_r_in;
		double m_r_out;

		// ****************
		// Methods
		// ****************

		// Baseline constructors
		void general_constructor(int enum_tube_mat);
		void clear_calc_member_data();

		// Solve 1D Energy Balance
		void radial_ss_E_bal();

		void thermal_stress_rad_profile(double d_local, S_thermal_stress_rad_profile_outputs & outputs);

		void avg_temps_and_props();

		void creep_fatigue_lifetime(double T_mat_C, const S_principal_stresses & inputs, S_creep_fatigue_outputs & outputs);
	};

	class C_calc_tube_min_th
	{
		/* Copy and paste following code to solve example of finding minimum inner diameter: */
		//int n_tube_nodes = 10;
		//double d_out = 0.012;			//[m]
		//double T_fluid_in = 470.0;		//[C]
		//double T_fluid_out = 650.0;		//[C]
		//double P_fluid_in = 25.0;		//[MPa]
		//double e_roughness = 4.5E-5;	//[m] Absolute tube roughness
		//double L_tube = 4.1;			//[m] Length of tube

		//N_sco2_rec::C_calc_tube_min_th      calc_min_th;

		//double q_abs_total_input = 300000.0;
		//vector<double> max_flux_in(n_tube_nodes);
		//for( int i = 0; i < n_tube_nodes; i++ )
		//	max_flux_in[i] = q_abs_total_input - 0.1*q_abs_total_input*(i);		//[W/m2]

		//bool is_tube_feasible = calc_min_th.calc_th_1Dmaxflux(
		//	max_flux_in, L_tube, d_out, T_fluid_in, T_fluid_out, P_fluid_in);

		//double d_in_min = calc_min_th.get_min_d_in();
		//double m_dot_class = calc_min_th.get_m_dot_tube_kgsec();
		// ***************************************************
		//    End of example code
		// ***************************************************

		public:
			C_calc_tube_min_th();

			double get_min_d_in();

			double get_m_dot_tube_kgsec();

			double get_T_out_C();

			double get_deltaP_kPa();

            double get_max_damage();

            vector<double> get_max_damage_matrix();

            vector<double> *get_fluid_temp_matrix();

            vector<double> *get_surface_temp_matrix();

            vector<double> *get_fluid_pres_matrix();

            util::matrix_t<double> *get_damage_matrix();

            void get_damage_matrix(vector<vector<double> > &damage);

			bool calc_th_flux_Tout(const vector<vector<double> > &flux_Wm2, double L_tube_m,
				                        double d_out_m, double T_fluid_in_C, double T_fluid_out_C, 
                                        double P_fluid_in_MPa);

			bool calc_th_flux_mdot(const vector<vector<double> > &flux_Wm2, double L_tube_m,
				                        double d_out_m, double T_fluid_in_C, double P_fluid_in_MPa, 
                                        double m_dot_tube_kgs);

            bool calc_perf_flux_mdot(const vector<vector<double> > &flux_Wm2, double L_tube_m,
				                          double d_out_m, double th_m, double T_fluid_in_C, double P_fluid_in_MPa, 
                                          double m_dot_tube_kgs);

		private:
			CO2_state co2_props;

			vector<vector<double> > m_flux_array;		//[W/m2] 2D axial array of max flux on tube. axial=m_flux_array.size(), circumferential=m_flux_array.element.size()
			vector<double> m_q_abs_array;			//[W] 1D axial array of absorbed flux on tube
            vector<double> m_q_max_array;           //[W/m2] 1D axial array of maximum flux on tube
            
			double m_d_out;				//[m]
			double m_T_fluid_in;		//[C]
			double m_T_fluid_out;		//[C]
			double m_P_fluid_in;		//[MPa]
			double m_e_roughness;		//[m] Absolute tube roughness
			double m_L_tube;			//[m] Length of tube
			double m_m_dot_tube;		//[kg/s]
			bool m_know_T_out;			// true = set T_out calc m_dot... false = set m_dot calc T_out

			int m_n_tube_elements;		//[-] Number of axial elements in flux arrays

			int m_n_temps;				//[-] Number of axial nodes in analysis. Nodes are located between elements, and beginning and end of tube

			// Calculated geometries/properties
			double m_d_in;
			double m_L_node;
			double m_deltaP_kPa;
            double m_max_damage;

            //iteration properties
            double m_th_min_guess;      //[m] Smallest possible thickness / initial thickness
            double m_th_step;           //[m] Step size to increase thickness each iteration
            double m_max_deltaP_frac;   //[-] Largest allowable fractional pressure drop
            int m_iter_d_in_max;        //maximum number of tube thickness iterations allowed

			vector<double> m_Temp, m_Pres, m_Enth, m_h_conv_ave, m_Tsurf;
			
			// Results
				// vectors (add to 'all' functions)
			vector<double> m_P_fluid_out;
			// vector inner diameter

				// matrix_t (add to 'all' functions)
			util::matrix_t<double> m_total_damage;
			// matrix_t Surface temps
			// matrix_t stresses

			util::matrix_t<double> m_element_results_temp;

			bool calc_th_flux(const vector<vector<double> > &flux_Wm2, double L_tube_m,
				double d_out_m, double T_fluid_in_C, double T_fluid_out_C, double P_fluid_in_MPa, double m_dot_tube, bool know_Tout);

			bool calc_min_thick_general();

			int m_n_results_cols;
			int m_n_vector_results;

			void initialize_all_output_columns();
			void initialize_output_column(util::matrix_t<double> & results_matrix);
			void initialize_vector(vector<double> & results_vector);
			
			void push_back_all_vectors();
			void push_back_vector(vector<double> & results_vector);
			
			void add_all_output_columns();
			void add_output_column(util::matrix_t<double> & results_matrix);
	};

	//class C_calc_min_th
	//{
	//public:
	//	C_calc_min_th();		

	//	~C_calc_min_th();

	//	void blah_blah_blah()
	//	{
	//		double balhdfa = 1.23;
	//	};

	//	bool calc_th_1Dmaxflux(const vector<double> max_flux_axial_1D_Wm2, double L_tube_m,
	//		double d_out_m, double T_fluid_in_C, double T_fluid_out_C, double P_fluid_in_MPa);

	//private:
	//	property_info co2_props;

	//	vector<double> m_max_flux_array;		//[W/m2] 1D axial array of max flux on tube
	//	vector<double> m_ave_flux_array;		//[W/m2] 1D axial array of ave flux on tube

	//	double m_d_out;				//[m]
	//	double m_T_fluid_in;		//[C]
	//	double m_T_fluid_out;		//[C]
	//	double m_P_fluid_in;		//[MPa]
	//	double m_e_roughness;		//[m] Absolute tube roughness
	//	double m_L_tube;			//[m] Length of tube

	//	int m_n_tube_elements;		//[-] Number of axial elements in flux arrays

	//	int m_n_temps;				//[-] Number of axial nodes in analysis. Nodes are located between elements, and beginning and end of tube

	//	// Calculated geometries/properties
	//	double m_d_in;
	//	double m_L_node;

	//	vector<double> m_Temp, m_Pres, m_Enth, m_h_conv_ave;

	//	util::matrix_t<double> m_total_damage;
	//	util::matrix_t<double> m_element_results_temp;

	//	bool calc_min_thick_general();

	//	int m_n_results_cols;
	//	void initialize_all_output_columns();
	//	void initialize_output_column(util::matrix_t<double> & results_matrix);
	//	void add_all_output_columns();
	//	void add_output_column(util::matrix_t<double> & results_matrix);

	//};
}

#endif
