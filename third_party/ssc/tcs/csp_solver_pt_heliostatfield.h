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

#ifndef __csp_solver_pt_heliostatfield_
#define __csp_solver_pt_heliostatfield_

#include "csp_solver_util.h"
#include "csp_solver_core.h"

#include "sort_method.h"
#include "interpolation_routines.h"
#include "AutoPilot_API.h" 
#include "IOUtil.h"


class C_pt_heliostatfield
{
private:
	// Class Instances
	GaussMarkov *field_efficiency_table;
	MatDoub m_flux_positions;
	//sp_flux_table fluxtab;
	
	double m_p_start;				//[kWe-hr] Heliostat startup energy
	double m_p_track;				//[kWe] Heliostat tracking power
	double m_hel_stow_deploy;		//[rad] converted from [deg] in ms_params
	double m_v_wind_max;			//[m/s]

	int m_n_flux_x;		//[-]
	int m_n_flux_y;		//[-]
	int m_N_hel;		//[-]

	//Stored Variables
	double m_eta_prev;			//[-]
	double m_v_wind_prev;		//[m/s]
	double m_v_wind_current;	//[m/s]

	// member string for exception messages
	std::string error_msg;

	double rdist(VectDoub *p1, VectDoub *p2, int dim = 2);

	// track number of calls per timestep, reset = -1 in converged() call
	int m_ncall;

public:
	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	C_pt_heliostatfield();

	~C_pt_heliostatfield();

	struct RUN_TYPE { enum A {AUTO, USER_FIELD, USER_DATA}; };

	// Callback funtion
	bool(*mf_callback)(simulation_info* siminfo, void *data);
	void *m_cdata;

	struct S_params
	{
        bool m_eta_map_aod_format;
		int m_run_type;
		double m_helio_width;				//[m]
		double m_helio_height;				//[m]
		double m_helio_optical_error;
		double m_helio_active_fraction;
		double m_dens_mirror;
		double m_helio_reflectance;
		double m_rec_absorptance;
		double m_rec_height;
		double m_rec_aspect;
		double m_rec_hl_perm2;
		double m_q_design;
		double m_h_tower;
		std::string m_weather_file;
		int m_land_bound_type;
		double m_land_max;
		double m_land_min;

		//double *m_land_bound_table;
		//int m_nrows_land_bound_table;
		//int m_ncols_land_bound_table;
		util::matrix_t<double> m_land_bound_table;

		//double *m_land_bound_list;
		//int m_nrows_land_bound_list;
		util::matrix_t<double> m_land_bound_list;

		double m_p_start;			//[kWe-hr] Heliostat startup energy
		double m_p_track;			//[kWe] Heliostat tracking power
		double m_hel_stow_deploy;	//[deg] convert to [rad] in init()
		double m_v_wind_max;		//[m/s] max wind speed
		double m_interp_nug;
		double m_interp_beta;

		int m_n_flux_x;
		int m_n_flux_y;

		//double *m_helio_positions;
		//int m_N_hel;
		//int m_pos_dim;
		util::matrix_t<double> m_helio_positions;

		//double *m_helio_aim_points;
		//int m_nrows_helio_aim_points;
		//int m_ncols_helio_aim_points;
		util::matrix_t<double> m_helio_aim_points;

		//double *m_eta_map;
		//int m_nrows_eta_map;
		//int m_ncols_eta_map;
		util::matrix_t<double> m_eta_map;

		//double *m_flux_positions;
		//int m_nfluxpos;
		//int m_nfposdim;
		util::matrix_t<double> m_flux_positions;

		//double *m_flux_maps;
		//int m_nfluxmap;
		//int m_nfluxcol;
		util::matrix_t<double> m_flux_maps;

        util::matrix_t<double> m_sf_adjust; // array of length equal to number of time steps

		double m_c_atm_0;
		double m_c_atm_1;
		double m_c_atm_2;
		double m_c_atm_3;
		
		int m_n_facet_x;
		int m_n_facet_y;
		int m_cant_type;
		int m_focus_type;
		int m_n_flux_days;
		int m_delta_flux_hrs;

		double m_dni_des;
		double m_land_area;

		double m_A_sf;		//[m2]

		S_params()
		{
			// Integers
			m_run_type = m_land_bound_type = /*m_nrows_land_bound_table = m_ncols_land_bound_table =*/ /* m_nrows_land_bound_list =*/ m_n_flux_x = m_n_flux_y = /*m_N_hel = m_pos_dim = */
				/*m_nrows_helio_aim_points = m_ncols_helio_aim_points =*/ /*m_nrows_eta_map = m_ncols_eta_map =*/ /*m_nfluxpos = m_nfposdim = */
				/*m_nfluxmap = m_nfluxcol =*/ m_n_facet_x = m_n_facet_y = m_cant_type = m_focus_type = m_n_flux_days = m_delta_flux_hrs = -1;

			// Doubles
			m_helio_width = m_helio_height = m_helio_optical_error = m_helio_active_fraction = m_dens_mirror = m_helio_reflectance = m_rec_absorptance = m_rec_height = m_rec_aspect =
				m_rec_hl_perm2 = m_q_design = m_h_tower = m_land_max = m_land_min = m_p_start = m_p_track = m_hel_stow_deploy = m_v_wind_max = m_interp_nug =
				m_interp_beta = m_c_atm_0 = m_c_atm_1 = m_c_atm_2 = m_c_atm_3 = m_dni_des = m_land_area = m_A_sf = std::numeric_limits<double>::quiet_NaN();

			// double *
			/*m_land_bound_table = m_land_bound_list = m_helio_positions = */ /*m_helio_aim_points =*/ /*m_eta_map =*/ /*m_flux_positions =*/ /*m_flux_maps =*/ /*NULL;*/

			// strings
			m_weather_file = "";
		}		
	};

	S_params ms_params;

	struct S_outputs
	{
		double m_q_dot_field_inc;	//[MWt] Field incident thermal power (from the sun!)

		util::matrix_t<double> m_flux_map_out;
		double m_pparasi;		//[MWe]
		double m_eta_field;		//[-]
        double m_sf_adjust_out;

		S_outputs()
		{
			m_q_dot_field_inc = m_pparasi = m_eta_field = m_sf_adjust_out =  std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_outputs ms_outputs;

	//void init(bool(*callback)(simulation_info* siminfo, void *data), void *cdata);
	void init();

	void call(const C_csp_weatherreader::S_outputs &weather, 
		double field_control_in, 
		const C_csp_solver_sim_info &sim_info);

	void off(const C_csp_solver_sim_info &sim_info);

	void converged();
};






#endif
