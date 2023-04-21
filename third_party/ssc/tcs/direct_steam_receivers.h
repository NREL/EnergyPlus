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

#ifndef __DSG_RECS_
#define __DSG_RECS_

//#include <shared/lib_util.h>
#include "lib_util.h"
#include "htf_props.h"
//#include "waterprop.h"
#include "water_properties.h"

// convert to class?
class C_DSG_macro_receiver
{
public:

	bool Initialize_Receiver(int n_panels, double d_rec, double per_rec, double hl_ffact, int flowtype, bool is_iscc, int n_panels_sh, double sh_h_frac);
	int Get_n_panels_rec() {return m_n_panels;};
	double Get_d_rec() {return m_d_rec;};
	double Get_per_rec() {return m_per_rec;};
	double Get_per_panel() {return m_per_panel;};
	double Get_hl_ffact() {return m_hl_ffact;};
	int Get_flowtype() {return m_flowtype;};
	bool is_iscc() {return m_is_iscc;};
	int Get_n_panels_sh() {return m_n_panels_sh;};
	double Get_sh_h_frac() {return m_sh_h_frac;};

private:
	int m_n_panels;		//[-] Number of panels
	double m_d_rec;		//[m] Diameter of receiver
	double m_per_rec;	//[m] Perimeter of receiver
	double m_per_panel; //[m] Perimeter of one panel
	double m_hl_ffact;	//[-] Heat Loss Fudge FACTor
	int m_flowtype;		//[-] Code for flow pattern
	int m_n_panels_sh;	//[-] Number of panels that contain superheat sections
	double m_sh_h_frac;	//[-] Fraction of panel composed of superheater seection
	bool m_is_iscc;		//[-] ISCC boiler-sh configuration
};


class C_DSG_Boiler
{

public:
	~C_DSG_Boiler() {};

	bool Initialize_Boiler( C_DSG_macro_receiver dsg_rec, double h_rec_full, double d_tube, double th_tube,
					   double eps_tube, double mat_tube, double h_sh_max, double th_fin,
					   double L_fin, double eps_fin, double mat_fin, bool is_iscc_sh );
	
	int Get_n_flowpaths() {return m_n_fr;};

	bool Solve_Boiler( double I_T_amb_K, double I_T_sky_K, double I_v_wind, double I_P_atm_Pa, double I_T_fw_K, double I_P_in_pb_kPa, 
					   double I_x_out_target, double I_m_dot_in, double I_m_dot_lower, double I_m_dot_upper, bool I_checkflux, 
					   util::matrix_t<double> & I_q_inc_b, int & O_boiler_exit, double & O_eta_b, double & O_T_boil_K, double & O_m_dot_vapor, 
					   double & O_h_fw_kJkg, double & O_P_b_out_kPa, double & O_hx1_kJkg, double & O_rho_fw, double & O_q_out_W, double & O_T_in_K );

	bool Solve_Superheater( double I_T_amb_K, double I_T_sky_K, double I_v_wind, double I_P_atm_Pa, double I_P_in_kPa, double I_m_dot_in, double I_h_in_kJkg,
									 double I_P_sh_out_min_Pa, bool I_checkflux, util::matrix_t<double> & I_q_inc_b, int & sh_exit, double I_T_target_out_K,
									 double & O_P_sh_out_kPa, double & O_eta_rec, double & O_rho_sh_out, double & O_h_sh_out_kJkg, double & O_q_out_W );

	void Get_Other_Boiler_Outputs( double & m_dot_in, double & T_max, double & q_out, double & q_in, double & q_conv, double & q_rad, double & q_abs );

	void Get_Other_Superheater_Outputs( double & q_conv_MW, double & q_rad_MW, double & q_abs_MW, double & T_surf_max_K, double & v_exit, double & q_in );

	C_DSG_Boiler()
	{
		flow_pattern = flow_pattern_adj = 0;
	}

private:
	C_DSG_macro_receiver m_dsg_rec;
	HTFProperties ambient_air;
	HTFProperties tube_material;
	water_state wp;

	util::matrix_t<double> m_h_rec;	//[m] Height of boiler - can differ per panel in iscc model
	util::matrix_t<double> m_L;		//[m] Length of flow path through one noe - can vary per panel in iscc model
	util::matrix_t<double> m_A_n_proj;		//[m2] Projected Area ** Node ** - can vary per panel in iscc model
	util::matrix_t<double> m_A_n_in_act;	//[m^2] ACTIVE inside surface area - nodal - can vary per panl in iscc model
	util::matrix_t<double> m_A_fin;	//[m^2] Area of 1/2 of fin - can vary per panel in iscc model

	//double m_h_rec;		//[m] Height of boiler
	//double m_L;			//[m] Length of flow path through one node
	//double m_A_n_proj;	//[m^2] Projected Area ** Node **
	//double m_A_n_in_act; //[m^2] ACTIVE inside surface area - nodal
	//double m_A_fin;		//[m^2] Area of 1/2 of fin

	int m_n_panels;		//[-] Number of panels active for receiver type (i.e. N_boiler, N_sh, etc)
	
	double m_d_tube;	//[m] O.D. of boiler tubes
	double m_th_tube;	//[m] Thickness of boiler tubes
	double m_eps_tube;	//[-] Emissivity of boiler tubes
	double m_mat_tube;	//[-] Code for tube material (2: stainless, 29: T-91)
	double m_th_fin;	//[m] Thickness of fin
	double m_L_fin;		//[m] Length of fin (distance between boiler tubes)
	double m_eps_fin;	//[-] Emissivity of fin material
	double m_mat_fin;	//[-] Code for fin material (2: stainless, 29: T-91)

	double m_abs_tube;	//[-] Absorptivity of boiler tubes
	double m_abs_fin;	//[-] Absorptivity of fin material

	int m_n_fr;			//[-] Number of flow paths: Hardcode to 2
	double m_m_mixed;	//[-] Exponential for calculating mixed convection
	int m_fin_nodes;	//[-] Number of nodes used to model fin
	double m_per_panel;	//[m] Perimeter of one panel
	int m_nodes;		//[m] Nodes per flow path
	double m_rel_rough;	//[-] Relative roughness of tubes
	int m_n_par;		//[-] Number of parallel assemblies per panel
	
	double m_d_in;		//[m] I.D. of boiler tube
	double m_A_t_cs;	//[m^2] Cross-sectional area of tubing
	
	
    
    double m_ksD;		//[-] The effective roughness of the cylinder [Siebers, Kraabel 1984]
	double m_L_eff_90;  //[m] Effective length for pressure drop for 90 degree bend
    double m_L_eff_45;	//[m] Effective length for pressure drop for 45 degree bend

	bool m_model_fin;	//[-] True: model fin, False: don't model fin
	double m_dx_fin;		//[m] !Distance between nodes in numerical fin model
	double m_L_fin_eff;	//[m] Half the distance between tubes = 1/2 fin length. Assuming symmetric, so it is all that needs to be modeled
	double m_q_fin;		//[W] Heat transfer contribution from fins separating boiler

	util::matrix_t<int> flow_pattern;	//[-] matrix defining order of panels that flow travels through for each flow path
	util::matrix_t<int> flow_pattern_adj;	//[-] Sorted number of independent panels in each flow path - applied when m_n_comb > 1 and panels should be modeled together

	util::matrix_t<double> m_q_inc;		//[W/m^2] N_panels x 1 matrix for flux on panel
	util::matrix_t<double> m_q_adj;		//[W/m^2] Flux on parallel flow configuration	
	util::matrix_t<double> m_q_conv;	//[W] Convective losses on parallel flow configuration
	util::matrix_t<double> m_q_rad;		//[W] Radiative losses on parallel flow configuration
	util::matrix_t<double> m_q_abs;		//[W] Absorbed power on parallel flow configuration
	
	util::matrix_t<double> m_x_path_out;	//[-] Outlet quality of each flow path
	util::matrix_t<double> m_h_path_out;	//[J/kg] Outlet enthalpy of each flow path
	util::matrix_t<double> m_P_path_out;	//[Pa] Outlet pressure of each flow path

	util::matrix_t<double> m_m_dot_path;	//[kg/s] Mass flow rate for each flow path in receiver

	util::matrix_t<double> m_q_wf_total;	//[W] Guess total absorbed thermal power by HTF in each flow path

	double m_h_sh_max;		//[J/kg] Corresponds to maximum possible temperature of lookup tables so steam code doesn't bug out

	// Boiler outputs not in "solve" call
	double mO_m_dot_in;		//[kg/s] Mass flow rate through boiler
	double mO_b_T1_max;		//[K] Maximum calculated boiler tube outer surface temperature
	double mO_b_q_out;		//[W] Thermal power to steam
	double mO_b_q_in;		//[W] Flux * receiverArea
	double mO_b_q_conv;		//[MW] Total convective loss from boiler
	double mO_b_q_rad;		//[MW] Total radiatiave loss from boiler
	double mO_b_q_abs;		//[MW] Total thermal power absorbed by boiler (before thermal losses)	

	// Superheater outputs not in "solve" call
	double mO_sh_q_conv;	//[MW] Convective losses
	double mO_sh_q_rad;		//[MW] Radiative losses
	double mO_sh_q_abs;		//[MW] Thermal power absorbed by receiver (before thermal losses)
	double mO_sh_T_surf_max;//[K] Maximum superheater surface temp
	double mO_sh_v_exit;	//[m/s] Superheater exit velocity
	double mO_sh_q_in;		//[W[ Flux * receiverArea

};

#endif