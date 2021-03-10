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

#include <algorithm>
#include "direct_steam_receivers.h"

#include "sam_csp_util.h"
//#include <shared/lib_util.h>
#include "lib_util.h"
//#include "waterprop.h"
#include "water_properties.h"

// void flow_patterns_DSR( int n_panels, int flow_type, util::matrix_t<int> & flow_pattern );
// double Nusselt_FC( double ksDin, double Re );
double h_mixed( HTFProperties &air, double T_node_K, double T_amb_K, double v_wind, double ksD, double hl_ffact, double P_atm_Pa, 
			    double grav, double beta, double h_rec, double d_rec, double m );
double Flow_Boiling( double T_sat, double T_surf, double G, double d, double x_in, double q_t_flux, double rho_l, double rho_v, double k_l,
					 double mu_l, double Pr_l, double enth_l, double h_diff, double grav, double mu_v, double c_v, double k_v, double RelRough );

bool C_DSG_macro_receiver::Initialize_Receiver( int n_panels, double d_rec, double per_rec, double hl_ffact, int flowtype, bool is_iscc, int n_panels_sh, double sh_h_frac )
{  
	m_n_panels = n_panels;
	m_is_iscc = is_iscc;

	// The number of receiver panels should not be less than 12
	if( !is_iscc && m_n_panels < 12 ) {return false;}
		// Pass message about error?
		// ISCC can interpolate flux for less than 12 panels - can think about adding this for CSP-only DSG

	m_d_rec = d_rec;
	m_per_rec = per_rec;
	m_hl_ffact = hl_ffact;
	m_flowtype = flowtype;

	m_per_panel = m_per_rec / (double)m_n_panels;

	if(m_is_iscc)
	{
		m_sh_h_frac = sh_h_frac;
		m_n_panels_sh = n_panels_sh;
	}
	else
	{
		m_sh_h_frac = 0.0;
		m_n_panels_sh = 0;
	}
	
	return true;
}

bool C_DSG_Boiler::Initialize_Boiler( C_DSG_macro_receiver dsg_rec, double h_rec_full, double d_tube, double th_tube,
					   double eps_tube, double mat_tube, double h_sh_max, double th_fin,
					   double L_fin, double eps_fin, double mat_fin, bool is_iscc_sh )
{ 
	m_dsg_rec = dsg_rec;

	m_d_tube = d_tube;
	m_th_tube = th_tube;
	m_eps_tube = eps_tube;
	m_mat_tube = mat_tube;
	tube_material.SetFluid((int)m_mat_tube);
	m_th_fin = th_fin;
	m_L_fin = L_fin;
	m_eps_fin = eps_fin;
	m_mat_fin = mat_fin;
	m_h_sh_max = h_sh_max;

	if( m_dsg_rec.is_iscc() )
	{
		if(is_iscc_sh)	// ISCC: Superheater
		{
			m_n_panels = m_dsg_rec.Get_n_panels_sh();		
		}
		else			// ISCC: Boiler
		{
			m_n_panels = m_dsg_rec.Get_n_panels_rec();
		}	
	}	// DSG
	else
	{
		m_n_panels = m_dsg_rec.Get_n_panels_rec();
	}
	
	int n_lines = 0;
				
	if( m_dsg_rec.is_iscc() )
	{
		if( is_iscc_sh )	// ISCC superheater
		{
			util::matrix_t<int> flow_pattern_temp;
			// Get full-receiver flow pattern
			CSP::flow_patterns(m_dsg_rec.Get_n_panels_rec(), 0, m_dsg_rec.Get_flowtype(), n_lines, flow_pattern_temp);
			m_n_fr = n_lines;
			m_nodes = m_n_panels / m_n_fr;			
			flow_pattern.resize_fill(m_n_fr, m_nodes, -1);
			for( int i = 0; i < m_n_fr; i++ )
			{
				for( int j = 0; j < m_nodes; j++ )
				{
				  	flow_pattern.at(i, j) = flow_pattern_temp.at(i, (m_dsg_rec.Get_n_panels_rec() - m_n_panels)/2 + j);
				}
			}
			m_h_rec.resize_fill(m_n_panels, h_rec_full*m_dsg_rec.Get_sh_h_frac());
		}
		else				// ISCC Boiler
		{
			CSP::flow_patterns(m_n_panels, 0, m_dsg_rec.Get_flowtype(), n_lines, flow_pattern);
			m_n_fr = n_lines;
			m_nodes = m_n_panels / m_n_fr;
			int m_nodes_sh = m_dsg_rec.Get_n_panels_sh() / m_n_fr;
			m_h_rec.resize(m_n_panels);
			for( int j = 0; j < m_n_fr; j++ )
			{
				for( int i = 0; i < m_nodes; i++ )
				{
					if(i >= m_nodes - m_nodes_sh)
						m_h_rec[i + m_nodes*j] = h_rec_full*(1.0 - m_dsg_rec.Get_sh_h_frac());						
					else
						m_h_rec[i + m_nodes*j] = h_rec_full;
				}
			}
		}
	}
	else
	{
		// Get flow pattern
		CSP::flow_patterns(m_n_panels, 0, m_dsg_rec.Get_flowtype(), n_lines, flow_pattern);
		m_n_fr = n_lines;
		m_nodes = m_n_panels / m_n_fr;
		m_h_rec.resize_fill(m_n_panels, h_rec_full);	//[m] Height of receiver section - can vary per panel in iscc model
	}

	/* Sorted number of independent panels in each flow path - applied when m_n_comb > 1 and panels should be modeled together
	/ Example: For 12 panel receiver with 2 parallel flow panels:*/
	flow_pattern_adj.resize( m_n_fr, m_nodes );
	for( int j = 0; j < m_n_fr; j++ )
		for( int i = 0; i < m_nodes; i++ )
			flow_pattern_adj.at( j, i ) = i + (m_nodes)*(j);   

	m_d_in = m_d_tube - 2.0*m_th_tube;		//[m] Inner diameter of tube
		
	
	m_L = m_h_rec;							//[m] Distance through one node

	m_A_n_proj.resize(m_n_panels);
	m_A_n_in_act.resize(m_n_panels);
	m_A_fin.resize(m_n_panels);
	for( int i = 0; i < m_n_panels; i++ )
	{
		m_A_n_proj[i] = m_d_tube * m_L[i];	//[m^2] Projected Area ** Node ** - can vary per panel in iscc model
		m_A_n_in_act[i] = CSP::pi*m_d_in*0.5*m_L[i];	//[m^2] ACTIVE inside surface area - nodal - can vary per panl in iscc model
		m_A_fin[i] = m_L_fin*0.5*m_L[i];	//[m^2] Area of 1/2 of fin - can vary per panel in iscc model
	}

	//m_A_n_proj = m_d_tube * m_L.at(0);			//[m^2] Projected Area ** Node **
	//m_A_n_in_act = CSP::pi*m_d_in*0.5*m_L.at(0); //[m^2] ACTIVE inside surface area - nodal
	//m_A_fin = m_L_fin*0.5*m_L.at(0);				//[m^2] Area of 1/2 of fin

	// DELSOL flux map has already included absorptance, so set to 1 here
	m_abs_tube = m_abs_fin = 1.0;

    m_m_mixed = 3.2;	//[-] Exponential for calculating mixed convection
    m_fin_nodes = 10;	//[-] Model fin with 10 nodes	

	m_per_panel = m_dsg_rec.Get_per_panel();
	m_nodes = m_n_panels / m_n_fr;

    double w_assem = m_d_tube + m_L_fin;	//[m] Total width of one tube/fin assembly
    m_n_par = (int) (m_per_panel/w_assem);	//[-] Number of parallel assemblies per panel
        
	m_A_t_cs = CSP::pi*pow(m_d_in,2)/4.0;	//[m^2] Cross-sectional area of tubing        
   
	m_ksD = (m_d_tube/2.0)/m_dsg_rec.Get_d_rec();			//[-] The effective roughness of the cylinder [Siebers, Kraabel 1984]
	m_rel_rough = (4.5E-5)/m_d_in;			//[-] Relative roughness of the tubes: http.www.efunda/formulae/fluids/roughness.cfm

    m_L_eff_90 = 30.0;						//[m] Effective length for pressure drop for 90 degree bend
    m_L_eff_45 = 16.0;						//[m] Effective length for pressure drop for 45 degree bend

	if( m_L_fin < 0.0001 )
	{
		m_model_fin = false;
		m_dx_fin = -1.234;
		m_q_fin = 0.0;
	}
	else
	{
		m_model_fin = true;
        m_dx_fin = (m_L_fin/2.0)/( (double) m_fin_nodes -1.0);	//[m] Distance between nodes in numerical fin model        
	}
	m_L_fin_eff = 0.5*m_L_fin;			//[m] Half the distance between tubes = 1/2 fin length. Assuming symmetric, so it is all that needs to be modeled

	 
	m_q_inc.resize(m_n_panels);
	m_q_inc.fill(0.0);
	
	m_q_adj.resize( m_n_fr*m_nodes );
	m_q_adj.fill( 0.0 );
	m_q_conv.resize( m_n_fr*m_nodes );
	m_q_conv.fill( 0.0 );
	m_q_rad.resize( m_n_fr*m_nodes );
	m_q_rad.fill( 0.0 );
	m_q_abs.resize( m_n_fr*m_nodes );
	m_q_abs.fill( 0.0 );

	m_x_path_out.resize( m_n_fr );
	m_x_path_out.fill( 0.0 );
	m_h_path_out.resize( m_n_fr );
	m_h_path_out.fill( 0.0 );
	m_P_path_out.resize( m_n_fr );
	m_P_path_out.fill( 0.0 );

	m_m_dot_path.resize( m_n_fr );
	m_m_dot_path.fill( 0.0 );

	m_q_wf_total.resize( m_n_fr );
	m_q_wf_total.fill( 0.0 );

	ambient_air.SetFluid( ambient_air.Air );

	return true;
}

void C_DSG_Boiler::Get_Other_Boiler_Outputs( double & m_dot_in, double & T_max, double & q_out, double & q_in, double & q_conv, double & q_rad, double & q_abs )
{
	m_dot_in = mO_m_dot_in;
	T_max	= mO_b_T1_max;
	q_out	= mO_b_q_out;
	q_in	= mO_b_q_in;
	q_conv	= mO_b_q_conv;
	q_rad	= mO_b_q_rad;
	q_abs	= mO_b_q_abs;

	return;
}


bool C_DSG_Boiler::Solve_Boiler( double I_T_amb_K, double I_T_sky_K, double I_v_wind, double I_P_atm_Pa, double I_T_fw_K, double I_P_in_pb_kPa, 
								 double I_x_out_target, double I_m_dot_in, double I_m_dot_lower, double I_m_dot_upper, bool I_checkflux, 
								 util::matrix_t<double> & I_q_inc_b, int & O_boiler_exit, double & O_eta_b, double & O_T_boil_K, double & O_m_dot_vapor, 
								 double & O_h_fw_kJkg, double & O_P_b_out_kPa, double & O_hx1_kJkg, double & O_rho_fw, double & O_q_out_W, double & O_T_in )
{

	double T_amb = I_T_amb_K;				//[K] Ambient temperature
	double T_sky = I_T_sky_K;				//[K] Sky temperature
	double v_wind = I_v_wind;				//[m/s] Wind Speed
	double P_atm = I_P_atm_Pa;				//[Pa] Ambient Pressure
	double T_fw = I_T_fw_K;					//[K] Feedwater temperature
	double P_in_pb = I_P_in_pb_kPa;			//[kPa] Inlet pressure
	double x_out_target = I_x_out_target;	//[-] Outlet quality
	double m_dot_in = I_m_dot_in;			//[kg/s] Guessed mass flow rate
	double m_dot_lower = I_m_dot_lower;		//[kg/s] Lower limit on possible mass flow rates
	double m_dot_upper = I_m_dot_upper;		//[kg/s] Upper limit on possible mass flow rates
	bool checkflux = I_checkflux;			//[-] Is this call just to check if enough flux

	// Set outputs to values that will indicate model has not solved
	O_boiler_exit = 0;
	O_eta_b = -999.9;
	O_T_boil_K = -999.9;
	O_m_dot_vapor = -999.9;
	O_h_fw_kJkg = -999.9;
	O_P_b_out_kPa = -999.9;
	O_hx1_kJkg = -999.9;
	O_rho_fw = -999.9;
	O_q_out_W = -999.9;
	O_T_in = -999.9;
	mO_m_dot_in		= -999.9;
	mO_b_T1_max		= -999.9;
	mO_b_q_out		= -999.9;
	mO_b_q_in		= -999.9;
	mO_b_q_conv		= -999.9;
	mO_b_q_rad		= -999.9;
	mO_b_q_abs		= -999.9;
	// *************************************************************

	double energy_in = 0.0;
	for( int i = 0; i < m_n_panels; i++ )
	{
		m_q_inc.at(i) = I_q_inc_b.at(i);
		energy_in += m_per_panel*m_h_rec.at(i)*m_q_inc.at(i);
	}

	// Create new flux arrays that allow for multiple panels in parallel flow
	// Q_adj sorts flux into flow path order and is indexed with Flow_Pat_Adj
	for( int j = 0; j < m_n_fr; j++ )
	{
		for( int i = 0; i < m_nodes; i++ )
		{
			m_q_adj.at(flow_pattern_adj.at(j, i)) = m_q_inc.at(flow_pattern.at(j, i));
		}
	}
	
	double beta = 1.0 / T_amb;		//[1/K] Volumetric expansion coefficient

		// [W/m^2-K] Calculates combined free and forced convection coefficient, same as used in fin HT model
	double h_c = h_mixed( ambient_air, T_fw, T_amb, v_wind, m_ksD, m_dsg_rec.Get_hl_ffact(), P_atm, CSP::grav, beta, m_h_rec.at(0), m_dsg_rec.Get_d_rec(), m_m_mixed );
	// Check if enough flux is available to produce positive net energy through each flow path
	for( int j = 0; j < m_n_fr; j++ )
	{
		double q_wf_total = 0.0;
		for( int i = 0; i < m_nodes; i++ )
		{						
			m_q_conv.at(flow_pattern_adj.at(j, i)) = h_c*(m_A_n_proj[flow_pattern_adj.at(j,i)] + 2.0*m_A_fin[flow_pattern_adj.at(j,i)])*(T_fw - T_amb);	//[W] Convective heat transfer
			m_q_rad.at( flow_pattern_adj.at(j,i)) = m_eps_tube*CSP::sigma*(m_A_n_proj[flow_pattern_adj.at(j,i)]+2.0*m_A_fin[flow_pattern_adj.at(j,i)])*(0.5*(pow(T_fw,4) - pow(T_sky,4)) + 0.5*(pow(T_fw,4) - pow(T_amb,4)));	//[W] Radiative heat transfer: 8/10/11, view factor to ground ~ ambient (1/2) 
			m_q_abs.at( flow_pattern_adj.at(j,i)) = m_q_adj.at( flow_pattern_adj.at(j,i))*m_abs_tube*(m_A_n_proj[flow_pattern_adj.at(j,i)]+2.0*m_A_fin[flow_pattern_adj.at(j,i)]);	//[W] Irradiance absorbed by panel
			q_wf_total += m_q_abs.at( flow_pattern_adj.at(j,i)) - m_q_conv.at( flow_pattern_adj.at(j,i)) - m_q_rad.at( flow_pattern_adj.at(j,i));	//[W] Heat transfer to working fluid
		}
		if( q_wf_total < 0.0 )
		{
			O_boiler_exit = 2;		// Set flag for controller
			return false;
		}
	}

	if( checkflux )		return true;

	// Set bounds on possible mass flow rate guesses
	double m_dot_min = m_dot_lower;
	double m_dot_max = m_dot_upper;

	// ************ Guess Average (w/r/t flow paths) Outlet Pressure *******
	double P_out_guess = 0.99 * P_in_pb;	//[kPa]
	double diff_Pout = 999.0;
	int iter_P_out = 0;
	int iter_x = -9;
	double diff_x_out, h_n_out_total, h_in;
	diff_x_out = h_n_out_total = h_in = std::numeric_limits<double>::quiet_NaN();
	double x_n_out = 999.0;

	double P_out_avg=0; 
	double T_n_in, h_fw, rho_fw, T_in, T1_max;
	T_n_in = h_fw = rho_fw = T_in = T1_max = std::numeric_limits<double>::quiet_NaN();
	// Adjust steam drum pressure to equal calculated boiler outlet pressure
	while ( fabs(diff_Pout) > 0.01 && iter_P_out < 20 )
	{
		iter_P_out++;
		// Guess a new boiler outlet pressure based on previous results
		if( iter_P_out > 1 )
		{
			P_out_guess = 0.5*P_out_guess + 0.5*P_out_avg;
			// Can also reset mass flow rate guesses and limits
				// If new pressure is lower then, by convention, feedwater enthalpy will be lower, requiring more specific energy into the flow
				// to reach a specified quality, therefore, mass flow rate will decrease
			if( P_out_avg < P_out_guess )
			{
				m_dot_lower = 0.95*m_dot_in;
				m_dot_upper = m_dot_in;
				m_dot_min = m_dot_lower;
				m_dot_max = m_dot_upper;
			}
			else
			{
				m_dot_lower = m_dot_in;
				m_dot_upper = 1.05*m_dot_in;
				m_dot_min = m_dot_lower;
				m_dot_max = m_dot_upper;
			}
		}
		
		water_PQ( P_out_guess, 0.0, &wp );
		double h_x0 = wp.enth;		//[kJ/kg] Enthalpy of saturated liquid recirculating to steam drum
		water_TP( T_fw, P_out_guess, &wp );
		h_fw = wp.enth; rho_fw = wp.dens;	//[kJ/kg] Enthalpy and [kg/m^3] density of feedwater entering steam drum

		h_in = x_out_target*h_fw + (1.0 - x_out_target)*h_x0;	//[kJ/kg] Energy balance to find enthalpy of feedwater/recirc mixture

		//P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
		water_PH( P_in_pb, h_in, &wp );
		double rho_in = wp.dens;					//[kg/m^3] Find density of mixture
		double deltaP_in = rho_in*CSP::grav*m_L.at(flow_pattern_adj.at(0,0));	//[Pa] Hydrostatic pressure assuming water level is at top of tubes

		//8/20/11 Need to account for the gravity head so we don't observe large pressure drops while not exceeding Dyreby props pressure limits
		double P_in = (P_in_pb + deltaP_in/1000.0);	//[kPa] Inlet pressure adding gravity head from steam drum
		water_PH( P_in, h_in, &wp );				
		T_in = wp.temp;		//[K] Temperature at first panel inlet
		h_in = h_in*1000.0;					//[J/kg] convert from kJ/kg

		int iter_x = 0;
		diff_x_out = 999.0;
		int Pave_flag = 0;
		int mdot_flag = 0;
		int x_br_upper = 0;
		int x_br_lower = 0;
		bool mupflag = false;
		bool mlowflag = false;
		double y_m_upper, y_m_lower;
		y_m_upper = y_m_lower = std::numeric_limits<double>::quiet_NaN();

		// Adjust mass flow rate to reach target quality
		// 297 -> TRNSYS GOTO
		bool break_to_massflow_calc = false;
		while( fabs(diff_x_out) > 0.0035 && iter_x < 20 )
		{
			break_to_massflow_calc = false;
			iter_x++;					//[-] Increase iteration counter
			T1_max = 0.0;		//[K]

			double diff_m_dot = (m_dot_upper - m_dot_lower)/m_dot_upper;

			if( iter_x > 1 )
			{
				if( Pave_flag==0 && mdot_flag == 0 )
				{
					if( mupflag && mlowflag )
					{
						if( diff_x_out > 0.0 )
						{
							x_br_upper = 1;
							m_dot_upper = m_dot_in;
							y_m_upper = diff_x_out;
						}
						else
						{
							x_br_upper = 1;
							m_dot_lower = m_dot_in;
							y_m_lower = diff_x_out;
						}
						m_dot_in = y_m_upper/(y_m_upper-y_m_lower)*(m_dot_lower-m_dot_upper) + m_dot_upper;
					}
					else
					{
						if( diff_x_out > 0.0 )
						{
							x_br_upper = 1;
							m_dot_upper = m_dot_in;
							mupflag = true;
							y_m_upper = diff_x_out;
						}
						else
						{
							x_br_lower = 1;
							m_dot_lower = m_dot_in;
							mlowflag = true;
							y_m_lower = diff_x_out;
						}
						m_dot_in = 0.5*m_dot_upper + 0.5*m_dot_lower;
					}
				}
				else if( Pave_flag == 1 )
				{
					x_br_upper = 2;
					m_dot_upper = m_dot_in;
					m_dot_in = 0.5*m_dot_upper + 0.5*m_dot_lower;
				}
				else if( mdot_flag == 1)
				{
					x_br_lower = 2;
					m_dot_lower = m_dot_in;
					m_dot_in = 0.5*m_dot_upper + 0.5*m_dot_lower;
				}
			}

			if( diff_m_dot < 0.01 )
			{
				if( m_dot_upper < 1.01*m_dot_min && x_br_lower==0 )		x_br_lower = 3;
				if( m_dot_lower > 0.99*m_dot_max && x_br_upper==0 )		x_br_upper = 3;

				// Should not occuer with enough iterations
				// if( x_br_lower==1 && x_br_upper==1 )

				// Exit quality is too high / Pressure drop is too higher
				if( x_br_lower==1 && x_br_upper==2 )
				{
					O_boiler_exit = 3;	// too much flux
					return false;
				}
				
				// Exit quality too high // No upper limit on mass flow rate
				else if( x_br_lower==1 && x_br_upper==3 )
				{
					O_boiler_exit = 3;	// too much flux
					return false;
				}

				// Mass flow rate too low for energy balance / Exit quality too low
				else if( x_br_lower==2 && x_br_upper==1 )
				{
					O_boiler_exit = 2;	// not enough flux
					return false;
				}
				
				// Mass flow rate too low for energy balance / Pressure drop too high
				else if( x_br_lower==2 && x_br_upper==2 )
				{
					O_boiler_exit = 3;	// too much flux
					return false;
				}

				// Mass flow rate too low for energy balance / No upper limit on mass flow rate
				else if( x_br_lower==2 && x_br_upper==3 )
				{
					O_boiler_exit = 3;	// too much flux
					return false;
				}

				// No lower limit on mass flow rate / Exit quality too low
				else if( x_br_lower==3 && x_br_upper==1 )
				{
					O_boiler_exit = 2;	// not enough flux
					return false;
				}

				// No lower limit on mass flow rate / Pressure drop too high
				else if( x_br_lower==3 && x_br_upper==2 )
				{
					O_boiler_exit = 3;	// too much flux
					return false;
				}
			}
			// Mass flow rate in one tube - assuming one flow path (for m_dot_total)
			double m_dot_total = m_dot_in /( (double) m_n_par );	//[kg/s]
			m_m_dot_path.fill( m_dot_total / (double) m_n_fr );		//[kg/s] mass flow rate in each flow path

			double h_n_in, rho_n_in, P_n_in, dp, diff_T_ht, m_dot;
			double h_n_out, P_out, rho_n_ave, u_n;
			// Solve for outlet conditions of each flow path
			for( int j = 0; j < m_n_fr; j++ )
			{
				h_n_in = h_in;				//[J/kg] Inlet enthlapy of first node
				T_n_in = T_in;				//[K] Inlet temperature of first node
				rho_n_in = rho_in;			//[kg/m^3] Inlet density of first node
				P_n_in = P_in*1000.0;		//[Pa] Inlet pressure of first node
				dp = 2.4E4;					//[Pa] Guess small pressure drop
				diff_T_ht = 45.0;			//[K] Estimate of difference between surface temp and inlet temp
				m_dot = m_m_dot_path.at(j);	//[kg/s] Use m_dot so we don't have to carry array through

				double T_1, grav_mult;
				T_1 = grav_mult = std::numeric_limits<double>::quiet_NaN();
				for( int i = 0; i < m_nodes; i++ )		// Now solve energy balance for each node (or panel) in flow path. Output of i is input to i+1
				{
					if(i==0)
						T_1 = T_n_in + 45.0;
					else
						T_1 = T_n_in + 1.5*diff_T_ht;

					if( i%2 == 1 )
						grav_mult = -1.0;		// For odd numbered panels, flow is downwards
					else
						grav_mult = 1.0;		// For even numbered panels, flow is upwards

					double diff_P_ave = 9999999.0;			//[Pa] Set diff > tolerance
					int iter_P_ave = 0;						//[-] Set iteration counter
					P_out = max(1.E6, P_n_in - dp);			//[Pa] Guess outlet pressure of flow path
					double P_ave = 0.5*P_n_in + 0.5*P_out;	//[Pa] Guess average pressure through flow path
					bool P_upguess = false;					//[-] Has upper guess on pressure been set?
					bool P_lowguess = false;				//[-] Has lower guess on pressure been set?
					double P_upper = P_n_in;				//[Pa] Initial upper pressure is equal to inlet pressure
					double P_lower = 500000.0;				//[Pa] Some minimum outlet pressure
					double diff_P_bracket = 999.0;			//[Pa] Difference between upper and lower guesses
					bool lowpres_flag = false;				//[-] Did model trip due to low pressure
					int P_br_low = 0;						//[-] Flag to show how energy balanced solved
					int Pave_flag = 0;						//[-]
					int mdot_flag = 0;						//[-]
					double P_b_min = 1.E6;					//[Pa] Minimum boiler outlet pressure
					double rho_n_out,T_in1;
					rho_n_out = T_in1 = std::numeric_limits<double>::quiet_NaN();

					// An average pressure of the node is used as one independent variable to calculate additional properties
					// After pressure drop through node is calculated, want to make sure calculated average pressure is within some % of average used for props
					while( fabs(diff_P_ave) > 0.001 && iter_P_ave < 125 )
					{
						// 348 -> GOTO NUMBER from TRNSYS
						iter_P_ave++;		//[-] Increase iteration counter

						if(iter_P_ave > 1)
						{
							if( P_upguess && P_lowguess )
							{
								if(diff_P_ave < 0.0)	
								{
									P_br_low = 1;
									P_upper = P_ave;
								}
								else
									P_upper = P_ave;
								P_ave = 0.5*P_lower + 0.5*P_upper;
							}
							else
							{
								if(diff_P_ave < 0.0)
								{
									P_br_low = 1;
									P_lower = P_ave;
									P_lowguess = true;
								}
								else
								{
									P_upper = P_ave;
									P_upguess = true;
								}
								P_ave = (P_n_in + P_out)/2.0;
							}							
							P_ave = max(1.E6, P_ave);
							P_out = 2.0*(P_ave - 0.5*P_n_in);
							diff_P_bracket = (P_upper - P_lower)/P_lower;
						}
						// If a pressure near the minimum pressure has caused the code to guess a lower pressure, then flag and guess lower mass flow
						if( P_upper < 1.01*P_b_min )
						{
							Pave_flag = 1;
							break_to_massflow_calc = true;
							break;
							// GOTO 297
						}
						lowpres_flag = false;		//[-] Reset flag marking an outlet pressure below allowed minimum

						double diff_T_1 = 999.0;	//[K] Set diff > tolerance
						int iter_T_1 = 0;			//[-] Initialize iteration counter
						double T_1_max = 800.0;		//[K] Set some maximum allowable surface temperature to control iteration
						double G = m_dot/m_A_t_cs;  //[kg/m^2-s] Quantity in boiling correlation

						// Set limits on surface temperature iteration
						double T_1_upper = T_1_max;		//[K]
						double T_1_lower = T_n_in-15.0; //[K]
						double T_1_min = T_1_lower;		//[K]
						T_1 = min( max(T_1_lower, T_1), T_1_upper );	//[K]
						double diff_T1_g = 999.9;		//[K] Reset difference between upper and lower iteration limits

						// Reset iteration logic and integer flags
						bool Tupflag = false;			//[-] Has a temperature difference at the upper bound been established 
						bool Tlowflag = false;			//[-] Has a temperature difference at the lower bound been established
						bool HT_flag = false;			//[-] Did the energy balance encounter low heat transfer rates
						bool enth_flag = false;			//[-] Did the energy balance encounter high heat transfer rates
						int T1_br_lower = 0;			//[-] Flag to show how energy balance solved
						int T1_br_upper = 0;			//[-] Flag to show how energy balance solved
						// Reset temperature calculation errors at high and low guesses
						double y_T_upper = 0.0;			//[K] Temperature difference at upper bound
						double y_T_lower = 0.0;			//[K] Temperature difference at lower bound

						// Need properties at saturated vapor in case some guess in energy balance results in x<1
						water_PQ( min(P_ave/1000.0,19.E3),1.0, &wp );
						double h_b_max = wp.enth;		//[kJ/kg]
						double T_2_guess, T_2;		//[K]
						double q_wf,x_n_ave,mu_l,mu_v,rho_l,f_fd;				//[W]
						q_wf = x_n_ave = mu_l = mu_v = rho_l = f_fd = std::numeric_limits<double>::quiet_NaN();

						// This loop ensures that the outlet flow conditions for each panel are solved correctly by finding the correct T1 (outer surface temp)
						while( fabs(diff_T_1/T_1) > 0.0025 && iter_T_1 < 50 )
						{
							iter_T_1++;			//[-] Add to iteration counter

							if(iter_T_1 > 1)
							{
								if( !HT_flag && !enth_flag )
								{
									if( fabs(diff_T_1)>50.0 )	// If error is very large, use bisection rather than false interpolation method
									{
										if( diff_T_1 > 0.0 )	// If old temp is higher than new temp, then old temp is too high, so:
										{
											T1_br_upper = 2;
											T_1_upper = T_1;		//[K] Set upper limit
										}	
										else
										{
											T1_br_lower = 2;
											T_1_lower = T_1;		//[K] Set lower limit
											Tlowflag = false;
										}
										T_1 = 0.5*T_1_lower + 0.5*T_1_upper;	//[K] Bisection method to calculate next T_1 guess
									}
									else if( Tupflag && Tlowflag )	// If bracket results are set, use false position
									{
										if( diff_T_1 > 0.0 )
										{
											T1_br_upper = 2;
											T_1_upper = T_1;		//[K] Set upper limit
											y_T_upper = diff_T_1;	//[K] Set upper bracket result
										}
										else
										{
											T1_br_lower = 2;
											T_1_lower = T_1;		//[K] Set lower limit
											y_T_lower = diff_T_1;	//[K] Set lower bracket result
										}
										T_1 = y_T_upper/(y_T_upper-y_T_lower)*(T_1_lower-T_1_upper) + T_1_upper;	//[K] False position method
									}
									else
									{
										if(diff_T_1 > 0.0)
										{
											T1_br_upper = 2;
											T_1_upper = T_1;		//[K] Set upper limit
											y_T_upper = diff_T_1;	//[K] Set upper bracket result
											Tupflag = true;			//[-] Set logic to show that upper bracket is set
										}
										else
										{
											T1_br_lower = 2;
											T_1_lower = T_1;		//[K] Set lower limit
											y_T_lower = diff_T_1;	//[K] Set upper bracket result
											Tlowflag = true;		//[-] Set logic to show that lower bracket is set
										}
										if(Tupflag && Tlowflag)
											T_1 = y_T_upper/(y_T_upper-y_T_lower)*(T_1_lower-T_1_upper) + T_1_upper;	//[K] False position method
										else
										{
											T_1 = T_1 - diff_T_1;
											if( T_1 < T_1_lower || T_1 > T_1_max )
												T_1 = 0.5*T_1_lower + 0.5*T_1_upper;
											else
												T_1 = max( T_1_lower, min(T_1_max, T_1) );
										}
									}
								}
								else if( enth_flag )
								{
									T1_br_lower = 1;
									T_1_lower = T_1;
									T_1 = 0.5*T_1_lower + 0.5*T_1_upper;		//[K] Bisection method
									enth_flag = false;
								}
								else if( HT_flag )
								{
									T1_br_upper = 1;
									T_1_upper = T_1;
									T_1 = 0.5*T_1_lower + 0.5*T_1_upper;
									HT_flag = false;
								}
							}

							diff_T1_g = (T_1_upper - T_1_lower);	//[K] If iteration window is small, look for way out

							//If the bracket from which to guess to T_1 is small, then try to exit loop
							if( diff_T1_g < 0.01 )
							{
								if( T_1_lower > T_1_max - 0.02 && T1_br_upper==0 )	T1_br_upper = 3;
								if( T_1_upper < 0.02 + T_1_min && T1_br_lower==0 )	T1_br_lower = 3;

								/* T1_br_lower==1: Enth_flag, the resulting enthalpy from the heat addition is too large for steam property look-up routine
								!T1_br_lower==2: Calculated surface temperature is higher than guessed
								!T1_br_lower==3: Lower limit has not been set during iteration and is equal to inlet temperature
								  
								!T1_br_upper==1: HT Flag, negative (or nearly negative) energy input to boiler
								!T1_br_upper==2: Calculated surface temperature is lower than guessed
								!T1_br_upper==3: Upper limit has not been set during iteration and is equal to maximum value
								
								!*********** New 8/26/11 ************************************
								! Basically means any energy gain to fluid will result in fluid reaching an enthalpy too high for fluid props.  This should
								! not happen since code is stopping if outlet on previous panel is greater than target temp
								!IF( ((T1_br_lower==1).and.(T1_br_upper==1)) )THEN */
 
								// Need to increase mass flow rate in hopes of eliminating enth_flag  
								if( T1_br_lower==1 && T1_br_upper==2 )
								{
									mdot_flag = 1;
									break_to_massflow_calc = true;
									break;
									// GOTO 297
								}

								// Need to increase mass flow rate in hopes of eliminating enth_flag
								if( T1_br_lower==1 && T1_br_upper==3 )
								{
									mdot_flag = 1;
									break_to_massflow_calc = true;
									break;
									// GOTO 297
								}

								// Increase mass flow rate to bring down average fluid temp
								else if( T1_br_lower==2 && T1_br_upper==1 )
								{
									mdot_flag = 1;
									break_to_massflow_calc = true;
									break;
									// GOTO 297
								}
								                                                                            
								// This should not happen given enough iterations
								// ELSEIF( ((T1_br_lower==2).and.(T1_br_upper==2)) )THEN
								  
								// Increase mass flow rate
								else if( T1_br_lower==2 && T1_br_upper==3 )
								{
									mdot_flag = 1;
									break_to_massflow_calc = true;
									break;
									// GOTO 297
								}

								// Increase mass flow rate
								else if( T1_br_lower==3 && T1_br_upper==1 )
								{
									mdot_flag = 1;
									break_to_massflow_calc = true;
									break;
									// GOTO 297
								}
								  
								// Increase mass flow rate
								else if( T1_br_lower==3 && T1_br_upper==2 )
								{
									mdot_flag = 1;
									break_to_massflow_calc = true;
									break;
									// GOTO 297
								}								 
								  
								//Should not happen
								//ELSEIF( ((T1_br_lower==3).and.(T1_br_upper==3)) )THEN                        
								//*********************************************************************************
							}

							// Calculate energy inputs and outputs on surface (with guessed T_1)
							if(m_model_fin)
							{
								// Add fin model 
							}
							double h_c = h_mixed(ambient_air, T_1, T_amb, v_wind, m_ksD, m_dsg_rec.Get_hl_ffact(), P_atm, CSP::grav, beta, m_h_rec.at(flow_pattern_adj.at(j, i)), m_dsg_rec.Get_d_rec(), m_m_mixed);	//[W/m^2-K] Function calculates combined free and forced convection coefficient, same as used in fin HT model
							m_q_conv.at( flow_pattern_adj.at(j,i)) = h_c*m_A_n_proj[flow_pattern_adj.at(j,i)]*(T_1 - T_amb);	//[W] Convective heat transfer
							m_q_rad.at( flow_pattern_adj.at(j,i)) = m_eps_tube*CSP::sigma*m_A_n_proj[flow_pattern_adj.at(j,i)]*(0.5*(pow(T_1,4)-pow(T_sky,4))+0.5*(pow(T_1,4)-pow(T_amb,4)));	//[W] Radiative heat transfer: 8/10/11, view factor to ground and ambient each 0.5
							m_q_abs.at(flow_pattern_adj.at(j, i)) = m_q_adj.at(flow_pattern_adj.at(j, i))*m_abs_tube*m_A_n_proj[flow_pattern_adj.at(j,i)] + 2.0*m_q_fin*m_L.at(flow_pattern_adj.at(j, i));			//[W] Absorbed radiation + fin contributions
							q_wf = m_q_abs.at(flow_pattern_adj.at(j,i)) - m_q_conv.at(flow_pattern_adj.at(j,i)) - m_q_rad.at(flow_pattern_adj.at(j,i));	//[W] Thermal power transferred to working fluid

							// Equation: m_dot*(h_out - h_in) = q_wf
							h_n_out = q_wf/m_dot + h_n_in;		//[J/kg] Calculate outlet enthalpy according to calculated energy addition to working fluid
							double h_n_ave = (h_n_in + h_n_out)/2.0;	//[J/kg] Determine average enthalpy in node

							// If the outlet enthalpy is greater than property routine can handle, flag and reguess T_1
							if( h_n_out > 1.5*h_b_max*1000.0 )
							{
								enth_flag = true;
								continue;
							}

							double mu_n_ave, k_n_ave, c_n_ave;
							mu_n_ave = k_n_ave = c_n_ave = std::numeric_limits<double>::quiet_NaN();
							bool props_succeed = true;
							do
							{
								props_succeed = true;
								water_PH( min(P_ave/1000.0,19.E3),h_n_ave/1000.0, &wp );
								rho_n_ave = wp.dens; x_n_ave = wp.qual; 
								mu_n_ave = water_visc(wp.dens, wp.temp)*1.E-6;
								k_n_ave = water_cond(wp.dens, wp.temp);
								c_n_ave = wp.cp*1000.0; 
								T_in1 = wp.temp;

								// Check for Dyreby props failing near vapor dome
								if( c_n_ave < 0.0 || k_n_ave < 0.0 )
								{
									h_n_ave = 0.99 * h_n_ave;
									props_succeed = false;
								}
							} while( !props_succeed );

							double q_t_flux = q_wf/m_A_n_in_act[flow_pattern_adj.at(j,i)];		//[W/m^2] Heat FLUX transferred to working fluid: required for boiling heat transfer correlation
							u_n = m_dot/(rho_n_ave*m_A_t_cs);	//[m/s] Velocity through tube

							double h_l, k_l, c_l, h_v, rho_v, k_v, c_v, h_fluid;
							h_l = k_l = c_l = h_v = rho_v = k_v = c_v = h_fluid = std::numeric_limits<double>::quiet_NaN();
							if( x_n_ave < 1.0 && x_n_ave > 0.0 )
							{
								// Need props at saturated liquid for boiling correlations
								water_PQ( min(P_ave/1000.0,19.E3), 0.0, &wp );
								h_l = wp.enth*1000.0; rho_l = wp.dens; 
								mu_l = water_visc(wp.dens, wp.temp)*1.E-6;
								k_l = water_cond(wp.dens, wp.temp);
								c_l = wp.cp*1000.0;

								// Need props at saturated vapor for boiling correlations
								water_PQ( min(P_ave/1000.0,19.E3), 1.0, &wp );
								h_v = wp.enth*1000.0; rho_v = wp.dens; 
								mu_v = water_visc(wp.dens, wp.temp)*1.E-6;
								k_v = water_cond(wp.dens, wp.temp);
								c_v = wp.cp*1000.0;

								double h_diff = h_v - h_l;			//[J/kg] Heat of Vaporization
								double alpha_l = k_l/(rho_l*c_l);	//[m^2/s] Thermal diffusivity of saturated liquid
								double Pr_l = (mu_l/rho_l)/alpha_l; //[-] Prandtl number of saturated liquid

								if( q_t_flux >= 0.0 )
								{// Determine heat transfer coefficient for boiling flow
									h_fluid = Flow_Boiling( T_in1,T_in1,G,m_d_in,x_n_ave,q_t_flux,rho_l,rho_v,k_l,mu_l,Pr_l,h_l,h_diff,CSP::grav,mu_v,c_v,k_v,m_rel_rough );
								}
								else
								{
									int iter_T_2 = 0;
									double diff_T_2 = 999.9;
									while( iter_T_2 < 20 && fabs(diff_T_2) > 0.01 )
									{
										iter_T_2++;
										if( iter_T_1 == 1 )		T_2_guess = T_in1 - 1.0;	//[K] If first iteration on energy balance, then T_2 is not set
										else					T_2_guess = T_2;
										h_fluid = Flow_Boiling( T_in1,T_2_guess,G,m_d_in,x_n_ave,q_t_flux,rho_l,rho_v,k_l,mu_l,Pr_l,h_l,h_diff,CSP::grav,mu_v,c_v,k_v,m_rel_rough );
										h_fluid = max( k_n_ave/m_d_in, h_fluid );
										double R_conv = 1.0/(h_fluid * m_A_n_in_act[flow_pattern_adj.at(j,i)]);	//[K/W] Thermal resistance to convection
										T_2 = q_wf*R_conv + T_in1;
										diff_T_2 = (T_2_guess - T_2)/T_2;		//[-]
									}	// End iteration on T_2 and h_fluid for condensing flow
								}	// End if/then to find h_fluid for either condensing or boiling flow
							}	// End equations to find h_fluid for 2-phase flow
							else
							{
								double Re = rho_n_ave*u_n*m_d_in / mu_n_ave;	//[-] Reynolds number
								f_fd = pow( (-2.0*log10(2.0*m_rel_rough/7.54 - 5.02*log10(2.0*m_rel_rough/7.54 + 13.0/Re)/Re)), -2);	//[-] (Moody) friction factor (Zigrang and Sylvester)
								double alpha_n = k_n_ave/(rho_n_ave*c_n_ave);	//[m^2/s] Thermal diffusivity of fluid
								double Pr = mu_n_ave/(alpha_n*rho_n_ave);		//[-] Prandtl number
								double Nusselt = ((f_fd/8.0)*(Re-1000.0)*Pr)/(1.0+12.7*sqrt(f_fd/8.0)*(pow(Pr,2.0/3.0)-1.0));	//[-] Turbulent Nusselt number (Gnielinski)
								h_fluid = Nusselt*k_n_ave/m_d_in;				//[W/m^2-K] Convective heat transfer coefficient
							}	// End equations to find h_fluid for single phase flow

							h_fluid = max( k_n_ave/m_d_in, h_fluid );

							double R_conv = 1.0 / (h_fluid * m_A_n_in_act[flow_pattern_adj.at(j,i)]);		//[K/W] Thermal resistance to convection
							T_2 = q_wf*R_conv + T_in1;							//[K] Calculate T_2
							double k_n = tube_material.cond( (T_1+T_2)/2.0 );	//[W/m-K] Conductivity of tube using average temperature
							double R_n = log(m_d_tube / m_d_in) / (k_n*2.0*CSP::pi*m_L.at(flow_pattern_adj.at(j, i)) / 2.0);	//[K/W] Thermal resistance of ACTIVE tube
							diff_T_1 = T_1 - (T_2 + q_wf*R_n);					//[K] Calculate difference between assumed T_1 and calculated T_1
						}	// End tube energy balance iteration

						if( break_to_massflow_calc )
							break;

						if(x_n_ave < -1.0)	
						{
							x_n_out = -10.0;
							water_PH( min(P_out/1000.0,19.E3),h_n_out/1000.0,&wp );
							rho_n_out = wp.dens;
						}
						else if(x_n_ave > 1.0)	
						{
							x_n_out = 10.0;
							water_PH( min(P_out/1000.0,19.E3),h_n_out/1000.0,&wp );
							rho_n_out = wp.dens;
						}
						else
						{
							water_PH( min(P_out/1000.0,19.E3),h_n_out/1000.0,&wp );
							rho_n_out = wp.dens; x_n_out = wp.qual;
						}

						double deltaP_tube = std::numeric_limits<double>::quiet_NaN();;												
						// Calculate the pressure drop through the panel
						if( x_n_ave < 1.0 && x_n_ave > 0.0 )	// 2-Phase pressure drop
						{

							// Frictional pressure drop equations taken from Chexal et al. pages 7-6, 7-7, 7-9
							//double mu_f = (1.0 - x_n_ave)*mu_l + x_n_ave*mu_v;	//[kg/m-s] Mixture viscosity
							double Re_LO = G*m_d_in / mu_l;						//[-] Liquid only Reynolds number
							//double Re_TP = G*m_d_in / mu_f;						//[-] Mixture Reynolds number

							double f_wLO = 0.0791/pow(Re_LO,0.25);				//[-] Liquid only friction factor: Blasius single-phase Fanning friction factor
							double f_wTP = f_wLO;								//[-] 2-phase friction factor defined as liquid

							double phi_LOsq = f_wTP*rho_l/(f_wLO*rho_n_ave);	//[-] 2-phase friction multiplier
							double dpdz_fLO = 4.0/m_d_in*0.5*f_wLO*G*G/rho_l;	//[-] Liquid-only pressure drop
							double dpdx_fTP = dpdz_fLO*phi_LOsq;				//[-] 2-phase friction factor

							double dpdx_grav = grav_mult*rho_n_ave*CSP::grav;	//[Pa/m] Pressure due to elevation
							double dp_acc = G*G*(1.0/rho_n_out - 1.0/rho_n_in); //[Pa/m] Pressure loss due to acceleration of fluid

							deltaP_tube = (dpdx_fTP + dpdx_grav)*m_L.at(flow_pattern_adj.at(j, i)) + dp_acc + (dpdx_fTP*m_L_eff_90*m_d_in*4.0 + dpdx_fTP*m_L_eff_45*m_d_in*2.0);	//[Pa] Pressure loss through tube
						}	// End 2-phase pressure drop calcs
						else
						{
							deltaP_tube = (f_fd*m_L.at(flow_pattern_adj.at(j, i))*rho_n_ave*pow(u_n, 2) / (2.0*m_d_in)) + 2.0*(f_fd*m_L_eff_45*rho_n_ave*pow(u_n, 2) / 2.0) + 4.0*(f_fd*m_L_eff_90*rho_n_ave*pow(u_n, 2) / 2.0);	//[Pa] Pressure loss through tube
							deltaP_tube = deltaP_tube + grav_mult*rho_n_ave*CSP::grav*m_L.at(flow_pattern_adj.at(j, i));
						}
						
						P_out = P_n_in - deltaP_tube;							//[Pa] Outlet pressure
						diff_P_ave = (P_ave - (P_n_in+P_out)/2.0)/P_n_in;		//[Pa] Difference between ave. pressure guessed and ave. pressure calculated
					}	// Average pressure iteration

					if( break_to_massflow_calc )
						break;

					// Don't let boiler approach superheater: it affects the temperature limits on the energy balance
					if( x_n_out > 0.85 && i < m_nodes - 1 )
					{
						mdot_flag = 1;
						break_to_massflow_calc = true;
						break;
						// GOTO 297 -->> where does this go?
					}

					P_n_in = P_out;				//[Pa] Calculate inlet pressure of next node
					h_n_in = h_n_out;			//[J/kg] Set inlet enthalpy for next node
					rho_n_in = rho_n_out;		//[kg/m^3] Set inlet density for next node
										
					bool props_succeed = true;
					do
					{
						props_succeed = true;
						water_PH( min(P_out/1000.0,19.E3),h_n_in/1000.0,&wp);
						T_n_in = wp.temp;		//[K] Calculate temperature corresponding to outlet enthalpy and pressure
					
						// Check for Dyreby props failing near vapor dome
						if( T_n_in < 0.0 )
						{
							h_n_in = 0.999 * h_n_in;
							props_succeed = false;
						}
					} while( !props_succeed );

					diff_T_ht = max( 0.0, T_1 - T_n_in );	//[K] Difference between surface temp and inlet temp: used to estimate T_1 of next node
					T1_max = max( T1_max, T_n_in + (T_1 - T_in1) );	//[K] Track maximum temperature
					// Tfin_max_out = max( Tfin_max_out, Tfin_max )
				}	// End evaluation of 1 flow path
				
				if( break_to_massflow_calc )
						break;

				double uplast_mult = std::numeric_limits<double>::quiet_NaN();;
				if( m_nodes%2 == 0 )	uplast_mult = 1.0;
				else					uplast_mult = 0.0;
				
				m_x_path_out.at(j) = x_n_out;		//[-] Outlet quality of path
				m_h_path_out.at(j) = h_n_out;		//[J/kg] Keep track of enthalpy outlet of flow path
				m_P_path_out.at(j) = P_out - uplast_mult*rho_n_ave*CSP::grav*m_L.at(flow_pattern_adj.at(j, m_nodes-1));	//[Pa] Keep track of pressure outlet of flow path
			}	// End evaluation of flow paths

			if( break_to_massflow_calc )
				continue;

			double P_path_out_sum = 0.0;
			for( int i = 0; i < m_n_fr; i++ )
				P_path_out_sum += m_P_path_out.at(i);

			P_out_avg = min( P_path_out_sum/((double)m_n_fr)/1.E3, 19.E3 );		//[kPa] Average (flow paths) outlet pressure
			
			// All flow paths have been solved (with equal pressure drops), so complete cumulative calculations to determine final mixed enthalpy
			double h_by_m = 0.0;		//[W] Enthalpy-mass flow rate product
			for( int i = 0; i < m_n_fr; i++ )
				h_by_m = h_by_m + m_h_path_out.at(i)*m_m_dot_path.at(i);

			h_n_out_total = h_by_m / m_dot_total;		//[J/kg] Total mass flow rate / mixed enthalpy product

			// 7.15.14, twn: add report for water error code
			int water_error = water_PH( P_out_avg, h_n_out_total/1000.0, &wp );
			x_n_out = wp.qual;

			diff_x_out = x_out_target - x_n_out;

		}	// End loop to Adjust mass flow rate to reach target quality	

		diff_Pout = (P_out_guess - P_out_avg)/P_out_guess;

	}	// End total boiler outlet pressure iteration
	
	if( iter_x == 20 && fabs(diff_x_out) > 0.0035 )
	{
		// Message: Boiler model did not converge on the correct quality
	}
	
	double energy_out = m_dot_in*(h_n_out_total - h_in);	//[W] Energy transferred to steam
	double m_dot_v_total = x_n_out*m_dot_in;				//[kg/s] Total mass flow rate of vapor
	//double m_dot_l_total = m_dot_in - m_dot_v_total;		//[kg/s] Total mass flow rate of liquid

	//m_q_conv.resize( m_n_fr*m_comb_nodes, 1 );
	double q_conv_sum = 0.0, q_rad_sum = 0.0, q_abs_sum = 0.0;
	for( int i = 0; i < m_n_fr*m_nodes; i++ )
	{
		q_conv_sum += m_q_conv.at( i );
		q_rad_sum += m_q_rad.at( i );
		q_abs_sum += m_q_abs.at( i );
	}

	double q_conv_boiler = q_conv_sum*(double)m_n_par/1.E6;		//[MW] Total convective loss from boiler
	double q_rad_boiler = q_rad_sum*(double)m_n_par/1.E6;		//[MW] Total radiative loss from boiler
	double q_abs_boiler = q_abs_sum*(double)m_n_par/1.E6;		//[MW] Total energy rate absorbed by boiler

	double eta_rec = energy_out / energy_in;			//[-] Efficiency of receiver

	water_PQ( P_out_avg, 1.0, &wp );
	double h_x1 = wp.enth;					//[kJ/kg] Superheater inlet enthalpy

	O_eta_b = eta_rec;
	O_T_boil_K = T_n_in;
	O_m_dot_vapor = m_dot_v_total;
	O_h_fw_kJkg = h_fw;
	O_P_b_out_kPa = P_out_avg;
	O_hx1_kJkg = h_x1;
	O_rho_fw = rho_fw;
	O_q_out_W = energy_out;
	O_T_in = T_in;
	
	mO_m_dot_in = m_dot_in;
	mO_b_T1_max	= T1_max;
	mO_b_q_out = energy_out;
	mO_b_q_in = energy_in;
	mO_b_q_conv	= q_conv_boiler;
	mO_b_q_rad = q_rad_boiler;
	mO_b_q_abs = q_abs_boiler;

	/*

	!!Outputs
	XOUT(1)         = m_dot_in                              ![kg/s] Total mass flow rate through boiler (liquid and vapor)
	XOUT(2)         = m_dot_v_total                         ![kg/s] Total mass flow rate of vapor exiting boiler
	XOUT(3)         = m_dot_l_total                         ![kg/s] Total mass flow rate of liquid exiting boiler
	XOUT(4)         = eta_rec                               ![-] Efficiency of receiver
	XOUT(5)         = T1_max                                ![K] Maximum calculated boiler tube outer surface temperature
	XOUT(6)         = Tfin_max_out                          ![K] Maximum fin (or rib) temperature (if applicable)
	XOUT(7)         = T_n_in                                ![K] Saturation Temperature
	XOUT(8)         = Energy_out                            ![W] Energy transferred to steam
	XOUT(9)         = eta_fin                               ![-] Efficiency of fin between tubes (if applicable)
	XOUT(10)        = Energy_in                             ![W] Flux * receiverArea
	XOUT(11)        = maxval(x_path_out)                    ![-] Maximum outlet quality
	XOUT(12)        = minval(x_path_out)                    ![-] Minimum outlet quality
	XOUT(13)        = P_out_avg                             ![kPa] Average outlet pressure
	XOUT(14)        = T_in                                  ![K] Boiler Inlet Temperature
	XOUT(15)        = rho_n_out                             ![kg/m^3] Outlet Density
	XOUT(16)        = h_fw                                  ![kJ/kg] Feedwater enthalpy
	XOUT(17)        = rho_fw                                ![kg/m^3] Feedwater density
	XOUT(18)        = h_x1                                  ![kJ/kg] Superheater inlet enthalpy
	XOUT(19)        = Q_conv_boiler                         ![MW] Total convective loss from boiler
	XOUT(20)        = Q_rad_boiler                          ![MW] Total radiative loss from boiler
	XOUT(21)        = Q_abs_boiler                          ![MW] Total energy rate absorbed by boiler */

	return true;
}

void C_DSG_Boiler::Get_Other_Superheater_Outputs( double & q_conv_MW, double & q_rad_MW, double & q_abs_MW, double & T_surf_max_K, double & v_exit, double & q_in )
{
	q_conv_MW	= mO_sh_q_conv;
	q_rad_MW	= mO_sh_q_rad;
	q_abs_MW	= mO_sh_q_abs;
	T_surf_max_K = mO_sh_T_surf_max;
	v_exit		= mO_sh_v_exit;
	q_in		= mO_sh_q_in;

	return;
}


bool C_DSG_Boiler::Solve_Superheater( double I_T_amb_K, double I_T_sky_K, double I_v_wind, double I_P_atm_Pa, double I_P_in_kPa, double I_m_dot_in, double I_h_in_kJkg,
									 double I_P_sh_out_min_Pa, bool I_checkflux, util::matrix_t<double> & I_q_inc_b, int & sh_exit, double I_T_target_out_K,
									 double & O_P_sh_out_kPa, double & O_eta_rec, double & O_rho_sh_out, double & O_h_sh_out_kJkg, double & O_q_out_W )
{
	// Inputs
	double T_amb = I_T_amb_K;		//[K] Ambient temperature
	double T_sky = I_T_sky_K;		//[K] Sky temperature
	double v_wind = I_v_wind;		//[m/s] Wind Speed
	double P_atm = I_P_atm_Pa;		//[Pa] Ambient Pressure
	double P_in = I_P_in_kPa;		//[kPa] Pressure entering superheater
	double m_dot_in = I_m_dot_in;	//[kg/s] Total mass flow rate through system
	double h_in = I_h_in_kJkg;		//[kPa] Inlet pressure
	double P_sh_out_min = I_P_sh_out_min_Pa;	//[Pa] Minimum allowable outlet pressure of superheater
	double T_target_out = I_T_target_out_K;		//[K] Target outlet temperature of boiler
	bool checkflux = I_checkflux;	//[-]

	// Set outputs to values that indicate model did not solve
	O_P_sh_out_kPa = -999.9;
	O_eta_rec = -999.9;
	O_rho_sh_out = -999.9;
	O_h_sh_out_kJkg = -999.9;
	O_q_out_W = -999.9;

	// Mass flow rate in one tube * ASSUMING ONE FLOW PATH *
	double m_dot_total = m_dot_in / ((double)m_n_par);		//[kg/s]

	

	double T_in = std::numeric_limits<double>::quiet_NaN();
	do
	{
		water_PH( P_in, h_in, &wp );
		T_in = wp.temp;	//[K]
		if( fabs(T_in) < 1.E4 )
			break;
		h_in *= 0.99;

	} while(true);

	h_in = h_in * 1000.0;			//[kJ/kg]
	P_in = P_in * 1000.0;			//[Pa]

	//( T_in - 273.15, P_in, &wp );
	//double h_in = wp.H*1000.0;	//[J/kg]
	//P_in = P_in*1.E3;			//[Pa]
	
	water_TQ( T_in, 1.0, &wp );

	double u_n_exit = 0.0;		//[m/s]

	double energy_in = 0.0;
	for( int i = 0; i < m_n_panels; i++ )
	{
		m_q_inc.at(i) = I_q_inc_b.at(i);
		energy_in += m_per_panel*m_h_rec.at(i)*m_q_inc.at(i);
	}

	// Create new flux arrays that allow for multiple panels in parallel flow
	// Q_adj sorts flux into flow path order and is indexed with Flow_Pat_Adj
	for( int j = 0; j < m_n_fr; j++ )
	{
		for( int i = 0; i < m_nodes; i++ )
		{
			m_q_adj.at(flow_pattern_adj.at(j, i)) = m_q_inc.at(flow_pattern.at(j, i));
		}
	}

	double beta = 1.0 / T_amb;		//[1/K] Volumetric expansion coefficient
	double T_1 = T_in + 30.0;		//[K] Guess T_1 of first panel
	m_m_dot_path.fill( m_dot_total/(double)m_n_fr );	//[kg/s] Mass flow rate through each flow route (divide evenly for now)

		// [W/m^2-K] Calculates combined free and forced convection coefficient, same as used in fin HT model
	double h_c = h_mixed(ambient_air, T_in, T_amb, v_wind, m_ksD, m_dsg_rec.Get_hl_ffact(), P_atm, CSP::grav, beta, m_h_rec.at(0), m_dsg_rec.Get_d_rec(), m_m_mixed);
	// Check if enough flux is available to produce positive net energy through each flow path
	for( int j = 0; j < m_n_fr; j++ )
	{
		m_q_wf_total.at(j) = 0.0;
		for( int i = 0; i < m_nodes; i++ )
		{						
			m_q_conv.at( flow_pattern_adj.at(j,i)) = h_c*m_A_n_proj[flow_pattern_adj.at(j,i)]*(T_in - T_amb);	//[W] Convective heat transfer
			m_q_rad.at( flow_pattern_adj.at(j,i)) = m_eps_tube*CSP::sigma*m_A_n_proj[flow_pattern_adj.at(j,i)]*(0.5*(pow(T_in,4) - pow(T_sky,4)) + 0.5*(pow(T_in,4) - pow(T_amb,4)));	//[W] Radiative heat transfer: 8/10/11, view factor to ground ~ ambient (1/2) 
			m_q_abs.at( flow_pattern_adj.at(j,i)) = m_q_adj.at( flow_pattern_adj.at(j,i))*m_abs_tube*m_A_n_proj[flow_pattern_adj.at(j,i)];	//[W] Irradiance absorbed by panel
			m_q_wf_total.at(j) += m_q_abs.at( flow_pattern_adj.at(j,i)) - m_q_conv.at( flow_pattern_adj.at(j,i)) - m_q_rad.at( flow_pattern_adj.at(j,i));	//[W] Heat transfer to working fluid
		}
		if( m_q_wf_total.at(j) < 0.0 )
		{
			sh_exit = 2;		// Set flag for controller
			return false;
		}
	}

	if( checkflux == 1 )
	{
		return true;
	}

	water_TP( T_target_out, P_in/1.E3, &wp );
	double h_out_target = wp.enth * 1000.0;		//[J/kg]
	// -> Required rate of energy addition [W] = mass flow rate [kg/s] * (enthalpy rise [J/kg])
	// q_wf_min = m_dot_total*(h_out_target - h_in)
	double q_wf_min = m_dot_total*(h_out_target - h_in);

	double sum_q_wf_total = 0;
	for( int i = 0; i < m_n_fr; i++ )
		sum_q_wf_total += m_q_wf_total.at(i);

	if( sum_q_wf_total < q_wf_min )
	{
		sh_exit = 2;	// Set flag for calling code
		return true;
	}

	double T1_max = 0.0;		//[K] Set T1_max

	// Solve for outlet conditions of each flow path
	for( int j = 0; j < m_n_fr; j++ )
	{
		double h_n_in = h_in;		//[J/kg-K] Inlet enthalpy of first node
		double T_n_in = T_in;		//[K] Inlet temperature of first node
		double P_n_in = P_in;		//[Pa] Inlet pressure of first node
		double dp = 2.E4;			//[Pa] Guess small pressure drop
		double diff_T_ht = 45.0;	//[K] Estimate of difference between surface temp and inlet temp
		double m_dot = m_m_dot_path.at(j);	//[kg/s] Use m_dot so we don't have to carry array through

		double T_n_ave, h_n_out, P_out, u_n;
		for( int i = 0; i < m_nodes; i++ )
		{
			if(i==0)	T_1 = T_n_in + 45.0;
			else		T_1 = T_n_in + 1.5*diff_T_ht;

			double diff_P_ave = 9999999.9;	// Set diff > tolerance
			int iter_P_ave = 0;				//[-] Set iteration counter
			double P_ave = max(1.E6, P_n_in - dp/2.0);	//[Pa] Guess average pressure through flow path
			bool P_upguess = false;
			bool P_lowguess = false;
			double P_upper = P_n_in;
			double P_lower = P_sh_out_min;
			double diff_P_bracket = 999.9;
			bool lowpres_flag = false;
			int P_br_low = 0;
			
			double f_fd, rho_n_ave; 
			f_fd = rho_n_ave = std::numeric_limits<double>::quiet_NaN();
			// An average pressure of the node is used as one indepedent variable to calculate additional properties
			// After pressure drop through node is calculated, want to make sure calculated average pressure is within some % of average used for props
			while( fabs(diff_P_ave) > 0.001 && iter_P_ave < 125 )
			{
				iter_P_ave++;

				if( iter_P_ave > 1 )
				{
					if( !lowpres_flag )
					{
						if( P_upguess && P_lowguess )
						{
							if( diff_P_ave < 0.0)
							{
								P_br_low = 1;
								P_lower = P_ave;
							}
							else
							{
								P_upper = P_ave;
							}
							P_ave = 0.5*P_lower + 0.5*P_upper;
						}
						else
						{
							if( diff_P_ave < 0.0 )
							{
								P_br_low = 1;
								P_lower = P_ave;
								P_lowguess = true;
							}
							else
							{
								P_upper = P_ave;
								P_upguess = true;
							}
							P_ave = (P_n_in + P_out)/2.0;
						}
					}	// end '!lowpres_flag'
					else if( lowpres_flag )	
					{
						P_br_low = 2;
						P_lower = P_ave;
						P_lowguess = true;
						P_ave = 0.5*P_lower + 0.5*P_upper;											
					}	// end 'else if lowpres_flag' 
					diff_P_bracket = (P_upper - P_lower)/P_lower;
				} // end 'if iter_P_ave > 1'

				// If bracket from which to choose average pressure is small, then try to get out of loop
				if( diff_P_bracket < 0.0005 )
				{
					/*Set lower pressure limit
					!P_br_low==1: Calculated average pressure is less than guessed
					!P_br_low==2: Calculated outlet pressure is less than specified minimum
					
					!Set upper pressure limit
					!P_br_up==1: Calculated average pressure is greater than guessed
					
					!If pressure flag, then decrease mass flow of problem flow path
					!IF(P_br_low==2)THEN
					!     IF(j==1) Pave_flag = 1
					!     IF(j==2) Pave_flag = 2
					!     GOTO 204
					!ENDIF */
					if( P_br_low == 2)
					{
						sh_exit = 1;
						return false;
					}
					// Should be able to iterate out of this
					// If( P_br_low==1 )
				}	// end 'if diff_P_bracket < 0.0005'
				
				lowpres_flag = false;		//[-] Reset flag marking an outlet pressure below allowed minimum
				double diff_T_1 = 999.9;	//[K] Set diff > tolerance
				int iter_T_1 = 0;			//[-] Set iteration counter

				double T_1_max = 1000.0;	//[K] Set some maximum allowable surface temperature to control iteration

				// Set limits on surface temperature iteration
				double T_1_upper = T_1_max;			//[K]
				double T_1_lower = T_n_in - 15.0;	//[K]
				double T_1_min = T_1_lower;			//[K]
				T_1 = min( max( T_1_lower, T_1 ), T_1_upper );	//[K]

				double diff_T1_g = 999.9;			//[K] Reset difference between upper and lower iteration limits

				// Reset iteration logic and integer flags
				bool Tupflag = false;
				bool Tlowflag = false;
				bool HT_flag = false;
				bool enth_flag = false;
				int T1_br_lower = 0;		//[-]
				int T1_br_upper = 0;		//[-]
				// Reset temperature calculation errors at high and low guesses
				double y_T_upper = 0.0;		//[K]
				double y_T_lower = 0.0;		//[K]

				// Need properties at saturated vapor in case some guess in energy balalnce results in x<1
				water_PQ( P_ave/1000.0, 1.0, &wp );
				double cp_x1 = wp.cp*1000.0;	//[J/kg-K]
				double rho_x1 = wp.dens; 
				double mu_x1 = water_visc(wp.dens, wp.temp)*1.E-6;
				double k_x1 = water_cond(wp.dens, wp.temp);

				double q_wf = std::numeric_limits<double>::quiet_NaN();
				// This loop ensures that the outlet flow conditions for each panel are solved correctly by finding the correct T1 (outer surface temp)
				while( fabs(diff_T_1/T_1)>0.0025 && iter_T_1 < 50 )
				{
					// GOTO 272 -> do something with this!?!?
					iter_T_1++;
					if( iter_T_1 > 1 )
					{
						if( !HT_flag && !enth_flag )
						{
							if( fabs(diff_T_1)>50.0 )		// If error is very large, use bisection rather than false interpolation method
							{
								if(diff_T_1 > 0.0)			// If old temp is higher than new temp, then old temp is too high, so:
								{
									T1_br_upper = 2;
									T_1_upper = T_1;		//[K] Set upper limit
									Tupflag = false;		
								}
								else
								{
									T1_br_lower = 2;
									T_1_lower = T_1;		//[K] Set lower limit
									Tlowflag = false;
								}
								T_1 = 0.5*T_1_lower + 0.5*T_1_upper;
							}
							else if( Tupflag && Tlowflag )		// If bracket results are set, use false position
							{
								if(diff_T_1>0.0)
								{
									T1_br_upper = 2;
									T_1_upper = T_1;		//[K] Set upper limit
									y_T_upper = diff_T_1;	//[K] Set upper bracket result
								}
								else	// If old temp is lower than new temp, then old temp is too low, so:
								{
									T1_br_lower = 2;	
									T_1_lower = T_1;		//[K] Set lower limit
									y_T_lower = diff_T_1;	//[K] Set lower bracket result
								}
								T_1 = y_T_upper/(y_T_upper-y_T_lower)*(T_1_lower - T_1_upper) + T_1_upper;		//[K] False position method
							}
							else
							{
								if(diff_T_1 > 0.0)
								{
									T1_br_upper = 2; 
									T_1_upper = T_1;		//[K] Set upper limit
									y_T_upper = diff_T_1;	//[K] Set upper bracket results
									Tupflag = true;			//[-] Set logic to show that upper bracket is set
								}
								else
								{
									T1_br_lower = 2;
									T_1_lower = T_1;		//[K] Set lower limit
									y_T_lower = diff_T_1;	//[K] Set lower bracket result
									Tlowflag = true;		//[-] Set logic to show that lower bracket is set
								}
								if( Tupflag && Tlowflag )
									T_1 = y_T_upper/(y_T_upper-y_T_lower)*(T_1_lower - T_1_upper) + T_1_upper;	//[K] False position
								else
								{
									T_1 = T_1 - diff_T_1;
									if( T_1 < T_1_lower )
										T_1 = 0.5*T_1_lower + 0.5*T_1_upper;
									else
										T_1 = max( T_1_lower, min( T_1_max, T_1 ) );
								}
							}
						/*
						IF(((.not.(HT_flag)).and.(.not.(enth_flag)))) THEN */

						}
						else if( enth_flag )
						{
							T1_br_lower = 1;
							T_1_lower = T_1;
							T_1 = 0.5*T_1_lower + 0.5*T_1_upper;
							enth_flag = false;
						}
						else if( HT_flag )
						{
							T1_br_upper = 1;
							T_1_upper = T_1;
							T_1 = 0.5*T_1_lower + 0.5*T_1_upper;
							HT_flag = false;
						}
					}	// end 'if iter_T_1 > 1'

					// GOTO 474 -> do something with this!!!?@?!?!@
					diff_T1_g = (T_1_upper - T_1_lower);

					// If the bracket from which to guess T_1 is small, then try to exit loop
					if( diff_T1_g < 0.01 )
					{
						if( T_1_lower > T_1_max - 0.02 && T1_br_upper==0 )	T1_br_upper = 3;
						if( T_1_upper < 0.02 + T_1_min && T1_br_lower==0 )	T1_br_lower = 3;

						/*T1_br_lower==1: Enth_flag, the resulting enthalpy from the heat addition is too large for steam property look-up routine
						!T1_br_lower==2: Calculated surface temperature is higher than guessed
						!T1_br_lower==3: Lower limit has not been set during iteration and is equal to inlet temperature
						
						!T1_br_upper==1: HT Flag, negative (or nearly negative) energy input to boiler
						!T1_br_upper==2: Calculated surface temperature is lower than guessed
						!T1_br_upper==3: Upper limit has not been set during iteration and is equal to maximum value */

						// Need to increase mass flow rate
						if( T1_br_lower==1 && T1_br_upper==2 )
						{
							sh_exit = 3;
							return false;
						}

						// Need to increase mass flow rate
						else if( T1_br_lower==1 && T1_br_upper==3 )
						{
							sh_exit = 3;
							return false;
						}

						// Need to increase mass flow rate
						else if( T1_br_lower==2 && T1_br_upper==3 )
						{
							sh_exit = 3;
							return false;
						}

						// Flux is too low
						else if( T1_br_lower==3 && T1_br_upper==2 )
						{
							sh_exit = 3;
						}
					} // end 'if diff_T1_g < 0.01' 
						//[W/m^2-K] Function calculates combined free and forced convection coefficient, same as used in fin HT model
					double h_c = h_mixed(ambient_air, T_1, T_amb, v_wind, m_ksD, m_dsg_rec.Get_hl_ffact(), P_atm, CSP::grav, beta, m_h_rec.at(flow_pattern_adj.at(j, i)), m_dsg_rec.Get_d_rec(), m_m_mixed);
					m_q_conv.at(flow_pattern_adj.at(j,i)) = h_c*m_A_n_proj[flow_pattern_adj.at(j,i)]*(T_1 - T_amb);		// Convective heat transfer
					m_q_rad.at(flow_pattern_adj.at(j,i)) = m_eps_tube*CSP::sigma*m_A_n_proj[flow_pattern_adj.at(j,i)]*(0.5*(pow(T_1,4)-pow(T_sky,4))+0.5*(pow(T_1,4)-pow(T_amb,4)));	//[W] Radiative heat transfer
					m_q_abs.at(flow_pattern_adj.at(j,i)) = m_q_adj.at(flow_pattern_adj.at(j,i))*m_A_n_proj[flow_pattern_adj.at(j,i)]*m_abs_tube;
					q_wf = m_q_abs.at(flow_pattern_adj.at(j,i)) - m_q_conv.at(flow_pattern_adj.at(j,i)) - m_q_rad.at(flow_pattern_adj.at(j,i));

					// Equation: m_dot*(h_out - h_in) = q_wf
					h_n_out = q_wf/m_dot + h_n_in;		//[J/kg] Calculate outlet enthalpy according to calculated energy addition to working fluid
					double h_n_ave = (h_n_in + h_n_out)/2.0;	//[J/kg] Determine average enthalpy in node

					// If the outlet enthalpy is greater than property routine can handle, flag and reguess T_1
					if( h_n_out > m_h_sh_max )
					{
						enth_flag = true;
						continue;
					}

					double mu_n_ave, k_n_ave, c_n_ave, x_n_ave;
					mu_n_ave = k_n_ave = c_n_ave = x_n_ave = std::numeric_limits<double>::quiet_NaN();
					bool props_succeed = true;
					do
					{
						water_PH( P_ave/1000.0, h_n_ave/1000.0, &wp );
						rho_n_ave = wp.dens; T_n_ave = wp.temp; 
						mu_n_ave = water_visc(wp.dens, wp.temp)*1.E-6;
						k_n_ave = water_cond(wp.dens, wp.temp); 
						c_n_ave = wp.cp*1000.0; x_n_ave = wp.qual;

						props_succeed = true;
						if( c_n_ave < 0.0 || k_n_ave < 0.0 )
						{
							h_n_ave = 0.99*h_n_ave;
							props_succeed = false;
						}
					} while (!props_succeed);

					if(x_n_ave < 1.0)
					{
						c_n_ave = cp_x1;
						rho_n_ave = rho_x1;
						k_n_ave = k_x1;
						mu_n_ave = mu_x1;
					}

					u_n = m_dot/(rho_n_ave*m_A_t_cs);		//[m/s] Velocity through tube
					double Re = rho_n_ave*u_n*m_d_in/mu_n_ave;		//[-] Reynolds number
					f_fd = pow( (-2.0*log10(2.0*m_rel_rough/7.54 - 5.02*log10(2.0*m_rel_rough/7.54 + 13.0/Re)/Re)), -2 );	//[-] (Moody) friction factor (Zigrang and Sylvester)

					double alpha_n = k_n_ave/(rho_n_ave*c_n_ave);	//[m^2/s] Thermal diffusivity of fluid
					double Pr = mu_n_ave/(alpha_n*rho_n_ave);		//[-] Prandtl number
					double Nusselt = ((f_fd/8.0)*(Re-1000.0)*Pr)/(1.0+12.7*sqrt(f_fd/8.0)*(pow(Pr,2.0/3.0)-1.0));	//[-] Turbulent Nusselt Number
					double h_fluid = max( k_n_ave/m_d_in, Nusselt*k_n_ave/m_d_in );	//[W/m^2-K] Convective heat transfer coefficient - set minimum to conductive ht

					// Equation: q_wf = h_fluid*Area*(T_2 - T_n_ave)
					double T_2 = q_wf/(h_fluid*m_A_n_in_act[flow_pattern_adj.at(j,i)]) + T_n_ave;		//[K] Temperature of inner tube surface
					double k_n = tube_material.cond( (T_1 + T_2)/2.0 );		//[W/m-K] Conductivity of tube using average temperature
					double R_n = log(m_d_tube / m_d_in) / (k_n*2.0*CSP::pi*m_L.at(flow_pattern_adj.at(j, i)) / 2.0);	//[K/W] Thermal resistance of ACTIVE tube
					diff_T_1 = T_1 - (T_2 + q_wf*R_n);						//[K] Calculate difference between guessed and calculated T_1					
				} // end loop solving tube energy balance (key on T_1)

				//DELTAP [Pa] =                DELTAP_tube                                     DELTAP_45                                   DELTAP_90
				dp = (f_fd*m_L.at(flow_pattern_adj.at(j, i))*rho_n_ave*pow(u_n, 2) / (2.0*m_d_in)) + 2.0*(f_fd*m_L_eff_45*rho_n_ave*pow(u_n, 2) / 2.0) + 4.0*(f_fd*m_L_eff_90*rho_n_ave*pow(u_n, 2) / 2.0);
				P_out = P_n_in - dp;				//[Pa] Calculate node outlet pressure
				diff_P_ave = (P_ave-(P_n_in+P_out)/2.0)/P_in;	//[Pa] Difference between guessed and calculated average pressure

				if(P_out < P_sh_out_min)
				{
					lowpres_flag = true;
					break;
				}
			}	// End loop solving for correct average pressure of node

			P_n_in = P_out;		//[Pa] Calculate inlet pressure of next node
			h_n_in = h_n_out;	//[J/kg] Set inlet enthalpy for next node

			water_PH( P_out/1000.0, h_n_in/1000.0, &wp );
			T_n_in = wp.temp;		//[K]

			//if( T_n_in > T_target_out+50.0 && iter_P_ave < 125 )
			if( T_n_in > T_target_out+50.0 && i < m_nodes - 1 )
			{
				sh_exit = 3;
				return false;
			}

			diff_T_ht = max( 0.0, T_1 - T_n_in );		//[K] Difference between surface temp and inlet temp: used to estimate T_1 of next node
			
			// Change max temperature to estimate surface temp at hot end, not center (where energy balance is calculated)
			T1_max = max( T1_max, T_n_in + (T_1 - T_n_ave) );
		}	// End loop to solve for a flow path

		m_h_path_out.at(j) = h_n_out;		//[J/kg] Keep track of enthalpy outlet of flow path
		m_P_path_out.at(j) = P_out;		//[Pa] Keep track of pressure outlet of flow path
		u_n_exit = max(u_n_exit, u_n);		//[m/s] Maximum outlet velocity
	} // End for loop to solve for each flow path

	// Energy balance to combine outlet flows (if more than 1): m1h1 + m2h2 = m3h3
	double mh_sum = 0.0;
	for( int j = 0; j < m_n_fr; j++ )
		mh_sum += m_m_dot_path.at(j)*m_h_path_out.at(j);	//[W] Total mh products for flow routes

	double h_out_comb = mh_sum / m_dot_total;			//[J/kg] Calculate final combined outlet enthalpy
	h_out_comb = min(h_out_comb, m_h_sh_max);	//[J/kg]

	double P_path_out_sum = 0.0;
	for( int j = 0; j < m_n_fr; j++ )
		P_path_out_sum += m_P_path_out.at(j);

	double P_out_avg = min( P_path_out_sum/(double)m_n_fr/1.E3, 19.0E3 );	//[kPa] Average (flow paths) outlet pressure

	water_PH( P_out_avg, h_out_comb/1000.0, &wp );
	double rho_out = wp.dens;

	double energy_out = m_dot_in * (h_out_comb - h_in);	//[W]
	double eta_rec = energy_out / energy_in;			//[-]

	double q_rad_sh_sum = 0.0;
	double q_conv_sh_sum = 0.0;
	double q_abs_sh_sum = 0.0;
	for( int j = 0; j < m_n_fr*m_nodes; j++ )
	{
		q_rad_sh_sum += m_q_rad.at(j);
		q_conv_sh_sum += m_q_conv.at(j);
		q_abs_sh_sum += m_q_abs.at(j);
	}

	double q_rad_sh = q_rad_sh_sum*(double)m_n_par/1.E6;	//[MW] Total radiative loss from superheater
	double q_conv_sh = q_conv_sh_sum*(double)m_n_par/1.E6;	//[MW] Total convective loss from superheater
	double q_abs_sh = q_abs_sh_sum*(double)m_n_par/1.E6;	//[MW] Total energy rate absorbed by superheater

	O_P_sh_out_kPa = P_out_avg;			//[kPa]
	O_eta_rec = eta_rec;
	O_rho_sh_out = rho_out;
	O_h_sh_out_kJkg = h_out_comb/1.E3;	//[kJ/kg]
	O_q_out_W = energy_out;				//[W]

	mO_sh_q_conv = q_conv_sh;
	mO_sh_q_rad = q_rad_sh;
	mO_sh_q_abs = q_abs_sh;
	mO_sh_T_surf_max = T1_max;
	mO_sh_v_exit = u_n_exit;
	mO_sh_q_in = energy_in;

	return true;
}
/*
Superheater(info,XIN,XOUT,N_panels,PAR,Q_inc,h_out_target,sh_exit,checkflux)
hmmmmmm?
XOUT    = 0.                !Set all outputs to 0 so it is simple to tell if they have been set   
    

!Set Outputs
XOUT(1)     = eta_rec           ![-] Thermal efficiency of receiver
XOUT(2)     = T_out             ![K] Temperature of steam exiting superheater
XOUT(3)     = P_out             ![Pa] Pressure of steam exiting superheater
XOUT(4)     = Energy_out        ![W] Rate of energy transferred to steam in superheater
XOUT(5)     = h_out_comb        ![J/kg] Outlet enthalpy
XOUT(6)     = h_in              ![J/kg] Inlet enthalpy
XOUT(7)     = T1_max            ![K] Maximum average temperature of superheater surface
XOUT(8)     = P_in - P_out_avg*1000.d0  ![Pa] Pressure drop through superheater
XOUT(9)     = u_n_exit          ![m/s] Velocity of flow
XOUT(10)    = Energy_in         ![W] Flux * receiverArea
XOUT(11)    = P_out_avg*1000.d0 ![Pa] SH outlet pressure
XOUT(12)    = rho_out           ![kg/m^3] SH outlet density             
XOUT(13)    = Q_conv_SH         ![MW] Total convective loss from superheater
XOUT(14)    = Q_rad_SH          ![MW] Total radiative loss from superheater
XOUT(15)    = Q_abs_SH          ![MW] Total energy rate absorbed by superheater

END SUBROUTINE


*/

//****************************************************************************************
//***** Flow Boiling Model: local HT coefficient: Nellis and Klein (2008)
//****************************************************************************************
double Flow_Boiling( double T_sat, double T_surf, double G, double d, double x_in, double q_t_flux, double rho_l, double rho_v, double k_l,
					 double mu_l, double Pr_l, double enth_l, double h_diff, double grav, double mu_v, double c_v, double k_v, double RelRough )
{
	double x = x_in;					//[-] Set quality
	double Re_l = G*d*(1.0-x)/mu_l;		//[-] Eq. 7-10: Reynolds number of saturated liquid flow
	double h_fluid;

	if( q_t_flux < 0.0 )	// Flow condensation correlations: Section 7.5.2 in Nellis and Klein
	{
		double X_tt = pow(rho_v/rho_l,0.5)*pow(mu_l/mu_v,0.1)*pow((1.0-x)/x,0.9);	// Eq. 7-105: Lockhard Martinelli parameter
				
		if( G > 500.0 )
			h_fluid = k_l/d * 0.023 * pow(Re_l,0.8) * pow(Pr_l,0.4) * (1.0 + pow(2.22/X_tt,0.89));	// Eq. 7-104
		else
		{
			double Ga = 9.81*rho_l*(rho_l - rho_v)*pow(d,3)/pow(mu_l,2);		// Eq. 7-109
			double Fr_mod;
			if( Re_l <= 1250.0 )
				Fr_mod = 0.025*pow(Re_l,1.59)/pow(Ga,0.5)*pow( ((1.0+1.09*pow(X_tt,0.39))/X_tt), 1.5 );	// Eq. 7-107
			else
				Fr_mod = 1.26*pow(Re_l,1.04)/pow(Ga,0.5)*pow( ((1.0+1.09*pow(X_tt,0.39))/X_tt), 1.5 );	// Eq. 7-108

			if( Fr_mod > 20.0 )
				h_fluid = k_l/d * 0.023 * pow(Re_l,0.8) * pow(Pr_l,0.4) * (1.0 + 2.22/pow(X_tt,0.89));	// Eq. 7-110
			else
			{
				double T_s;
				if( T_surf > T_sat )
					T_s = T_sat - 1.0;
				else
					T_s = T_surf;

				double c_l = Pr_l*k_l/mu_l;
				double vf = pow( (1.0 + (1.0-x)/x*pow((rho_v/rho_l),(2.0/3.0))),-1.0 );		// Eq. 7-113
				double A = acos(2.0*vf - 1.0)/3.141;										// Eq. 7-112
				double Fr_1 = pow(G,2)/(pow(rho_l,2)*9.81*d);								// Eq. 7-115

				double C_1, C_2;
				if( Fr_1 > 0.7 )		// Eq. 7-116
				{
					C_1 = 7.242;
					C_2 = 1.655;		
				}
				else					// Eq. 7-117
				{
					C_1 = 4.172 + 5.48*Fr_1 - 1.564*pow(Fr_1,2);
					C_2 = 1.773 - 0.169*Fr_1;
				}
				double Nu_fc = 0.0195*pow(Re_l,0.8)*pow(Pr_l,0.4)*(1.376 + C_1/pow(X_tt,C_2));	// Eq. 7-114
				h_fluid = (k_l/d)*( (0.23/(1.0+1.11*pow(X_tt,0.58))) * pow(G*d/mu_l,0.12) * pow( (h_diff/(c_l*(T_sat-T_s))),0.25 )*pow(Ga,0.25) + A*Nu_fc);	// Eq. 7-111
			}

		}
	}
	else
	{
		bool interp = false;
		double h_fluid_v;
		if( Re_l < 2300 )		// If Re < 2300
		{
			Re_l = 2300.0;				// Set Re to 2300
			x = 1.0 - Re_l*mu_l/(G*d);	// Calculate x corresponding to Re=2300
			interp = true;				// Now need to interpolate to find final heat transfer coefficient

			double u_n_v = G/rho_v;		
			//***** Heat transfer coefficient for x = 1 **********************
			double Re_v = rho_v * u_n_v * d / mu_v;		//[-] Reynolds number of single phase (x=1) flow
			double f_fd = pow( (-2.0*log10(2.0*RelRough/7.54 - 5.02*log10(2.0*RelRough/7.54 + 13.0*Re_v)/Re_v)), -2 );	//[-] (Moody) friction factor (Zigrang and Sylvester)

			double alpha_v = k_v/(rho_v*c_v);			//[m^2/s] Thermal diffusivity of fluid
			double Pr_v = mu_v/(alpha_v*rho_v);			//[-] Prandtl number
			double Nusselt = ((f_fd/8.0)*(Re_v-1000.0)*Pr_v)/(1.0+12.7*pow(f_fd/8.0,0.5)*(pow(Pr_v,2.0/3.0)-1.0));	//[-] Turbulent Nusselt Number (Gnielinski)
			h_fluid_v = Nusselt*k_v/d;			//[W/m^2-K] Convective heat transfer coefficient
		}
		double f_l = pow( (0.79*log(Re_l)-1.64), -2 );	//[-] Eq. 7-12: Friction factor for saturated liquid flow
		double h_l = ((f_l/8.0)*(Re_l-1000.0)*Pr_l)/(1.0+12.7*(pow(Pr_l,2.0/3.0)-1.0)*pow(f_l/8.0,0.5))*(k_l/d);	//[W/m^2-K] Eq. 7-9: Heat transfer correlation for saturated liquid flow
		double Co = pow( (1.0/x - 1.0), 0.8 )*pow( (rho_v/rho_l), 0.5);		//[-] Eq. 7-13: Dimensionless parameter
		double Bo = q_t_flux/(G*h_diff);				//[-] Eq. 7-14: Dimensionless parameter: Boiling number
		//double Fr = pow(G, 2) / (grav*d*pow(rho_l, 2));		//[-] Eq. 7-15: Dimensionless parameter: Froude number
		double N = Co;									//[-] Eq. 7-16: For vertical tubes

		double h_cb = 1.8*pow(N,-0.8);			//[-] Eq. 7-17

		double h_nb;
		if( Bo >= 0.3E-4 )
			h_nb = 230.0*pow(Bo,0.5);
		else
			h_nb = 1.0 + 46.0*pow(Bo,0.5);

		double h_bs1, h_bs2;
		if( Bo >= 11.E-4 )
		{
			h_bs1 = 14.7*pow(Bo,0.5)*exp(2.74*pow(N,-0.1));		//[-] Eq. 7-19
			h_bs2 = 14.7*pow(Bo,0.5)*exp(2.47*pow(N,-0.15));	//[-] Eq. 7-20
		}
		else
		{
			h_bs1 = 15.43*pow(Bo,0.5)*exp(2.74*pow(N,-0.1));	//[-] Eq. 7-19
			h_bs2 = 15.43*pow(Bo,0.5)*exp(2.47*pow(N,-0.15));	//[-] Eq. 7-20
		}

		double h_dim;
		if(N<=0.1)						//[-] Eq. 7-21
			h_dim = max(h_cb,h_bs2);
		else if(N<=1.0)
			h_dim = max(h_cb,h_bs1);
		else
			h_dim = max(h_cb, h_nb);

		h_fluid = h_dim * h_l;		//[W/m^2-K] Eq. 7-8

		if( interp )
			h_fluid = (h_fluid_v - h_fluid)/(1.0 - x) * (x_in - x) + h_fluid;

	}


	return h_fluid;
}

double h_mixed( HTFProperties &air, double T_node_K, double T_amb_K, double v_wind, double ksD, double hl_ffact, double P_atm_Pa, 
			    double grav, double beta, double h_rec, double d_rec, double m )
{
	// Calculate convective heat transfer coefficient (originally in Type 222)
	// Fluid properties are calculated using Film Temperature!
	double T_film	= (T_node_K + T_amb_K)/2.0;				//[K] Film temperature
	double k_film	= air.cond( T_film );					//[W/m-K] Conductivity
	double mu_film	= air.visc( T_film );					//[kg/m-s] Dynamic viscosity
	double rho_film = air.dens( T_film, P_atm_Pa );			//[kg/m^3] Density

	// Forced convection
	double Re_for	= rho_film*v_wind*d_rec/mu_film;		//[-] Reynolds number
	double Nusselt_for = CSP::Nusselt_FC( ksD, Re_for );	//[-] S&K
	double h_for	= Nusselt_for*k_film/d_rec*hl_ffact;	//[W/m^2-K]	The forced convection heat transfer coefficient

	// Free convection: use Ambient temperature for fluid props
	double nu_amb = air.visc( T_amb_K ) / air.dens( T_amb_K, P_atm_Pa );	//[m^2/s] Kinematic viscosity
	double Gr_nat = max( 0.0, grav*beta*(T_node_K - T_amb_K)*pow( h_rec, 3 )/pow( nu_amb, 2 ));	//[-] Grashof number at ambient conditions, MJW 8.4.2010 :: Hard limit of 0 on Gr #	
	double Nusselt_nat = 0.098*pow( Gr_nat, 1./3. )*pow( (T_node_K/T_amb_K), -0.14 );	//[-] Nusselt number
	double h_nat = Nusselt_nat*air.cond( T_amb_K )/h_rec*hl_ffact;			//[W/m^2-K] The natural convection cofficient ; conductivity calculation corrected
	
	double h_mixed = pow( pow( h_for, m ) + pow( h_nat, m ), 1.0/m )*4.0;	//[W/m^2-K] MJW 7.30.2010:: (4.0) is a correction factor to match convection losses at Solar II (correspondance with G. Kolb, SNL)

	return h_mixed;
}

/*
double Nusselt_FC( double ksDin, double Re )
{
	// This is the forced convection correlation presented in [Siebers and Kraabel, 1984]
	// The value of ks\D determines the interpolation that takes place between these 4 data points

	double ksD = ksDin;
	double Nomval = ksD;
	int rerun = 0;

	double Nu_FC, ValHi, ValLo, Nu_Lo, Nu_Hi, ValHi2, ValLo2;
	// Select the bounding conditions
	
	bool repeat_loop = true;

	do
	{
		repeat_loop = false;
	
		// Point 1: ksD = 0.0
		if(ksD < 75.E-5)
		{
			Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
			ValHi = 75.E-5;
			ValLo = 0.0;
		}
		else		// Point 2: ksD = 75 E-5
		{
			if(  ksD>=75.E-5 && ksD<300.E-5 )
			{
				ValHi = 300.E-5;
				ValLo = 75.E-5;
				if( Re <= 7.E5 )
					Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
				else
				{
					if( Re>7.0E5 && Re<2.2E7 )
						Nu_FC = 2.57E-3*pow( Re, 0.98 );
					else
						Nu_FC = 0.0455*pow( Re, 0.81 );
				}
			}	
			else	// Point 3: ksD = 300E-5
			{
				if( ksD>=300.E-5 && ksD<900.E-5 )
				{
					ValHi = 900.E-5;
					ValLo = 300.E-5;
					if( Re <= 1.8E5 )
						Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
					else
					{
						if( Re>1.8E5 && Re<4.E6 )
							Nu_FC = 0.0135*pow( Re, 0.89 );
						else
							Nu_FC = 0.0455*pow( Re, 0.81 );
					}			
				}
				else	// Point 4: ksD = 900 E -5
				{
					if( ksD >= 900.0E-5 )
					{
						ValHi = 900.0E-5;
						ValLo = 900.0E-5;
						if( Re <= 1E5 )
							Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
						else
							Nu_FC = 0.0455*pow( Re, 0.81 );
					}
				}
			}
		}

		if( rerun != 1 )
		{
			rerun = 1;
			Nu_Lo = Nu_FC;
			ksD = ValHi;
			ValLo2 = ValLo;
			ValHi2 = ValHi;
			repeat_loop = true;
		}

	} while( repeat_loop );

	Nu_Hi = Nu_FC;

	double chi;
	if( Nomval >= 900.E-5 )
		chi = 0.0;
	else
		chi = (Nomval - ValLo2)/(ValHi2 - ValLo2);

	Nu_FC = Nu_Lo + (Nu_Hi - Nu_Lo)*chi;

	return Nu_FC;
} */

/*
void flow_patterns_DSR( int n_panels, int flow_type, util::matrix_t<int> & flow_pattern )
{
	// !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// ! This subroutine takes the number of panels, the requested flow type, and 
	// ! returns the corresponding flow pattern (the order of panels through which the
	// ! WF passes, this code is modified from the version in Type222
	// !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
	int n_lines;
	int n_p_quarter = n_panels/4;

	switch( flow_type )
	{
	case 1:
		// This flow pattern begins at the northmost 2 panels, splits into 2 flows, and crosses over
		// at the quarter position, exiting in 2 flows on the southmost 2 panels. This is the flow
		// configuration that was used for SOLAR II
		// !Example = [13,14,15,16,17,18,6,5,4,3,2,1] [12,11,10,9,8,7,19,20,21,22,23,24]
		n_lines = 2;
		//flow_pattern.resize( n_lines, n_panels/n_lines );

		for( int i = 0; i < n_p_quarter; i++)
		{
			flow_pattern.at( 0, n_p_quarter + i) = n_p_quarter - 1 - i;			// NE Quadrant - final half of flow path 0
			flow_pattern.at( 1, i ) = 2*n_p_quarter - 1 - i;					// SE Quadrant - first half of flow path 1
			flow_pattern.at( 0, i ) = 2*n_p_quarter + i;						// SW Quadrant - first half of flow path 0
			flow_pattern.at( 1, n_p_quarter + i) = 3*n_p_quarter + i;			// NW Quadrant - final half of flow path 1
		}
		return;
	case 2:
		// This flow pattern is the same as flow pattern #1, but in reverse. The salt enters
		// on the 2 southmost panels, crosses over, and exits on the 2 northmost panels.
		// Example = [1,2,3,4,5,6,17,16,15,14,13,12] [24,23,22,21,20,19,18,7,8,9,10,11,12]
		n_lines = 2;
		//flow_pattern.resize( n_lines, n_panels/n_lines );

		for( int i = 0; i < n_p_quarter; i++)
		{
			flow_pattern.at( 0, i ) = i;										// NE Quadrant - first half of flow path 0
			flow_pattern.at( 1, n_p_quarter + i ) = n_p_quarter + i;			// SE Quadrant - final half of flow path 1
			flow_pattern.at( 0, n_p_quarter + i ) = 3*n_p_quarter - 1 - i;		// SW Quadrant - final half of flow path 0
			flow_pattern.at( 1, i ) = 4*n_p_quarter - 1 - i;					// NW Quadrant - first half of flow path 1
		}
		return;
	};



	return;
}
*/
/*

integer,intent(in)::N_panels
integer,dimension(N_panels)::Flow_pattern(2,N_panels/2),Z_all(N_panels)
integer::nlines,flowtype,i
                 
case(2)
    !This flow pattern is the same as flow pattern #1, but in reverse. The salt enters
    !  on the 2 southmost panels, crosses over, and exits on the 2 northmost panels.
    nlines = 2
                
    !New: Accounts for receiver with numbers of panels divisible by 2 but not 4      
    Flow_pattern(1,:) = (/ (i,i=1,N_panels/4),(N_panels-N_panels/4-i+1,i=1,N_panels-N_panels/4-N_panels/2) /)
    Flow_pattern(2,:) = (/ (N_panels-i+1,i=1,N_panels/4),(N_panels/4+i,i=1,N_panels/2-N_panels/4) /)
    ![1,2,3,4,5,6,17,16,15,14,13,12,24,23,22,21,20,19,18,7,8,9,10,11,12]
    !Old: Only accounts for receiver with numbers of panels divisible by 4
!    Flow_pattern(1,:) = (/ (i,i=1,N_panels/4),((3*N_panels/4-(i-1)),i=1,N_panels/4) /)
!    Flow_pattern(2,:) = (/ ((N_panels-(i-1)),i=1,N_panels/4),(i,i=N_panels/4+1,N_panels/2) /)

*/
