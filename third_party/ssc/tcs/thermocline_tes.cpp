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

#include "thermocline_tes.h"
//#include "sam_csp_util.h"
#include <algorithm>
#include <vector>

using namespace std;

bool Thermocline_TES::Initialize_TC( double H_m, double A_m2, int Fill_in, double U_kJ_hrm2K, double Utop_kJ_hrm2K, double Ubot_kJ_hrm2K,
										double f_void, double capfac, double Thmin_C, double Tcmax_C, int nodes, double T_hot_init_C,
										double T_cold_init_C, double TC_break, double T_htr_set_C, double tank_max_heat_MW, int tank_pairs,
										HTFProperties & htf_fluid_props )
{
	htfProps = htf_fluid_props;	//[-] Set HTF property class to fluid property class from calling method

	m_num_TC_max = 10000;		//[-] Maximum number of timesteps that can be evaluated during 1 call	

	m_H = H_m;					//[m] Height of the rock bed storage tank  
	m_A = A_m2;					//[m^2] Cross-sectional area of storage tank
	double Fill = Fill_in;				//[-] Filler material number
	m_U = U_kJ_hrm2K;			//[kJ/hr-m2-K] loss coefficient   
		//gjk 4 new parameters
	m_U_top = Utop_kJ_hrm2K;	//[kJ/hr-m2-K] top surface conductance
	m_U_bot = Ubot_kJ_hrm2K;	//[kJ/hr-m2-K] bottom surface conductance
	m_void = f_void;			//[-] rock bed void fraction
		//gjk capfac to add thermal mass to bottom of tank
	m_capfac = capfac;			//[-] Bottom thermal mass capacitance factor multiplier
		//mjw add temperature control parameters
	m_Thmin = Thmin_C;				//[C] Minimum allowable hot side outlet temperature during discharge
	m_Th_avail_min = m_Thmin + 5.0;	//[C] Min allowable hot side outlet temp used for availability calcs
		
	m_Tcmax = Tcmax_C;				//[C] Maximum allowable cold side outlet temperature during charge
	m_Tc_avail_max = m_Tcmax - 5.0; //[C] Max allowable cold side outlet temp used for availability calcs
		
	m_nodes = nodes;					//[-] Number of nodes in thermocline

	m_T_hot_init = T_hot_init_C;		//[C] Initial TC hot temp
	m_T_cold_init = T_cold_init_C;		//[C] Initial TC cold temp
	m_TC_break = TC_break;				//[-] Fraction into tank where TC exists (0: entire tank is hot, 1: entire tank is cold)
	m_T_htr_set = T_htr_set_C;			//[C] Min allowable cold tank fluid temp before aux heater turns on
	m_tank_max_heat = tank_max_heat_MW; //[MW] Capacity of tank heater
	m_tank_pairs = tank_pairs;			//[-] Number of equivalent tank pairs

	int nodes_break = (int)( (1.0 - m_TC_break)*(m_nodes) ) - 1;

	m_T_hot_in_min = 0.9*m_T_hot_init + 0.1*m_T_cold_init;		//[C] Min allowable inlet charging temp
	m_T_cold_in_max = 0.1*m_T_hot_init + 0.9*m_T_cold_init;	//[C] Max allowable inlet discharging temp

	m_T_prev.resize(m_nodes,1);
	m_T_start.resize(m_nodes,1);
	m_T_ave.resize(m_nodes,1);
	m_T_end.resize(m_nodes,1);

	// Define initial thermocline array based on initial hot and cold temperatures and cold fraction
	if( nodes_break <= 0 )
		m_T_prev.assign(m_T_prev.size(), m_T_cold_init);
	else if( nodes_break >= m_nodes - 1 )
		m_T_prev.assign(m_T_prev.size(), m_T_hot_init);
	else
	{
		for( int i = 0; i < nodes_break; i++ )
			m_T_prev[i] = m_T_hot_init;
		for( int i = nodes_break; i < m_nodes; i++ )
			m_T_prev[i] = m_T_cold_init;
	}

	// Determine the average rockbed temperature
	m_T_final_ave_prev = 0.0;
	for( int i = 0; i < m_nodes; i++ )
		m_T_final_ave_prev += m_T_prev[i];   // .at(i, 0);
	m_T_final_ave_prev /= m_nodes;

	if( !fillProps.Set_TC_Material((int) Fill ) )
		return false;		// Invalid fill material number

	double T_prop = 0.5*(m_T_hot_init + m_T_cold_init);
	double cond_htf = htfProps.cond( T_prop + 273.15 );
	double cond_fill = fillProps.k_bed();
	m_cond = f_void*cond_htf + (1.0 - f_void)*cond_fill;	//[W/m-K]
	m_cp_a = htfProps.Cp( T_prop + 273.15 );					//[kJ/kg-K]
	m_rho_a = htfProps.dens( T_prop + 273.15, 1.0 );			//[kg/m^3]
	m_cp_r = fillProps.cp_bed();								//[kJ/kg-K]
	m_rho_r = fillProps.dens_bed();							//[kg/m^3]

	//mjw 4.26.11 Assume a cylindrical tank and calculate perimeter
	m_P = sqrt(m_A/CSP::pi)*2*CSP::pi;				//[m] Perimeter of cylindrical tank
	m_vol = m_A*m_H;								//[m^3] Volume of tank
	m_UA = m_U * m_P * m_H / m_nodes;				//[kJ/hr-K]->[kJ/hr-m2-K]*surface area of node[m^2] (P*H/XNODES)
	// gjk add UATOP and UABOT
	m_UA_top = m_U_top * m_A;                       //[kJ/hr-K]->[kJ/hr-m2-K]*top surface area[m^2] (A)
	m_UA_bot = m_U_bot * m_A;						//[kJ/hr-K]->[kJ/hr-m2-K]*bottom surface area[m^2] (A)

	m_ef_cond = m_cond*m_A*m_nodes/m_H;
	m_cap = m_vol*m_void*m_cp_a*m_rho_a + m_vol*(1.0-m_void)*m_cp_r*m_rho_r;	//[kJ/K] Tank
	m_e_tes = m_cap*(m_T_hot_init - m_T_cold_init);								//[kJ] Available energy in completely hot tank
	m_cap_node = m_cap / m_nodes;

	double tol_q = 0.01;		//[-]
	m_tol_TC = 0.5*0.5*tol_q*m_UA / (m_cond*m_A/(m_H/m_nodes));
	m_tol_TC = max(1.E-10, min( 0.001, m_tol_TC ) );

	return true;
}

bool Thermocline_TES::Solve_TC( double T_hot_in_C, double flow_h_kghr, double T_cold_in_C, double flow_c_kghr, double T_env_C, int mode_in,
		              double Q_dis_target_W, double Q_cha_target_W, double f_storage_in, double time_hr,
					  double & m_dis_avail_tot, double & T_dis_avail_C, double & m_ch_avail_tot, double & T_ch_avail_C,
					  double & Q_dot_out_W, double & Q_dot_losses, double & T_hot_bed_C, double & T_cold_bed_C, double & T_max_bed_C,
					  double & f_hot, double & f_cold, double & Q_dot_htr_kJ)
{

	double T_hot = T_hot_in_C;					//[C] Temperature into the top of the tank (charging)
	double flow_h = flow_h_kghr/m_tank_pairs;	//[kg/hr] Flowrate into top (charging)
	double T_cold = T_cold_in_C;				//[C] Temperature into the bottom of the tank (discharging)
	double flow_c = flow_c_kghr/m_tank_pairs;	//[kg/hr] Flowrate into bottom (charging)
	double T_env = T_env_C;						//[C] Environmental temperature
	int mode = mode_in;							//[-] Flag for whether call is to check availability (=2 then only check availability)
	double Q_dis_target = Q_dis_target_W/m_tank_pairs;	//[W] Discharge rate required by controller
	double Q_cha_target = Q_cha_target_W/m_tank_pairs;  //[W] Charge rate required by controller
	double f_storage = f_storage_in;			//[-] Storage dispatch for timestep
	double delt = time_hr;

	// Reset beginning and middle TC node temperatures to previous timestep final values
	m_T_start = m_T_prev;
	m_T_ave = m_T_prev;
	m_T_end = m_T_prev;

	int I_flow = -1;
	bool know_mdot = false;
	double q_target = std::numeric_limits<double>::quiet_NaN();
	double m_dot = std::numeric_limits<double>::quiet_NaN();
	// If discharge thermal power rate is specified then
	if(Q_dis_target > 0.0)
	{
		I_flow = 2;				//[-] Set flow direction to discharge
		know_mdot = false;		//[-] Need to solve for mass flow
		q_target = Q_dis_target;	//[W] Target thermal power is discharge taraget
	}
	else if(Q_cha_target > 0.0)		//If charge thermal power rate is specified then
	{
		I_flow = 1;				//[-] Set flow direction to charge
		know_mdot = false;		//[-] Need to solve for mass flow
		q_target = Q_cha_target;	//[W] Target thermal power is charge target
	}
	else if(flow_c > 0.0)			//If discharge flow rate is specified then
	{
		I_flow = 2;				//[-] Set flow direction to discharge
		know_mdot = true;		//[-] Know mass flow rate
		m_dot = flow_c;			//[kg/hr] Set mass flow rate
	}
	else if(flow_h > 0.0)			//If charge flow rate is specified then
	{
		I_flow = 1;				//[-] Set flow direction to charge
		know_mdot = true;		//[-] Know mass flow rate
		m_dot = flow_h;			//[kg/hr] Set mass flow rate
	}
	else							// "Idle" mode
	{
		I_flow = 1;				//[-] Set flow direction to charge, but doesn't matter
		know_mdot = false;		//[-]
		m_dot = flow_h;			//[kg/hr] Set mass flow rate: should be 0
		q_target = 0.0;			//[W] Target thermal power is 0
	}


	//***********************************************************************************************************
	//mjw 4.25.11 Calulcate the available volumes for charge/discharge given the control temperature limitations
	//Updated 11/28/11, TN
	//   - The availability check should be completed before the performance simulation, so use StoreTransfer(1:Nodes)
	//***********************************************************************************************************
	
	int iclim = 0;
	int ihlim = 0;
	double Thtemp = 0.0;
	double Tctemp = 0.0;
	for( int k = 0; k < m_nodes; k++ )
	{
		// Find the last node where the HTF temperature is above the hot side limit
		if( m_T_start[k] > m_Th_avail_min )
		{
			ihlim = k;						//[-]
			Thtemp += m_T_start[k];	//[C]
		}
		// Find the last node where the HTF temperature is below the cold side limit
		if( m_T_start[m_nodes-1-k] < m_Tc_avail_max )
		{
			iclim = k;						//[-]
			Tctemp += m_T_start[m_nodes-1-k];
		}
	}

	double fhlim = (double) ihlim;
	double fclim = (double) iclim;

	// Average temperature of TC above limit (assumes uniform props)
	Thtemp /= max( fhlim+1, 1.0 );
	// Average temperature of TC below limit
	Tctemp /= max( fclim+1, 1.0 );
	
	// Calculate the fraction of the HTF that the hot filler can bring up to temperature
	fhlim = (fhlim+1)/m_nodes;
	fclim = (fclim+1)/m_nodes;

	int ChargeNodes = min(m_nodes-1, max( 0, (int)floor(f_storage*m_nodes) ) );

	double Qd_fill = std::numeric_limits<double>::quiet_NaN();
	double flc = std::numeric_limits<double>::quiet_NaN();
	// If cold inlet temperature is greater than the cold maximum, then no thermal energy is available for discharage
	if( T_cold > m_T_cold_in_max )
	{
		Qd_fill = 0.0;
		flow_c = 0.0;
		flc = m_cp_a*flow_c;		//[kJ/hr-K]
	}
	else	// Otherwise, estimate the maximum thermal energy available for discharge
	{
		Qd_fill = max( 0.0, m_vol*fhlim*m_rho_r*m_cp_r*(1.0-m_void)*max( Thtemp - T_cold, 0.0 ) ) + max( 0.0, m_vol*fhlim*m_void*m_rho_a*m_cp_a*max( Thtemp - T_cold, 0.0 ) );	//[kJ]
	}

	double Qc_fill = std::numeric_limits<double>::quiet_NaN();
	double flh = std::numeric_limits<double>::quiet_NaN();
	// If hot inlet temperature is less than hot minimum, then no thermal energy is available for charging
	if( T_hot < m_T_hot_in_min )
	{
		Qc_fill = 0.0;
		flow_h = 0.0;
		flh = m_cp_a*flow_h;
	}
	else    // Otherwise, estimate the maximum thermal energy available for charge
	{
		Qc_fill = max( 0.0, m_vol*fclim*m_rho_r*m_cp_r*(1.0-m_void)*max(T_hot-Tctemp,0.0)) + max( 0.0, m_vol*fclim*m_void*m_rho_a*m_cp_a*max(T_hot-Tctemp,0.0));	//[kJ]
	}

	double m_disch_avail = 0.0;
	// ******Discharging****************************************************************
	// Convert maximum discharge energy to thermal power over timestep
	Qd_fill = Qd_fill/(3.6*delt);					//[kJ]*[1/hr]*1000[J/kJ]*(1/3600)[hr/s]=>[W] 
	//Approximate mass flow available over hour
	m_disch_avail = Qd_fill / (m_cp_a*max(Thtemp - T_cold, 1.0))*3.6;      //[J/s]*[kg-K/kJ]*[1/K]*(1/1000)[J/kJ]*3600[s/hr]=>[kg/hr]
	// ******************************************************************************
	
	double m_charge_avail = 0.0;
	// ******Charging****************************************************************
	Qc_fill = Qc_fill/(3.6*delt);                  //[kJ]*[1/hr]*1000[J/kJ]*(1/3600)[hr/s]=>[W]
	m_charge_avail = Qc_fill / (m_cp_a*max( T_hot - Tctemp, 1.0 ))*3.6;    //[J/s]*[kg-K/kJ]*[1/K]*(1/1000)[J/kJ]*3600[s/hr]=>[kg/hr]
	// ******************************************************************************
	
	// If call to subroutine is only interested in availability, then set outputs and exit
	if( mode == 2 )
	{
		m_dis_avail_tot = m_disch_avail * m_tank_pairs;
		T_dis_avail_C = m_T_start[0];
		m_ch_avail_tot = m_charge_avail * m_tank_pairs;
		T_ch_avail_C = m_T_start[m_nodes-1];
		return true;
	}
	
	// If mass flow rate is known, there is still the possibility of that it will over-(discharge) the thermocline,
	//     so we need to establish a range of possible mass flow rates for the solver
	double m_dot_lower = std::numeric_limits<double>::quiet_NaN();
	double m_dot_upper = std::numeric_limits<double>::quiet_NaN();
	if( know_mdot )
	{
		m_dot_lower = 0.0;
		m_dot_upper = m_dot;
	}
	else if( I_flow == 2 )
		m_dot = min( Q_dis_target, Qd_fill ) / (m_cp_a*max( Thtemp - T_cold, 1.0 ))*3.6;		//[kg/hr]
	else if( I_flow == 1 )
		m_dot = min( Q_cha_target, Qc_fill ) / (m_cp_a*max( T_hot - Tctemp, 1.0 ))*3.6;			//[kg/hr]

	
	// Set bounds a bit past realistic bounds so if the correct mass flow rate is at the bounds, iteration gets there faster
	m_dot_upper = 1.5*m_dot;
	m_dot_lower = 0.5*m_dot;
	
	// Keep track of initial upper and lower mass flow rate estimates
	double m_dot_low0 = m_dot_lower;
	double m_dot_up0 = m_dot_upper;
	
	double diff_q_target = 999.0; //[-] Set difference greater than tolerance
	int q_iter = 0;               //[-] Iteration counter on achieving target (dis)charge energy
	int TC_limit = 0;             //[-] Flag signaling whether current mass flow rate has over-(dis)charged thermocline
	bool upflag = false;          //[-] Flag signaling that upper limit on mass flow has been found with a corresponding 'diff_q_target'
	bool lowflag = false;         //[-] Flag signaling that lower limit on mass flow has been found with a corresponding 'diff_q_target'
	bool mdot_iter = false;       //[-] Flag signaling that allow a mass flow rate was specified, it must be iterated on (when = true)
	
	double q_tol = 0.0001;          //[-] Relative tolerance for energy rate in/out
	double t_tol = 0.050;			  //[C] Absolute tolerance for packed bed filling/depletion limits

	double y_upper = std::numeric_limits<double>::quiet_NaN();
	double y_lower = std::numeric_limits<double>::quiet_NaN();
	double q_calc = std::numeric_limits<double>::quiet_NaN();

	bool full;
	int num_TC = -1;

	double T_disch_avail = std::numeric_limits<double>::quiet_NaN();
	double T_charge_avail = std::numeric_limits<double>::quiet_NaN();

	while( (fabs(diff_q_target)>q_tol || TC_limit != 0) && q_iter < 40 )
	{
		q_iter++;		//[-] Increase iteration counter
		full = true;	//[-] Reset target energy flag

		// After first run, begin to iterate on mass flow rate
		if( q_iter > 1 )
		{
			if(TC_limit==1)
			{
				m_dot_upper = m_dot;
				upflag = false;
				m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower;
			}
			else if(TC_limit==2)
			{
				m_dot_lower = m_dot;
				lowflag = false;
				m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower;
			}
			else if( upflag && lowflag )
			{
				if(diff_q_target < q_tol)
				{
					m_dot_upper = m_dot;
					y_upper = diff_q_target;
				}
				else
				{
					m_dot_lower = m_dot;
					y_lower = diff_q_target;
				}
				m_dot = y_upper/(y_upper - y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
			}
			else
			{
				if(diff_q_target > q_tol)
				{
					m_dot_upper = m_dot;
					upflag = true;
					y_upper = diff_q_target;
				}
				else if(diff_q_target < q_tol)
				{
					m_dot_lower = m_dot;
					lowflag = true;
					y_lower = diff_q_target;
				}
				if( upflag && lowflag )
					m_dot = y_upper/(y_upper - y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
				else
					m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower;
			}
		}

		// If iterated m_dot approaches lower bound, adjust lower bound to zero
		if( (m_dot - m_dot_low0)/m_dot_low0 < 0.0005 )
		{
			m_dot_low0 = 0.0;
			m_dot_lower = m_dot_low0;
			m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower;
			lowflag = false;
		}

		// If iterated m_dot approaches upper bound, increase upper bound
		if( (m_dot_up0 - m_dot)/m_dot_up0 < 0.0005 )
		{
			m_dot_up0 = 2.0*m_dot_up0;
			m_dot_upper = m_dot_up0;
			m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower;
			upflag = true;
		}

		if( I_flow == 1 )
			flow_h = m_dot;			//[kg/hr]
		else if( I_flow == 2 )
			flow_c = m_dot;			//[kg/hr]

		// Compute capacitance flow rates
		flh = m_cp_a*flow_h;		//[kJ/hr-K]
		flc = m_cp_a*flow_c;		//[kJ/hr-K]

		// Calculate time constant: This value will change every mass flow rate iteration
		double tau = (0.632)*(m_cap/m_nodes) / max(flh,flc);		//[kJ/kg]*[hr-K/kJ] => hr
		double TC_timestep = tau;		//[hr]

		num_TC = -1;
		// If calculated timestep is less than calling method timestep, adjust it so tht there is a whole number of TC timesteps
		if( TC_timestep < delt )
		{
			// Okay, so if the simulation timestep is not divisible by the thermocline timestep then we need to adjust the TC timestep so it is.  Lower TC timestep to accomplish this
			if( delt - (int)(delt/TC_timestep)*TC_timestep != 0.0 )
				num_TC = (int)ceil(delt/TC_timestep);		//[-] Number of thermocline timesteps per simulation timestep
			else
				num_TC = (int)(delt/TC_timestep);				//[-]

			num_TC = min( m_num_TC_max, num_TC );
			TC_timestep = delt / (double) num_TC;		//[hr] Corrected timestep (such that the number of TC timesteps in a sim timestep is an integer) at which to evaluate thermocline
		}
		else
		{
			num_TC = 1;				//[-]
			TC_timestep = delt;		//[hr]
		}

		// Reset start, middle, and end temperature arrays to initial timestep values
		m_T_start = m_T_prev;
		m_T_ave = m_T_prev;
		m_T_end = m_T_prev;

		m_T_ts_ave.clear();
		m_T_ts_ave.assign(num_TC, 0.0);

		m_Q_losses.clear();
		m_Q_losses.assign(num_TC, 0.0);

		m_Q_htr.clear();
		m_Q_htr.assign(num_TC, 0.0);

		m_T_cout_ave.clear();
		m_T_cout_ave.assign(num_TC, 0.0);

		m_T_hout_ave.clear();
		m_T_hout_ave.assign(num_TC, 0.0);
		
		// Okay, add outer loop for number of thermocline timesteps in a simulation timestep
		for( int tcn = 0; tcn < num_TC; tcn++ )
		{
			// Set convergence parameters for current run of packed bed simulation
			int iter = 0;				//[-]
			double max_T_diff = 999.9;	//[-]

			// Set coefficients for analytical solutions of individual nodes
			while( iter < 30 && max_T_diff > m_tol_TC )
			{
				iter++;

				m_T_ts_ave[tcn] = 0.0;		//[C] Array storing average packed bed temperature at each timestep
				m_Q_losses[tcn] = 0.0;		//[kJ/hr] Array storing summed rate of heat loss at each timestep

				max_T_diff = 0.0;		// Reset
												
				for( int j = 0; j < m_nodes; j++ )
				{
					double aa = std::numeric_limits<double>::quiet_NaN();
					double bb = std::numeric_limits<double>::quiet_NaN();
					double UA_hl = std::numeric_limits<double>::quiet_NaN();
					int i = -1;

					if( I_flow == 1 )	// Charging
					{
						i = j;
						if( i == 0 )	// Top (hot) node 
						{
							aa = -(flh+m_UA+m_UA_top+m_ef_cond)/m_cap_node;
							bb = (flh*T_hot+(m_UA+m_UA_top)*T_env+m_ef_cond*m_T_ave[i+1])/m_cap_node;
							UA_hl = m_UA + m_UA_top;
						}
						else if( i == m_nodes - 1 )	// Bottom (cold) node
						{
							aa = -(flh+m_UA+m_UA_bot+m_ef_cond)/(m_cap_node*m_capfac);
							bb = (flh*m_T_ave[i-1]+(m_UA+m_UA_bot)*T_env+m_ef_cond*m_T_ave[i-1])/(m_cap_node*m_capfac);
							UA_hl = m_UA + m_UA_bot;
						}
						else	// Middle nodes
						{
							aa = -(flh+m_UA+2.0*m_ef_cond)/m_cap_node;
							bb = (flh*m_T_ave[i-1]+m_UA*T_env+m_ef_cond*(m_T_ave[i-1]+m_T_ave[i+1]))/m_cap_node;
							UA_hl = m_UA;
						}
					}
					else   // Dischargning
					{
						i = m_nodes - 1 - j;
						if( i == 0 )	// Top (hot) node
						{
							aa = -(flc+m_UA+m_UA_top+m_ef_cond)/m_cap_node;
							bb = (flc*m_T_ave[i+1]+(m_UA+m_UA_top)*T_env+m_ef_cond*m_T_ave[i+1])/m_cap_node;
							UA_hl = m_UA + m_UA_top;
							
						}
						else if( i == m_nodes - 1 )	// Bottom (cold) node
						{
							aa = -(flc+m_UA+m_UA_bot+m_ef_cond)/(m_cap_node*m_capfac);
							bb = (flc*T_cold+(m_UA+m_UA_bot)*T_env+m_ef_cond*m_T_ave[i-1])/(m_cap_node*m_capfac);
							UA_hl = m_UA + m_UA_bot;
						}
						else	// Middle nodes
						{
							aa = -(flc+m_UA+2.0*m_ef_cond)/m_cap_node;
							bb = (flc*m_T_ave[i+1]+m_UA*T_env+m_ef_cond*(m_T_ave[i+1]+m_T_ave[i-1]))/m_cap_node;
							UA_hl = m_UA;
						}
					}
					double T_node_initial = m_T_start[i];
					// Solve analytical solution
					double T_final;
					double T_average;
					if( aa != 0 )
					{
						T_final = (T_node_initial+bb/aa)*exp(aa*TC_timestep) - bb/aa;
						T_average = (T_node_initial+bb/aa)/aa/TC_timestep*(exp(aa*TC_timestep)-1.0) - bb/aa;
					}
					else
					{
						T_final = bb*TC_timestep + T_node_initial;
						T_average = (T_final + T_node_initial)/2.0;
					}

					// If final node temperature is below cold limit, then apply heater
					double Q_htr_max = std::numeric_limits<double>::quiet_NaN();
					if( T_final < m_T_htr_set )
					{
						Q_htr_max = m_tank_max_heat*1000.0*TC_timestep*3600.0;	//[MW]*1000(kJ/MJ)*dt(hr)*3600(s/hr)=>kg  Maximum heat rate of tank heater

						// If heat has capacity to increase node temperature to setpoint
						if( m_Q_htr[tcn] + m_cap_node*(m_T_htr_set - T_final) < m_tank_max_heat )
						{
							m_Q_htr[tcn] = m_Q_htr[tcn] + m_cap_node*(m_T_htr_set - T_final);		//[kJ]   Thermal energy required by heater to maintain cold limit in tank
							T_final = m_T_htr_set;		//[C] Node hits setpoint
						}
						else	// If the heater does not have capacity
						{
							T_final = (m_tank_max_heat - m_Q_htr[tcn])/m_cap_node + T_final;
						}
					}

					m_T_end[i] = T_final;
					max_T_diff = max( max_T_diff, fabs( m_T_ave[i] - T_average ) );		//[C] Difference between old average node temp and new average node temp
					m_T_ave[i] = T_average;											//[C] Update guess on average node temp now that difference is calculated
					m_T_ts_ave[tcn] += T_average;										//[C] Add average node temps
					m_Q_losses[tcn] += UA_hl*(T_average - T_env);						//[kJ/hr-K]*[K] -> [kJ/hr] Heat loss
				}	// End step through thermocline nodes

				m_T_ts_ave[tcn] /= (double) m_nodes;				//[C] Spatial average of nodal time averaged temps
				max_T_diff /= 290.0;									//[-] Max nodal relative temperature difference

			}	// End iteration on average (time) nodal temperatures

			m_T_cout_ave[tcn] = m_T_ave[m_nodes - 1];
			m_T_hout_ave[tcn] = m_T_ave[0];

			// After timestep has solved, set initial nodes of next timestep to end nodes of current timestep
			m_T_start = m_T_end;
		}

		// Determine average rockbed temperatures and average derivatives
		m_T_final_ave = 0.0;
		for( int i = 0; i < m_nodes; i++ )
			m_T_final_ave += m_T_end[i];
		m_T_final_ave /= m_nodes;					//[C] Spatial average of temps at end of final timestep

		// TN: Average rockbed temp is now the average (sub timesteps in simulation) of averages (nodal in each sub timestep) of averages ( time based in each timestep )
		double T_ave = 0.0;
		for( int i = 0; i < num_TC; i++ )
			T_ave += m_T_ts_ave[i];
		T_ave /= (double)num_TC;
		// *************************************************

		// TN: Outlet temperature to pass should be average (sub timesteps in simulation) of averages (time based in each sub timestep)
		T_disch_avail = 0.0;
		T_charge_avail = 0.0;
		for( int i = 0; i < num_TC; i++ )
		{
			T_disch_avail += m_T_hout_ave[i];
			T_charge_avail += m_T_cout_ave[i];
		}
		T_disch_avail /= (double)num_TC;		//[C] Time-averaged discharging OUTLET temperature (this is a CONFUSING name)
		T_charge_avail /= (double)num_TC;		//[C] Time-averaged charging OUTLET temperature (this is a CONFUSING name)

		double diff_Tcmax = std::numeric_limits<double>::quiet_NaN();
		double diff_Thmin = std::numeric_limits<double>::quiet_NaN();
		// Check if tank has been over-(dis)charged with current mass flow rate
		if( I_flow == 1 )	// Charging, so look for min cold temp hotter than allowed
			diff_Tcmax = m_T_end[m_nodes-1] - m_Tcmax;
		else if( I_flow == 2 )	// Discharging, so look for max hot temp cooled than allowed
			diff_Thmin = m_T_end[ChargeNodes] - m_Thmin;

		TC_limit = 0;		// Reset Flag

		// If solving thermocline for specified mass flow rate:
		if( know_mdot )
		{
			full = false;

			// Charging, specified mass flow rate
			if( I_flow == 1 )
			{
				q_calc = m_dot*m_cp_a*(T_hot - T_charge_avail)/3.6;		//[W] Calculate rate of energy change

				// If specified mass flow caused over-charging and thus iteration:
				if( mdot_iter )
				{
					// If within tolerance on max cold temperature then get out
					if( diff_Tcmax > -t_tol && diff_Tcmax <= 0.0 )
					{
						// Set convergence criteria to 0 to get out
						diff_q_target = 0.0;
						TC_limit = 0;
					}
					else if( diff_Tcmax > 0.0 )		// If min cold temp is greater than allowed:
					{
						TC_limit = 1;		//[-] Flag noting that upper limit on mass flow should be set
					}
					else		// Else, if tank is not "close enough" to being fully charged:
					{  
						TC_limit = 2;		//[-] Flag noting that lower limit on mass flow should be set
					}
				}
				else	// If first iteration for a specified mass flow rate
				{
					// If tank does NOT overcharge, then get out
					if( diff_Tcmax <= 0.0 )
					{
						// Set convergence criteria to 0 to get out
						diff_q_target = 0.0;
						TC_limit = 0;
					}
					else	// Else, know that mass flow rate is too high
					{
						mdot_iter = true;		//[-] Flag noting that correct mass flow rate must be found via iteration
						TC_limit = 1;			//[-] Flag noting that upper limit on mass flow should be set
					}
				}
			}
			else	// Discharging, specified mass flow rate
			{
				q_calc = m_dot*m_cp_a*(T_disch_avail - T_cold)/3.6;	//[W] Calculated rate of energy discharge

				// If specified mass flow caused over-charging and thus iteration:
				if( mdot_iter ) 
				{
					// If within tolerance on min hot temperature then get out
					if( diff_Thmin >= 0.0 && diff_Thmin < t_tol )
					{
						// Set convergence criteria to 0 to get out
						diff_q_target = 0.0;
						TC_limit = 0;
					}
					else if( diff_Thmin < 0.0 )
					{
						TC_limit = 1;		//[-] Flag noting that upper limit on mass flow should be set
					}
					else
					{
						TC_limit = 2;		//[-] Flag noting that lower limit on mass flow should be set
					}
				}
				else
				{
					// If tank does NOT over-discharge, then get out
					if( diff_Thmin >= 0.0 )
					{
						// Set convergence criteria to 0 to get out
						diff_q_target = 0.0;
						TC_limit = 0;
					}
					else	// Else, know that mass flow rate is too high
					{
						mdot_iter = true;		//[-] Flag noting that correct mass flow rate must be found through iteration
						TC_limit = 1;			//[-] Flag noting that upper limit on mass flow should be set
					}
				}
			}
		}
		else if( q_target == 0.0 )		// Only calling model to calculate new thermocline (dwelling, so still need to calculate losses and conduction), get out
		{
			// Set convergence criteria to 0 to get out
			diff_q_target = 0.0;
			TC_limit = 0;
		}
		else if( I_flow == 1 )		// Solving thermocline for specified charging energy
		{
			q_calc = m_dot*m_cp_a*(T_hot - T_charge_avail)/3.6;		//[kg/hr]*[kJ/kg-K]*[K]*(1000)[J/kJ]*(1/3600)[hr/s]->[W] Calculated rate of energy charge  

			// If not removing enough energy and "close enough (but not going over)" to completely filling tank, then get out
			if( q_calc < q_target && diff_Tcmax > -t_tol && diff_Tcmax <= 0.0 )
			{
				// Set convergence criteria to 0 to get out
				diff_q_target = 0.0;		//[W]
				TC_limit = 0;				//[-]
				full = false;				//[-] Note that code did not result in target energy rate
			}
			else if( diff_Tcmax > 0.0 )		// Over-charging tank, flag to indicate mass flow is too high
				TC_limit = 1;
			else				// Not over-charging tank -> compare calculated to target
				diff_q_target = (q_calc - q_target)/q_target;		//[W] Relative difference between calculated and required rate of energy
		}
		else	// Solving thermocline for specified discharging energy
		{
			q_calc = m_dot*m_cp_a*(T_disch_avail - T_cold)/3.6;	//[kg/hr]*[kJ/kg-K]*K*[W/kW][hr/s]=>[W]: Calculated rate of energy discharge

			// If not getting enough energy and "close enough (but not going over)" to depleting tank, then get out
			if( q_calc < q_target && diff_Thmin < t_tol && diff_Thmin >= 0.0 )
			{
				// Set convergence criteria to 0 to get out
				diff_q_target = 0.0;		//[W]
				TC_limit = 0;				//[-]
				full = false;				//[-] Note that code did not result in target energy rate
			}
			else if( diff_Thmin < 0.0 )		// Over-discharing tank, flag to indicate mass flow is too high
				TC_limit = 1;
			else				// Not over-discharging tank -> compare calculated to target
				diff_q_target = (q_calc - q_target)/q_target;	//[W] Relative difference between calculated and required rate of energy
		}
	}	// End of iteration on mass flow rate

	if( q_iter == 40 && ( fabs(diff_q_target) > q_tol || TC_limit != 0 ) )
		full = false;

	if( full )		// If within tolerance on target energy rate, then set to target => this is beneficial to the solver (Type 251)
		q_calc = q_target;	//[W]

	// Calculate some important outputs
	double Q_losses_sum = 0.0;
	for( int i = 0; i < num_TC; i++ )
		Q_losses_sum += m_Q_losses[i];
	double Q_loss_total = Q_losses_sum*delt/num_TC;

	double q_charge = std::numeric_limits<double>::quiet_NaN();
	double q_discharge = std::numeric_limits<double>::quiet_NaN();
	double q_stored = std::numeric_limits<double>::quiet_NaN();
	double q_error = std::numeric_limits<double>::quiet_NaN();
	if( I_flow == 1)	// Charging
	{
		m_charge_avail = m_dot;		//[kg/hr]

		// Energy Balance Calculations
		q_charge = delt*m_dot*m_cp_a*(T_hot - T_charge_avail);		//[hr]*[kg/hr]*[kJ/kg-K]*[K]->[kJ] 
		q_stored = m_cap*(m_T_final_ave - m_T_final_ave_prev);		//[kJ/K]*[K]->[kJ]
		//[-] Relative difference. Scale by m_dot such that the losses don't create huge errors on timesteps with no mass flow rate
		q_error = (q_charge - q_stored - Q_loss_total)/max(0.01,fabs(q_stored));
	}
	else	// Discharging
	{
		m_disch_avail = m_dot;		//[kg/hr]

		// Energy balance calculations
		q_discharge = delt*m_dot*m_cp_a*(T_disch_avail - T_cold);		//[hr]*[kg/hr]*[kJ/kg-K]*[K]->[kJ]
		q_stored = m_cap*(m_T_final_ave - m_T_final_ave_prev);			//[kJ/K]*[K]->[kJ]
		q_error = (-q_stored - q_discharge - Q_loss_total)/max(0.01,fabs(q_stored));
	}

	double Q_htr_total = 0.0;
	for( int i = 0; i < num_TC; i++ )
		Q_htr_total += m_Q_htr[i];		//[kJ] Total energy required by heater during timestep

	m_dis_avail_tot = m_disch_avail * m_tank_pairs;		//[kg/hr]
	T_dis_avail_C = T_disch_avail;						//[C] Discharge temp (HOT)
	m_ch_avail_tot = m_charge_avail * m_tank_pairs;		//[kg/hr]
	T_ch_avail_C = T_charge_avail;						//[C] Charge temp (COLD)
	Q_dot_out_W = q_calc*m_tank_pairs;					//[W] Thermal power out of thermocline
	Q_dot_losses = Q_loss_total*m_tank_pairs;			//[kJ] Heat losses
	T_hot_bed_C = m_T_end[0];						//[C] Final temperature at hot node
	T_cold_bed_C = m_T_end[m_nodes-1];				//[C] Final temperature at cold node
	
	double T_max_bed = 0.0;
	for( int i = 0; i < m_nodes; i++ )
		T_max_bed = max( T_max_bed, m_T_end[i] );
	T_max_bed_C = T_max_bed;

	// Reset counters
	ihlim = 0;
	iclim = 0;

	for( int i = 0; i < m_nodes; i++ )
	{
		// Find the last node where the HTF temperature is above the hot side limit
		if( m_T_end[i] > m_T_hot_in_min )
			ihlim = i;
		if( m_T_end[m_nodes-1-i] < m_T_cold_in_max )
			iclim = i;
	}

	f_hot = (double) (ihlim+1)/ (double) m_nodes;				//[-] Fraction of depth at which hot temperature decreases below minimum hot temperature limit
	f_cold = (double) (m_nodes - iclim - 1) / (double) m_nodes;		//[-] Fraction of depth at which cold temperature increases above maximum cold temperature limit
	Q_dot_htr_kJ = Q_htr_total*m_tank_pairs;					//[kJ] Total energy required by heater to keep tank above minimum cold temperature

	// Set "saved" outputs for output-return method
	m_Q_dot_htr_kJ = Q_dot_htr_kJ;
	m_Q_dot_losses = Q_dot_losses;
	m_T_hot_node = T_hot_bed_C;
	m_T_cold_node = T_cold_bed_C;
	m_T_max = T_max_bed_C;
	m_f_hot = f_hot;
	m_f_cold = f_cold;

	return true;
}
