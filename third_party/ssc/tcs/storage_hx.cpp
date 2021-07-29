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

#include "storage_hx.h"
#include "htf_props.h"
#include "tcstype.h"

using namespace std;

Storage_HX::Storage_HX()
{  
	m_config = -1;
	m_vol_des = std::numeric_limits<double>::quiet_NaN();
	m_h_des = std::numeric_limits<double>::quiet_NaN();
	m_u_des = std::numeric_limits<double>::quiet_NaN();
	m_tank_pairs_des = std::numeric_limits<double>::quiet_NaN();
	m_Thtr_hot_des = std::numeric_limits<double>::quiet_NaN();
	m_Thtr_cold_des = std::numeric_limits<double>::quiet_NaN();
	m_dt_cold_des = std::numeric_limits<double>::quiet_NaN();
	m_dt_hot_des = std::numeric_limits<double>::quiet_NaN();
	m_eff_des = std::numeric_limits<double>::quiet_NaN(); 
	m_UA_des = std::numeric_limits<double>::quiet_NaN();
	m_a_cs = std::numeric_limits<double>::quiet_NaN();
	m_dia = std::numeric_limits<double>::quiet_NaN();
	m_ua = std::numeric_limits<double>::quiet_NaN();
	m_dot_des = std::numeric_limits<double>::quiet_NaN();
	m_max_q_htr_cold = m_max_q_htr_hot = std::numeric_limits<double>::quiet_NaN();
}

bool Storage_HX::define_storage( HTFProperties &fluid_field, HTFProperties &fluid_store, bool is_direct,
		int config, double duty_des, double vol_des, double h_des, 
		double u_des, double tank_pairs_des, double hot_htr_set_point_des, double cold_htr_set_point_des,
		double max_q_htr_cold, double max_q_htr_hot, double dt_hot_des, double dt_cold_des, double T_h_in_des, double T_h_out_des )
{	
	/* Author: Michael J. Wagner
	Converted from Fortran (sam_mw_trough_Type251) to c++ in November 2012 by Ty Neises	*/

	// Set storage tank dimensions as class data
	// For indirect tank, calculate heat exchanger sizing

	// Set member data
	m_field_htfProps = fluid_field;
	m_store_htfProps = fluid_store;
	m_config = config;
	m_vol_des = vol_des;
	m_h_des = h_des;
	m_u_des = u_des;
	m_tank_pairs_des = tank_pairs_des;
	m_Thtr_hot_des = hot_htr_set_point_des;
	m_Thtr_cold_des = cold_htr_set_point_des;
	m_dt_cold_des = dt_cold_des;
	m_dt_hot_des = dt_hot_des;
	m_max_q_htr_cold = max_q_htr_cold;
	m_max_q_htr_hot = max_q_htr_hot;

	// Geometric Calculations
	m_a_cs	= m_vol_des/(m_h_des*m_tank_pairs_des);		//[m2] Cross-sectional area of a single tank
	m_dia	= pow( (m_a_cs/CSP::pi), 0.5)*2.;				//[m] The diameter of a single tank
	
	// Calculate heat loss coefficient
	m_ua	= m_u_des*(m_a_cs + CSP::pi*m_dia*m_h_des)*m_tank_pairs_des;		//[W/K]

	if( is_direct )
		m_eff_des = m_UA_des = -1.2345;
	else
	{
		double q_trans = duty_des;					//[W] heat exchanger duty
		double T_ave = (T_h_in_des + T_h_out_des)/2.;	//[K] Average hot side temperature
		double c_h = m_field_htfProps.Cp(T_ave)*1000.;	//[J/kg-K] Specific heat of hot side fluid at hot side average temperature 
		double c_c = m_store_htfProps.Cp(T_ave)*1000.;	//[J/kg-K] Specific heat of cold side fluid at hot side average temperature (estimate, but should be close)
		// HX inlet and outlet temperatures
		double T_c_out = T_h_in_des - dt_hot_des;	//[K]
		double T_c_in = T_h_out_des - dt_cold_des;	//[K]
		// Mass flow rates
		double m_dot_h = q_trans/(c_h*(T_h_in_des - T_h_out_des));	//[kg/s]
		double m_dot_c = q_trans/(c_c*(T_c_out - T_c_in));	//[kg/s]
		m_dot_des = 0.5*(m_dot_h + m_dot_c);		//[kg/s] 7/9/14, twn: added
		// Capacitance rates
		double c_dot_h = m_dot_h * c_h;			//[W/K]
		double c_dot_c = m_dot_c * c_c;			//[W/K]
		double c_dot_max = max(c_dot_h, c_dot_c);	//[W/K]
		double c_dot_min = min(c_dot_h, c_dot_c);	//[W/K]
		double cr = c_dot_min / c_dot_max;			//[W/K]
		// Maximum possible energy flow rate
		double q_max = c_dot_min * (T_h_in_des - T_c_in);	//[W]
		// Effectiveness
		m_eff_des = q_trans/q_max;							//[-]

		// Check for realistic conditions
		if(cr > 1. || cr < 0.) {return false;}		// cr > 1 is a problem
		// xx = (1.-m_eff*cr)/(1.-m_eff);

		double NTU,ff,ee1,ee;
		switch( config )
		{
		case Counter_flow:				
			if(cr < 1.) 
			{
				NTU = log( (1.-m_eff_des*cr)/(1.-m_eff_des) ) / (1. - cr);
			}
			else
			{
				NTU = m_eff_des / (1. - m_eff_des);
			}
			break;
		case Parallel_flow:
			NTU = log(1.-m_eff_des*(1.+cr))/(1.+cr);
			break;
		case Cross_flow_unmixed:
			// This is actually the relationship for c_min unmixed, c_max mixed
			NTU = -log(1.+log(1.-m_eff_des*cr)/cr);
			break;
		case Shell_and_tube:
			ff = (m_eff_des*cr - 1.)/(m_eff_des-1.);
			ee1 = (ff-1.)/(ff-cr);
			ee = (2.-ee1*(1.+cr))/(ee1*pow( (1.+cr*cr),0.5 ));
			NTU = log( (ee+1.)/(ee-1.) ) / pow( (1.+cr*cr),0.5 );
			break;
		default:
			return false;
		}
		m_UA_des = NTU * c_dot_min;		//[W/K]
	}
	return true;
}

//bool Storage_HX::hx_size( HTFProperties &fluid_field, HTFProperties &fluid_store, 
//		int config, double duty_des, double vol_des, double h_des, 
//		double u_des, double tank_pairs_des, double hot_htr_set_point_des, double cold_htr_set_point_des,
//		double max_q_htr, double dt_hot_des, double dt_cold_des, double T_h_in_des, double T_h_out_des )
//{	
//	/* Author: Michael J. Wagner
//	Converted from Fortran (sam_mw_trough_Type251) to c++ in November 2012 by Ty Neises	*/
//
//	//Inputs: Hot fliud instance, cold fluid instance, heat exchanger configuration [-]
//	//			duty [W], dt_hot [K], dt_cold [K], T_h_in [K]
//	//Output: HX effectiveness [-], HX UA [W/K]
//
//	// Set member data
//	m_field_htfProps = fluid_field;
//	m_store_htfProps = fluid_store;
//	m_config = config;
//	m_vol_des = vol_des;
//	m_h_des = h_des;
//	m_u_des = u_des;
//	m_tank_pairs_des = tank_pairs_des;
//	m_Thtr_hot_des = hot_htr_set_point_des;
//	m_Thtr_cold_des = cold_htr_set_point_des;
//	m_dt_cold_des = dt_cold_des;
//	m_dt_hot_des = dt_hot_des;
//	m_max_q_htr = max_q_htr;
//
//	// Geometric Calculations
//	m_a_cs	= m_vol_des/(m_h_des*m_tank_pairs_des);		//[m2] Cross-sectional area of a single tank
//	m_dia	= pow( (m_a_cs/CSP::pi), 0.5)*2.;				//[m] The diameter of a single tank
//	
//	// Calculate heat loss coefficient
//	m_ua	= m_u_des*(m_a_cs + CSP::pi*m_dia*m_h_des)*m_tank_pairs_des;		// u [W/m2-K]
//
//	double q_trans = duty_des;					//[W] heat exchanger duty
//	double T_ave = (T_h_in_des + T_h_out_des)/2.;	//[K] Average hot side temperature
//	double c_h = m_field_htfProps.Cp(T_ave)*1000.;	//[J/kg-K] Specific heat of hot side fluid at hot side average temperature 
//	double c_c = m_store_htfProps.Cp(T_ave)*1000.;	//[J/kg-K] Specific heat of cold side fluid at hot side average temperature (estimate, but should be close)
//	// HX inlet and outlet temperatures
//	double T_c_out = T_h_in_des - dt_hot_des;	//[K]
//	double T_c_in = T_h_out_des - dt_cold_des;	//[K]
//	// Mass flow rates
//	double m_dot_h = q_trans/(c_h*(T_h_in_des - T_h_out_des));	//[kg/s]
//	double m_dot_c = q_trans/(c_c*(T_c_out - T_c_in));	//[kg/s]
//	// Capacitance rates
//	double c_dot_h = m_dot_h * c_h;			//[W/K]
//	double c_dot_c = m_dot_c * c_c;			//[W/K]
//	double c_dot_max = max(c_dot_h, c_dot_c);	//[W/K]
//	double c_dot_min = min(c_dot_h, c_dot_c);	//[W/K]
//	double cr = c_dot_min / c_dot_max;			//[W/K]
//	// Maximum possible energy flow rate
//	double q_max = c_dot_min * (T_h_in_des - T_c_in);	//[W]
//	// Effectiveness
//	m_eff_des = q_trans/q_max;							//[-]
//
//	// Check for realistic conditions
//	if(cr > 1. || cr < 0.) {return false;}		// cr > 1 is a problem
//	// xx = (1.-m_eff*cr)/(1.-m_eff);
//
//	double NTU,ff,ee1,ee;
//	switch( config )
//	{
//	case Counter_flow:				
//		if(cr < 1.) 
//		{
//			NTU = log( (1.-m_eff_des*cr)/(1.-m_eff_des) ) / (1. - cr);
//		}
//		else
//		{
//			NTU = m_eff_des / (1. - m_eff_des);
//		}
//		break;
//	case Parallel_flow:
//		NTU = log(1.-m_eff_des*(1.+cr))/(1.+cr);
//		break;
//	case Cross_flow_unmixed:
//		// This is actually the relationship for c_min unmixed, c_max mixed
//		NTU = -log(1.+log(1.-m_eff_des*cr)/cr);
//		break;
//	case Shell_and_tube:
//		ff = (m_eff_des*cr - 1.)/(m_eff_des-1.);
//		ee1 = (ff-1.)/(ff-cr);
//		ee = (2.-ee1*(1.+cr))/(ee1*pow( (1.+cr*cr),0.5 ));
//		NTU = log( (ee+1.)/(ee-1.) ) / pow( (1.+cr*cr),0.5 );
//		break;
//	default:
//		return false;
//	}
//	m_UA_des = NTU * c_dot_min;		//[W/K]
//	return true;
//}


bool Storage_HX::hx_perf_q_transfer(bool is_hot_side_mdot, bool is_storage_side, double T_hot_in, double m_dot_known, double T_cold_in, double &q_trans)
{
	double eff, T_hot_out, T_cold_out, m_dot_solved;
	eff = T_hot_out = T_cold_out = m_dot_solved = std::numeric_limits<double>::quiet_NaN();

	return hx_performance(is_hot_side_mdot, is_storage_side, T_hot_in, m_dot_known, T_cold_in, eff, T_hot_out, T_cold_out, q_trans, m_dot_solved);
}

bool Storage_HX::hx_performance( bool is_hot_side_mdot, bool is_storage_side,  double T_hot_in, double m_dot_known, double T_cold_in, 
							double &eff, double &T_hot_out, double &T_cold_out, double &q_trans, double &m_dot_solved )
{
	/* Author: Michael J. Wagner
	Converted from Fortran (sam_mw_trough_Type251) to c++ in November 2012 by Ty Neises
	This function combines "hx_perf" and "hx_reverse" fortran subroutines 
	7/19/14, twn: Modified performance calcs so UA is fixed, not deltaTs */


	// Subroutine for storage heat exchanger performance
	// Pass a flag to specify whether the known mass flow rate is the cold side or hot side. Return mass flow rate will be the other
	// Also pass a flag to specify whether the known mass flow rate is the storage or field htf.
	// Inputs: hot or cold side mass flow rate [kg/s], hot side inlet temp [K], cold side inlet temp [K]
	// Outputs: HX effectiveness [-], hot side outlet temp [K], cold side outlet temp [K], 
	//				Heat transfer between fluids [MWt], cold or hot side mass flow rate [kg/s]
	
	double m_dot_hot, m_dot_cold, c_hot, c_cold, c_dot;

	// T_hot_out	= T_cold_in + m_dt_cold_des;	//[K]
	// T_cold_out	= T_hot_in - m_dt_hot_des;		//[K]
	double T_ave	= (T_hot_in + T_cold_in)/2.;			//[K]

	if( is_hot_side_mdot )	//know hot side mass flow rate - assuming always know storage side and solving for field
	{
		if(is_storage_side)
		{
			c_cold	= m_field_htfProps.Cp( T_ave )*1000.;	//[J/kg-K]
			c_hot	= m_store_htfProps.Cp( T_ave )*1000.;	//[J/kg-K]
		}
		else
		{
			c_hot	= m_field_htfProps.Cp( T_ave )*1000.;	//[J/kg-K]
			c_cold	= m_store_htfProps.Cp( T_ave )*1000.;	//[J/kg-K]
		}
		
		m_dot_hot = m_dot_known;
		// Calculate flow capacitance of hot stream
		double c_dot_hot = m_dot_hot*c_hot;
		c_dot = c_dot_hot;
		// Choose a cold stream mass flow rate that results in c_dot_h = c_dot_c
		m_dot_cold = c_dot_hot / c_cold;
		// q_trans		= max( m_dot_hot*c_hot*(T_hot_in-T_hot_out), 0.0 );	//[W]
		// m_dot_cold	= q_trans / (c_cold*(T_cold_out - T_cold_in));		//[kg/s]
		m_dot_solved= m_dot_cold;
	}
	else						//know cold side mass flow rate - assuming always know storage side and solving for field
	{
		if(is_storage_side)
		{
			c_hot	= m_field_htfProps.Cp( T_ave )*1000.;	//[J/kg-K]
			c_cold	= m_store_htfProps.Cp( T_ave )*1000.;	//[J/kg-K]
		}
		else
		{
			c_cold	= m_field_htfProps.Cp( T_ave )*1000.;	//[J/kg-K]
			c_hot	= m_store_htfProps.Cp( T_ave )*1000.;	//[J/kg-K]
		}
		m_dot_cold	= m_dot_known;
		// Calculate flow capacitance of cold stream
		double c_dot_cold = m_dot_cold*c_cold;
		c_dot = c_dot_cold;
		// Choose a cold stream mass flow rate that results in c_dot_c = c_dot_h
		m_dot_hot = c_dot_cold / c_hot;
		// q_trans		= max( m_dot_cold*c_cold*(T_cold_out - T_cold_in), 1.e-6);	//[W]
		// m_dot_hot	= q_trans/(c_hot*(T_hot_in - T_hot_out));	//[kg/s]
		m_dot_solved= m_dot_hot;
	}

	// Scale UA
	double m_dot_od = 0.5*(m_dot_cold + m_dot_hot);
	double UA = m_UA_des*pow(m_dot_od / m_dot_des, 0.8);

	// Calculate effectiveness
	double NTU = UA / c_dot;
	eff = NTU / (1.0 + NTU);

	// Calculate heat transfer in HX
	double q_dot_max = c_dot*(T_hot_in - T_cold_in);
	q_trans = eff*q_dot_max;

	T_hot_out = T_hot_in - q_trans / c_dot;
	T_cold_out = T_cold_in + q_trans / c_dot;

	q_trans		= q_trans * 1.e-6;			//[MWt] 

	if( eff <= 0. || eff > 1. )		{return false;}
	else	{return true;}
}

bool Storage_HX::mixed_tank( bool is_hot_tank, double dt, double m_prev, double T_prev, double m_dot_in, double m_dot_out, 
							double T_in, double T_amb, double &T_ave, double &vol_ave, 
							double &q_loss, double &T_fin, double &vol_fin, double &m_fin, double &q_heater)
{

/* !Subroutine for storage tanks based on type230 - but rewritten by Mike
	Converted from Fortran (sam_mw_trough_Type251) to c++ in November 2012 by Ty Neises	*/

/**********************************************************************************
!*********************************************************************************
!** This function consists largely of TRNSYS code and a new, independent
!** function should replace the functionality of this code before code is publically released.
!*********************************************************************************
!*********************************************************************************
!----------Inputs and parameters-------------------------------------------------------------------
!   * m0        - [kg] total HTF mass in the tank at the end of the last time step
!   * T0        - [K] Temperature of the HTF at the end of the last time step
!   * m_dot_in  - [kg/s] mass flow rate of HTF into the tank
!   * m_dot_out - [kg/s] mass flow rate leaving the tank
!   * T_in      - [K] Temperature of the HTF entering the tank
!   * T_amb     - [K] Temperature of the ambient air
!----------Outputs---------------------------------------------------------------------------------
!   * T_ave     - [K] Average HTF temperature throughout the timestep
!   * vol_ave   - [m3] Average HTF volume level during the timestep
!   * q_loss    - [MWt] Total thermal loss rate from the tank
!   * T_fin     - [K] Temperature of the HTF at the end of the timestep
!   * vol_fin   - [m3] Volume of the HTF at the end of the timestep (total volume)
!   * m_fin     - [kg] total mass at the end of the timestep
!   * q_heater  - [MWt] Total energy consumed by the freeze protection heater
!--------------------------------------------------------------------------------------------------*/

	// Calculate conditions at last time step
	double rho	= m_store_htfProps.dens( T_prev, 1.0 );		//[kg/m^3] Density
	double cp	= m_store_htfProps.Cp( T_prev )*1000.0;		//[J/kg-K] Specific heat

	// Calculate ending volume levels
	m_fin = m_prev + dt*(m_dot_in - m_dot_out);	//[kg] Available mass at the end of the timestep
    double m_min, m_dot_out_adj;  // limit m_dot_out so the ending mass is above a given minimum to eliminate erratic behavior
    bool tank_is_empty = false;
    m_min = 0.001;                // minimum tank mass for use in the calculations
    if (m_fin < m_min) {
        m_fin = m_min;
        tank_is_empty = true;
        m_dot_out_adj = m_dot_in - (m_min - m_prev) / dt;
    }
    else {
        m_dot_out_adj = m_dot_out;
    }
	double m_ave	= (m_prev + m_fin)/2.0;	//[kg] Average mass 
	vol_fin	= m_fin/rho;					//[m3] Available volume at the end of the timestep
	vol_ave	= m_ave/rho;					//[m3] Average volume

    // Check for continual empty tank
    if (m_prev <= 1e-4 && tank_is_empty == true) {
        if (m_dot_in > 0) {
            T_fin = T_ave = T_in;
        }
        else {
            T_fin = T_ave = T_prev;
        }
        vol_ave = q_loss = vol_fin = m_fin = q_heater = 0.;
        return false;
    }

	// Check for no flow
	double B = m_dot_in + m_ua/cp;					//[kg/s] + [W/K]*[kg-K/J]
	double D, G, H1, A1, E, C, CC, DD, AA, BB;
	if( (fabs(m_dot_in-m_dot_out_adj) < B*1.e-5) ||
	   ( (m_dot_in < 0.001) && (m_dot_out_adj < 0.001) ))
	{
		// Equations for no flow or zero net flow
		D	= m_dot_in*T_in + (m_ua/cp)*T_amb;
		G	= -B/m_prev;
		H1	= 1.0/(dt*(-B));
		A1	= D - B*T_prev;
		E	= A1 * exp(dt*G);
		T_fin	= (E-D)/(-B);
		T_ave	= H1*((E-A1)/G)+D/B;
	}
	else
	{
		// Equations for unbalanced flow
		C	= m_dot_in - m_dot_out_adj;
		D	= m_dot_in*T_in + m_ua/cp*T_amb;
		CC	= T_prev - D/B;
		DD	= pow( max( (1.+(C*dt)/m_prev), 0.0), (-B/C));	// MJW 9.2.2010 :: limit to positive argument
		T_fin	= CC*DD+D/B;
		AA	= (T_prev - D/B)/(C-B);
		BB	= pow( max( (1.+(C*dt)/m_prev), 0.0), (1.-B/C) );
		T_ave	= AA*(m_prev/dt)*(BB-1.0) + D/B;
	}

	// If the temperature of the fluid in the tank is less than the 
	// aux heater set point, heat the fluid in - and passing through the tank. MJW
	double htr_set_point, max_q_htr, Q_vol, Q_flow;
	
	if( is_hot_tank )
	{
		htr_set_point = m_Thtr_hot_des;
		max_q_htr = m_max_q_htr_hot;
	}
	else
	{
		htr_set_point = m_Thtr_cold_des;
		max_q_htr = m_max_q_htr_cold;
	}

	if( T_fin < htr_set_point)
	{
		Q_vol	= cp*vol_fin*rho/dt*(htr_set_point-T_fin)/(1.E6);	// MW  4/30/12 - Fixed unit conversion
		Q_flow	= cp*m_dot_out_adj*(htr_set_point-T_fin)/(1.E6);		// MW  4/30/12 - Fixed unit conversion
    
		q_heater = min(Q_flow+Q_vol, max_q_htr);					// MW
		T_fin	= T_prev + dt * min(Q_vol*1.e6, max_q_htr*1.e6)/(cp*rho*vol_fin);
		T_ave	=(T_fin + T_prev)/2.;
	}
	else
	{q_heater = 0.;}

	// Calculate losses
	q_loss = m_ua*(T_ave - T_amb)/1.e6;		// [MW]

    if (tank_is_empty) {
        // set to actual values
        vol_fin = 0.;
        m_fin = 0.;
    }

	return false;
}
