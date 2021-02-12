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

#include "sco2_rec_util.h"
#include <math.h>
#include <limits>
#include <algorithm>
#include "sam_csp_util.h"
//#include "co2props.h"
//#include "co2props_nn.h"
#include "CO2_properties.h"

using namespace std;

N_sco2_rec::C_rec_des_props::C_rec_des_props(int material)
{
	m_material = material;
}

double N_sco2_rec::C_rec_des_props::alpha_inst(double T_C)
{
	// Instantaneous thermal expansion coefficient
	// mm/km-C

	switch( m_material )
	{
	case Haynes_230:	// Page 718, BPVC: Section II
		return 12.2619521 + 0.00647096736*T_C - 0.0000234157719*pow(T_C,2) + 
			1.50217826E-7*pow(T_C,3) - 2.83989121E-10*pow(T_C,4) + 1.67497618E-13*pow(T_C,5);
	
	default:
		return std::numeric_limits<double>::quiet_NaN();
	}

	return std::numeric_limits<double>::quiet_NaN();
}

double N_sco2_rec::C_rec_des_props::modE(double T_C)
{
	// Moduli of elasticity
	// GPa

	switch( m_material )
	{
	case Haynes_230:	// Page 742, BPVC: Section II
		return 212.258813 - 0.063305782*T_C + 0.0000298956743*pow(T_C,2) - 4.27361456E-8*pow(T_C,3);

	default:
		return std::numeric_limits<double>::quiet_NaN();
	}

	return std::numeric_limits<double>::quiet_NaN();
}

double N_sco2_rec::C_rec_des_props::cond(double T_C)
{
	// Thermal conductivity
	// W/m-K

	switch( m_material )
	{
	case Haynes_230:
		//return 9.22094197 + 0.0182432303*T_C - 0.0000105468774*pow(T_C,2) + 8.74157228E-9*pow(T_C,3);	// This is Haynes 625 data from somewhere
		return 8.4 + 0.02*T_C;

	default:
		return std::numeric_limits<double>::quiet_NaN();
	}

	return std::numeric_limits<double>::quiet_NaN();
}

double N_sco2_rec::C_rec_des_props::poisson()
{
	switch( m_material )
	{
	case Haynes_230:
		return 0.31;
	}
	return -999.9;
}

double N_sco2_rec::C_rec_des_props::creep_life(double sigma_MPa, double T_C)
{
	double T_F = (9.0/5.0)*T_C + 32.0;				// Convert to F
	double sigma_ksi = 0.145 * sigma_MPa;			// Convert to ksi

	//double time_hours = std::numeric_limits<double>::quiet_NaN();

	switch( m_material )
	{
	case Haynes_230:	// Data from Haynes
		
		double T_start = haynes230_enum_creep_temps(T_1050F);
		double T_end = haynes230_enum_creep_temps(T_1800F);

		if( T_F <= T_start )
			return haynes230_creep_life(T_1050F, sigma_ksi);
		else if( T_F >= T_end )
			return haynes230_creep_life(T_1800F, sigma_ksi);
		else
		{
			for( int temps_int = T_1050F; temps_int != T_1800F; temps_int++ )
			{
				double T_high = haynes230_enum_creep_temps(temps_int + 1);
				if(T_F < T_high)
					return interpolate_creep_life(temps_int, temps_int+1, T_F, sigma_ksi);
			}
		}
	}

	return -999.9;
}

double N_sco2_rec::C_rec_des_props::interpolate_creep_life(int enum_T_low, int enum_T_high, double T_F, double sigma_ksi)
{
	double t_low = haynes230_creep_life(enum_T_low, sigma_ksi);
	double t_high = haynes230_creep_life(enum_T_high, sigma_ksi);
	double T_F_low = haynes230_enum_creep_temps(enum_T_low);
	double T_F_high = haynes230_enum_creep_temps(enum_T_high);
	
	return pow(10, (T_F - T_F_low) / (T_F_high - T_F_low)*log10(t_high) + (T_F_high - T_F) / (T_F_high - T_F_low)*log10(t_low));
}

double N_sco2_rec::C_rec_des_props::haynes230_enum_creep_temps(int enum_T_F)
{
	switch( enum_T_F )
	{
	case T_1050F:
		return 1050.0;
	case T_1100F:
		return 1100.0;
	case T_1200F:
		return 1200.0;
	case T_1300F:
		return 1300.0;
	case T_1400F:
		return 1400.0;
	case T_1500F:
		return 1500.0;
	case T_1600F:
		return 1600.0;
	case T_1700F:
		return 1700.0;
	case T_1800F:
		return 1800.0;
	default:
		return 0;
	}
}

double N_sco2_rec::C_rec_des_props::haynes230_creep_life(int enum_T_F, double sigma_ksi)
{
	switch(enum_T_F)
	{
	case T_1050F:
		return 1.E8;

	case T_1100F:
		// This is the one call for Haynes creep life that uses MPa units for stress
		// Equation is from Katcher paper
		{
		double sigma_MPa = 6.8948*sigma_ksi;
		if( sigma_MPa > 100.0 )
			return min(1.E8, exp(-18.073*log(sigma_MPa) + 117.495));
		else
			return 1.E8;
		}

	case T_1200F:
		// Equation taken from curve fit in creep life rupture data charts from Haynes
		return min(1.E8, pow(10, -7.3368*log10(sigma_ksi) + 14.8349));

	case T_1300F:
		// Equation taken from curve fit in creep life rupture data charts from Haynes
		return min(1.E8, pow(10, -6.8634*log10(sigma_ksi) + 13.1366));

	case T_1400F:
		// Equation taken from curve fit in creep life rupture data charts from Haynes
		return min(1.E8, pow(10, -7.6453*log10(sigma_ksi) + 12.9472));

	case T_1500F:
		// Equation taken from curve fit in creep life rupture data charts from Haynes
		return min(1.E8, pow(10, -7.2307*log10(sigma_ksi) + 11.2307));

	case T_1600F:
		// Equation taken from curve fit in creep life rupture data charts from Haynes
		return min(1.E8, pow(10, -6.2657*log10(sigma_ksi) + 9.0733));

	case T_1700F:
		// Equation taken from curve fit in creep life rupture data charts from Haynes
		return min(1.E8, pow(10, -4.5434*log10(sigma_ksi) + 6.5797));

	case T_1800F:
		// Equation taken from curve fit in creep life rupture data charts from Haynes
		return min(1.E8, pow(10, -3.7908*log10(sigma_ksi) + 4.9022));

	default:
		return -999;
	}

}

double N_sco2_rec::C_rec_des_props::haynes230_enum_cycle_temps(int enum_T_C)
{
	switch( enum_T_C )
	{
	case T_427C:
		return 427.0;
	case T_538C:
		return 538.0;
	case T_649C:
		return 649.0;
	case T_760C:
		return 760.0;
	case T_871C:
		return 871.0;
	case T_982C:
		return 982.0;
	default:
		return 0;
	}
}

double N_sco2_rec::C_rec_des_props::cycles_to_failure(double epsilon_equiv, double T_C)
{
	switch( m_material )
	{
	case Haynes_230:
		double T_start = haynes230_enum_cycle_temps(T_427C);
		double T_end = haynes230_enum_cycle_temps(T_982C);

		if( T_C <= T_start )
		{
			if( epsilon_equiv < haynes230_eps_min(T_427C) )
				return 100000.0;
			else
				return haynes230_cycles_to_failure(T_427C, epsilon_equiv);
		}
		else if( T_C >= T_end )
		{
			if( epsilon_equiv < haynes230_eps_min(T_982C) )
				return 100000.0;
			else
				return haynes230_cycles_to_failure(T_982C, epsilon_equiv);
		}
		else
		{
			for( int temps_int = T_427C; temps_int != T_982C; temps_int++ )
			{
				double T_high = haynes230_enum_cycle_temps(temps_int + 1);
				if( T_C < T_high )
				{
					if( epsilon_equiv < haynes230_eps_min(temps_int + 1) )
						return 100000.0;
					else
						return interpolate_cycles_to_failure(temps_int, temps_int + 1, T_C, epsilon_equiv);
				}
			}
		}
	}

	return -999.0;
}

double N_sco2_rec::C_rec_des_props::haynes230_eps_min(int enum_T_C)
{
	switch( enum_T_C )
	{
	case T_427C:
		return 0.55;
	case T_538C:
		return 0.52;
	case T_649C:
		return 0.45;
	case T_760C:
		return 0.38;
	case T_871C:
		return 0.29;
	case T_982C:
		return 0.27;
	default:
		return -999.9;
	}
}

double N_sco2_rec::C_rec_des_props::haynes230_cycles_to_failure(int enum_T_C, double eps_equiv)
{
	double OF_E, b, e_f, c;
	OF_E = b = e_f = c = 0.0;

	switch( enum_T_C )
	{
	case T_427C:
		OF_E = 0.2;
		b    = 0.01;
		e_f  = 18.0;
		c    = 0.45;
		break;
	case T_538C:
		OF_E = 0.2;
		b    = 0.0005;
		e_f  = 45.0;
		c    = 0.60;
		break;
	case T_649C:
		OF_E = 0.2;
		b    = 0.001;
		e_f  = 45.0;
		c    = 0.65;
		break;
	case T_760C:
		OF_E = 0.2;
		b    = 0.02;
		e_f  = 45.0;
		c    = 0.70;
		break;
	case T_871C:
		OF_E = 0.15;
		b    = 0.02;
		e_f  = 12.0;
		c    = 0.55;
		break;
	case T_982C:
		OF_E = 0.22;
		b    = 0.05;
		e_f  = 45.0;
		c    = 0.80;	
		break;
	default:
		return -999.9;
	}

	double N_low_baseline = 1.0;
	double N_low = N_low_baseline;
	double N_high_baseline = 300000.0;
	double N_high = N_high_baseline;

	double eps_guess = 2*(OF_E*pow(N_high,-b)+e_f*pow(N_high,-c));

	double N_allowable = 0.0;
	// 6/11/14, twn: the choice of N_high_baseline is somewhat arbritary here, as the eps_min is used
	// such that strains resulting in very large N_cycles are not sampled
	if( eps_guess > eps_equiv )		// if eps_guess(N_max) > eps_in, then N(eps_in) is > N_max, so set to N_max and get out
		return N_high;
	else
	{
		int counter = 0;
		do
		{
			counter++;
			N_allowable = pow(10, 0.5*log10(N_low) + 0.5*log10(N_high));
			eps_guess = 2 * (OF_E*pow(N_allowable, -b) + e_f*pow(N_allowable, -c));
			double eps_err = (eps_guess - eps_equiv) / eps_equiv;
			if( fabs(eps_err) < 1.E-8 )
				return N_allowable;
			else
			{
				if( eps_err > 0.0 )
					N_low = N_allowable;
				else
					N_high = N_allowable;
			}
			if( counter > 100 )
				return -999.9;
		} while( true );
	}
}

double N_sco2_rec::C_rec_des_props::interpolate_cycles_to_failure(int enum_T_low, int enum_T_high, double T_C, double eps_equiv)
{
	double N_low = haynes230_cycles_to_failure(enum_T_low, eps_equiv);
	double N_high = haynes230_cycles_to_failure(enum_T_high, eps_equiv);
	double T_C_low = haynes230_enum_cycle_temps(enum_T_low);
	double T_C_high = haynes230_enum_cycle_temps(enum_T_high);

	return pow(10.0, (T_C - T_C_low)/(T_C_high-T_C_low)*log10(N_high) + (T_C_high - T_C)/(T_C_high - T_C_low)*log10(N_low) );
}

//******************************************************
//     Class: C_calc_tube_min_thickness Functions
//******************************************************

// Constructor
N_sco2_rec::C_calc_tube_min_th::C_calc_tube_min_th()
{
	m_d_out = m_T_fluid_in = m_T_fluid_out = m_P_fluid_in = m_L_tube = m_d_in 
        = m_L_node = m_m_dot_tube = m_deltaP_kPa = m_max_damage = std::numeric_limits<double>::quiet_NaN();

	m_e_roughness = 4.5E-5;

	m_n_tube_elements = m_n_temps = m_n_results_cols - 1;

	m_know_T_out = true;

    m_th_min_guess = 0.0002;
    m_th_step = 0.00005;
    m_max_deltaP_frac = 0.2;
    m_iter_d_in_max = 9999;

};

double N_sco2_rec::C_calc_tube_min_th::get_min_d_in()
{
	return m_d_in;
}

double N_sco2_rec::C_calc_tube_min_th::get_m_dot_tube_kgsec()
{
	return m_m_dot_tube;
}

double N_sco2_rec::C_calc_tube_min_th::get_T_out_C()
{
	return m_T_fluid_out;
}

double N_sco2_rec::C_calc_tube_min_th::get_deltaP_kPa()
{
	return m_deltaP_kPa;
}

double N_sco2_rec::C_calc_tube_min_th::get_max_damage(){
    return m_max_damage;
}

vector<double> N_sco2_rec::C_calc_tube_min_th::get_max_damage_matrix()
{
    /* 
    Return a copy of the array containing the max damage at each axial position
    */
    vector<double> d;
    int nr = (int)m_total_damage.nrows();
    int nc = (int)m_total_damage.ncols();
    d.reserve(nr);
    for(int i=0; i<nr; i++)
    {
        double dmax =0.;
        for(int j=0; j<nc; j++)
            dmax = max(dmax, m_total_damage.at(i,j) );
        d.push_back(dmax);
    }

    return d;
}

util::matrix_t<double> *N_sco2_rec::C_calc_tube_min_th::get_damage_matrix()
{
    return &m_total_damage;
}

void N_sco2_rec::C_calc_tube_min_th::get_damage_matrix(vector<vector<double> > &damage){
    //resize
    int nr = (int)m_total_damage.nrows();
    int nc = (int)m_total_damage.ncols();
    damage.resize(nr, vector<double>(nc));
    for(int i=0; i<nr; i++)
        for(int j=0; j<nc; j++)
        damage.at(i).at(j) = m_total_damage.at(i,j);
}

vector<double> *N_sco2_rec::C_calc_tube_min_th::get_fluid_temp_matrix()
{
    return &m_Temp;
}

vector<double> *N_sco2_rec::C_calc_tube_min_th::get_surface_temp_matrix()
{
    return &m_Tsurf;
}

vector<double> *N_sco2_rec::C_calc_tube_min_th::get_fluid_pres_matrix()
{
    return &m_Pres;
}


bool N_sco2_rec::C_calc_tube_min_th::calc_th_flux_Tout(const vector<vector<double> > &flux_Wm2, double L_tube_m,
	double d_out_m,
	double T_fluid_in_C, double T_fluid_out_C, double P_fluid_in_MPa)
{
	/* Calculate tube thickness when a 1D flux profile and the fluid outlet temperature are known */
	return calc_th_flux(flux_Wm2, L_tube_m, d_out_m, T_fluid_in_C, T_fluid_out_C, P_fluid_in_MPa, -999.0, true);
}

bool N_sco2_rec::C_calc_tube_min_th::calc_th_flux_mdot(const vector<vector<double> > &flux_Wm2, double L_tube_m,
	double d_out_m, double T_fluid_in_C, double P_fluid_in_MPa, double m_dot_tube_kgs)
{
	/* Calculate tube thickness when a 1D flux profile and the mass flow rate are known */
	return calc_th_flux(flux_Wm2, L_tube_m, d_out_m, T_fluid_in_C, -999.9, P_fluid_in_MPa, m_dot_tube_kgs, false);
}

bool N_sco2_rec::C_calc_tube_min_th::calc_perf_flux_mdot(const vector<vector<double> > &flux_Wm2, double L_tube_m,
		double d_out_m, double th_m, double T_fluid_in_C, double P_fluid_in_MPa, double m_dot_tube_kgs)
{
    int last_iter_max = m_iter_d_in_max;    //save the current max iteration setting
    double last_th_min = m_th_min_guess;    //save the current min thickness

    m_iter_d_in_max = 1;    //Don't allow iteration on thickness
    m_th_min_guess = th_m;  //use the specified thickness
    
    bool simok = calc_th_flux(flux_Wm2, L_tube_m, d_out_m, T_fluid_in_C, -999.9, P_fluid_in_MPa, m_dot_tube_kgs, false);
    
    //return member values to original
    m_iter_d_in_max = last_iter_max;
    m_th_min_guess = last_th_min;

    return simok;
}

bool N_sco2_rec::C_calc_tube_min_th::calc_th_flux(const vector<vector<double> > &flux_Wm2, 
    double L_tube_m, double d_out_m, double T_fluid_in_C, double T_fluid_out_C, double P_fluid_in_MPa, 
    double m_dot_tube, bool know_Tout)
{
	// Initialize member data
	m_d_out = d_out_m;					//[m]
	m_T_fluid_in = T_fluid_in_C;		//[C]
	m_T_fluid_out = T_fluid_out_C;		//[C]
	m_P_fluid_in = P_fluid_in_MPa;		//[MPa]
	m_L_tube = L_tube_m;				//[m]
	m_m_dot_tube = m_dot_tube;			//[kg/s] ?? MJW 10-6-14
	m_know_T_out = know_Tout;			//[-]

	// Could check T_out > T_in and that pressure is in MPa...

	m_flux_array = flux_Wm2;	//[W/m2]
	m_n_tube_elements = (int)m_flux_array.size();

    //Calculate average and max flux on each axial tube element
	m_q_abs_array.resize(m_n_tube_elements);
    m_q_max_array.resize(m_n_tube_elements);

	m_L_node = m_L_tube / (double)m_n_tube_elements;

	for( int i = 0; i < m_n_tube_elements; i++ )
    {
        double qmax=0.;
        double qave=0.;
        int ncirc = (int)m_flux_array.at(i).size();
        for(int j=0; j<ncirc; j++)
        {
            double thisflux = m_flux_array.at(i).at(j);
            qmax = qmax > thisflux ? qmax : thisflux;
            qave += thisflux;
        }
        qave /= (double)ncirc;
        m_q_abs_array.at(i) = qave * 2. * m_d_out * m_L_node;       //W
        m_q_max_array.at(i) = qmax;     //W/m2      maximum flux on the current axial node
    }

	return calc_min_thick_general();
}

bool N_sco2_rec::C_calc_tube_min_th::calc_min_thick_general()
{
	// Number of temperature nodes is 1 + number of flux elements
	m_n_temps = m_n_tube_elements + 1;

	// Size temperature, pressure, and enthalpy vectors: nodes
	m_Temp.resize(m_n_temps);
	m_Pres.resize(m_n_temps);
	m_Enth.resize(m_n_temps);
    m_Tsurf.resize(m_n_temps);

	// Size convective and length vectors: elemental
	m_h_conv_ave.resize(m_n_tube_elements);	

	// Calculate total absorbed flux
	//double A_surf_per_node = m_d_out*CSP::pi*m_L_node;			//[m^2] Total tube surface area
	double q_abs_total = 0.0;
	for( int i = 0; i < m_n_tube_elements; i++ )
	{
		q_abs_total += m_q_abs_array[i];			//[W]
	}

	// Set first value in nodal vectors
	m_Temp[0] = m_T_fluid_in;
    m_Tsurf[0] = m_T_fluid_in;  //just set as fluid temp without other info.
	m_Pres[0] = m_P_fluid_in*1000.0;		//[kPa]
	CO2_TP(m_Temp[0] + 273.15, m_Pres[0], &co2_props);
	m_Enth[0] = co2_props.enth*1000.0;				//[J/kg], convert from [kJ/kg]

	// Set up Class and Structures for creep-fatigue lifetime model
	N_sco2_rec::C_tube_slice      tube_slice(N_sco2_rec::C_rec_des_props::Haynes_230);
	N_sco2_rec::C_tube_slice::S_ID_OD_perf_and_lifetime_inputs     tube_inputs;
	N_sco2_rec::C_tube_slice::S_ID_OD_perf_and_lifetime_outputs      tube_outputs;

	// Set up parameters for minimum thickness search
	// May want to make these inputs at some point
	double th_min_guess = m_th_min_guess;	// Smallest possible thickness = 1 mm
	double th_step = m_th_step;		// Increase to find next thickness to try
	double max_deltaP_frac = m_max_deltaP_frac;	// Largest allowable fractional pressure drop through tubes

	// Set up numerical control parameters
	bool search_min_th = true;				// Will be set to false when/if min thickness is found
	bool is_deltaP_too_large = false;		// When thickness results in large pressure drop, this will = true and stop iteration
	double P_tube_out_prev = m_Pres[0];		// Set to some realistic value so iterative part of code can guess an outlet pressure
	double P_tube_out_min = (1.0-max_deltaP_frac)*m_Pres[0];	// Convert relative max pressure drop to absolute minimum pressure
	int iter_d_in = -1;						// Count (starting at 0) d_in (= thickness) iterations

	// Declare values that will be calculated in iterative loops
	if( m_know_T_out )
		m_m_dot_tube = std::numeric_limits<double>::quiet_NaN();
	else
		m_T_fluid_out = std::numeric_limits<double>::quiet_NaN();

	m_d_in = std::numeric_limits<double>::quiet_NaN();
	
	// Initialize 'matrix_t' for all 2D results (# elements x # of diameters evaluated)
	// Also initializes 'vector' results
	initialize_all_output_columns();


	do	// Outer loop: iterates until min thickness is found or broken because pressure drop is exceeded
	{
        iter_d_in++;
		
        m_d_in = m_d_out - 2.0*(th_min_guess + th_step*iter_d_in);	//[m] Inner diameter

		double A_cs = 0.25*CSP::pi*pow(m_d_in, 2);		//[m2] flow cross-section area

		double relRough = m_e_roughness / m_d_in;		//[-] Relative roughness

		// Guess tube outlet pressure
		double P_tube_out_guess = 0.95*P_tube_out_prev;
		// Set up iteration parameters
		double P_tube_out_tolerance = 0.001;			//[-] Solve outlet pressure to this tolerance
		double P_tube_out_diff = 2.0*P_tube_out_tolerance;	//[-] Set initial difference to larger than tolerance 
		double P_tube_guess_high = P_tube_out_prev;		// [kPa] Increasing thickness, so pressure drop must be greater than previous case with more A_cs
		double P_tube_guess_low = -999.9;				// Must be negative value for decision logic to work properly

		int iter_P_tube = 0;		//[-] Count iterations starting at 1

		do	// Solve for correct mass flow rate given pressure drops through tube
		{
			iter_P_tube++;	

			if( iter_P_tube > 1 )	// Guess new outlet pressure
			{
				if( P_tube_out_diff > 0.0 )	// Calculated P_tube_Out > Guessed P_tube_out
				{
					P_tube_guess_low = P_tube_out_guess;	// Reset lower bounds
					P_tube_out_guess = 0.5*(P_tube_guess_low + P_tube_guess_high);	// Use bisection method
				}
				else						// Calculated P_tube_out < Guessed P_tube_out
				{
					P_tube_guess_high = P_tube_out_guess;	// Reset upper bounds
					if( P_tube_guess_low < 0.0 )	// If no lower bounds, just decrease by fraction
						P_tube_out_guess = 0.95*m_Pres[m_n_temps - 1];
					else		// If lower bounds defined, use bisection method
						P_tube_out_guess = 0.5*(P_tube_guess_low + P_tube_guess_high);
				}
				if( P_tube_guess_high <= P_tube_out_min )
				{
					// If highest possible outlet pressure is <= minimum allowable then set flag and get out
					is_deltaP_too_large = true;
					break;
				}
			}

			// If guess is less than minimum, set to minimum
			if( P_tube_out_guess < P_tube_out_min )
				P_tube_out_guess = P_tube_out_min;

			if(m_know_T_out)	// Know outlet temperature - calculate mass flow rate
			{
				// Get co2 props at tube outlet
				int ret = CO2_TP(m_T_fluid_out + 273.15, P_tube_out_guess, &co2_props);
                if(ret != 0)
                    throw sco2_exception( CO2_error_message(ret) );

				double h_tube_out = co2_props.enth*1000.0;

				// Energy balance to calculate mass flow rate
				m_m_dot_tube = q_abs_total / (h_tube_out - m_Enth[0]);
			}
			else	// Know mass flow rate - calculate outlet temperature
			{
				double h_tube_out = q_abs_total/m_m_dot_tube + m_Enth[0];
                int ret = CO2_PH(P_tube_out_guess, h_tube_out/1000.0, &co2_props);
                if(ret != 0 )
                    throw sco2_exception( CO2_error_message(ret) );

				m_T_fluid_out = co2_props.temp - 273.15;
			}

			// Set up iteration constants for converging on local pressure
			double P_node_out_tolerance = P_tube_out_tolerance;		// Set nodal pressure convergence tolerance to tube's
			double P_node_out_diff = 2.0*P_node_out_tolerance;		// Set initial difference to larger than tolerance
			bool is_P_out_too_low = false;		// Flag to catch outlet pressures below minimum allowable

			for( int i = 1; i < m_n_temps; i++ )
			{
				double P_node_out_guess = -999.9;	// Must be negative value for decision logic to work properly
				
				// Guess outlet pressure of element
				// Try to guess a bit low to set lower bound
				if( i == 1 )	// Evaluating first element, so using pressure drop guess for whole tube
					P_node_out_guess = m_Pres[0] - (double)i/(double)m_n_temps*(m_Pres[0] - P_tube_out_guess);
				else			// Guess based on pressure drop from previous element
					P_node_out_guess = m_Pres[i-1] - 1.25*(m_Pres[i-1] - m_Pres[i-2]);

				double P_guess_high = m_Pres[i - 1];	//[kPa] Upper guess is always pressure at previous node
				double P_guess_low = -999.9;		//[kPa] If negative then it's a flag that lower guess hasn't been calculated

				int iter_P_local = 0;		// Track iterations

				do	// Converge local pressure
				{
					iter_P_local++;

					if( iter_P_local > 1 )		// Reguess P_node_out_guess until convergence
					{
						if( P_node_out_diff > 0.0 )	// Calculated P_out > Guessed P_out
						{
							P_guess_low = m_Pres[i];
							P_node_out_guess = 0.5*(P_guess_high + P_guess_low);
						}
						else						// Calculated P_out < Guessed P_out
						{
							P_guess_high = m_Pres[i];
							if( P_guess_low < 0.0 )		// Lower bound hasn't been reached
								P_node_out_guess = 0.95*m_Pres[i];
							else
								P_node_out_guess = 0.5*(P_guess_high + P_guess_low);
						}

						if( P_guess_high <= P_tube_out_min )
						{
							// If upper bound on pressure guess is <= minimum allowable pressure
							// Then set flag and get out of local iteration loop
							is_P_out_too_low = true;
							break;
						}
					}

					// If guess is less than minimum, set to minimum
					if( P_node_out_guess < P_tube_out_min )
						P_node_out_guess = P_tube_out_min;

					// Calculate outlet enthalpy
					//h_out = h_in + q_abs/m_dot
					m_Enth[i] = m_Enth[i - 1] + m_q_abs_array[i - 1] / m_m_dot_tube;

					// Know enthalpy and guessed pressure, so get props
					// ***
					// Check and catch errors in property calls
                    {
                        int ret = CO2_PH(P_node_out_guess, m_Enth[i] / 1000.0, &co2_props);
                        if(ret != 0)
                            throw sco2_exception( CO2_error_message(ret) );
                    }

					m_Temp[i] = co2_props.temp - 273.15;		//[C], convert from K

					// Calculate friction factor, Reynolds number, nusselt number and h_conv at average nodal P and h
					double P_ave = 0.5*(P_node_out_guess + m_Pres[i - 1]);
					double h_ave = 0.5*(m_Enth[i] + m_Enth[i - 1]);

					// Properties at midpoint of element
					int ret = CO2_PH(P_ave, h_ave / 1000.0, &co2_props);
                    if(ret != 0)
                        throw sco2_exception( CO2_error_message(ret) );

					double visc_dyn = CO2_visc(co2_props.dens, co2_props.temp)*1.E-6;
					double Re = m_m_dot_tube*m_d_in / (A_cs*visc_dyn);

					double rho = co2_props.dens;
					double visc_kin = visc_dyn / rho;
					double cond = CO2_cond(co2_props.dens, co2_props.temp);
					double specheat = co2_props.cp*1000.0;
					double alpha = cond / (specheat*rho);
					double Pr = visc_kin / alpha;

					double Nusselt = -999.9;
					double f = -999.9;

					// Specifying the length over diameter = 1000 sets the problem as Fully Developed Flow
					CSP::PipeFlow(Re, Pr, 1000.0, relRough, Nusselt, f);

					m_h_conv_ave[i - 1] = Nusselt*cond / m_d_in;

					double u_m = m_m_dot_tube / (rho*A_cs);
					m_Pres[i] = m_Pres[i - 1] - f*m_L_node*rho*pow(u_m, 2) / (2.0*m_d_in) / 1000.0;

					P_node_out_diff = (m_Pres[i] - P_node_out_guess) / P_node_out_guess;

				} while( fabs(P_node_out_diff)>P_node_out_tolerance );
				// End iteration on local node pressure

				// Pressure drops in one of the nodes was too large, so set outlet pressure to minimum and break out of loop iteration
				if( is_P_out_too_low )
				{
					m_Pres[m_n_temps - 1] = 0.9*P_tube_out_guess;		// Ensures P_tube_out_diff is negative
					break;
				}

			}
			// End local node pressure iterations for series of nodes in tube

			P_tube_out_diff = (m_Pres[m_n_temps - 1] - P_tube_out_guess) / P_tube_out_guess;
			P_tube_out_prev = m_Pres[m_n_temps - 1];

		} while( fabs(P_tube_out_diff)>P_tube_out_tolerance );
		// End iteration on tube outlet pressure and dependent mass flow rate calculation

		// Set pressure and thickness result vectors here
		// Add element if first element has been defined
		if( m_P_fluid_out[0] == m_P_fluid_out[0] )
			push_back_all_vectors();

		m_P_fluid_out[m_n_vector_results - 1] = m_Pres[m_n_temps - 1];

		// If pressure drop is too large, then increasing thickness will further increase pressure drop
		// So get out of tube thickness iteration
		if( is_deltaP_too_large )
			break;

		// Have solved for correct mass flow rate given tube thickness, outlet temp, and flux
		// Now solve for the creep-fatigue lifetime at this thickness

		double total_damage = 0.0;

		// Add column for new data in matrix_t output results
		// Don't need to add column for first thickness iteration
		// Test by checking if very fist element is filled
		if( m_total_damage(1, 0) == m_total_damage(1, 0) )
			add_all_output_columns();

		for( int i = 1; i < m_n_temps; i++ )
		{
			// Define input structure for performance and lifetime model
			tube_inputs.m_P_internal = m_Pres[0] / 1.E3;	//[MPa] Constant: always max pressure
			tube_inputs.m_T_fluid = m_Temp[i];
			tube_inputs.m_d_out = m_d_out;				// Constant
			tube_inputs.m_d_in = m_d_in;					// Constant
			tube_inputs.m_flux = m_q_max_array[i-1];
			tube_inputs.m_h_conv = m_h_conv_ave[i-1];

			tube_slice.calc_ID_OD_perf_and_lifetime(tube_inputs, tube_outputs);

			double inner_total_damage = tube_outputs.s_ID_lifetime_outputs.m_total_damage;
			double outer_total_damage = tube_outputs.s_OD_lifetime_outputs.m_total_damage;
			total_damage = max(total_damage, max(inner_total_damage, outer_total_damage));

			m_total_damage(i-1,m_n_results_cols-1) =  max(inner_total_damage, outer_total_damage);

            //mjw
            m_Tsurf.at(i) = tube_outputs.m_T_surf_out;
		}

        m_max_damage = total_damage;  //keep track of the max damage value

		if( total_damage <= 1.0 )
			search_min_th = false;

        //check to see if the iteration number is at the max
        if(! (iter_d_in < m_iter_d_in_max ) )
            search_min_th = false;
	
	} while( search_min_th );

	// Write other member data (eventually go back and hardcode this to get rid of local copy...)
	// m_m_dot_tube = m_dot_tube;

	m_deltaP_kPa = m_Pres[m_n_temps - 1] - m_Pres[0];

	if( is_deltaP_too_large ||  m_max_damage > 1.)
        return false;

	return true;
}

void N_sco2_rec::C_calc_tube_min_th::initialize_all_output_columns()
{
	// matrix_t results
	initialize_output_column(m_total_damage);
	m_n_results_cols = 1;

	// vector results
	initialize_vector(m_P_fluid_out);
	m_n_vector_results = 1;
}

void N_sco2_rec::C_calc_tube_min_th::initialize_output_column(util::matrix_t<double> & results_matrix)
{
	results_matrix.resize_fill(m_n_tube_elements, 1, std::numeric_limits<double>::quiet_NaN());
}

void N_sco2_rec::C_calc_tube_min_th::initialize_vector(vector<double> & results_vector)
{
	results_vector.resize(1);
	results_vector[0] = std::numeric_limits<double>::quiet_NaN();
}

void N_sco2_rec::C_calc_tube_min_th::push_back_all_vectors()
{
	push_back_vector(m_P_fluid_out);
	m_n_vector_results++;
}

void N_sco2_rec::C_calc_tube_min_th::push_back_vector(vector<double> & results_vector)
{
	results_vector.push_back(std::numeric_limits<double>::quiet_NaN());
}

void N_sco2_rec::C_calc_tube_min_th::add_all_output_columns()
{
	add_output_column(m_total_damage);
}

void N_sco2_rec::C_calc_tube_min_th::add_output_column(util::matrix_t<double> & results_matrix)
{
	m_element_results_temp = results_matrix;
	results_matrix.resize(m_n_tube_elements, m_n_results_cols+1);
	for( int i = 0; i < m_n_tube_elements; i++ )
	{
		for( int j = 0; j < m_n_results_cols; j++ )
			results_matrix(i, j) = m_element_results_temp(i, j);
		results_matrix(i, m_n_results_cols) = std::numeric_limits<double>::quiet_NaN();
	}
	m_n_results_cols++;
}

//******************************************************
// Class: C_tube_slice        Methods ******************
//*******************************************************

N_sco2_rec::C_tube_slice::C_tube_slice(int enum_tube_mat)
{
	general_constructor(enum_tube_mat);
	reset_SFs_and_design_targets();
}

N_sco2_rec::C_tube_slice::C_tube_slice(int enum_tube_mat, double F_avg, double SF_fatigue, double F_inelastic, double N_design_cycles, double t_hours_design)
{
	general_constructor(enum_tube_mat);
	specify_SFs_and_design_targets(F_avg, SF_fatigue, F_inelastic, N_design_cycles, t_hours_design);
}

void N_sco2_rec::C_tube_slice::general_constructor(int enum_tube_mat)
{
	p_tube_mat = new C_rec_des_props(enum_tube_mat);
	clear_calc_member_data();
}

void N_sco2_rec::C_tube_slice::clear_calc_member_data()
{
	
	m_T_surf_in = m_T_surf_out = m_nu_poisson = m_E = m_alpha = m_r_in = m_r_out = std::numeric_limits<double>::quiet_NaN();
}

// Destructor, delete dynamically allocated property class
N_sco2_rec::C_tube_slice::~C_tube_slice()
{
	delete p_tube_mat;
}

// Initialize structure members
N_sco2_rec::C_tube_slice::S_ID_OD_perf_and_lifetime_inputs::S_ID_OD_perf_and_lifetime_inputs()
{
	m_P_internal = m_T_fluid = m_d_out = m_d_in = m_flux = m_h_conv = std::numeric_limits<double>::quiet_NaN();
}

N_sco2_rec::C_tube_slice::S_ID_OD_perf_and_lifetime_outputs::S_ID_OD_perf_and_lifetime_outputs()
{
	m_T_surf_in = m_T_surf_out = std::numeric_limits<double>::quiet_NaN();
}

N_sco2_rec::C_tube_slice::S_ID_OD_stress_and_lifetime_inputs::S_ID_OD_stress_and_lifetime_inputs()
{
	m_P_internal = m_T_fluid = m_d_out = m_d_in = std::numeric_limits<double>::quiet_NaN();
}

N_sco2_rec::C_tube_slice::S_principal_stresses::S_principal_stresses()
{
	m_sigma_a = m_sigma_r = m_sigma_t = std::numeric_limits<double>::quiet_NaN();
}

N_sco2_rec::C_tube_slice::S_creep_fatigue_outputs::S_creep_fatigue_outputs()
{
	m_eps_a_perc_inel = m_eps_r_perc_inel = m_eps_t_perc_inel =
	m_eps_equiv_perc_SF = m_N_cycles = m_fatigue_damage = 
	m_max_stress_SF = m_creep_life = m_creep_damage = 
	m_total_damage = std::numeric_limits<double>::quiet_NaN();
}

void N_sco2_rec::C_tube_slice::reset_SFs_and_design_targets()
{
	// Safety factors and inelastic multiplier applied to calculated stresses here
	m_F_avg = 0.67;		// Page 928: BIPV - safety factor/multiplier for creep stress"

	// Safety factor for equivalent strain
	m_SF_fatigue = 0.5;	
	
	// "Multplier to estimate inelastic strain from elastic calculations"
	m_F_inelastic = 1.1;	

	m_N_design_cycles = 10000.0;	// [-] Number of design cycles: Kistler
	m_t_hours_design = 100000.0;	// [hr] Number of hours operating at design: justification in ASME-ES paper
}

void N_sco2_rec::C_tube_slice::specify_SFs_and_design_targets(double F_avg, double SF_fatigue, double F_inelastic, double N_design_cycles, double t_hours_design)
{
	// First set all to hardcoded values
	reset_SFs_and_design_targets();

	// Then change to input value as necessary
	if( F_avg > 0.0 )
		m_F_avg = F_avg;
	if( SF_fatigue > 0.0 )
		m_SF_fatigue = SF_fatigue;
	if( F_inelastic > 0.0 )
		m_F_inelastic = F_inelastic;
	if( N_design_cycles > 0.0 )
		m_N_design_cycles = N_design_cycles;
	if( t_hours_design > 0.0 )
		m_t_hours_design = t_hours_design;
}

void N_sco2_rec::C_tube_slice::calc_ID_OD_perf_and_lifetime(const S_ID_OD_perf_and_lifetime_inputs & s_inputs, S_ID_OD_perf_and_lifetime_outputs & s_outputs)
{
	clear_calc_member_data();

	// Call 1-D Energy balance to get correct temperatures
	s_ID_OD_perf_and_lifetime_inputs = s_inputs;
	radial_ss_E_bal();
	// Set outputs
	s_outputs.m_T_surf_in = m_T_surf_in;
	s_outputs.m_T_surf_out = m_T_surf_out;

	// Calculate and save average material temperatures and properties
	avg_temps_and_props();

	// Using saved average values, calculate thermal (uniform flux temperature gradient) and pressure stresses and total superimposed
		// Inner Diameter
	thermal_stress_rad_profile(s_ID_OD_perf_and_lifetime_inputs.m_d_in, s_outputs.s_ID_stress_outputs);
		// Outer Diameter
	thermal_stress_rad_profile(s_ID_OD_perf_and_lifetime_inputs.m_d_out, s_outputs.s_OD_stress_outputs);
	
	// Using calculated total superimposed stress values, calculate creep-fatigue lifetime
		// Inner Diameter
	creep_fatigue_lifetime(m_T_surf_in, s_outputs.s_ID_stress_outputs.s_total_stresses, s_outputs.s_ID_lifetime_outputs);
		// Outer Diameter
	creep_fatigue_lifetime(m_T_surf_out, s_outputs.s_OD_stress_outputs.s_total_stresses, s_outputs.s_OD_lifetime_outputs);
}

void N_sco2_rec::C_tube_slice::calc_ID_OD_stress_and_lifetime(const S_ID_OD_stress_and_lifetime_inputs & s_inputs, S_ID_OD_stress_and_lifetime_outputs & s_outputs)
{
	s_ID_OD_perf_and_lifetime_inputs.m_P_internal = s_inputs.m_P_internal;
	s_ID_OD_perf_and_lifetime_inputs.m_T_fluid = s_inputs.m_T_fluid;
	s_ID_OD_perf_and_lifetime_inputs.m_d_out = s_inputs.m_d_out;
	s_ID_OD_perf_and_lifetime_inputs.m_d_in = s_inputs.m_d_in;

	s_ID_OD_perf_and_lifetime_inputs.m_flux = 0.0;
	s_ID_OD_perf_and_lifetime_inputs.m_h_conv = 0.0;

	m_T_surf_in = s_inputs.m_T_surf_in;
	m_T_surf_out = s_inputs.m_T_surf_out;

	// Calculate and save average material temperatures and properties
	avg_temps_and_props();

	// Using saved average values, calculate thermal (uniform flux temperature gradient) and pressure stresses and total superimposed
		// Inner Diameter
	thermal_stress_rad_profile(s_ID_OD_perf_and_lifetime_inputs.m_d_in, s_outputs.s_ID_stress_outputs);
		// Outer Diameter
	thermal_stress_rad_profile(s_ID_OD_perf_and_lifetime_inputs.m_d_out, s_outputs.s_OD_stress_outputs);

	// Using calculated total superimposed stress values, calculate creep-fatigue lifetime
	// Inner Diameter
	creep_fatigue_lifetime(m_T_surf_in, s_outputs.s_ID_stress_outputs.s_total_stresses, s_outputs.s_ID_lifetime_outputs);
	// Outer Diameter
	creep_fatigue_lifetime(m_T_surf_out, s_outputs.s_OD_stress_outputs.s_total_stresses, s_outputs.s_OD_lifetime_outputs);
}

void N_sco2_rec::C_tube_slice::radial_ss_E_bal()
{
	// Calculate heat transfer through tube (W/m)
	double q_max = s_ID_OD_perf_and_lifetime_inputs.m_flux * s_ID_OD_perf_and_lifetime_inputs.m_d_out * CSP::pi;	//[W/m] = [W/m2 * m]

	// Calculate tube inner surface temperature (C)
	// [W/m] * [1/m] * [m2-C/W] = [C]
	double T_surf_in = q_max / (s_ID_OD_perf_and_lifetime_inputs.m_d_in*CSP::pi*s_ID_OD_perf_and_lifetime_inputs.m_h_conv) + s_ID_OD_perf_and_lifetime_inputs.m_T_fluid;

	// Guess an average surface temperature to calculate tube conductivity
	double T_surf_out_guess = T_surf_in;				//[C]

	// Set up iteration to converge on average surface temperature
	double T_low = T_surf_in;
	double T_high = std::numeric_limits<double>::quiet_NaN();
	bool high_flag = false;

	do
	{
		double T_surf_avg_guess = (T_surf_in + T_surf_out_guess) / 2.0;	//[C]
		double k_tube = p_tube_mat->cond(T_surf_avg_guess);	//[W/m-K]
		double T_surf_out_calc = q_max*log(s_ID_OD_perf_and_lifetime_inputs.m_d_out / s_ID_OD_perf_and_lifetime_inputs.m_d_in) / (2.0*CSP::pi*k_tube) + T_surf_in;		//[C]
		double T_surf_out_err = (T_surf_out_guess - T_surf_out_calc) / T_surf_out_calc;

        if( T_surf_out_err != T_surf_out_err ) 
            throw sco2_exception("Convergence failed in the sCO2 receiver tube model: radial_ss_E_bal().");

		if( fabs(T_surf_out_err) < 1.E-10 )
			break;
		else
		{
			if( T_surf_out_err > 0.0 )
			{
				high_flag = true;
				T_high = T_surf_out_guess;
				T_surf_out_guess = (T_low + T_high) / 2.0;					//[C]
			}
			else
			{
				T_low = T_surf_out_guess;
				if( high_flag )
					T_surf_out_guess = (T_low + T_high) / 2.0;					//[C]
				else
					T_surf_out_guess = T_surf_out_calc;
			}
		}
	} while( true );

	m_T_surf_in = T_surf_in;
	m_T_surf_out = T_surf_out_guess;
}

void N_sco2_rec::C_tube_slice::avg_temps_and_props()
{
	// Set Poisson's Ratio to constant
	m_nu_poisson = p_tube_mat->poisson();

	// Get Modulus of Elasticity and Coefficient of Thermal Expansion at average temperature
	double T_surf_avg = 0.5*(m_T_surf_in + m_T_surf_out);
	m_E = p_tube_mat->modE(T_surf_avg)*1.E3;							//[MPa], convert from GPa
	m_alpha = p_tube_mat->alpha_inst(T_surf_avg) / (1.E6);				//[mm/mm-C], convert from [mm/km-C]

	// Easier to work with radius rather than diameter
	m_r_in = s_ID_OD_perf_and_lifetime_inputs.m_d_in / 2.0;
	m_r_out = s_ID_OD_perf_and_lifetime_inputs.m_d_out / 2.0;
}

void N_sco2_rec::C_tube_slice::thermal_stress_rad_profile(double d_local, S_thermal_stress_rad_profile_outputs & outputs)
{
	double r_local = d_local / 2.0;

	// Calculate thermal stress from 1D steady state heat transfer through tube wall
	// Ref: S. Timoshenko and J.N. Goodier, Theory of Elasticity, McGray-Hill, 1951
	outputs.s_thermal_stresses.m_sigma_r = m_alpha*m_E*(m_T_surf_in - m_T_surf_out) / (2.0*(1.0 - m_nu_poisson)*log(m_r_out / m_r_in))*
		(-log(m_r_out / r_local) - pow(m_r_in, 2) / (pow(m_r_out, 2) - pow(m_r_in, 2))*(1.0 - (pow(m_r_out, 2) / pow(r_local, 2)))*log(m_r_out / m_r_in));

	outputs.s_thermal_stresses.m_sigma_t = m_alpha*m_E*(m_T_surf_in - m_T_surf_out) / (2.0*(1.0 - m_nu_poisson)*log(m_r_out / m_r_in))*
		(1.0 - log(m_r_out / r_local) - pow(m_r_in, 2) / (pow(m_r_out, 2) - pow(m_r_in, 2))*(1.0 + (pow(m_r_out, 2) / pow(r_local, 2)))*log(m_r_out / m_r_in));

	outputs.s_thermal_stresses.m_sigma_a = m_alpha*m_E*(m_T_surf_in - m_T_surf_out) / (2.0*(1.0 - m_nu_poisson)*log(m_r_out / m_r_in))*
		(1.0 - 2.0*log(m_r_out / r_local) - 2.0*pow(m_r_in, 2) / (pow(m_r_out, 2) - pow(m_r_in, 2))*log(m_r_out / m_r_in));

	// Thick-walled tube pressure stresses
	// http://www.mae.ncsu.edu/zhu/courses/mae316/lecture/3-ThickWalledCylinder_and_PressShrinkFit_Shig.pdf
	outputs.s_pressure_stresses.m_sigma_r = s_ID_OD_perf_and_lifetime_inputs.m_P_internal*pow(m_r_in, 2) / (pow(m_r_out, 2) - pow(m_r_in, 2))*(1.0 - (pow(m_r_out, 2) / pow(r_local, 2)));

	outputs.s_pressure_stresses.m_sigma_t = s_ID_OD_perf_and_lifetime_inputs.m_P_internal*pow(m_r_in, 2) / (pow(m_r_out, 2) - pow(m_r_in, 2))*(1.0 + (pow(m_r_out, 2) / pow(r_local, 2)));

	outputs.s_pressure_stresses.m_sigma_a = s_ID_OD_perf_and_lifetime_inputs.m_P_internal*pow(m_r_in, 2) / (pow(m_r_out, 2) - pow(m_r_in, 2));

	// Superimpose thermal and pressure stresses
	outputs.s_total_stresses.m_sigma_a = outputs.s_pressure_stresses.m_sigma_a + outputs.s_thermal_stresses.m_sigma_a;
	outputs.s_total_stresses.m_sigma_r = outputs.s_pressure_stresses.m_sigma_r + outputs.s_thermal_stresses.m_sigma_r;
	outputs.s_total_stresses.m_sigma_t = outputs.s_pressure_stresses.m_sigma_t + outputs.s_thermal_stresses.m_sigma_t;
}

void N_sco2_rec::C_tube_slice::creep_fatigue_lifetime(double T_mat_C, const S_principal_stresses & inputs, S_creep_fatigue_outputs & outputs)
{
	// Equivalent strain and number of allowable cycles calcs
	outputs.m_eps_a_perc_inel = m_F_inelastic*inputs.m_sigma_a / m_E*100.0;		//[%], convert from [-]

	outputs.m_eps_r_perc_inel = m_F_inelastic*inputs.m_sigma_r / m_E*100.0;		//[%], convert from [-]

	outputs.m_eps_t_perc_inel = m_F_inelastic*inputs.m_sigma_t / m_E*100.0;		//[%], convert from [-]

	// Equivalent stress calculation
	// Ref: Equation 3, Section 3: An Interim Structural Design Standard for Solar Energy Applications 
	outputs.m_eps_equiv_perc_SF = sqrt(2.0) / 3.0*sqrt(pow(outputs.m_eps_t_perc_inel - outputs.m_eps_a_perc_inel, 2.0) + pow(outputs.m_eps_t_perc_inel - outputs.m_eps_r_perc_inel, 2.0) + pow(outputs.m_eps_a_perc_inel - outputs.m_eps_r_perc_inel, 2.0)) / m_SF_fatigue;
	outputs.m_N_cycles = p_tube_mat->cycles_to_failure(outputs.m_eps_equiv_perc_SF, T_mat_C);
	outputs.m_fatigue_damage = m_N_design_cycles / outputs.m_N_cycles;

	// Maximum strain and creep calcs
	outputs.m_max_stress_SF = max(inputs.m_sigma_a, max(inputs.m_sigma_r, inputs.m_sigma_t)) / m_F_avg;
	outputs.m_creep_life = p_tube_mat->creep_life(outputs.m_max_stress_SF, T_mat_C);			//[hr]
	outputs.m_creep_damage = m_t_hours_design / outputs.m_creep_life;

	// Total damage
	outputs.m_total_damage = outputs.m_fatigue_damage + outputs.m_creep_damage;
}
