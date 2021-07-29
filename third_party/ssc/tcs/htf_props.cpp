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

#include "htf_props.h"
#include "csp_solver_util.h"
#include <cmath>

HTFProperties::HTFProperties()
{
	m_fluid = 0;
	uf_err_msg = "The user-defined htf property table is invalid (rows=%d cols=%d)";

	m_is_temp_enth_avail = false;
}

bool HTFProperties::SetUserDefinedFluid(const util::matrix_t<double> &table, bool calc_temp_enth_table)
{
	m_is_temp_enth_avail = calc_temp_enth_table;

	return SetUserDefinedFluid(table);
}

bool HTFProperties::SetUserDefinedFluid( const util::matrix_t<double> &table )
{
	// If a user defined fluid, check for correct number of columns
	if ( table.ncols() != 7 ) return false;

	// Set class member data
	m_userTable = table;	
	m_fluid = User_defined;

	// Specific which columns are used as the independent variable; these must be monotonically increasing
	int ind_var_index[2] = {0, 6};	
	int n_ind_var = 2;
	int error_index = -99;

	// Set up interpolation class and check for monotonically increasing temperatures
	if( !User_Defined_Props.Set_1D_Lookup_Table( table, ind_var_index, n_ind_var, error_index ) )
	{
		if( error_index == -1 )
			uf_err_msg = "Interpolation table must have at least 3 rows (rows=%d cols=%d)";
		if( error_index == 0 )
			uf_err_msg = "Temperature must monotonically increase (rows=%d cols=%d)";
		if( error_index == 1 )
			uf_err_msg = "Enthalpy must monotonically increase (rows=%d cols=%d)";
		return false;
	}

	if(m_is_temp_enth_avail)
	{
		set_temp_enth_lookup();
	}

	return true;
}

void HTFProperties::set_temp_enth_lookup()
{
	double T_low = 270.0 + 273.15;
	double T_high = 600.0 + 273.15;

	double delta_T_target = 1.0;

	int n_rows = (int)(ceil((T_high - T_low)/delta_T_target) + 1.0);
	double delta_T = (T_high - T_low)/double(n_rows-1);

	util::matrix_t<double> table(n_rows, 2);

	double T, T_next, cp, h, h_next;
	T_next = T_low;
	h_next = 0.0;	// specific heat[kJ / kg - K]
	table(0, 0) = T_next;		//[K]
	table(0, 1) = h_next;		//[kJ/kg-K]
	for(int i = 0; i<n_rows-1; i++)
	{
		h = h_next;
		T = T_next;

		T_next = T + delta_T;
	
		cp = Cp(0.5*(T+T_next));	// specific heat[kJ / kg - K]

		h_next = h + cp*delta_T;

		table(i+1,0) = T_next;		//[K]
		table(i+1,1) = h_next;		//[kJ/kg-K]
	}

	// Specific which columns are used as the independent variable; these must be monotonically increasing
	int ind_var_index[2] = {0, 1};
	int n_ind_var = 2;
	int error_index = -99;
	if( !mc_temp_enth_lookup.Set_1D_Lookup_Table(table,ind_var_index,n_ind_var,error_index) )
	{
		if(error_index == -1)
		{
			throw(C_csp_exception("Interpolation table must have at least 3 rows (rows=%d cols=%d)", 
				"HTFProperties::set_temp_enth_lookup"));
		}
		if(error_index == 0)
		{
			throw(C_csp_exception("Temperature must monotonically increase (rows=%d cols=%d)",
				"HTFProperties::set_temp_enth_lookup"));
		}
		if(error_index == 1)
		{
			throw(C_csp_exception("Enthalpy must monotonically increase (rows=%d cols=%d)",
				"HTFProperties::set_temp_enth_lookup"));
		}
	}

}

double HTFProperties::temp_lookup( double enth /*kJ/kg*/)
{
	if(!m_is_temp_enth_avail)
	{
		/*set_temp_enth_lookup();
		m_is_temp_enth_avail = true;*/
		throw(C_csp_exception("The enth-temp-lookup method is only available if fluid is set with optional Boolean to enable it"));
	}

	return mc_temp_enth_lookup.linear_1D_interp(1, 0, enth);	//[K]
}

double HTFProperties::enth_lookup( double temp /*K*/)
{
	if( !m_is_temp_enth_avail )
	{
		/*set_temp_enth_lookup();
		m_is_temp_enth_avail = true;*/
		throw(C_csp_exception("This enth-temp-lookup method is only available if fluid is set with optional Boolean to enable it"));
	}

	return mc_temp_enth_lookup.linear_1D_interp(0, 1, temp);	//[kJ/kg]
}

bool HTFProperties::SetFluid( int fluid, bool calc_temp_enth_table)
{
	m_is_temp_enth_avail = calc_temp_enth_table;
	
	return SetFluid(fluid);
}

bool HTFProperties::SetFluid( int fluid )
{
	// If using stored fluid properties, set member fluid number
	m_fluid = fluid;

	if( m_is_temp_enth_avail )
	{
		set_temp_enth_lookup();
	}

	return true;
}

const util::matrix_t<double> *HTFProperties::get_prop_table()
{
	return &m_userTable;
}

bool HTFProperties::equals(HTFProperties *comp_class)
{
	return m_userTable.equals(	(*comp_class->get_prop_table()) );
}

double HTFProperties::Cp_ave(double T_cold_K, double T_hot_K, int n_points)
{
	// Check that temperatures are at least positive values
	if(T_cold_K <= 0.0)
	{
		throw(C_csp_exception("Cold temperature must be greater than 0.0", 
		"HTFProperties::Cp_ave",1));
	}
	if(T_hot_K <= 0.0)
	{
		throw(C_csp_exception("Hot temperature must be greater than 0.0",
		"HTFProperties::Cp_ave",1));
	}
	
	// Check that 2 < n_points < 500
	if(n_points < 2)
		n_points = 2;

	if(n_points > 500)
		n_points = 500;

	double cp_sum = 0.0;
	double T_i = std::numeric_limits<double>::quiet_NaN();
	double delta_T = (T_hot_K - T_cold_K)/double(n_points-1);
	for(int i = 0; i < n_points; i++)
	{
		T_i = T_cold_K + delta_T*i;
		cp_sum += Cp(T_i);
	}

	return cp_sum/double(n_points);
}

double HTFProperties::Cp( double T_K )
{
	/* Inputs: temperature [K]
	Outputs: constant pressure specific heat [kJ/kg-K]
	Converted to c++ from Fortran code Type 229 in November 2012 by Ty Neises
	Original author: Michael J. Wagner */

	double T_C = T_K - 273.15;		// Also provide temperature in C

	switch(m_fluid)
	{
	case Air: 
		return 1.03749 - 0.000305497*T_K + 7.49335E-07*T_K*T_K - 3.39363E-10*T_K*T_K*T_K;
	case Stainless_AISI316:	
		return 0.368455 + 0.000399548*T_K - 1.70558E-07*T_K*T_K;
	case Water_liquid: 
		return 4.181;
	case Salt_68_KCl_32_MgCl2:	

		// Xu et al. 2018 Experimental Test of Properties of KCl-MgCl2 Eutectic Molten Salt for Heat Transfer and Thermal Storage Fluid in Concentrated Solar Power Systems
		return 1.9700E-08*std::pow(T_C,2) - 1.2203E-05*T_C + 1.0091;	//[kJ/kg-K]

		//return 1.156;				// replaced 20.04.06 twn

	case Salt_8_NaF_92_NaBF4:	
		return 1.507;
	case Salt_25_KF_75_KBF4:	
		return 1.306;
	case Salt_31_RbF_69_RbBF4:	
		return 9.127;
	case Salt_465_LiF_115_NaF_42KF:	
		return 2.010;
	case Salt_49_LiF_29_NaF_29_ZrF4:
		return 1.239;
	case Salt_58_KF_42_ZrF4:
		return 1.051;
	case Salt_58_LiCl_42_RbCl:
		return 8.918;
	case Salt_58_NaCl_42_MgCl2:
		return 1.080;
	case Salt_595_LiCl_405_KCl:
		return 1.202;
	case Salt_595_NaF_405_ZrF4:
		return 1.172;
	case Salt_60_NaNO3_40_KNO3:
		return -1E-10*T_K*T_K*T_K + 2E-07*T_K*T_K + 5E-06*T_K + 1.4387;
	case Nitrate_Salt:
		return (1443. + 0.172 * (T_K-273.15))/1000.0;
	case Caloria_HT_43:
		return (3.88 * (T_K-273.15) + 1606.0)/1000.0;
	case Hitec_XL:
		return fmax(1536.0 - 0.2624 * T_C- 0.0001139 * T_C * T_C,1000.0)/1000.0;
	case Therminol_VP1:
		return (1.509 + 0.002496 * T_C + 0.0000007888 * T_C*T_C);
	case Hitec:
		return (1560.0)/1000.0;			// Convert to kJ/kg-k
	case Dowtherm_Q:
		return (-0.00053943 * T_C*T_C + 3.2028 * T_C + 1589.2)/1000.;               // Russ 10-2-03
	case Dowtherm_RP:
		return (-0.0000031915 * T_C*T_C + 2.977 * T_C + 1560.8)/1000.;					// !Russ 10-2-03
	case Argon_ideal:
		return 0.5203;	// Cp only, Cv is different
	case Hydrogen_ideal:
		return fmin(fmax(-45.4022 + 0.690156*T_K - 0.00327354*T_K*T_K + 0.00000817326*T_K*T_K*T_K - 1.13234E-08*T_K*T_K*T_K*T_K + 8.24995E-12*T_K*T_K*T_K*T_K*T_K - 2.46804E-15*T_K*T_K*T_K*T_K*T_K*T_K,11.3),14.7);
	case T91_Steel:
		return 0.0004*T_C*T_C + 0.2473*T_C + 450.08;
	case Therminol_66:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
		return 0.0036*T_C + 1.4801;
	case Therminol_59:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
		return 0.0033*T_C + 1.6132;
	case Pressurized_Water:
		return 1.E-5*T_C*T_C - 0.0014*T_C + 4.2092;
    case N06230:
        return 0.2888*T_C + 397.42; // BPVC II D
    case N07740:
        return -1.E-9*std::pow(T_C, 4) + 3.E-6*std::pow(T_C, 3) -
            0.0022*std::pow(T_C, 2) + 0.6218*T_C + 434.06;  // BPVC_CC_BPV_2017 Case 2702 - 3
	case User_defined:
		{
			if ( m_userTable.nrows() < 3 ) return std::numeric_limits<double>::quiet_NaN();
			// Interpolate
			return User_Defined_Props.linear_1D_interp( 0, 1, T_C );
		}
		break;
	default:
		return std::numeric_limits<double>::quiet_NaN();
	}
}

double HTFProperties::dens(double T_K, double P)
{
	/*Inputs: temperature [K] pressure [Pa]
	Output: density [kg/m^3]
	Converted to c++ from Fortran code Type 229 in November 2012 by Ty Neises
	Original author: Michael J. Wagner */

	double T_C = T_K - 273.15;		// This function accepts as inputs temperature[K]. Convert to [C] for correlations

	switch(m_fluid)
	{
		case Air:
			return P/(287.0*T_K);
		case Stainless_AISI316:
			return 8349.38 - 0.341708*T_K - 0.0000865128*T_K*T_K;	// EES
		case Water_liquid:
			return 1000.0;
		case Salt_68_KCl_32_MgCl2:
			
			// Xu et al. 2018 Experimental Test of Properties of KCl-MgCl2 Eutectic Molten Salt for Heat Transfer and Thermal Storage Fluid in Concentrated Solar Power Systems
			return (-5.0997E-4*T_C + 1.8943) * 1.E3;	//[kg/m3], convert from g/cm3

			// return 1E-10*T_K*T_K*T_K - 3E-07*T_K*T_K - 0.4739*T_K + 2384.2;	// replaced 20.04.06

		case Salt_8_NaF_92_NaBF4:
			return 8E-09*T_K*T_K*T_K - 2E-05*T_K*T_K - 0.6867*T_K + 2438.5;
		case Salt_25_KF_75_KBF4:
			return 2E-08*T_K*T_K*T_K - 6E-05*T_K*T_K - 0.7701*T_K + 2466.1;
		case Salt_31_RbF_69_RbBF4:
			return -1E-08*T_K*T_K*T_K + 4E-05*T_K*T_K - 1.0836*T_K + 3242.6;
		case Salt_465_LiF_115_NaF_42KF:
			return -2E-09*T_K*T_K*T_K + 1E-05*T_K*T_K - 0.7427*T_K + 2734.7;
		case Salt_49_LiF_29_NaF_29_ZrF4:
			return -2E-11*T_K*T_K*T_K + 1E-07*T_K*T_K - 0.5172*T_K + 3674.3;
		case Salt_58_KF_42_ZrF4:
			return -6E-10*T_K*T_K*T_K + 4E-06*T_K*T_K - 0.8931*T_K + 3661.3;
		case Salt_58_LiCl_42_RbCl:
			return -8E-10*T_K*T_K*T_K + 1E-06*T_K*T_K - 0.689*T_K + 2929.5;
		case Salt_58_NaCl_42_MgCl2:
			return -5E-09*T_K*T_K*T_K + 2E-05*T_K*T_K - 0.5298*T_K + 2444.1;
		case Salt_595_LiCl_405_KCl:
			return 1E-09*T_K*T_K*T_K - 5E-06*T_K*T_K - 0.864*T_K + 2112.6;
		case Salt_595_NaF_405_ZrF4:
			return -5E-09*T_K*T_K*T_K + 2E-05*T_K*T_K - 0.9144*T_K + 3837.0;
		case Salt_60_NaNO3_40_KNO3:
			return fmax(-1E-07*T_K*T_K*T_K + 0.0002*T_K*T_K - 0.7875*T_K + 2299.4,1000.0);
		case Nitrate_Salt:
			return fmax(2090.0 - 0.636 * (T_K-273.15),1000.0);
		case Caloria_HT_43:
			return fmax(885.0 - 0.6617 * T_C - 0.0001265 * T_C*T_C,100.0);
		case Hitec_XL:
			return fmax(2240.0 - 0.8266 * T_C,800.0);
		case Therminol_VP1:
			return fmax(1074.0 - 0.6367 * T_C - 0.0007762 * T_C*T_C,400.0);
		case Hitec:
			return fmax(2080.0 - 0.733 * T_C,1000.0);
		case Dowtherm_Q:
			return fmax(-0.757332 * T_C + 980.787,100.0);						// Russ 10-2-03
		case Dowtherm_RP:
			return fmax(-0.000186495 * T_C*T_C - 0.668337 * T_C + 1042.11,200.0);		// Russ 10-2-03
		case Argon_ideal:
			return fmax(P/(208.13*T_K),1.E-10);
		case Hydrogen_ideal:
			return fmax(P/(4124.0*T_K),1.E-10);
		case T91_Steel: //"Thermo hydraulic optimisation of the EURISOL DS target" - Paul Scherrer Institut
			return -0.3289*T_C + 7742.5;
		case Therminol_66:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
			return -0.7146*T_C + 1024.8;
		case Therminol_59:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
			return -0.0003*T_C*T_C - 0.6963*T_C + 988.44;
		case Pressurized_Water:
			return -0.0023*T_C*T_C - 0.2337*T_C + 1005.6;
        case N06230:
            return 8970.0; // BPVC II D
        case N07740:
            return 8072.0;  // BPVC_CC_BPV_2017 Case 2702 - 3
		case User_defined:
			if ( m_userTable.nrows() < 3 )
						return std::numeric_limits<double>::quiet_NaN();

			// Interpolate
			return User_Defined_Props.linear_1D_interp( 0, 2, T_C );
		default:
			return std::numeric_limits<double>::quiet_NaN();
	}		
}

double HTFProperties::visc(double T_K)
{
	/*Inputs: temperature [K]
	Outputs: dynamic viscosity [kg/m-s] or [Pa-s]
	Converted to c++ from Fortran code Type 229 in November 2012 by Ty Neises
	Original author: Michael J. Wagner */

	double T_C = T_K - 273.15;		// This function accepts as inputs temperature[K]. Convert to [C] for correlations

	switch(m_fluid)
	{
	case Air:
		return fmax(0.0000010765 + 7.15173E-08*T_K - 5.03525E-11*T_K*T_K + 2.02799E-14*T_K*T_K*T_K,1.E-6);
	case Salt_68_KCl_32_MgCl2:

		// Xu et al. 2018 Experimental Test of Properties of KCl-MgCl2 Eutectic Molten Salt for Heat Transfer and Thermal Storage Fluid in Concentrated Solar Power Systems
		// Xu data also combined with another source for this curve fit
		return (1.8075E-5*std::pow(T_C,2) - 2.8496E-2*T_C + 1.3489E1)*0.001;	// [kg/m-s] convert from cP

		//return .0146*exp(2230.0/T_K)*0.001;			// replaced 20.04.06 twn

	case Salt_8_NaF_92_NaBF4:
		return .0877*exp(2240.0/T_K)*0.001;			// convert cP to kg/m-s
	case Salt_25_KF_75_KBF4:
		return .0431*exp(3060.0/T_K)*0.001;			// convert cP to kg/m-s
	case Salt_31_RbF_69_RbBF4:
		return .0009;
	case Salt_465_LiF_115_NaF_42KF:
		return .0400*exp(4170.0/T_K)*0.001;			// convert cP to kg/m-s
	case Salt_49_LiF_29_NaF_29_ZrF4:
		return .0069;
	case Salt_58_KF_42_ZrF4:
		return .0159*exp(3179./T_K)*0.001;			// convert cP to kg/m-s
	case Salt_58_LiCl_42_RbCl:
		return .0861*exp(2517./T_K)*0.001;			// convert cP to kg/m-s        
	case Salt_58_NaCl_42_MgCl2:
		return .0286*exp(1441./T_K)*0.001;			// convert cP to kg/m-s
	case Salt_595_LiCl_405_KCl:
		return .0861*exp(2517./T_K)*0.001;			// convert cP to kg/m-s
	case Salt_595_NaF_405_ZrF4:
		return .0767*exp(3977./T_K)*0.001;			// convert cP to kg/m-s
	case Salt_60_NaNO3_40_KNO3:
		return fmax(-1.473302E-10*pow(T_C,3) + 2.279989E-07*pow(T_C,2) - 1.199514E-04*T_C + 2.270616E-02,.0001);
	case Nitrate_Salt:
		return fmax((22.714 - 0.12 * T_C + 0.0002281 *T_C*T_C - 0.0000001474 * pow(T_C,3)) / 1000.0,1.e-6);
	case Caloria_HT_43:		
		return (0.040439268 * pow(fmax(T_C,10.0),-1.946401872)) * dens(T_K, 0.0); 
	case Hitec_XL:  
		return 1372000. * pow(T_C,-3.364);
	case Therminol_VP1:
		return 0.001 * (pow(10.,0.8703)*pow(fmax(T_C,20.),(0.2877 + log10(pow(fmax(T_C,20.),-0.3638)))));
	case Hitec:
		return fmax(0.00622 - 0.0000102 * T_C,1.e-6);
	case Dowtherm_Q:
		return 1. / (132.40658 + 4.36107 * T_C + 0.0781417*T_C*T_C - 0.00011035416*pow(T_C,3));		// Hank 10-2-03
	case Dowtherm_RP:
		return 1. / (4.523003 + 0.39156855 * T_C + 0.028604206*T_C*T_C);		// Hank 10-2-03
	case Argon_ideal:
		return 4.4997e-6 + 6.38920E-08*T_K - 1.24550E-11*T_K*T_K;
	case Hydrogen_ideal:
		return 0.00000231 + 2.37842E-08*T_K - 5.73624E-12*T_K*T_K;
	case Therminol_66:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
		if(T_C < 80.) 
		{
			return 1.31959963 - 0.171204729*T_C + 0.0100351594*pow(T_C,2) - 0.000313556341*pow(T_C,3) + 0.0000053430666*pow(T_C,4) - 4.66597650E-08*pow(T_C,5) + 1.63046296E-10*pow(T_C,6);
		}
		else
		{
			return 0.0490075884 - 0.00120478233*T_C + 0.0000130162082*pow(T_C,2) - 7.58913847E-08*pow(T_C,3) + 2.47856063E-10*pow(T_C,4) - 4.26872345E-13*pow(T_C,5) + 3.01949160E-16*pow(T_C,6);
		}
		break;
	case Therminol_59:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
		if(T_C < 25.)
		{
			return 0.0137267822 - 0.000218740224*T_C + 0.0000759248815*pow(T_C,2) - 0.00000473464744*pow(T_C,3) - 1.97083667E-07*pow(T_C,4) + 4.35487179E-09*pow(T_C,5) + 2.40243056E-10*pow(T_C,6);
		}
		else
		{
			return 0.0114608807 - 0.000313431056*T_C + 0.00000416778121*pow(T_C,2) - 3.04668508E-08*pow(T_C,3) + 1.23719006E-10*pow(T_C,4) - 2.60834697E-13*pow(T_C,5) + 2.22227675E-16*pow(T_C,6);
		}
	case Pressurized_Water:
		return 3.E-8*T_C*T_C - 1.E-5*T_C + 0.0011;
	case User_defined:
		if ( m_userTable.nrows() < 3 )
					return std::numeric_limits<double>::quiet_NaN();

		// Interpolate
		return User_Defined_Props.linear_1D_interp( 0, 3, T_C );
	default:
		return std::numeric_limits<double>::quiet_NaN();
	}
}

double HTFProperties::cond(double T_K)
{
	/* Input: temperature [K]
	Output: conductivity [W/m-K]
	Converted to c++ from Fortran code Type 229 in November 2012 by Ty Neises
	Original author: Michael J. Wagner */

	double T_C = T_K - 273.15;

	switch(m_fluid)
	{
	case Air:
		return fmax(0.00145453 + 0.0000872152*T_K - 2.20614E-08*T_K*T_K,1.e-4);
	case Stainless_AISI316:
		return 3E-09*pow(T_K,3) - 8E-06*pow(T_K,2) + 0.0177*T_K + 7.7765;
	case Salt_68_KCl_32_MgCl2:

		// Xu et al. 2018 Experimental Test of Properties of KCl-MgCl2 Eutectic Molten Salt for Heat Transfer and Thermal Storage Fluid in Concentrated Solar Power Systems
		return (-1.0000E-04*T_C + 5.0470E-01);	//[W/m-K]

		// return 0.39;			// replaced 20.04.06 twn

	case Salt_8_NaF_92_NaBF4:
		return 0.5;
	case Salt_25_KF_75_KBF4:
		return 0.4;
	case Salt_31_RbF_69_RbBF4:
		return 0.28;
	case Salt_465_LiF_115_NaF_42KF:
		return 0.92;
	case Salt_49_LiF_29_NaF_29_ZrF4:
		return 0.53;
	case Salt_58_KF_42_ZrF4:
		return 0.45;
	case Salt_58_LiCl_42_RbCl:
		return 0.39;
	case Salt_58_NaCl_42_MgCl2:
		return 0.43;
	case Salt_595_LiCl_405_KCl:
		return 0.43;
	case Salt_595_NaF_405_ZrF4:
		return 0.49;
	case Salt_60_NaNO3_40_KNO3:
		return -1E-11*pow(T_K,3) + 3E-08*pow(T_K,2) + 0.0002*T_K + 0.3922;
	case Nitrate_Salt:
		return 0.443 + 0.00019 * T_C;
	case Caloria_HT_43:
		return fmax(-0.00014 * T_C + 0.1245,.01);
	case Hitec_XL:
		return 0.519;
	case Therminol_VP1:
		return fmax(0.1381 - 0.00008708 * T_C - 0.0000001729 * pow(T_C,2),.001);
	case Hitec:
		return 0.588 - 0.000647 * T_C;
	case Dowtherm_Q:
		return fmax(-0.0000000626555 * pow(T_C,2) - 0.000124864 * T_C + 0.124379,1.e-5);		// Russ 10-2-03
	case Dowtherm_RP:
		return -0.00012963 * T_C + 0.13397;			// Russ 10-2-03
	case Argon_ideal:
		return 0.00548 + 0.0000438969*T_K - 6.81410E-09*T_K*T_K;
	case Hydrogen_ideal:
		return fmax(0.0302888 + 0.00053634*T_K - 1.59604E-07*T_K*T_K,.01);
	case T91_Steel:	//"Thermo hydraulic optimisation of the EURISOL DS target" - Paul Scherrer Institut
		return -2.E-5*T_C*T_C + 0.017*T_C + 25.535;
	case Therminol_66:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
		return -2.E-7*T_C*T_C - 3.E-5*T_C + 0.1183;
	case Therminol_59:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
		return -1.E-7*T_C*T_C - 6.E-5*T_C + 0.1227;
	case Pressurized_Water:
		return -6.E-6*T_C*T_C + 0.0016*T_C*T_C + 0.5631;
    case N06230:
        return 0.0197*T_C + 8.5359; // BPVC II D
    case N07740:
        return 0.0155*T_C + 9.7239;  // BPVC_CC_BPV_2017 Case 2702 - 3
	case User_defined:
		if ( m_userTable.nrows() < 3 )
					return std::numeric_limits<double>::quiet_NaN();

		// Interpolate
		return User_Defined_Props.linear_1D_interp( 0, 5, T_C );
	default:
		return std::numeric_limits<double>::quiet_NaN();
	}
}

double HTFProperties::temp(double H)
{
	/*Inputs: enthalpy [J/kg]
	Outputs: temperature [K]
	Converted to c++ from Fortran code Type 229 in November 2012 by Ty Neises
	Original author: Michael J. Wagner */

	double H_kJ;

	switch(m_fluid)
	{
	case Nitrate_Salt:
		return -0.0000000000262*H*H + 0.0006923 * H + 0.03058;
	case Caloria_HT_43:
		return 6.4394E-17*pow(H,3) - 0.00000000023383*pow(H,2) + 0.0005821*H + 1.2744;
	case Hitec_XL:
		return 0.00000000005111*H*H + 0.0006466*H + 0.2151;
	case Therminol_VP1:
		return 7.4333E-17*pow(H,3) - 0.00000000024625*pow(H,2) + 0.00063282 * H + 12.403;
	case Hitec:
		return -3.309E-24*pow(H,2) + 0.000641 * H + 0.000000000001364;
	case Dowtherm_Q:
		return 6.186E-17*pow(H,3) - 0.00000000022211*pow(H,2) + 0.00059998 * H + 0.77742;
	case Dowtherm_RP:
		return 6.6607E-17*pow(H,3) - 0.00000000023347*pow(H,2) + 0.00061419 * H + 0.77419;
	case Therminol_66:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
		H_kJ = H / 1000.0;
		return -0.00018*H_kJ*H_kJ + 0.521*H_kJ + 7.;
	case Therminol_59:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
		H_kJ = H / 1000.0;
		return -0.000204*H_kJ*H_kJ + 0.539*H_kJ - 0.094;
	case User_defined:
		if ( m_userTable.nrows() < 3 )
					return std::numeric_limits<double>::quiet_NaN();

		// Interpolate
		return User_Defined_Props.linear_1D_interp( 6, 0, H );
	default:
		return std::numeric_limits<double>::quiet_NaN();
	}
}

double HTFProperties::enth(double T_K)
{
	/*Inputs: temperature [K]
	Outputs: enthalpy [J/kg]
	Converted to c++ from Fortran code Type 229 in November 2012 by Ty Neises
	Original author: Michael J. Wagner */

	double T_C = T_K - 273.15;

	switch(m_fluid)
	{
	case Nitrate_Salt:
		return 1443.*T_C + 0.086*T_C*T_C;
	case Caloria_HT_43:
		return 1.94*T_C*T_C + 1606.0*T_C;
	case Hitec_XL:
		return 1536*T_C - 0.1312*T_C*T_C - 0.0000379667*pow(T_C,3);
	case Therminol_VP1:
		return 1000. * (-18.34 + 1.498*T_C + 0.001377*T_C*T_C);
	case Hitec:
		return 1560. * T_C;
	case Dowtherm_Q:
		return (0.00151461*T_C*T_C + 1.59867*T_C - 0.0250596) * 1000.;    // Hank 10-2-03
	case Dowtherm_RP:
		return (0.0014879*T_C*T_C + 1.5609*T_C - 0.0024798) * 1000.;		// Hank 10-2-03
	case Therminol_66:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
		return 1000.*(0.0038*T_C*T_C + 1.4363*T_C + 1.6142);
	case Therminol_59:	//Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
		return 1000.*(0.0034*T_C*T_C + 1.5977*T_C - 0.0926);
	case Pressurized_Water:
		return 4.2711*T_C - 4.3272;
	case User_defined:
		if ( m_userTable.nrows() < 3 )
		return std::numeric_limits<double>::quiet_NaN();

		// Interpolate
		return User_Defined_Props.linear_1D_interp( 0, 6, T_C );
	default:
		return std::numeric_limits<double>::quiet_NaN();
	}
}

double HTFProperties::Cv(double T_K)
{
	/*Inputs: temperature [K]
	Outputs: constant volume specific heat [kJ/kg-k]
	Converted to c++ from Fortran code Type 229 in November 2012 by Ty Neises
	Original author: Michael J. Wagner */

	switch(m_fluid)
	{
	case Air:
		return 0.750466 - 0.000305497*T_K + 7.49335E-07*T_K*T_K - 3.39363E-10*pow(T_K,3);
	case Argon_ideal:
		return 0.3122;
	case Hydrogen_ideal:
		return fmin(fmax(-49.5264 + 0.690156*T_K - 0.00327354*T_K*T_K + 0.00000817326*pow(T_K,3) - 1.13234E-08*pow(T_K,4) + 8.24995E-12*pow(T_K,5) - 2.46804E-15*pow(T_K,6),7.20),10.60);
	default:
		return std::numeric_limits<double>::quiet_NaN();
	}
}

double HTFProperties::kin_visc(double T_K, double P)
{
	// Inputs: temperature [K], pressure [Pa]
	// Outpus: kinematic viscosity [m^2/s]
	double k_visc = visc(T_K)/dens(T_K, P);
	return k_visc;
}

double HTFProperties::therm_diff(double T_K, double P)
{
	// Inputs: temperature [K], pressure [Pa]
	// Outputs: thermal diffusivity [m^2/s]
	double diff = cond(T_K) / (dens(T_K, P) * Cp(T_K) * 1000.);
	return diff;
}

double HTFProperties::Pr(double T_K, double P)
{
	// Inputs: temperature [K], pressure [Pa]
	// Outputs: Prandtl number [-]
	double Pr_num = visc(T_K) / (dens(T_K, P) * therm_diff(T_K,P)); //ARD changed pressure in density call to 'P' instead of '0.0'. This affects calculation for Argon, Hydrogen, and air.
	return Pr_num;
}

double HTFProperties::Re(double T_K, double P, double vel, double d)
{
	// Inputs: temperature [K], pressure [Pa], velocity [m/s], characteristic length [m]
	// Outputs: Reynolds number [-]
	double Re_num = dens(T_K, P) * vel * d / visc(T_K);
	return Re_num;
}
