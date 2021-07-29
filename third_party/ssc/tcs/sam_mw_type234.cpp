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

#define _TCSTYPEINTERFACE_
#include "tcstype.h"
//#include "waterprop.h"
#include "water_properties.h"
#include "sam_csp_util.h"

using namespace std;

enum{
	// Parameters
	P_P_REF,       
	P_ETA_REF,     
	P_T_HOT_REF,   
	P_T_COLD_REF,  
	P_DT_CW_REF,   
	P_T_AMB_DES,   
	P_Q_SBY_FRAC,  
	P_P_BOIL_DES,      
	P_IS_RH,       
	P_P_RH_REF,    
	P_T_RH_HOT_REF,
	P_RH_FRAC_REF, 
	P_CT,          
	P_STARTUP_TIME,
	P_STARTUP_FRAC,
	P_TECH_TYPE,   
	P_T_APPROACH,  
	P_T_ITD_DES,   
	P_P_COND_RATIO,
	P_PB_BD_FRAC,  
	P_P_COND_MIN,  
	P_N_PL_INC,    
	P_F_WC,      

	// Inputs
	I_MODE,           
	I_T_HOT,          
	I_M_DOT_ST,       
	I_T_WB,           
	I_DEMAND_VAR,     
	I_STANDBY_CONTROL,
	I_T_DB,           
	I_P_AMB,          
	I_TOU,            
	I_RH,             
	I_F_RECSU,
	I_DP_B,
	I_DP_SH,          
	I_DP_RH,       

	// Outputs
	O_P_CYCLE,     
	O_ETA,         
	O_T_COLD,      
	O_M_DOT_MAKEUP,
	O_M_DOT_DEMAND,
	O_M_DOT_OUT,   
	O_M_DOT_REF,   
	O_W_COOL_PAR,  
	O_P_REF_OUT,   
	O_F_BAYS,      
	O_P_COND,      
	O_P_BOILER_IN,     
	O_F_RH,        
	O_P_RH_IN,     
	O_T_RH_IN,     
	O_T_RH_OUT,    

	//Include N_max
	N_MAX
};

tcsvarinfo sam_mw_type234_variables[] = {
	{ TCS_PARAM,          TCS_NUMBER,      P_P_REF,                  "P_ref",           "Reference output electric power at design condition",               "MW",     "",   "",   "111" },
	{ TCS_PARAM,          TCS_NUMBER,      P_ETA_REF,                "eta_ref",         "Reference conversion efficiency at design condition",               "none",   "",   "",   "0.3774" },
	{ TCS_PARAM,          TCS_NUMBER,      P_T_HOT_REF,              "T_hot_ref",       "Reference HTF inlet temperature at design",                         "C",      "",   "",   "391" },
	{ TCS_PARAM,          TCS_NUMBER,      P_T_COLD_REF,             "T_cold_ref",      "Reference HTF outlet temperature at design",                        "C",      "",   "",   "293" },
	{ TCS_PARAM,          TCS_NUMBER,      P_DT_CW_REF,              "dT_cw_ref",       "Reference condenser cooling water inlet/outlet T diff",             "C",      "",   "",   "10" },
	{ TCS_PARAM,          TCS_NUMBER,      P_T_AMB_DES,              "T_amb_des",       "Reference ambient temperature at design point",                     "C",      "",   "",   "20" },																																						     
	{ TCS_PARAM,          TCS_NUMBER,      P_Q_SBY_FRAC,             "q_sby_frac",      "Fraction of thermal power required for standby mode",               "none",  "",    "",   "0.2" },
	{ TCS_PARAM,          TCS_NUMBER,      P_P_BOIL_DES,             "P_boil_des",      "Boiler operating pressure @ design",                                "bar",   "",    "",   "100" },	
	{ TCS_PARAM,          TCS_NUMBER,      P_IS_RH,                  "is_rh",           "Flag indicating whether reheat is used 0:no, 1:yes",                "none",  "",    "",   "1" },
	{ TCS_PARAM,          TCS_NUMBER,      P_P_RH_REF,               "P_rh_ref",        "Reheater operating pressure at design",                             "bar",   "",    "",   "40" },
	{ TCS_PARAM,          TCS_NUMBER,      P_T_RH_HOT_REF,           "T_rh_hot_ref",    "Reheater design outlet temperature",                                "C",     "",    "",   "500" },
	{ TCS_PARAM,          TCS_NUMBER,      P_RH_FRAC_REF,            "rh_frac_ref",     "Reheater flow fraction at design",                                  "none",  "",    "",   "0.9" },																																						     																																						     
	{ TCS_PARAM,          TCS_NUMBER,      P_CT,                     "CT",              "Flag for using dry cooling or wet cooling system",                  "none",  "",    "",   "1" },
	{ TCS_PARAM,          TCS_NUMBER,      P_STARTUP_TIME,           "startup_time",    "Time needed for power block startup",                               "hr",    "",    "",   "0.5" },
	{ TCS_PARAM,          TCS_NUMBER,      P_STARTUP_FRAC,           "startup_frac",    "Fraction of design thermal power needed for startup",               "none",  "",    "",   "0.2" },
	{ TCS_PARAM,          TCS_NUMBER,      P_TECH_TYPE,              "tech_type",       "Flag indicating which coef. set to use. (1=tower,2=trough,3=user)", "none",  "",    "",   "2" },
	{ TCS_PARAM,          TCS_NUMBER,      P_T_APPROACH,             "T_approach",      "Cooling tower approach temperature",                                "C",     "",    "",   "5" },
	{ TCS_PARAM,          TCS_NUMBER,      P_T_ITD_DES,              "T_ITD_des",       "ITD at design for dry system",                                      "C",     "",    "",   "16" },
	{ TCS_PARAM,          TCS_NUMBER,      P_P_COND_RATIO,           "P_cond_ratio",    "Condenser pressure ratio",                                          "none",  "",    "",   "1.0028" },
	{ TCS_PARAM,          TCS_NUMBER,      P_PB_BD_FRAC,             "pb_bd_frac",      "Power block blowdown steam fraction ",                              "none",  "",    "",   "0.02" },
	{ TCS_PARAM,          TCS_NUMBER,      P_P_COND_MIN,             "P_cond_min",      "Minimum condenser pressure",                                        "inHg",  "",    "",   "1.25" },
	{ TCS_PARAM,          TCS_NUMBER,      P_N_PL_INC,               "n_pl_inc",        "Number of part-load increments for the heat rejection system",      "none",  "",    "",   "2" },
	{ TCS_PARAM,          TCS_ARRAY,       P_F_WC,                   "F_wc",            "Fraction indicating wet cooling use for hybrid system",             "none",  "9 indices for each TOU Period",    "",   "0,0,0,0,0,0,0,0,0" },

	{ TCS_INPUT,          TCS_NUMBER,      I_MODE,                   "mode",            "Cycle part load control, from plant controller",                    "none",  "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_T_HOT,                  "T_hot",           "Hot HTF inlet temperature, from storage tank",                      "C",     "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_M_DOT_ST,               "m_dot_st",        "HTF mass flow rate",                                                "kg/hr", "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_T_WB,                   "T_wb",            "Ambient wet bulb temperature",                                      "C",     "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_DEMAND_VAR,             "demand_var",      "Control signal indicating operational mode",                        "none",  "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_STANDBY_CONTROL,        "standby_control", "Control signal indicating standby mode",                            "none",  "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_T_DB,                   "T_db",            "Ambient dry bulb temperature",                                      "C",     "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_P_AMB,                  "P_amb",           "Ambient pressure",                                                  "atm",   "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_TOU,                    "TOU",             "Current Time-of-use period",                                        "none",  "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_RH,                     "relhum",          "Relative humidity of the ambient air",                              "none",  "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_F_RECSU,                "f_recSU",         "Fraction powerblock can run due to receiver startup",               "none",  "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_DP_B,                   "dp_b",            "Pressure drop in boiler",                                           "Pa",    "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_DP_SH,                  "dp_sh",           "Pressure drop in superheater",                                      "Pa",    "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_DP_RH,                  "dp_rh",           "Pressure drop in reheater",                                         "Pa",    "",    "",   "" },


	{ TCS_OUTPUT,          TCS_NUMBER,     O_P_CYCLE,                "P_cycle",         "Cycle power output",                                                "MWe",   "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_ETA,                    "eta",             "Cycle thermal efficiency",                                          "none",  "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_T_COLD,                 "T_cold",          "Heat transfer fluid outlet temperature ",                           "C",     "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_M_DOT_MAKEUP,           "m_dot_makeup",    "Cooling water makeup flow rate",                                    "kg/hr", "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_M_DOT_DEMAND,           "m_dot_demand",    "HTF required flow rate to meet power load",                         "kg/hr", "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_M_DOT_OUT,              "m_dot_out",       "Actual HTF flow rate passing through the power cycle",              "kg/hr", "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_M_DOT_REF,              "m_dot_ref",       "Calculated reference HTF flow rate at design",                      "kg/hr", "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_W_COOL_PAR,             "W_cool_par",      "Cooling system parasitic load",                                     "MWe",   "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_P_REF_OUT,              "P_ref_out",       "Reference power level output at design (mirror param)",             "MWe",   "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_F_BAYS,                 "f_bays",          "Fraction of operating heat rejection bays",                         "none",  "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_P_COND,                 "P_cond",          "Condenser pressure",                                                "Pa",    "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_P_BOILER_IN,            "P_boiler_in",     "Superheater inlet pressure",                                        "bar",   "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_F_RH,                   "f_rh",            "Reheat mass flow fraction",                                         "none",  "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_P_RH_IN,                "P_rh_in",         "Reheater inlet pressure",                                           "bar",   "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_T_RH_IN,                "T_rh_in",         "Reheater inlet temperature",                                        "C",     "",    "",   "" },
	{ TCS_OUTPUT,          TCS_NUMBER,     O_T_RH_OUT,               "T_rh_out",        "Reheater outlet temperature",                                       "C",     "",    "",   "" },


	{ TCS_INVALID,    TCS_INVALID,    N_MAX,                0,                    0,                                                        0,                0,        0,        0 }
};

class sam_mw_type234 : public tcstypeinterface
{
private:
	
	water_state wp;
	P_max_check check_pressure;

	double m_P_ref;			
	double m_eta_ref;		
	double m_T_hot_ref;
	double m_T_cold_ref;	
	double m_dT_cw_ref;		
	double m_T_amb_des;		
	double m_q_sby_frac;	
	double m_P_boil_des;		
	bool   m_is_rh;			
	double m_P_rh_ref;		
	double m_T_rh_hot_ref;	
	double m_rh_frac_ref;	
	int    m_CT;			
	double m_startup_time;	
	double m_startup_frac;	
	int    m_tech_type;		
	double m_T_approach;	
	double m_T_ITD_des;		
	double m_P_cond_ratio;	
	double m_pb_bd_frac;	
	double m_P_cond_min;	
	int    m_n_pl_inc;	
	double m_F_wc[9];

	double m_F_wcmin;
	double m_F_wcmax;

	double m_P_max;
	double m_startup_energy;
	double m_Psat_ref;
	double m_eta_adj;
	double m_q_dot_ref;
	double m_m_dot_ref;
	double m_q_dot_rh_ref;
	double m_q_dot_st_ref;

	util::matrix_t<double> m_db;

	int    m_standby_control_prev;
	int    m_standby_control;
	double m_time_su_prev;
	double m_time_su;
	double m_E_su_prev;
	double m_E_su;
	
public:

	sam_mw_type234(tcscontext *cxt, tcstypeinfo *ti)
		: tcstypeinterface(cxt, ti)
	{
		m_P_ref = std::numeric_limits<double>::quiet_NaN();			
		m_eta_ref = std::numeric_limits<double>::quiet_NaN();		
		m_T_hot_ref = std::numeric_limits<double>::quiet_NaN();
		m_T_cold_ref = std::numeric_limits<double>::quiet_NaN();	
		m_dT_cw_ref = std::numeric_limits<double>::quiet_NaN();		
		m_T_amb_des = std::numeric_limits<double>::quiet_NaN();		
		m_q_sby_frac = std::numeric_limits<double>::quiet_NaN();	
		m_P_boil_des = std::numeric_limits<double>::quiet_NaN();		
		m_is_rh = false;			
		m_P_rh_ref = std::numeric_limits<double>::quiet_NaN();		
		m_T_rh_hot_ref = std::numeric_limits<double>::quiet_NaN();	
		m_rh_frac_ref = std::numeric_limits<double>::quiet_NaN();	
		m_CT = -1;			
		m_startup_time = std::numeric_limits<double>::quiet_NaN();	
		m_startup_frac = std::numeric_limits<double>::quiet_NaN();	
		m_tech_type = -1;		
		m_T_approach = std::numeric_limits<double>::quiet_NaN();	
		m_T_ITD_des = std::numeric_limits<double>::quiet_NaN();		
		m_P_cond_ratio = std::numeric_limits<double>::quiet_NaN();	
		m_pb_bd_frac = std::numeric_limits<double>::quiet_NaN();	
		m_P_cond_min = std::numeric_limits<double>::quiet_NaN();	
		m_n_pl_inc = -1;	

		for (unsigned i = 0; i < sizeof(m_F_wc) / sizeof(double); i++) {
		    m_F_wc[i] = std::numeric_limits<double>::quiet_NaN();
		}
		
		m_F_wcmin = std::numeric_limits<double>::quiet_NaN();
		m_F_wcmax = std::numeric_limits<double>::quiet_NaN();
		
		m_P_max = std::numeric_limits<double>::quiet_NaN();
		m_startup_energy = std::numeric_limits<double>::quiet_NaN();
		m_Psat_ref = std::numeric_limits<double>::quiet_NaN();
		m_eta_adj = std::numeric_limits<double>::quiet_NaN();
		m_q_dot_ref = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_ref = std::numeric_limits<double>::quiet_NaN();
		m_q_dot_rh_ref = std::numeric_limits<double>::quiet_NaN();
		m_q_dot_st_ref = std::numeric_limits<double>::quiet_NaN();
		
		m_standby_control_prev = -1;
		m_standby_control = -1;
		m_time_su_prev = std::numeric_limits<double>::quiet_NaN();
		m_time_su = std::numeric_limits<double>::quiet_NaN();
		m_E_su_prev = std::numeric_limits<double>::quiet_NaN();
		m_E_su = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_mw_type234(){
	}

	virtual int init()
	{

		//double tstep = time_step();

		m_P_ref			= value( P_P_REF )*1.E3;				//[kW] Reference output electric power at design condition
		m_eta_ref		= value( P_ETA_REF );					//[-] Reference conversion efficiency at design condition		
		m_T_hot_ref		= value( P_T_HOT_REF );					//[C] Reference inlet temperature at design
		m_T_cold_ref	= value( P_T_COLD_REF );				//[C] Reference outlet temperature at design
		m_dT_cw_ref		= value( P_DT_CW_REF );					//[C] Reference condenser cooling water inlet/outlet Temp difference
		m_T_amb_des		= value( P_T_AMB_DES );					//[C] Reference ambient temperature at design point
		m_q_sby_frac	= value( P_Q_SBY_FRAC );				//[-] Fraction of thermal power required for standby mode
		m_P_boil_des	= value( P_P_BOIL_DES );				//[bar] Boiler operating pressure at design
		m_is_rh			= (bool) (value( P_IS_RH ) != 0);				//[-] Flag indicating whether reheat is used 0:no, 1:yes
		m_P_rh_ref		= value( P_P_RH_REF );					//[bar] Reheater operating pressure at design
		m_T_rh_hot_ref	= value( P_T_RH_HOT_REF );				//[C] Reheater design outlet temperature
		m_rh_frac_ref	= value( P_RH_FRAC_REF );				//[-] Reheater flow fraction at design
		m_CT			= (int) value( P_CT );					//[-] Flag for using dry cooling or wet cooling system
		m_startup_time	= value( P_STARTUP_TIME );				//[hr] Time needed for power block startup
		m_startup_frac	= value( P_STARTUP_FRAC );				//[-] Fraction of design thermal power needed for startup
		m_tech_type		= (int) value( P_TECH_TYPE );			//[-] Flag indicating which coef. set to use. (1=tower..2=trough..3=user..5=DSG tower)
		m_T_approach	= value( P_T_APPROACH );				//[C] Cooling tower approach temperature
		m_T_ITD_des		= value( P_T_ITD_DES );					//[C] ITD at design for dry system
		m_P_cond_ratio	= value( P_P_COND_RATIO );				//[-] Condenser pressure ratio
		m_pb_bd_frac	= value( P_PB_BD_FRAC );				//[-] Power block blowdown steam fraction
		m_P_cond_min	= value( P_P_COND_MIN )*3386.388667;	//[inHg] Minimum condenser pressure
		m_n_pl_inc		= (int) value( P_N_PL_INC );			//[-] Number of part-load increments for the heat rejection system	

		double* F_wc_in;							//Fraction indicating wet cooling use for hybrid system
		int nval_F_wc;
		F_wc_in = value(P_F_WC, &nval_F_wc);		//Fraction indicating wet cooling use for hybrid system [none]

		if( nval_F_wc != 9 )
			return -1;

		m_F_wcmax = 0.0;
		m_F_wcmin = 1.0;
		for(int i=0; i<9; i++)
		{
			m_F_wc[i] = F_wc_in[i];
			m_F_wcmin = min( m_F_wcmin, m_F_wc[i] );
			m_F_wcmax = max( m_F_wcmax, m_F_wc[i] );
		}

		//m_fcall = true;
		m_P_max = 190.0;		//[bar]

		if( m_P_boil_des > m_P_max )
		{
			m_P_boil_des = m_P_max;
			// return angry warning
		}

		check_pressure.set_P_max( m_P_max );

		// Calculate the startup energy needed
		m_startup_energy = m_startup_frac*m_P_ref/m_eta_ref;		//[kWt]

		// Initialize stored variables
		m_standby_control_prev = 3;
		m_time_su_prev = m_startup_time;
		m_E_su_prev = m_startup_energy;

		m_time_su = m_time_su_prev;
		m_E_su = m_E_su_prev;

		if( m_P_boil_des > 190.0 )
		{
			m_P_boil_des = 190.0;		// [bar]
			// Return warning...
		}

		// Initialize coefficients
		Set_PB_coefficients();

		// Initialize Power Cycle models
		Set_PB_ref_values();

		return 0;
	}

	void Set_PB_coefficients(){
		if( m_tech_type == 1)
		{
			double dTemp[18][20] =
			{
				{0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316, 0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895, 0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474, 1.14737, 1.20000},
				{0.16759, 0.21750, 0.26932, 0.32275, 0.37743, 0.43300, 0.48910, 0.54545, 0.60181, 0.65815, 0.71431, 0.77018, 0.82541, 0.88019, 0.93444, 0.98886, 1.04378, 1.09890, 1.15425, 1.20982},
				{0.19656, 0.24969, 0.30325, 0.35710, 0.41106, 0.46497, 0.51869, 0.57215, 0.62529, 0.67822, 0.73091, 0.78333, 0.83526, 0.88694, 0.93838, 0.98960, 1.04065, 1.09154, 1.14230, 1.19294},
				{3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00},
				{1.07401, 1.04917, 1.03025, 1.01488, 1.00201, 0.99072, 0.98072, 0.97174, 0.96357, 0.95607, 0.94914, 0.94269, 0.93666, 0.93098, 0.92563, 0.92056, 0.91573, 0.91114, 0.90675, 0.90255},
				{1.00880, 1.00583, 1.00355, 1.00168, 1.00010, 0.99870, 0.99746, 0.99635, 0.99532, 0.99438, 0.99351, 0.99269, 0.99193, 0.99121, 0.99052, 0.98988, 0.98926, 0.98867, 0.98810, 0.98756},
				{0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000},
				{0.09403, 0.16542, 0.23861, 0.31328, 0.38901, 0.46540, 0.54203, 0.61849, 0.69437, 0.76928, 0.84282, 0.91458, 0.98470, 1.05517, 1.12536, 1.19531, 1.26502, 1.33450, 1.40376, 1.47282},
				{0.10659, 0.18303, 0.25848, 0.33316, 0.40722, 0.48075, 0.55381, 0.62646, 0.69873, 0.77066, 0.84228, 0.91360, 0.98464, 1.05542, 1.12596, 1.19627, 1.26637, 1.33625, 1.40593, 1.47542},
				{0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316, 0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895, 0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474, 1.14737, 1.20000},
				{1.03323, 1.04058, 1.04456, 1.04544, 1.04357, 1.03926, 1.03282, 1.02446, 1.01554, 1.00944, 1.00487, 1.00169, 0.99986, 0.99926, 0.99980, 1.00027, 1.00021, 1.00015, 1.00006, 0.99995},
				{0.98344, 0.98630, 0.98876, 0.99081, 0.99247, 0.99379, 0.99486, 0.99574, 0.99649, 0.99716, 0.99774, 0.99826, 0.99877, 0.99926, 0.99972, 1.00017, 1.00060, 1.00103, 1.00143, 1.00182},
				{3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00},
				{0.99269, 0.99520, 0.99718, 0.99882, 1.00024, 1.00150, 1.00264, 1.00368, 1.00464, 1.00554, 1.00637, 1.00716, 1.00790, 1.00840, 1.00905, 1.00965, 1.01022, 1.01075, 1.01126, 1.01173},
				{0.99768, 0.99861, 0.99933, 0.99992, 1.00043, 1.00087, 1.00127, 1.00164, 1.00197, 1.00227, 1.00255, 1.00282, 1.00307, 1.00331, 1.00353, 1.00375, 1.00395, 1.00415, 1.00433, 1.00451},
				{0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000},
				{1.00812, 1.00513, 1.00294, 1.00128, 0.99980, 0.99901, 0.99855, 0.99836, 0.99846, 0.99883, 0.99944, 1.00033, 1.00042, 1.00056, 1.00069, 1.00081, 1.00093, 1.00104, 1.00115, 1.00125},
				{1.09816, 1.07859, 1.06487, 1.05438, 1.04550, 1.03816, 1.03159, 1.02579, 1.02061, 1.01587, 1.01157, 1.00751, 1.00380, 1.00033, 0.99705, 0.99400, 0.99104, 0.98832, 0.98565, 0.98316},
			};
			m_db.assign( dTemp[0], 18, 20 );
		}
				
		if( m_tech_type == 2 )
		{
			double dTemp[18][20] =
			{
				{0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211, 0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263, 0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316, 1.33158, 1.40000},
				{0.08547, 0.14823, 0.21378, 0.28166, 0.35143, 0.42264, 0.49482, 0.56747, 0.64012, 0.71236, 0.78378, 0.85406, 0.92284, 0.98989, 1.05685, 1.12369, 1.19018, 1.25624, 1.32197, 1.38744},
				{0.10051, 0.16934, 0.23822, 0.30718, 0.37623, 0.44534, 0.51443, 0.58338, 0.65209, 0.72048, 0.78848, 0.85606, 0.92317, 0.98983, 1.05604, 1.12182, 1.18718, 1.25200, 1.31641, 1.38047},
				{3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00},
				{1.08827, 1.06020, 1.03882, 1.02145, 1.00692, 0.99416, 0.98288, 0.97273, 0.96350, 0.95504, 0.94721, 0.93996, 0.93314, 0.92673, 0.92069, 0.91496, 0.90952, 0.90433, 0.89938, 0.89464},
				{1.01276, 1.00877, 1.00570, 1.00318, 1.00106, 0.99918, 0.99751, 0.99601, 0.99463, 0.99335, 0.99218, 0.99107, 0.99004, 0.98907, 0.98814, 0.98727, 0.98643, 0.98563, 0.98487, 0.98413},
				{0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000},
				{0.09307, 0.16421, 0.23730, 0.31194, 0.38772, 0.46420, 0.54098, 0.61763, 0.69374, 0.76896, 0.84287, 0.91511, 0.98530, 1.05512, 1.12494, 1.19447, 1.26373, 1.33273, 1.40148, 1.46999},
				{0.10741, 0.18443, 0.26031, 0.33528, 0.40950, 0.48308, 0.55610, 0.62861, 0.70066, 0.77229, 0.84354, 0.91443, 0.98497, 1.05520, 1.12514, 1.19478, 1.26416, 1.33329, 1.40217, 1.47081},
				{0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211, 0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263, 0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316, 1.33158, 1.40000},
				{1.01749, 1.03327, 1.04339, 1.04900, 1.05051, 1.04825, 1.04249, 1.03343, 1.02126, 1.01162, 1.00500, 1.00084, 0.99912, 0.99966, 0.99972, 0.99942, 0.99920, 0.99911, 0.99885, 0.99861},
				{0.99137, 0.99297, 0.99431, 0.99564, 0.99681, 0.99778, 0.99855, 0.99910, 0.99948, 0.99971, 0.99984, 0.99989, 0.99993, 0.99993, 0.99992, 0.99992, 0.99992, 1.00009, 1.00010, 1.00012},
				{3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00},
				{0.99653, 0.99756, 0.99839, 0.99906, 0.99965, 1.00017, 1.00063, 1.00106, 1.00146, 1.00183, 1.00218, 1.00246, 1.00277, 1.00306, 1.00334, 1.00361, 1.00387, 1.00411, 1.00435, 1.00458},
				{0.99760, 0.99831, 0.99888, 0.99934, 0.99973, 1.00008, 1.00039, 1.00067, 1.00093, 1.00118, 1.00140, 1.00161, 1.00180, 1.00199, 1.00217, 1.00234, 1.00250, 1.00265, 1.00280, 1.00294},
				{0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000},
				{1.01994, 1.01645, 1.01350, 1.01073, 1.00801, 1.00553, 1.00354, 1.00192, 1.00077, 0.99995, 0.99956, 0.99957, 1.00000, 0.99964, 0.99955, 0.99945, 0.99937, 0.99928, 0.99919, 0.99918},
				{1.02055, 1.01864, 1.01869, 1.01783, 1.01508, 1.01265, 1.01031, 1.00832, 1.00637, 1.00454, 1.00301, 1.00141, 1.00008, 0.99851, 0.99715, 0.99586, 0.99464, 0.99347, 0.99227, 0.99177},
			};
			m_db.assign( dTemp[0], 18, 20 );
		}

		if( m_tech_type == 3 )
		{
            double dTemp[18][10] =
            {
                {0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556, 0.76667, 0.87778, 0.98889, 1.10000}, // 1, A: Independent Variable - normalized turbine inlet temperature [-]
                {0.89280, 0.90760, 0.92160, 0.93510, 0.94820, 0.96110, 0.97370, 0.98620, 0.99860, 1.01100},	// 2, PA: Main effect, normalized power at A 
                {0.93030, 0.94020, 0.94950, 0.95830, 0.96690, 0.97520, 0.98330, 0.99130, 0.99910, 1.00700},	// 3, QA: Main effect, normalized heat at A 
                {4000.00, 6556.00, 9111.00, 11677.0, 14222.0, 16778.0, 19333.0, 21889.0, 24444.0, 27000.0},	// 4, B: Independent Variable - Condenser Pressure [Pa]
                {1.04800, 1.01400, 0.99020, 0.97140, 0.95580, 0.94240, 0.93070, 0.92020, 0.91060, 0.90190},	// 5, PA: Main effect, normalized power at B 
                {0.99880, 0.99960, 1.00000, 1.00100, 1.00100, 1.00100, 1.00100, 1.00200, 1.00200, 1.00200},	// 6, QB: Main effect, normalized heat at B 
                {0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333, 0.90000, 1.01667, 1.13333, 1.25000},	// 7, C: Independent Variable - normalized steam mass flow rate [-]
                {0.16030, 0.27430, 0.39630, 0.52310, 0.65140, 0.77820, 0.90060, 1.01600, 1.12100, 1.21400}, // 8, PC: Main effect, normalized power at C
                {0.22410, 0.34700, 0.46640, 0.58270, 0.69570, 0.80550, 0.91180, 1.01400, 1.11300, 1.20700},	// 9, QC: Main effect, normalized heat at C
                {0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556, 0.76667, 0.87778, 0.98889, 1.10000},	// 10, AC: Independent Variable for AC interactions - (same as A)
                {1.05802, 1.05127, 1.04709, 1.03940, 1.03297, 1.02480, 1.01758, 1.00833, 1.00180, 0.99307},	// 11, PAC: Interaction effect of A on power vs C
                {1.03671, 1.03314, 1.02894, 1.02370, 1.01912, 1.01549, 1.01002, 1.00486, 1.00034, 0.99554},	// 12, QAC: Interaction effect of A on heat vs C
                {4000.00, 6556.00, 9111.00, 11677.0, 14222.0, 16778.0, 19333.0, 21889.0, 24444.0, 27000.0},	// 13, BA: Independent Variable for BA interactions - (same as B)
                {1.00825, 0.98849, 0.99742, 1.02080, 1.02831, 1.03415, 1.03926, 1.04808, 1.05554, 1.05862},	// 14, PBA: Interaction effect of B on power vs A
                {1.01838, 1.02970, 0.99785, 0.99663, 0.99542, 0.99183, 0.98897, 0.99299, 0.99013, 0.98798},	// 15, QBA: Interaction effect of B on heat vs A	//!tweaked entry #4 to be the average of 3 and 5. it was an outlier in the simulation. mjw 3.31.11
                {0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333, 0.90000, 1.01667, 1.13333, 1.25000},	// 16, CD: Independent Variable for CB interactions - (same as C)
                {1.43311, 1.27347, 1.19090, 1.13367, 1.09073, 1.05602, 1.02693, 1.00103, 0.97899, 0.95912},	// 17, PCB: Interaction effect of C on power vs B
                {0.48342, 0.64841, 0.64322, 0.74366, 0.76661, 0.82764, 0.97792, 1.15056, 1.23117, 1.31179},	// 18, QCB: Interaction effect of C on heat vs B		//!tweaked entry #9 to be the average of 8 and 10. it was an outlier in the simulation mjw 3.31.11
            };
            m_db.assign(dTemp[0], 18, 10);
		}

		if( m_tech_type == 5 )
		{
			double dTemp[24][20] = 
			{
				{0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316, 0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895, 0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474, 1.14737, 1.20000},
				{0.74230, 0.76080, 0.77870, 0.79620, 0.81340, 0.83050, 0.84740, 0.86440, 0.88120, 0.89780, 0.91440, 0.93090, 0.94730, 0.96380, 0.98030, 0.99670, 1.01300, 1.03000, 1.04700, 1.06300},
				{0.80770, 0.82580, 0.84220, 0.85740, 0.87170, 0.88510, 0.89800, 0.91030, 0.92220, 0.93380, 0.94500, 0.95600, 0.96670, 0.97730, 0.98770, 0.99790, 1.00800, 1.01800, 1.02800, 1.03800},
				{0.86950, 0.85270, 0.84380, 0.84070, 0.84210, 0.84730, 0.85550, 0.86630, 0.87890, 0.89280, 0.90780, 0.92390, 0.94090, 0.95860, 0.97710, 0.99620, 1.01600, 1.03600, 1.05700, 1.07800},
				{4000.00, 5368.42, 6736.84, 8105.26, 9473.68, 10842.11, 12210.53, 13578.95, 14947.37, 16315.79, 17684.21, 19052.63, 20421.05, 21789.47, 23157.89, 24526.32, 25894.74, 27263.16, 28631.58, 30000.00},
				{1.03800, 1.02200, 1.01000, 0.99930, 0.99030, 0.98230, 0.97520, 0.96870, 0.96280, 0.95730, 0.95220, 0.94740, 0.94300, 0.93870, 0.93480, 0.93100, 0.92730, 0.92390, 0.92060, 0.91740},
				{1.00100, 1.00100, 1.00000, 0.99990, 0.99950, 0.99920, 0.99890, 0.99860, 0.99840, 0.99810, 0.99790, 0.99770, 0.99750, 0.99730, 0.99720, 0.99700, 0.99690, 0.99670, 0.99660, 0.99640},
				{0.99430, 0.99670, 0.99860, 1.00000, 1.00200, 1.00300, 1.00400, 1.00500, 1.00600, 1.00700, 1.00700, 1.00800, 1.00900, 1.01000, 1.01000, 1.01100, 1.01100, 1.01200, 1.01200, 1.01300},
				{0.10000, 0.15263, 0.20526, 0.25789, 0.31053, 0.36316, 0.41579, 0.46842, 0.52105, 0.57368, 0.62632, 0.67895, 0.73158, 0.78421, 0.83684, 0.88947, 0.94211, 0.99474, 1.04737, 1.10000},
				{0.08098, 0.12760, 0.17660, 0.22780, 0.28070, 0.33520, 0.39090, 0.44730, 0.50030, 0.55780, 0.61510, 0.67200, 0.72820, 0.78370, 0.83820, 0.89160, 0.94400, 0.99500, 1.04500, 1.09300},
				{0.10940, 0.16700, 0.22450, 0.28210, 0.33970, 0.39730, 0.45490, 0.51240, 0.56520, 0.61620, 0.66630, 0.71560, 0.76420, 0.81190, 0.85890, 0.90520, 0.95070, 0.99550, 1.04000, 1.08300},
				{0.07722, 0.12110, 0.16690, 0.21440, 0.26350, 0.31380, 0.36520, 0.41720, 0.44960, 0.50670, 0.56500, 0.62440, 0.68480, 0.74580, 0.80740, 0.86940, 0.93160, 0.99390, 1.05600, 1.11800},
				{0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316, 0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895, 0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474, 1.14737, 1.20000},
				{0.98450, 0.98907, 0.99169, 0.99305, 0.99451, 0.99467, 0.99555, 0.99573, 0.99599, 0.99695, 0.99727, 0.99763, 0.99802, 0.99778, 0.99755, 0.99790, 1.00412, 1.00094, 0.99786, 1.00541},
				{0.96043, 0.96885, 0.97567, 0.98076, 0.98406, 0.98772, 0.99019, 0.99223, 0.99443, 0.99800, 0.99722, 0.99780, 1.00043, 0.99745, 1.00199, 1.00164, 1.00199, 1.00298, 0.99803, 0.99903},
				{1.13799, 1.12215, 1.10761, 1.09474, 1.08321, 1.07117, 1.06082, 1.05054, 1.04231, 1.03449, 1.03001, 1.02000, 1.01481, 1.01075, 1.00730, 1.00112, 0.99609, 0.99440, 0.98833, 0.98641},
				{4000.00, 5368.42, 6736.84, 8105.26, 9473.68, 10842.11, 12210.53, 13578.95, 14947.37, 16315.79, 17684.21, 19052.63, 20421.05, 21789.47, 23157.89, 24526.32, 25894.74, 27263.16, 28631.58, 30000.00},
				{0.99027, 1.00027, 1.00141, 1.00607, 1.00445, 1.01006, 1.00791, 1.01656, 1.02015, 1.02004, 1.02471, 1.02659, 1.02510, 1.02729, 1.02884, 1.03094, 1.03294, 1.03397, 1.03576, 1.03745},
				{0.99692, 0.99900, 0.99870, 1.00116, 0.99392, 0.99817, 1.00268, 0.99376, 0.99685, 1.00111, 1.00394, 0.99386, 0.99669, 0.99953, 1.00121, 1.00405, 1.00573, 0.99511, 0.99679, 0.99964},
				{1.02527, 1.01447, 1.00819, 0.99962, 0.99609, 0.99128, 0.98686, 0.98321, 0.97395, 0.97108, 0.97250, 0.97040, 0.96232, 0.96099, 0.95682, 0.95587, 0.95208, 0.95152, 0.94811, 0.94756},
				{0.10000, 0.15263, 0.20526, 0.25789, 0.31053, 0.36316, 0.41579, 0.46842, 0.52105, 0.57368, 0.62632, 0.67895, 0.73158, 0.78421, 0.83684, 0.88947, 0.94211, 0.99474, 1.04737, 1.10000},
				{1.09510, 1.10157, 1.10663, 1.10675, 1.11166, 1.10757, 1.10278, 1.13312, 1.14401, 1.11992, 1.09445, 1.07305, 1.05616, 1.03925, 1.02666, 1.01617, 1.00719, 1.00320, 1.00355, 1.00405},
				{0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.35237, 0.98785, 0.97371, 1.07142, 1.10630, 1.09046, 1.18378, 1.20956, 1.17838, 1.21840, 1.24170, 1.01496, 1.43634},
				{3.04989, 3.04563, 3.06817, 3.07452, 3.07856, 3.06611, 3.07191, 1.77603, 0.66771, 0.72244, 0.75574, 0.79547, 0.83991, 0.86983, 0.90542, 0.93997, 0.96970, 0.98014, 1.00831, 1.06526}
			};
			m_db.assign( dTemp[0], 24, 20 );
		}
	};

	bool Set_PB_ref_values( )
	{
		/*The user provides a reference efficiency, ambient temperature, and cooling system parameters. Using
		this information, we have to adjust the provided reference efficiency to match the normalized efficiency
		that is part of the power block regression coefficients. I.e. if the user provides a ref. ambient temperature
		of 25degC, but the power block coefficients indicate that the normalized efficiency equals 1.0 at an ambient 
		temp of 20degC, we have to adjust the user's efficiency value back to the coefficient set.*/
				
		switch( m_CT )
		{
		case 1:
			if( m_tech_type != 4 )
			{
				water_TQ(m_dT_cw_ref + 3.0 + m_T_approach + m_T_amb_des + 273.15, 1.0, &wp);
				m_Psat_ref = wp.pres*1000.0;		// [Pa]
			}
			else
				m_Psat_ref = CSP::P_sat4( m_dT_cw_ref + 3.0 + m_T_approach + m_T_amb_des );
				
			break;

		case 2:
		case 3:
			if( m_tech_type != 4 )
			{
				water_TQ(m_T_ITD_des + m_T_amb_des + 273.15, 1.0, &wp);
				m_Psat_ref = wp.pres*1000.0;		// [Pa]
			}
			else
				m_Psat_ref = CSP::P_sat4( m_T_ITD_des + m_T_amb_des );

			break;
		}
		
		m_eta_adj = m_eta_ref/(CycleMap_DSG( 12, 2, m_Psat_ref )/CycleMap_DSG( 22, 2, m_Psat_ref ));
		m_q_dot_ref = m_P_ref/m_eta_adj;		//[kW] The reference heat flow
			
		if( m_tech_type == 5 )
		{
			/*!TN 2011.10.12 For Direct steam receiver ONLY
			!This section is used to recalculate the reference cold feedwater inlet temperature given other
			!constraints and design point values provided by the user. This results in an adjusted T_cold_ref
			!value, and it could be different than the value provided by the user... Will remove T_cold_ref from GUI */
				
			water_TP(m_T_hot_ref + 273.15, m_P_boil_des*100.0, &wp);
			double h_hot_ref = wp.enth;	//[kJ/kg] HP turbine inlet conditions
			double s_t = wp.entr;			//[kJ/kg-K]

			water_PQ( m_P_boil_des*100.0, 1.0, &wp );
			double h_sh_in = wp.enth;			//[kJ/kg]

			double h_t_out, h_rh_out, h_LP_out;
			if( m_is_rh )
			{
				water_PS( m_P_rh_ref*100.0, s_t, &wp );
				double h_t_outs = wp.enth;	//[kJ/kg] Isentropic HP outlet enthlapy
				h_t_out = h_hot_ref - (h_hot_ref - h_t_outs)*0.88;		//[kJ/kg] HP outlet enthalpy
				water_PH( m_P_rh_ref*100.0, h_t_out, &wp );
				//double T_rh_in = wp.temp - 273.15;	//[C] Reheat inlet temperature
				water_TP(m_T_rh_hot_ref + 273.15, m_P_rh_ref*100.0, &wp);
				h_rh_out = wp.enth;	//[kJ/kg] LP turbine inlet conditions
				double s_rh_out = wp.entr;	//[kJ/kg-K]
				water_PS( m_Psat_ref/1000.0, s_rh_out, &wp );
				double h_LP_out_isen = wp.enth;	//[kJ/kg] LP outlet enthalpy
				h_LP_out = h_rh_out - (h_rh_out - h_LP_out_isen)*0.88;		//[kJ/kg] Turbine outlet enthalpy										
			}
			else
			{
				m_rh_frac_ref = 0.0;
				water_PS( m_Psat_ref*1000.0, s_t, &wp );
				double h_t_outs = wp.enth;		//[kJ/kg]
				h_t_out = h_hot_ref - (h_hot_ref - h_t_outs)*0.88;	//[kJ/kg] Turbine outlet enthlapy
				h_rh_out = 0.0;
				h_LP_out = 0.0;
			}
			m_m_dot_ref = m_P_ref/( (h_hot_ref - h_t_out) + m_rh_frac_ref*(h_rh_out - h_LP_out) );		//[kg/s] Reference mass flow rate

			m_q_dot_rh_ref = m_m_dot_ref*m_rh_frac_ref*(h_rh_out - h_t_out);		//[kW] Reference heat input to reheater
			double q_dot_sh_ref = m_m_dot_ref*(h_hot_ref - h_sh_in);				//[kW] Reference heat input to superheater

			double q_b_des = m_q_dot_ref - m_q_dot_rh_ref - q_dot_sh_ref;			//[kW] Reference heat input to boiler

			double h_cold_ref = h_sh_in - q_b_des/m_m_dot_ref;						//[kJ/kg] Design feedwater outlet temperature
			water_PH( m_P_boil_des*100.0, h_cold_ref, &wp );
			m_T_cold_ref = wp.temp - 273.15;													//[C] Design feedwater outlet temperature

			m_q_dot_st_ref = m_m_dot_ref*(h_hot_ref - h_cold_ref);					//[kW] Reference heat input between feedwater and HP turbine
		}
		else
		{
			water_TP(m_T_hot_ref + 273.15, m_P_boil_des*100.0, &wp);
			double h_hot_ref = wp.enth;	//[kJ/kg] HP turbine inlet enthalpy and entropy
			double s_t = wp.entr;			//[kJ/kg-K]
			water_TP(m_T_cold_ref + 273.15, m_P_boil_des*100.0, &wp);
			double h_cold_ref = wp.enth;	//[kJ/kg]

			double h_rh_out, h_t_out;
			if( m_is_rh )
			{
				//Calculate the reheater inlet temperature assuming an isentropic efficiency model
				water_PS( m_P_rh_ref*100.0, s_t, &wp );
				double h_t_outs = wp.enth;
				double h_t_in = h_hot_ref;
				h_t_out = h_t_in - (h_t_in - h_t_outs)*0.88;		//[kJ/kg]
				water_PH( m_P_rh_ref*100, h_t_out, &wp );		
				//double T_rh_in = wp.temp - 273.15;		//[C]
				water_TP(m_T_rh_hot_ref + 273.15, m_P_rh_ref*100.0, &wp);
				h_rh_out = wp.enth;
			}
			else
			{
				m_rh_frac_ref = 0.0;
				h_rh_out = 0.0;
				h_t_out = 0.0;
			}
			// Design-point mass flow rate
			m_m_dot_ref = m_q_dot_ref/( (h_rh_out - h_t_out)*m_rh_frac_ref + (h_hot_ref - h_cold_ref) );

			// Design-point reheater thermal input
			if( m_is_rh )
				m_q_dot_rh_ref = m_m_dot_ref*(h_rh_out - h_t_out)*m_rh_frac_ref;
			else
				m_q_dot_rh_ref = 0.0;

			m_q_dot_st_ref = m_m_dot_ref*(h_hot_ref - h_cold_ref);
		}

		m_Psat_ref = m_Psat_ref*1.E-5;		// Convert Pa to bar

		return true;
	}

	double CycleMap_DSG( int YT, int XT, double X )
	{
		//Set_PB_coefficients();

		int XI, YI;
		if( m_tech_type != 5 )
		{
			switch( XT )
			{
			case 1: XI = 1; break;		//A
			case 2: XI = 4; break;		//B
			case 3: XI = 7; break;		//C

			case 13: XI = 10; break;	//AC
			case 12: XI = 13; break;	//AB
			case 23: XI = 16; break;	//BC
			}

			switch( YT )
			{
			case 11: YI = 2; break;		//PA
			case 12: YI = 5; break;		//PB
			case 13: YI = 8; break;		//PC
			case 112: YI = 14; break;	//PAB
			case 113: YI = 11; break;	//PAC 
			case 123: YI = 17; break;   //PBC
			case 21: YI = 3; break;		//QA
			case 22: YI = 6; break;		//QB
			case 23: YI = 9; break;		//QC
			case 212: YI = 15; break;	//QAB
			case 213: YI = 12; break;	//QAC
			case 223: YI = 18; break;	//QBC
			}
		}
		else
		{
			switch( XT )
			{
			case 1: XI = 1; break;		//A
			case 2: XI = 5; break;		//B
			case 3: XI = 9; break;		//C

			case 13: XI = 13; break;	//AC
			case 12: XI = 17; break;	//AB
			case 23: XI = 21; break;	//BC
			}

			switch( YT )
			{
			case 11: YI = 2; break;		//PA
			case 12: YI = 6; break;		//PB
			case 13: YI = 10; break;	//PC
			case 112: YI = 18; break;	//PAB
			case 113: YI = 14; break;	//PAC 
			case 123: YI = 22; break;   //PBC
			case 21: YI = 3; break;		//QA
			case 22: YI = 7; break;		//QB
			case 23: YI = 11; break;	//QC
			case 212: YI = 19; break;	//QAB
			case 213: YI = 15; break;	//QAC
			case 223: YI = 23; break;	//QBC
			case 31: YI = 4; break;		//RA
			case 32: YI = 8; break;		//RB
			case 33: YI = 12; break;	//RC
			case 312: YI = 20; break;	//RAB
			case 313: YI = 16; break;	//RAC
			case 323: YI = 24; break;	//RBC
			}
		}

		if ( (XI==0) || (YI==0) ) return 0.0;

		XI--; YI--; // C++ arrays start index at 0 instead of 1, like Fortran arrays

		int i_last_index = (int)m_db.ncols() - 1;
		int lbi, ubi;
		for( int i = 0; i < (int)m_db.ncols(); i++ )
		{
			// if we got to the last one, then set bounds and end loop
			if(i == i_last_index)
			{
				lbi = i_last_index;
				ubi = i_last_index;
				break;
			}

			// if the x variable is outside the table range, set the bounds and get out
			if(i == 0) 
			{
				if(m_db.at(XI,1) > m_db.at(XI,0))	// The table is in ascending order
				{ 
					if(X <= m_db.at(XI,0))
					{
						lbi=0; ubi=0; break;
					}
					if(X >= m_db.at(XI,i_last_index))
					{
						lbi=i_last_index; ubi=i_last_index; break;
					}
				}
				else								// The table is in descending order
				{
					if(X >= m_db.at(XI,0))
					{
						lbi=0; ubi=0; break;
					}
					if(X <= m_db.at(XI,i_last_index))
					{
						lbi=i_last_index; ubi=i_last_index; break;
					}
				}
			}

			// if i = iLastIndex, the code above will catch it and break out of the loop before getting here.
			// so the reference [i+1], where i = iLastIndex, will never happen
			if( ( (X >= m_db.at(XI,i)) && (X < m_db.at(XI,i+1)) ) || ( (X <= m_db.at(XI,i)) && (X > m_db.at(XI,i+1)) ) )
			{
				lbi = i;
				ubi = i+1;
				break;
			}
		}

		double ind;
		if(m_db.at(XI,ubi) == m_db.at(XI,lbi))
			ind = 0.0;
		else
			ind = (X-m_db.at(XI,lbi)) / (m_db.at(XI,ubi) - m_db.at(XI,lbi));
		
		return m_db.at(YI,lbi) + ind * (m_db.at(YI,ubi)- m_db.at(YI,lbi));

	}

	bool DSGRankineCycle( double T_db, double T_wb, double P_amb, double T_hot, double m_dot_st, int mode, double demand_var, double F_wc_tou, double dp_rh, double & P_cycle, double & eta, double & T_cold,
							double & m_dot_demand, double & m_dot_makeup, double & W_cool_par, double & f_hrsys, double & P_cond, double & P_turb_in,
							double & m_dot_rh, double & P_rh_in, double & T_rh_in, double & T_rh_out )
	{
		m_dot_st = m_dot_st/3600.0;		//[kg/s] Mass flow rate, convert from [kg/hr]

		// Calculate the htf mass flow rate in non-dimensional form
		double m_dot_ND = m_dot_st/m_m_dot_ref;

		// Calculate the reheater and boiler pressures based on the mass flow fraction
		if( m_is_rh )
			P_rh_in = pow( ( pow(m_Psat_ref,2) + pow(max(0.5,m_dot_ND),2)*(pow(m_P_rh_ref,2)-pow(m_Psat_ref,2)) ), 0.5 );	//Patnode thesis, p. 69
		else
		{
			P_rh_in = m_Psat_ref;
			m_P_rh_ref = m_Psat_ref;
		}
        /* MW 2015.4.27
        For fixed pressure applications, I think we can comment out the sliding pressure calculation and just set to the design
        point pressure. Do this here..
        */
		P_turb_in = pow( ( pow(P_rh_in,2) + pow(max(0.5,m_dot_ND),2)*(pow(m_P_boil_des,2)-pow(m_P_rh_ref,2)) ), 0.5 );
        //P_turb_in = m_P_boil_des;     // For fixed pressure applications...

		double h_t_out = 0.0;
		// Determine HP turbine outlet conditions
		if( m_is_rh )
		{
			// Calculate the reheater inlet temperature assuming an isentropic efficiency model
			water_TP(T_hot + 273.15, check_pressure.P_check(P_turb_in)*100.0, &wp);	//Turbine inlet conditions
			double h_t_in = wp.enth;
			double s_t = wp.entr;
			water_PS( check_pressure.P_check( P_rh_in )*100.0, s_t, &wp );		//Reheat extraction enthalpy assuming isentropic expansion
			double h_t_outs = wp.enth;
			double eta_t = 0.88*CSP::eta_pl(m_dot_ND);
			h_t_out = h_t_in - (h_t_in - h_t_outs)*eta_t;	// The actual reheat inlet enthalpy
			water_PH( check_pressure.P_check( P_rh_in )*100.0, h_t_out, &wp );	// Reheat inlet temp
			T_rh_in = wp.temp - 273.15;
		}

		// The saturation temperature at the boiler. Using the floating pressure value is consistent with the regression model formulation in this case.
		water_PQ( check_pressure.P_check( P_turb_in )*100.0, 0.5, &wp );
		double T_ref = wp.temp - 273.15;

		// Calculate the hot inlet steam temperature, in non-dimensional form
		double T_hot_ND = (T_hot - T_ref)/(m_T_hot_ref - T_ref);

		double h_rh_out = 0.0;
		if( m_is_rh )
		{
			// Calculate the reheat outlet temperature, assuming reheat ND is equal to hot ND
			water_PQ( check_pressure.P_check( P_rh_in )*100.0, 0.0, &wp );
			double T_s_rh = wp.temp - 273.15;
			T_rh_out = T_s_rh + (m_T_rh_hot_ref - T_s_rh)*T_hot_ND;
			// Using temperature and pressure, calculate reheat outlet enthalpy
			water_TP(T_rh_out + 273.15, check_pressure.P_check(P_rh_in - dp_rh)*100.0, &wp);
			h_rh_out = wp.enth;
		}

		// Do an initial cooling tower call to estimate the turbine back pressure. 
		double q_reject_est = m_q_dot_ref*1000.0*(1.0-m_eta_adj)*m_dot_ND*T_hot_ND;

		double T_cond, m_dot_air, W_cool_parhac, W_cool_parhwc;
		switch( m_CT )
		{
		case 1:
			CSP::evap_tower( m_tech_type, m_P_cond_min, m_n_pl_inc, m_dT_cw_ref, m_T_approach, m_P_ref*1000.0, m_eta_adj, T_db, T_wb, P_amb, q_reject_est, m_dot_makeup, W_cool_par, P_cond, T_cond, f_hrsys );
			break;
		case 2:
			CSP::ACC( m_tech_type, m_P_cond_min, m_n_pl_inc, m_T_ITD_des, m_P_cond_ratio, m_P_ref*1000.0, m_eta_adj, T_db, P_amb, q_reject_est, m_dot_air, W_cool_par, P_cond, T_cond, f_hrsys );
			m_dot_makeup = 0.0;
			break;
		case 3:
			CSP::HybridHR( m_tech_type, m_P_cond_min, m_n_pl_inc, F_wc_tou, m_F_wcmax, m_F_wcmin, m_T_ITD_des, m_T_approach, m_dT_cw_ref, m_P_cond_ratio, m_P_ref*1000.0, m_eta_adj, T_db, T_wb, P_amb, q_reject_est, m_dot_makeup,
							W_cool_parhac, W_cool_parhwc, W_cool_par, P_cond, T_cond, f_hrsys );
			break;
		}

		// Set initial values
		double ADJ = 1.0;
		double err = 1.0;
		int qq = 0;

		// Do a quick check to see if there is actually a mass flow being supplied to the cycle
		// If not, go to the end
		if( fabs(m_dot_ND) < 1.E-3 )
		{
			P_cycle = 0.0;
			eta = 0.0;
			T_cold = m_T_hot_ref;
			m_dot_demand = m_m_dot_ref;
			W_cool_par = 0.0;
			m_dot_makeup = 0.0;
			// Set the error to zero, since we don't want to iterate
			err = 0.0;
		}

		double P_dem_ND;
		double P_ND[3];
		double Q_ND[3];
		double R_ND[3] = {0,0,0};
		double R_ND_tot;

		double P_cond_guess = 0.0;
		double P_cond_low = -1.0;
		double P_cond_high = -1.0;

		// Begin iterations
		while( err > 1.E-6 && qq < 100 )
		{
			qq++;
			// Now use the constrained variable to calculate the demand mass flow rate
			if( mode == 1 )
			{
				P_dem_ND = demand_var/m_P_ref;
				if( qq == 1 )
					m_dot_ND = P_dem_ND;		// An initial guess (function of power)
			}

			// ************ Correlations ***************************
			// Calculate the correlations 
			// *****************************************************
			// Power
			// Main effects
			P_ND[0] = CycleMap_DSG( 11, 1, T_hot_ND ) - 1.0;
			P_ND[1] = CycleMap_DSG( 12, 2, P_cond ) - 1.0;
			P_ND[2] = CycleMap_DSG( 13, 3, m_dot_ND ) - 1.0;

			// Interactions
			double P_CA = CycleMap_DSG( 113, 13, T_hot_ND );
			double P_AB = CycleMap_DSG( 112, 12, P_cond );
			double P_BC = CycleMap_DSG( 123, 23, m_dot_ND );

			P_ND[0] = P_ND[0]*P_AB;
			P_ND[1] = P_ND[1]*P_BC;
			P_ND[2] = P_ND[2]*P_CA;

			// Heat
			// Main effects
			Q_ND[0] = CycleMap_DSG( 21, 1, T_hot_ND ) - 1.0;
			Q_ND[1] = CycleMap_DSG( 22, 2, P_cond ) - 1.0;
			Q_ND[2] = CycleMap_DSG( 23, 3, m_dot_ND ) - 1.0;

			// Interactions
			double Q_CA = CycleMap_DSG( 213, 13, T_hot_ND );
			double Q_AB = CycleMap_DSG( 212, 12, P_cond );
			double Q_BC = CycleMap_DSG( 223, 23, m_dot_ND );

			Q_ND[0] = Q_ND[0]*Q_AB;
			Q_ND[1] = Q_ND[1]*Q_BC;
			Q_ND[2] = Q_ND[2]*Q_CA;

			if( m_is_rh )
			{
				// Reheat
				// Main effects
				R_ND[0] = CycleMap_DSG( 31, 1, T_hot_ND ) - 1.0;
				R_ND[1] = CycleMap_DSG( 32, 2, P_cond ) - 1.0;
				R_ND[2] = CycleMap_DSG( 33, 3, m_dot_ND ) - 1.0;

				// Interactions
				double R_CA = CycleMap_DSG( 313, 13, T_hot_ND );
				double R_AB = CycleMap_DSG( 312, 12, P_cond );
				double R_BC = CycleMap_DSG( 323, 23, m_dot_ND );

				R_ND[0] = R_ND[0]*R_AB;
				R_ND[1] = R_ND[1]*R_BC;
				R_ND[2] = R_ND[2]*R_CA;
			}
			// Default should be = 0 if no reheat

			// Calculate the cumulative values
			double P_ND_tot = 1.0;
			double Q_ND_tot = 1.0;
			R_ND_tot = 1.0;

			// Increment main effects. MJW 8.11.2010 :: For this system, the effects are multiplicative
			for( int i = 0; i < 3; i++ )
			{
				P_ND_tot = P_ND_tot * (1.0 + P_ND[i]);
				Q_ND_tot = Q_ND_tot * (1.0 + Q_ND[i]);
				R_ND_tot = R_ND_tot * (1.0 + R_ND[i]);
			}

			// Calculate the output values:
			P_cycle = P_ND_tot * m_P_ref;
			water_TP(T_hot + 273.15, check_pressure.P_check(P_turb_in)*100.0, &wp);
			double h_hot = wp.enth;
			double h_cold = h_hot - Q_ND_tot*m_q_dot_st_ref/m_dot_st;
			do
			{
				water_PH( check_pressure.P_check( P_turb_in )*100.0, h_cold, &wp );
				T_cold = wp.temp - 273.15;
				water_TP(T_cold + 273.15, P_turb_in*100.0, &wp);
				if( fabs(wp.enth - h_cold)/h_cold < 0.01 )
				{					
					break;
				}
				h_cold*=0.999;
			} while( true );

			eta = P_cycle/(Q_ND_tot*m_q_dot_st_ref + R_ND_tot*m_q_dot_rh_ref);
			m_dot_demand = max( m_dot_ND*m_m_dot_ref, 0.00001 );	//[kg/s]

			// Call the cooling tower model to update the condenser pressure
			double q_reject = (1.0 - eta)*(m_q_dot_st_ref*Q_ND_tot + m_q_dot_rh_ref*R_ND_tot)*1000.0;
		
			if( qq < 10 )
			{
				switch( m_CT )
				{
				case 1:
					CSP::evap_tower( m_tech_type, m_P_cond_min, m_n_pl_inc, m_dT_cw_ref, m_T_approach, m_P_ref*1000.0, m_eta_adj, T_db, T_wb, P_amb, q_reject, m_dot_makeup, W_cool_par, P_cond_guess, T_cond, f_hrsys );
					break;
				case 2:
					CSP::ACC( m_tech_type, m_P_cond_min, m_n_pl_inc, m_T_ITD_des, m_P_cond_ratio, m_P_ref*1000.0, m_eta_adj, T_db, P_amb, q_reject, m_dot_air, W_cool_par, P_cond_guess, T_cond, f_hrsys );
					break;
				case 3:
					CSP::HybridHR( m_tech_type, m_P_cond_min, m_n_pl_inc, F_wc_tou, m_F_wcmax, m_F_wcmin, m_T_ITD_des, m_T_approach, m_dT_cw_ref, m_P_cond_ratio, m_P_ref*1000.0, m_eta_adj, T_db, T_wb,
										P_amb, q_reject, m_dot_makeup, W_cool_parhac, W_cool_parhwc, W_cool_par, P_cond_guess, T_cond, f_hrsys );
					break;
				}
			}
			// Check to see if the calculated and demand values match
			// If they don't match, calculate the "ADJ" factor
			if( mode == 1 )
			{
				ADJ = ( demand_var - P_cycle ) / demand_var;		//MJW 10.31.2010 Adjustment factor
				err = fabs(ADJ);										//MJW 10.31.2010 Take absolute value of the error...
				m_dot_ND = m_dot_ND + ADJ*0.75;						//MJW 10.31.2010 Iterate the mass flow rate. Take a step smaller than the calculated adjustment
			}
			else
				err = 0.0;


			err = (P_cond_guess - P_cond)/P_cond;

			if( err > 0 )
				P_cond_low = P_cond;
			else
				P_cond_high = P_cond;

			if( P_cond_low > 0.0 && P_cond_high > 0.0 )
			{
				P_cond_guess = 0.5*P_cond_low + 0.5*P_cond_high;
				if( (P_cond_high - P_cond_low)/P_cond_high < 1.E-6 )
					err = 0.0;
			}

			P_cond = P_cond_guess;

			err = fabs(err);


			if( qq == 99 )
			{
				// call messages: "Power cycle model did not converge after 100 iterations"
				P_cycle = 0.0;
				eta = 0.0;
				T_cold = m_T_hot_ref;
				m_dot_demand = m_m_dot_ref;
			}
			// If this is not true, the cycle has not yet converged, and we should iterating
		}

		// Calculate the reheat mass flow rate, convert temperatures
		if( m_is_rh ) 
			m_dot_rh = m_q_dot_rh_ref*R_ND_tot/(h_rh_out - h_t_out)*3600.0;
		else
		{
			m_dot_rh = 0.0;
			T_rh_in = 0.0;
			T_rh_out = 0.0;
		}

		// Finally, convert the values back to their original units
		m_dot_demand = m_dot_demand*3600.0;  //[kg/s]->[kg/hr]
		m_dot_st = m_dot_st*3600.0;          //![kg/s]->[kg/hr]

		return true;
	}


	virtual int call(double /*time*/, double step, int ncall){
		
		int mode = (int)value( I_MODE );					//[-] Cycle part load control... from plant controller
		double T_hot = value( I_T_HOT );			//[C] Hot inlet temperature
		double m_dot_st = value( I_M_DOT_ST );		//[kg/s] Mass flow rate to HP turbine
		double T_wb = value( I_T_WB )+273.15;		//[K] Wet bulb temperature, convert from C
		double demand_var = value( I_DEMAND_VAR );	//[?] Control signal indicating operational mode - only used when mode == 1
		m_standby_control = (int)value( I_STANDBY_CONTROL);	//[-] Control signal indicating standby mode
		double T_db = value( I_T_DB )+273.15;		//[K] Ambient dry bulb temperature, convert from C
		//double P_amb = value( I_P_AMB )*101325.0;	//[Pa] Ambient pressure, convert from bar
		double P_amb = value( I_P_AMB )*100.0;		//[Pa] Ambient pressure, convert from mbar
		//int tou = value( I_TOU );					//[-] Current Time-Of-Use period
		int tou = (int)value(I_TOU) - 1;			// control value between 1 & 9, have to change to 0-8 for array index
		//double rh = value( I_RH )/100.0;			//[-] Relative humidity of the ambient air, convert from %

		double F_wc_tou = m_F_wc[tou];				//[-] Hybrid fraction at current Time-Of-Use period

		//*********************************************
		// Values for TRNSYS timestep comparison
		//*********************************************
		/*m_standby_control_prev = 1;
		m_time_su_prev = 0.0;
		m_E_su_prev = 0.0;*/

		double f_rec_su = 1.0;
		if(m_tech_type == 5)
			f_rec_su = value( I_F_RECSU );

		double dp_b = value( I_DP_B )/1.E5;					//[bar] Pressure drop in boiler
		double dp_sh = value( I_DP_SH )/1.E5 + dp_b;		//[bar] Pressure drop in superheater
		double dp_rh = value( I_DP_RH )/1.E5;				//[bar] Pressure drop in reheater

		if( mode == 1)
			demand_var = demand_var*1000.0;			//[?] If the mode is to operate in power demand, convert from MW to KW

		double m_dot_rh;
		if( ncall > 10 )
		{
			m_dot_rh = m_dot_st*m_rh_frac_ref;
			value( O_F_RH, m_dot_rh/max(1.0, m_dot_st) );	//[-] Reheat mass flow rate fraction
			return 0;
		}

		// Set outputs to 0, let model solve for correct values
		double m_dot_st_bd, P_turb_in, P_rh_in, T_rh_in, T_rh_out;
		m_dot_rh = m_dot_st_bd = P_turb_in = P_rh_in = T_rh_in = T_rh_out = 0.0;

		// Declare variables that will be solved by 'switch' options
		double P_cycle, eta, T_cold, m_dot_demand, m_dot_makeup, W_cool_par, f_hrsys, P_cond;

		switch (m_standby_control)
		{
		case 1:		// The cycle is in normal operation
			DSGRankineCycle( T_db, T_wb, P_amb, T_hot, m_dot_st, mode, demand_var, F_wc_tou, dp_rh, P_cycle, eta, T_cold, m_dot_demand, m_dot_makeup, W_cool_par, f_hrsys, P_cond, P_turb_in, m_dot_rh, P_rh_in, T_rh_in, T_rh_out );

			if( eta > 1.0 || eta < 0.0 || T_cold > T_hot || T_cold < m_T_cold_ref - 100.0 )
			{
				P_cycle = 0.0;
				eta = 0.0;
				T_cold = m_T_cold_ref;
				m_dot_demand = 0.0;
				m_dot_makeup = 0.0;
				W_cool_par = 0.0;
				f_hrsys = 0.0;
				P_cond = 0.0;
			}

			P_cycle = f_rec_su * P_cycle;

			// Calculate the blowdown fraction
			m_dot_st_bd = m_dot_st/3600.0*m_pb_bd_frac;

			break;

		case 2:		// The cycle is in standby operation

			{double q_tot = m_P_ref/m_eta_ref;

			// Calculate the actual q_sby_needed from the reference flow
			double q_sby_needed = q_tot * m_q_sby_frac;

			// Now calculate the mass flow rate knowing the inlet temp of the steam and holding the outlet temperature at the reference outlet temp
			water_TP(T_hot + 273.15, m_P_boil_des*100.0, &wp);
			double h_st_hot = wp.enth;		//[kJ/kg]
			water_TP(m_T_cold_ref + 273.15, m_P_boil_des*100.0, &wp);
			double h_st_cold = wp.enth;			//[kJ/kg]
			double m_dot_sby = q_sby_needed/(h_st_hot - h_st_cold);

			// Set other output values
			P_cycle = 0.0;
			eta = 0.0;
			T_cold = m_T_cold_ref;
			m_dot_demand = m_dot_sby;
			m_dot_makeup = 0.0;
			W_cool_par = 0.0;
			f_hrsys = 0.0;
			P_cond = 0.0;}

			break;
			
		case 3:  // The cycle has been completely shut down
			
			P_cycle = 0.0;
			eta = 0.0;
			T_cold = m_T_cold_ref;
			m_dot_demand = 0.0;
			m_dot_makeup = 0.0;
			W_cool_par = 0.0;
			f_hrsys = 0.0;
			P_cond = 0.0;

			break;
		}

		// If the cycle is going from completely shut down to starting up, set the remaining startup
		// time to be equal to the designated startup time
		if( m_standby_control_prev == 3 && m_standby_control == 1 )
		{
			m_time_su_prev = m_startup_time;
			m_E_su_prev = m_startup_energy;
			m_startup_time = m_time_su_prev;
			m_E_su = m_E_su_prev;
		}

		/*If the cycle is starting up beginning in this time period, or it is continuing to start
		up from the last time period, then subtract the appropriate fraction of electric power
		from the output.  Note that during the startup time, not only is the cycle not producing power,
		but it is also consuming thermal energy*/

		double startup_e_used;
		if( P_cycle > 0.0 )
		{
			if( (m_standby_control_prev == 3 && m_standby_control == 1) || (m_time_su_prev + m_E_su_prev) > 0.0 )
			{
				/*Adjust the power cycle output. Both the energy and time requirement must be met before power is produced,
				so subtract the maximum of these two values*/
				double Q_cycle = P_cycle/eta;
				startup_e_used = min(Q_cycle*step/3600.0, m_E_su_prev);

				double f_st = 1.0 - max( min(1.0, m_time_su_prev/(step/3600.0)), startup_e_used/(Q_cycle*step/3600.0) );
				P_cycle = P_cycle*f_st;

				/* Fraction of the timestep running at full capacity
				The power cycle still requires mass flow to satisfy the energy demand requirement, so only subtract demand mass flow
				for the case when startup time exceeds startup energy. */
				m_dot_demand = m_dot_demand*(1.0 - max( min( 1.0, m_time_su_prev/(step/3600.0)) - startup_e_used/(Q_cycle*step/3600.0), 0.0 ));

				// 9.21.11, twn: Average the reported outlet temperature based on the amount of time the cycle has been operating
				if( f_st > 0.0 )
					T_cold = f_st*T_cold + (1.0 - f_st)*m_T_cold_ref;

				m_time_su = max( m_time_su_prev - step/3600.0, 0.0 );
				m_E_su = max( m_E_su_prev - startup_e_used, 0.0 );
			}

		}
		else
			startup_e_used = 0.0;

		// SET THE OUTPUTS FROM THIS MODEL
		// 900 continue !MJW 12.10.2010

		// Cycle power output
		value( O_P_CYCLE, P_cycle/1000.0 );		//Convert from kW to MW

		// Cycle thermal efficiency
		value( O_ETA, eta ); 
		// Heat transfer fluid outlet temp
		value( O_T_COLD, T_cold );
		// Wet cooling makeup water flow rate [kg/hr]
		value( O_M_DOT_MAKEUP, (m_dot_makeup + m_dot_st_bd)*3600.0 );
		// Heat transfer fluid demand flow rate [kg/hr]
		value( O_M_DOT_DEMAND, m_dot_demand );
		// Heat transfer fluid flow rate [kg/hr]
		value( O_M_DOT_OUT, m_dot_st );
		// Calculated reference flow rate [kg/hr]		
		value( O_M_DOT_REF, m_m_dot_ref*3600.0 );
		// Cooling tower parasitic load [MW]
		value( O_W_COOL_PAR, W_cool_par );
		// Reference power level output
		value( O_P_REF_OUT, m_P_ref/1000.0 );	//Convert from kW to MW
		// Fraction of cooling system in operation
		value( O_F_BAYS, f_hrsys );
		// Condenser pressure (Pa)
		value( O_P_COND, P_cond );

		// add new outputs for reheater
		value( O_P_BOILER_IN, P_turb_in + dp_sh );		//[bar] Turbine inlet pressure

		// 8/2/11 TN: Reheat mass flow fraction is more useful than mass flow rate
		value( O_F_RH, m_dot_rh/max(1.0, m_dot_st) );	//[-] Reheat mass flow rate fraction

		value( O_P_RH_IN, P_rh_in );    // [bar] reheater inlet pressure
		value( O_T_RH_IN, T_rh_in );	// [C] reheater inlet temperature
		value( O_T_RH_OUT, T_rh_out );  // [C] reheater outlet temperature */

		return 0;
	}

	virtual int converged(double /*time*/)
	{

		m_standby_control_prev = m_standby_control;
		m_time_su_prev = m_time_su;
		m_E_su_prev = m_E_su;

		return 0;
	}
	
	
};


TCS_IMPLEMENT_TYPE( sam_mw_type234, "Direct power cycle model", "Ty Neises", 1, sam_mw_type234_variables, NULL, 1 );
