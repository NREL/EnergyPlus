#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "htf_props.h"
#include "waterprop.h"
#include "sam_csp_util.h"

using namespace std;

enum{
	// Parameters
	P_P_REF,       
	P_ETA_REF,     
	P_T_HOT_REF,   
	P_T_COLD_REF,  
	P_DT_HX_REF,
	P_DT_CW_REF,   
	P_QGEO_FRAC_REF,
	P_T_AMB_DES,   
	P_Q_SBY_FRAC,
	P_CT,          
	P_STARTUP_TIME,
	P_STARTUP_FRAC,
	P_T_APPROACH,  
	P_T_ITD_DES,   
	P_P_COND_RATIO,
	P_PB_BD_FRAC,  
	P_P_COND_MIN,  
	P_N_PL_INC,    
	P_F_WC,      
	P_FLUID,
	P_FLUID_PROPS,

	// Inputs
	I_MODE,           
	I_T_HOT,          
	I_M_DOT_HTF,       
	I_T_WB,           
	I_DEMAND_VAR,     
	I_STANDBY_CONTROL,
	I_T_DB,           
	I_P_AMB,          
	I_TOU,            
	I_RH,             
	I_F_RECSU,
	
	// Outputs
	O_P_CYCLE,
	O_Q_SOLAR,
	O_Q_GEO,
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
	
	//Include N_max
	N_MAX
};

tcsvarinfo sam_geothermal_hybrid_pb_vars[] = {
	{ TCS_PARAM,          TCS_NUMBER,      P_P_REF,                  "P_ref",           "Reference output electric power at design condition",               "MW",    "",    "",   "111" },
	{ TCS_PARAM,          TCS_NUMBER,      P_ETA_REF,                "eta_ref",         "Reference conversion efficiency at design condition",               "none",  "",    "",   "0.3774" },
	{ TCS_PARAM,          TCS_NUMBER,      P_T_HOT_REF,              "T_hot_ref",       "Reference HTF inlet temperature at design",                         "C",     "",    "",   "391" },
	{ TCS_PARAM,          TCS_NUMBER,      P_T_COLD_REF,             "T_cold_ref",      "Reference HTF outlet temperature at design",                        "C",     "",    "",   "293" },
	{ TCS_PARAM,          TCS_NUMBER,      P_DT_HX_REF,              "dT_hx_ref",       "Reference superheater hot side temperature diff",                   "C",     "",    "",   "20" },
	{ TCS_PARAM,          TCS_NUMBER,      P_DT_CW_REF,              "dT_cw_ref",       "Reference condenser cooling water inlet/outlet T diff",             "C",     "",    "",   "10" },
	{ TCS_PARAM,          TCS_NUMBER,      P_QGEO_FRAC_REF,          "qgeo_frac_ref",   "Reference geothermal power input fraction",                         "none",  "",    "",   ".1" }, 
	{ TCS_PARAM,          TCS_NUMBER,      P_T_AMB_DES,              "T_amb_des",       "Reference ambient temperature at design point",                     "C",     "",    "",   "20" },																																						     
	{ TCS_PARAM,          TCS_NUMBER,      P_Q_SBY_FRAC,             "q_sby_frac",      "Fraction of thermal power required for standby mode",               "none",  "",    "",   "0.2" },
	{ TCS_PARAM,          TCS_NUMBER,      P_CT,                     "CT",              "Flag for using dry cooling or wet cooling system",                  "none",  "",    "",   "1" },
	{ TCS_PARAM,          TCS_NUMBER,      P_STARTUP_TIME,           "startup_time",    "Time needed for power block startup",                               "hr",    "",    "",   "0.5" },
	{ TCS_PARAM,          TCS_NUMBER,      P_STARTUP_FRAC,           "startup_frac",    "Fraction of design thermal power needed for startup",               "none",  "",    "",   "0.2" },
	{ TCS_PARAM,          TCS_NUMBER,      P_T_APPROACH,             "T_approach",      "Cooling tower approach temperature",                                "C",     "",    "",   "5" },
	{ TCS_PARAM,          TCS_NUMBER,      P_T_ITD_DES,              "T_ITD_des",       "ITD at design for dry system",                                      "C",     "",    "",   "16" },
	{ TCS_PARAM,          TCS_NUMBER,      P_P_COND_RATIO,           "P_cond_ratio",    "Condenser pressure ratio",                                          "none",  "",    "",   "1.0028" },
	{ TCS_PARAM,          TCS_NUMBER,      P_PB_BD_FRAC,             "pb_bd_frac",      "Power block blowdown steam fraction ",                              "none",  "",    "",   "0.02" },
	{ TCS_PARAM,          TCS_NUMBER,      P_P_COND_MIN,             "P_cond_min",      "Minimum condenser pressure",                                        "inHg",  "",    "",   "1.25" },
	{ TCS_PARAM,          TCS_NUMBER,      P_N_PL_INC,               "n_pl_inc",        "Number of part-load increments for the heat rejection system",      "none",  "",    "",   "2" },
	{ TCS_PARAM,          TCS_ARRAY,       P_F_WC,                   "F_wc",            "Fraction indicating wet cooling use for hybrid system",             "none",  "9 indices for each TOU Period",    "",   "0,0,0,0,0,0,0,0,0" },
	{ TCS_PARAM,          TCS_NUMBER,      P_FLUID,                  "HTF",             "Heat transfer fluid type",                                          "none",  "",    "",   "21" },
	{ TCS_PARAM,          TCS_MATRIX,      P_FLUID_PROPS,            "HTF_props",       "User defined field fluid property data",                            "none", "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",        "",        ""},

	{ TCS_INPUT,          TCS_NUMBER,      I_MODE,                   "mode",            "Cycle part load control, from plant controller",                    "none",  "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_T_HOT,                  "T_hot",           "Hot HTF inlet temperature, from storage tank",                      "C",     "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_M_DOT_HTF,               "m_dot_htf",        "HTF mass flow rate",                                                "kg/hr", "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_T_WB,                   "T_wb",            "Ambient wet bulb temperature",                                      "C",     "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_DEMAND_VAR,             "demand_var",      "Control signal indicating operational mode",                        "none",  "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_STANDBY_CONTROL,        "standby_control", "Control signal indicating standby mode",                            "none",  "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_T_DB,                   "T_db",            "Ambient dry bulb temperature",                                      "C",     "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_P_AMB,                  "P_amb",           "Ambient pressure",                                                  "atm",   "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_TOU,                    "TOU",             "Current Time-of-use period",                                        "none",  "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_RH,                     "relhum",          "Relative humidity of the ambient air",                              "none",  "",    "",   "" },
	{ TCS_INPUT,          TCS_NUMBER,      I_F_RECSU,                "f_recSU",         "Fraction powerblock can run due to receiver startup",               "none",  "",    "",   "" },
	
	{ TCS_OUTPUT,          TCS_NUMBER,     O_P_CYCLE,                "P_cycle",         "Cycle power output",                                                "MWe",   "",    "",   "" },
	{ TCS_OUTPUT,	       TCS_NUMBER,     O_Q_SOLAR,			     "q_solar",			"Thermal load from solar",											 "MWt",   "",    "",   "" },
	{ TCS_OUTPUT,		   TCS_NUMBER,     O_Q_GEO,				     "q_geo",	        "Thermal load from geothermal",										 "MWt",   "",    "",   "" },
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
	

	{ TCS_INVALID,    TCS_INVALID,    N_MAX,                0,                    0,                                                        0,                0,        0,        0 }
};

class sam_geothermal_hybrid_pb : public tcstypeinterface
{
private:
	HTFProperties m_htfProps;
	property_info wp;
	
	double m_P_ref;			
	double m_eta_ref;		
	double m_T_hot_ref;
	double m_T_cold_ref;
	double m_dT_hx_ref;
	double m_dT_cw_ref;		
	double m_T_amb_des;		
	double m_q_sby_frac;	
	double m_qgeo_frac_ref;
	int    m_CT;			
	double m_startup_time;	
	double m_startup_frac;	
	double m_T_approach;	
	double m_T_ITD_des;		
	double m_P_cond_ratio;	
	double m_pb_bd_frac;	
	double m_P_cond_min;	
	int    m_n_pl_inc;	
	double m_F_wc[9];
	int m_HTF;

	double m_F_wcmin;
	double m_F_wcmax;

	double m_startup_energy;
	double m_Psat_ref;
	double m_eta_adj;
	double m_q_dot_ref;
	double m_q_solar_ref;
	double m_q_geo_ref;
	double m_m_dot_ref;
	double m_q_dot_st_ref;
	double m_P_boil_des;

	util::block_t<double> m_Solar_Thermal_Norm;
	util::block_t<double> m_PB_Power_Nominal_Norm;
	util::block_t<double> m_GeoThermal_Norm;
    struct lookup_range {
		int nsteps;
		double varmax, varmin, delta;
	} m_mdot, m_Thot, m_pcond;

	int    m_standby_control_prev;
	int    m_standby_control;
	double m_time_su_prev;
	double m_time_su;
	double m_E_su_prev;
	double m_E_su;
	
public:

	sam_geothermal_hybrid_pb(tcscontext *cxt, tcstypeinfo *ti)
		: tcstypeinterface(cxt, ti)
	{
		m_P_ref = std::numeric_limits<double>::quiet_NaN();			
		m_eta_ref = std::numeric_limits<double>::quiet_NaN();		
		m_T_hot_ref = std::numeric_limits<double>::quiet_NaN();
		m_T_cold_ref = std::numeric_limits<double>::quiet_NaN();
		m_dT_hx_ref = std::numeric_limits<double>::quiet_NaN();
		m_dT_cw_ref = std::numeric_limits<double>::quiet_NaN();		
		m_T_amb_des = std::numeric_limits<double>::quiet_NaN();		
		m_q_sby_frac = std::numeric_limits<double>::quiet_NaN();	
		m_qgeo_frac_ref = std::numeric_limits<double>::quiet_NaN();	
		m_CT = -1;			
		m_startup_time = std::numeric_limits<double>::quiet_NaN();	
		m_startup_frac = std::numeric_limits<double>::quiet_NaN();	
		m_T_approach = std::numeric_limits<double>::quiet_NaN();	
		m_T_ITD_des = std::numeric_limits<double>::quiet_NaN();		
		m_P_cond_ratio = std::numeric_limits<double>::quiet_NaN();	
		m_pb_bd_frac = std::numeric_limits<double>::quiet_NaN();	
		m_P_cond_min = std::numeric_limits<double>::quiet_NaN();	
		m_n_pl_inc = -1;	
		m_F_wc[9] = std::numeric_limits<double>::quiet_NaN();
		m_HTF = -1;
		
		m_F_wcmin = std::numeric_limits<double>::quiet_NaN();
		m_F_wcmax = std::numeric_limits<double>::quiet_NaN();
		
		m_startup_energy = std::numeric_limits<double>::quiet_NaN();
		m_Psat_ref = std::numeric_limits<double>::quiet_NaN();
		m_eta_adj = std::numeric_limits<double>::quiet_NaN();
		m_q_dot_ref = std::numeric_limits<double>::quiet_NaN();
		m_q_solar_ref = std::numeric_limits<double>::quiet_NaN();
		m_q_geo_ref = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_ref = std::numeric_limits<double>::quiet_NaN();
				
		m_standby_control_prev = -1;
		m_standby_control = -1;
		m_time_su_prev = std::numeric_limits<double>::quiet_NaN();
		m_time_su = std::numeric_limits<double>::quiet_NaN();
		m_E_su_prev = std::numeric_limits<double>::quiet_NaN();
		m_E_su = std::numeric_limits<double>::quiet_NaN();
		m_P_boil_des = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_geothermal_hybrid_pb(){
	}

	virtual int init()
	{

		double tstep = time_step();

		m_P_ref			= value( P_P_REF )*1.E3;				//[kW] Reference output electric power at design condition
		m_eta_ref		= value( P_ETA_REF );					//[-] Reference conversion efficiency at design condition		
		m_T_hot_ref		= value( P_T_HOT_REF );					//[C] Reference inlet temperature at design
		m_T_cold_ref	= value( P_T_COLD_REF );				//[C] Reference outlet temperature at design
		m_dT_hx_ref     = value( P_DT_HX_REF );					//[C] Reference hot side superheater temp diff
		m_dT_cw_ref		= value( P_DT_CW_REF );					//[C] Reference condenser cooling water inlet/outlet Temp difference
		m_T_amb_des		= value( P_T_AMB_DES );					//[C] Reference ambient temperature at design point
		m_q_sby_frac	= value( P_Q_SBY_FRAC );				//[-] Fraction of thermal power required for standby mode
		m_qgeo_frac_ref	= value( P_QGEO_FRAC_REF );				//[-] Reference geothermal power input fraction
		m_CT			= (int) value( P_CT );					//[-] Flag for using dry cooling or wet cooling system
		m_startup_time	= value( P_STARTUP_TIME );				//[hr] Time needed for power block startup
		m_startup_frac	= value( P_STARTUP_FRAC );				//[-] Fraction of design thermal power needed for startup
		m_T_approach	= value( P_T_APPROACH );				//[C] Cooling tower approach temperature
		m_T_ITD_des		= value( P_T_ITD_DES );					//[C] ITD at design for dry system
		m_P_cond_ratio	= value( P_P_COND_RATIO );				//[-] Condenser pressure ratio
		m_pb_bd_frac	= value( P_PB_BD_FRAC );				//[-] Power block blowdown steam fraction
		m_P_cond_min	= value( P_P_COND_MIN )*3386.388667;	//[inHg] Minimum condenser pressure
		m_n_pl_inc		= (int) value( P_N_PL_INC );			//[-] Number of part-load increments for the heat rejection system	
		m_HTF           = (int) value( P_FLUID );				//[-] Heat transfer fluid number
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

		// Declare instance of fluid class for FIELD fluid.
		// Set fluid number and copy over fluid matrix if it makes sense.
		if( m_HTF != HTFProperties::User_defined )
		{
			m_htfProps.SetFluid( m_HTF ); // field_fl should match up with the constants
		}
		else
		{
			int nrows = 0, ncols = 0;
			double *fl_mat = value( P_FLUID_PROPS, &nrows, &ncols );
			if ( fl_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<double> mat( nrows, ncols, 0.0 );
				for (int r=0;r<nrows;r++)
					for (int c=0;c<ncols;c++)
						mat.at(r,c) = TCS_MATRIX_INDEX( var( P_FLUID_PROPS ), r, c );

				if ( !m_htfProps.SetUserDefinedFluid( mat ) )
				{
					//message( "user defined htf property table was invalid (rows=%d cols=%d)", nrows, ncols );
					message( m_htfProps.UserFluidErrMessage(), nrows, ncols );
					return -1;
				}
			}
		}

		//Set the power block design high pressure
		m_P_boil_des = 75.;	//bar -- from the IpsePro simulations

		// Calculate the startup energy needed
		m_startup_energy = m_startup_frac*m_P_ref/m_eta_ref;		//[kWt]

		// Initialize stored variables
		m_standby_control_prev = 3;
		m_time_su_prev = m_startup_time;
		m_E_su_prev = m_startup_energy;

		m_time_su = m_time_su_prev;
		m_E_su = m_E_su_prev;

		//Set the power cycle coefficients
		Set_PB_coefficients();

		// Initialize Power Cycle models
		Set_PB_ref_values();

		return 0;
	}

	void Set_PB_coefficients(){
		
		/* 
		Design mass flow rate: 106.1 kg/s
		Design T_in: 375 C
		Design P_condensing: 0.102 bar
		*/
		m_mdot.nsteps = 6;
		m_mdot.varmax = 1.;
		m_mdot.varmin = 59.8/119.6;
		m_mdot.delta = (m_mdot.varmax - m_mdot.varmin)/((double)m_mdot.nsteps - 1.);

		m_pcond.nsteps = 15;
		m_pcond.varmax = 20000.;
		m_pcond.varmin = 6000.;
		m_pcond.delta = (m_pcond.varmax - m_pcond.varmin)/((double)m_pcond.nsteps - 1.);

		m_Thot.nsteps = 3;
		m_Thot.varmax = 375.;
		m_Thot.varmin = 325.;
		m_Thot.delta = (m_Thot.varmax - m_Thot.varmin)/((double)m_Thot.nsteps - 1.);

		const double Solar_Thermal_Norm[3*15*6] =   // Solar Thermal Power (-):Normalized at   287.000 MWth
        {   // blocks for constant turbine inlet temperature
            // rows for constant condensing pressure
            // columns for constant turbine inlet mass
            //    
            //Turbine Inlet Steam Temperature (C)     Mininum     d_spacing       Max           n      
            //               325.00000        25.00    375.00000            3
            //    
            //Condensing Prssure (bar)   Mininum     d_spacing       Max           n      
            //                    0.06         0.01         0.20           15
            //    
            //Turbine Inlet Steam Mass (kg/s)       Mininum     d_spacing       Max           n      
            //                   59.80        11.96       119.60            6
            //    
            // turbine inlet temperature =   325.00 C
                0.5281,   0.6226,   0.7145,   0.8041,   0.8913,   0.9764, 
                0.5281,   0.6226,   0.7145,   0.8041,   0.8913,   0.9764, 
                0.5281,   0.6226,   0.7145,   0.8041,   0.8914,   0.9764, 
                0.5281,   0.6226,   0.7145,   0.8041,   0.8914,   0.9765, 
                0.5281,   0.6226,   0.7145,   0.8041,   0.8914,   0.9765, 
                0.5281,   0.6226,   0.7145,   0.8041,   0.8914,   0.9765, 
                0.5281,   0.6226,   0.7145,   0.8041,   0.8914,   0.9765, 
                0.5281,   0.6226,   0.7145,   0.8041,   0.8914,   0.9765, 
                0.5281,   0.6225,   0.7145,   0.8041,   0.8914,   0.9765, 
                0.5281,   0.6225,   0.7145,   0.8041,   0.8914,   0.9765, 
                0.5281,   0.6225,   0.7145,   0.8041,   0.8914,   0.9765, 
                0.5281,   0.6225,   0.7145,   0.8041,   0.8914,   0.9765, 
                0.5281,   0.6225,   0.7145,   0.8041,   0.8914,   0.9765, 
                0.5281,   0.6225,   0.7145,   0.8041,   0.8914,   0.9765, 
                0.5281,   0.6225,   0.7145,   0.8041,   0.8914,   0.9765, 
            // turbine inlet temperature =   350.00 C
                0.5328,   0.6284,   0.7217,   0.8127,   0.9017,   0.9888, 
                0.5328,   0.6284,   0.7217,   0.8127,   0.9018,   0.9888, 
                0.5328,   0.6284,   0.7217,   0.8128,   0.9018,   0.9889, 
                0.5328,   0.6284,   0.7217,   0.8128,   0.9018,   0.9889, 
                0.5328,   0.6284,   0.7217,   0.8128,   0.9018,   0.9889, 
                0.5328,   0.6284,   0.7217,   0.8128,   0.9018,   0.9889, 
                0.5328,   0.6284,   0.7217,   0.8128,   0.9018,   0.9889, 
                0.5328,   0.6284,   0.7217,   0.8128,   0.9018,   0.9889, 
                0.5328,   0.6284,   0.7217,   0.8128,   0.9018,   0.9889, 
                0.5328,   0.6284,   0.7217,   0.8128,   0.9018,   0.9889, 
                0.5328,   0.6284,   0.7217,   0.8128,   0.9018,   0.9889, 
                0.5328,   0.6284,   0.7217,   0.8128,   0.9018,   0.9889, 
                0.5328,   0.6284,   0.7217,   0.8127,   0.9018,   0.9889, 
                0.5328,   0.6284,   0.7216,   0.8127,   0.9018,   0.9889, 
                0.5327,   0.6284,   0.7216,   0.8127,   0.9018,   0.9889, 
            // turbine inlet temperature =   375.00 C
                0.5375,   0.6343,   0.7288,   0.8212,   0.9117,   1.0003, 
                0.5375,   0.6343,   0.7288,   0.8212,   0.9117,   1.0003, 
                0.5375,   0.6343,   0.7288,   0.8212,   0.9117,   1.0003, 
                0.5375,   0.6343,   0.7288,   0.8213,   0.9117,   1.0003, 
                0.5375,   0.6343,   0.7288,   0.8213,   0.9117,   1.0004, 
                0.5375,   0.6343,   0.7288,   0.8213,   0.9117,   1.0004, 
                0.5375,   0.6343,   0.7288,   0.8213,   0.9117,   1.0004, 
                0.5375,   0.6343,   0.7288,   0.8213,   0.9117,   1.0004, 
                0.5375,   0.6343,   0.7288,   0.8213,   0.9117,   1.0004, 
                0.5375,   0.6343,   0.7288,   0.8213,   0.9117,   1.0004, 
                0.5375,   0.6343,   0.7288,   0.8213,   0.9117,   1.0004, 
                0.5375,   0.6343,   0.7288,   0.8212,   0.9117,   1.0004, 
                0.5375,   0.6343,   0.7288,   0.8212,   0.9117,   1.0004, 
                0.5375,   0.6343,   0.7288,   0.8212,   0.9117,   1.0004, 
                0.5375,   0.6343,   0.7288,   0.8212,   0.9117,   1.0004
        }; // end of data.
        m_Solar_Thermal_Norm.assign( Solar_Thermal_Norm, m_pcond.nsteps, m_mdot.nsteps, m_Thot.nsteps );



		const double PB_Power_Nominal_Norm[3*15*6] =   // Power Block Power Nominal: Adding ACC Power Back (-):Normalized at   110.418 MWe
        {   // blocks for constant turbine inlet temperature
            // rows for constant condensing pressure
            // columns for constant turbine inlet mass
            //    
            //Turbine Inlet Steam Temperature (C)     Mininum     d_spacing       Max           n      
            //               325.00000        25.00    375.00000            3
            //    
            //Condensing Prssure (bar)   Mininum     d_spacing       Max           n      
            //                    0.06         0.01         0.20           15
            //    
            //Turbine Inlet Steam Mass (kg/s)       Mininum     d_spacing       Max           n      
            //                   59.80        11.96       119.60            6
            //    
			// turbine inlet temperature =   325.00 C
				0.5140,   0.6113,   0.7050,   0.7948,   0.8818,   0.9663, 
				0.5088,   0.6073,   0.7020,   0.7932,   0.8807,   0.9653, 
				0.5018,   0.6024,   0.6981,   0.7901,   0.8787,   0.9641, 
				0.4939,   0.5961,   0.6935,   0.7862,   0.8756,   0.9618, 
				0.4854,   0.5889,   0.6874,   0.7818,   0.8717,   0.9586, 
				0.4770,   0.5808,   0.6805,   0.7762,   0.8674,   0.9548, 
				0.4692,   0.5723,   0.6730,   0.7697,   0.8621,   0.9504, 
				0.4615,   0.5640,   0.6650,   0.7625,   0.8558,   0.9454, 
				0.4545,   0.5562,   0.6566,   0.7546,   0.8489,   0.9393, 
				0.4475,   0.5486,   0.6484,   0.7465,   0.8415,   0.9327, 
				0.4410,   0.5414,   0.6407,   0.7383,   0.8338,   0.9258, 
				0.4345,   0.5345,   0.6331,   0.7303,   0.8255,   0.9180, 
				0.4280,   0.5277,   0.6259,   0.7226,   0.8175,   0.9104, 
				0.4220,   0.5214,   0.6191,   0.7152,   0.8095,   0.9020, 
				0.4164,   0.5151,   0.6122,   0.7078,   0.8019,   0.8942, 
			// turbine inlet temperature =   350.00 C
				0.5232,   0.6226,   0.7184,   0.8107,   0.9003,   0.9876, 
				0.5180,   0.6186,   0.7154,   0.8090,   0.8992,   0.9866, 
				0.5110,   0.6137,   0.7115,   0.8059,   0.8972,   0.9855, 
				0.5031,   0.6074,   0.7070,   0.8021,   0.8941,   0.9832, 
				0.4946,   0.6002,   0.7010,   0.7977,   0.8903,   0.9800, 
				0.4863,   0.5921,   0.6941,   0.7921,   0.8859,   0.9762, 
				0.4784,   0.5836,   0.6865,   0.7856,   0.8807,   0.9719, 
				0.4708,   0.5754,   0.6786,   0.7785,   0.8744,   0.9669, 
				0.4637,   0.5675,   0.6701,   0.7706,   0.8675,   0.9609, 
				0.4567,   0.5599,   0.6620,   0.7625,   0.8602,   0.9543, 
				0.4503,   0.5527,   0.6542,   0.7543,   0.8524,   0.9474, 
				0.4438,   0.5458,   0.6467,   0.7462,   0.8442,   0.9396, 
				0.4373,   0.5390,   0.6394,   0.7385,   0.8362,   0.9321, 
				0.4312,   0.5327,   0.6326,   0.7311,   0.8282,   0.9237, 
				0.4256,   0.5264,   0.6257,   0.7238,   0.8206,   0.9159, 
			// turbine inlet temperature =   375.00 C
				0.5326,   0.6343,   0.7323,   0.8268,   0.9188,   1.0086, 
				0.5274,   0.6303,   0.7293,   0.8251,   0.9177,   1.0076, 
				0.5205,   0.6254,   0.7254,   0.8221,   0.9158,   1.0065, 
				0.5126,   0.6191,   0.7209,   0.8182,   0.9126,   1.0043, 
				0.5041,   0.6119,   0.7149,   0.8138,   0.9089,   1.0011, 
				0.4958,   0.6038,   0.7080,   0.8084,   0.9045,   0.9973, 
				0.4879,   0.5953,   0.7005,   0.8018,   0.8993,   0.9930, 
				0.4802,   0.5871,   0.6926,   0.7947,   0.8930,   0.9880, 
				0.4732,   0.5792,   0.6841,   0.7868,   0.8862,   0.9821, 
				0.4662,   0.5716,   0.6759,   0.7788,   0.8789,   0.9755, 
				0.4597,   0.5644,   0.6681,   0.7705,   0.8711,   0.9686, 
				0.4532,   0.5575,   0.6606,   0.7625,   0.8629,   0.9609, 
				0.4467,   0.5507,   0.6533,   0.7548,   0.8549,   0.9534, 
				0.4407,   0.5444,   0.6465,   0.7474,   0.8469,   0.9450, 
				0.4350,   0.5381,   0.6396,   0.7400,   0.8392,   0.9371
        }; // end of data.
        m_PB_Power_Nominal_Norm.assign( PB_Power_Nominal_Norm, m_pcond.nsteps, m_mdot.nsteps, m_Thot.nsteps );
        
		const double GeoThermal_Norm[3*15*6] =   // Geothermal Power (-):Normalized at    37.168 MWth
        {   // blocks for constant turbine inlet temperature
            // rows for constant condensing pressure
            // columns for constant turbine inlet mass
            //    
            //Turbine Inlet Steam Temperature (C)     Mininum     d_spacing       Max           n      
            //               325.00000        25.00    375.00000            3
            //    
            //Condensing Prssure (bar)   Mininum     d_spacing       Max           n      
            //                    0.06         0.01         0.20           15
            //    
            //Turbine Inlet Steam Mass (kg/s)       Mininum     d_spacing       Max           n      
            //                   59.80        11.96       119.60            6
            //    
            // turbine inlet temperature =   325.00 C
                0.5876,   0.6935,   0.7973,   0.8992,   0.9991,   1.0973,
                0.5721,   0.6752,   0.7762,   0.8754,   0.9727,   1.0683,
                0.5584,   0.6590,   0.7576,   0.8545,   0.9494,   1.0428,
                0.5461,   0.6445,   0.7410,   0.8357,   0.9286,   1.0198,
                0.5349,   0.6314,   0.7258,   0.8186,   0.9096,   0.9990,
                0.5247,   0.6193,   0.7120,   0.8030,   0.8922,   0.9799,
                0.5152,   0.6081,   0.6991,   0.7885,   0.8762,   0.9623,
                0.5064,   0.5977,   0.6872,   0.7751,   0.8612,   0.9459,
                0.4981,   0.5879,   0.6760,   0.7625,   0.8472,   0.9305,
                0.4903,   0.5787,   0.6655,   0.7506,   0.8341,   0.9161,
                0.4832,   0.5703,   0.6557,   0.7393,   0.8213,   0.9018,
                0.4763,   0.5622,   0.6463,   0.7288,   0.8096,   0.8890,
                0.4697,   0.5545,   0.6375,   0.7188,   0.7985,   0.8768,
                0.4635,   0.5471,   0.6290,   0.7092,   0.7879,   0.8652,
                0.4575,   0.5401,   0.6209,   0.7001,   0.7778,   0.8541,
			// turbine inlet temperature =   350.00 C
                0.5881,   0.6944,   0.7986,   0.9011,   1.0020,   1.1014,
                0.5726,   0.6761,   0.7775,   0.8773,   0.9755,   1.0723,
                0.5589,   0.6599,   0.7589,   0.8563,   0.9522,   1.0466,
                0.5466,   0.6454,   0.7422,   0.8375,   0.9313,   1.0236,
                0.5354,   0.6322,   0.7271,   0.8204,   0.9123,   1.0027,
                0.5251,   0.6201,   0.7132,   0.8047,   0.8948,   0.9835,
                0.5157,   0.6089,   0.7003,   0.7902,   0.8787,   0.9658,
                0.5068,   0.5985,   0.6883,   0.7768,   0.8637,   0.9494,
                0.4986,   0.5887,   0.6771,   0.7641,   0.8497,   0.9339,
                0.4908,   0.5795,   0.6666,   0.7523,   0.8365,   0.9194,
                0.4836,   0.5711,   0.6568,   0.7409,   0.8237,   0.9051,
                0.4767,   0.5629,   0.6474,   0.7304,   0.8120,   0.8923,
                0.4702,   0.5552,   0.6385,   0.7204,   0.8008,   0.8800,
                0.4639,   0.5478,   0.6300,   0.7108,   0.7902,   0.8683,
                0.4580,   0.5408,   0.6219,   0.7016,   0.7800,   0.8572,                   
            // turbine inlet temperature =   375.00 C
                0.5887,   0.6953,   0.7998,   0.9028,   1.0042,   1.1042,
                0.5731,   0.6769,   0.7787,   0.8790,   0.9777,   1.0750,
                0.5594,   0.6607,   0.7601,   0.8579,   0.9543,   1.0493,
                0.5471,   0.6461,   0.7433,   0.8391,   0.9333,   1.0262,
                0.5359,   0.6329,   0.7282,   0.8219,   0.9143,   1.0053,
                0.5256,   0.6208,   0.7142,   0.8062,   0.8968,   0.9861,
                0.5161,   0.6096,   0.7014,   0.7917,   0.8806,   0.9683,
                0.5073,   0.5992,   0.6894,   0.7782,   0.8656,   0.9518,
                0.4990,   0.5894,   0.6782,   0.7656,   0.8516,   0.9363,
                0.4912,   0.5802,   0.6676,   0.7537,   0.8383,   0.9218,
                0.4838,   0.5717,   0.6578,   0.7424,   0.8259,   0.9081,
                0.4768,   0.5636,   0.6484,   0.7317,   0.8140,   0.8951,
                0.4702,   0.5559,   0.6395,   0.7216,   0.8027,   0.8827,
                0.4643,   0.5485,   0.6310,   0.7119,   0.7920,   0.8709,
                0.4584,   0.5414,   0.6229,   0.7027,   0.7817,   0.8596
        }; // end of data.
        m_GeoThermal_Norm.assign( GeoThermal_Norm, m_pcond.nsteps, m_mdot.nsteps, m_Thot.nsteps );

		
	}

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
			water_TQ( m_dT_cw_ref + 3.0 + m_T_approach + m_T_amb_des, 1.0, &wp );
			m_Psat_ref = wp.P*1000.;		// [Pa]
				
			break;

		case 2:
		case 3:
			water_TQ( m_T_ITD_des + m_T_amb_des, 1.0, &wp );
			m_Psat_ref = wp.P*1000.;		// [Pa]
			
			break;
		}
		
		//Calculate the geothermal thermal contribution at design
		double qtot = m_P_ref/m_eta_ref;		//total thermal contribution includes both solar and geothermal sources
		double qndtot = 1. + m_qgeo_frac_ref;	//the total non-dimensional thermal fraction includes the solar fraction (1) plus the fraction of geothermal relative to solar (qgeo/qsolar)
		double qsol_nd_ref = 1./qndtot;			//The actual fraction of thermal energy supplied by solar at design
		double qgeo_nd_ref = m_qgeo_frac_ref/qndtot;	//The actual fraction thermal energy supplied by geothermal at design
		m_q_solar_ref = qsol_nd_ref * qtot;		//The dimensional amount of thermal energy supplied by solar at design
		m_q_geo_ref = qgeo_nd_ref * qtot;		//The dimensional amount of thermal energy supplied by geothermal at design


		double wnet_nd_adj, qsol_nd_adj, qgeo_nd_adj;
		CycleMap(m_Psat_ref, m_T_hot_ref-m_dT_hx_ref, 1., wnet_nd_adj, qsol_nd_adj, qgeo_nd_adj);	//Look up the performance at the specified design-point condenser condition
		double qfrac = (qsol_nd_adj * m_q_solar_ref + qgeo_nd_adj * m_q_geo_ref)/qtot;	//What is fraction of total thermal energy
		m_eta_adj = m_eta_ref*qfrac/wnet_nd_adj;  //The reference efficiency, adjusted for the user-specified condenser conditions
		m_q_dot_ref = m_P_ref/m_eta_adj;		//[kW] The reference heat flow
			
		
		//water_TP( m_T_hot_ref, m_P_boil_des*100.0, &wp );
		//double h_hot_ref = wp.H;	//[kJ/kg] HP turbine inlet enthalpy and entropy
		//double s_t = wp.S;			//[kJ/kg-K]
		//water_TP( m_T_cold_ref, m_P_boil_des*100.0, &wp );
		//double h_cold_ref = wp.H;	//[kJ/kg]
		double c_htf_ref = m_htfProps.Cp( (m_T_hot_ref+m_T_cold_ref)/2.0 + 273.15 );
		
		// Design-point mass flow rate
		m_m_dot_ref = m_q_solar_ref/( c_htf_ref * (m_T_hot_ref - m_T_cold_ref) ); 

		
		return true;
	}
	//--------------------------------------------------------------------------------------------
	//--------------------------------------------------------------------------------------------
	//--------------------------------------------------------------------------------------------

	bool CycleMap( double pcond, double Tin, double mdot_ND, double &wnet_ND, double &qsolar_ND, double &qgeo_ND)
	{
		/* 
		Evaluate the cycle performance map using the inputs provided. 

		INPUTS:
		pcond		[Pa]	Condenser inlet pressure
		Tin			[C]		Solar field HTF inlet temperature
		mdot_ND		[-]		Non-dimensional mass flow rate of solar field HTF to the cycle
		---- Variables set by this method ----
		wnet_ND		[-]		Fraction of design-point power provided by the cycle
		qsolar_ND	[-]		Fraction of design-point heat contributed by the solar field, relative to design solar contribution ONLY
		qgeo_ND		[-]		Fraction of design-point heat contributed by the geothermal heat source, relative to design geothermal contribution ONLY

		RETURNS:
		BOOL	Did the method successfully set output variables?
		

		*/

		try{
			//calculate the interpolation index for each component
			double
				find_pcond = (pcond - m_pcond.varmin)/m_pcond.delta,
				find_mdot = (mdot_ND - m_mdot.varmin)/m_mdot.delta,
				find_Thot = (Tin - m_Thot.varmin)/m_Thot.delta;

			//lower index for each
			int 
				ilo_pcond = min(max(0, (int)find_pcond), m_pcond.nsteps-2),
				ilo_mdot = min(max(0, (int)find_mdot), m_mdot.nsteps-2),
				ilo_Thot = min(max(0, (int)find_Thot), m_Thot.nsteps-2);
			
			//interpolation factor
			double
				f_pcond = find_pcond - ilo_pcond,
				f_mdot = find_mdot - ilo_mdot, 
				f_Thot = find_Thot - ilo_Thot; 
			
			//3D interpolation
			vector<util::block_t<double>*> datas = {&m_PB_Power_Nominal_Norm, &m_Solar_Thermal_Norm, &m_GeoThermal_Norm};
			vector<double *> outputs = {&wnet_ND, &qsolar_ND, &qgeo_ND};

			for(int d=0; d<(int)datas.size(); d++){
				util::block_t<double>* dat = datas.at(d);
				double intcube[2][2][2];
				//Get all points in the surrounding cube
				for(int i = ilo_Thot; i < ilo_Thot+2; i++){
					int iuse = min(i, m_Thot.nsteps-1);
					for(int j = ilo_mdot; j < ilo_mdot+2; j++){
						int juse = min(j, m_mdot.nsteps-1);
						for(int k = ilo_pcond; k < ilo_pcond+2; k++){
							int kuse = min(k, m_pcond.nsteps-1);
							intcube[i-ilo_Thot][j-ilo_mdot][k-ilo_pcond] = dat->at(kuse,juse,iuse);
						}
					}
				}

				//square in 1st dim
				double intsquare[2][2];
				for(int j = 0; j<2; j++){
					for( int k=0; k<2; k++){
						intsquare[j][k] = intcube[0][j][k] + (intcube[1][j][k] - intcube[0][j][k])*f_Thot;
					}
				}
				//line in 2nd dim
				double intline[2];
				for( int k=0; k<2; k++){
					intline[k] = intsquare[0][k] + (intsquare[1][k] - intsquare[0][k])*f_mdot;
				}

				//final interpolation point
				*outputs.at(d) = intline[0] + (intline[1] - intline[0])*f_pcond;
			}
		}
		catch(...){
			return false;
		}
		return true;
	}

	//--------------------------------------------------------------------------------------------
	//--------------------------------------------------------------------------------------------
	//--------------------------------------------------------------------------------------------

	bool GeoCSP_RankineCycle( double T_db, double T_wb, double P_amb, double T_hot, double m_dot_htf, int mode, double demand_var, double F_wc_tou, 
							double & P_cycle, double &Q_solar, double &Q_geo, double & eta, double & T_cold, double & m_dot_demand, double & m_dot_makeup, 
							double & W_cool_par, double & f_hrsys, double & P_cond )
	{
		m_dot_htf = m_dot_htf/3600.0;		//[kg/s] Mass flow rate, convert from [kg/hr]

		// Calculate the htf mass flow rate in non-dimensional form
		double m_dot_ND = m_dot_htf/m_m_dot_ref;

		// The saturation temperature at the boiler. Using the floating pressure value is consistent with the regression model formulation in this case.
		water_PQ( m_P_boil_des*100.0, 0.5, &wp );
		double T_ref = wp.T;

		// Calculate the hot inlet steam temperature, in non-dimensional form
		double T_hot_ND = (T_hot - T_ref)/(m_T_hot_ref - T_ref);

		// Do an initial cooling tower call to estimate the turbine back pressure. 
		double q_reject_est = m_q_dot_ref*1000.0*(1.0-m_eta_adj)*m_dot_ND*T_hot_ND;

		double T_cond, m_dot_air, W_cool_parhac, W_cool_parhwc;
		switch( m_CT )
		{
		case 1:
			CSP::evap_tower( 1, m_P_cond_min, m_n_pl_inc, m_dT_cw_ref, m_T_approach, m_P_ref*1000.0, m_eta_adj, T_db, T_wb, P_amb, q_reject_est, m_dot_makeup, W_cool_par, P_cond, T_cond, f_hrsys );
			break;
		case 2:
			CSP::ACC( 1, m_P_cond_min, m_n_pl_inc, m_T_ITD_des, m_P_cond_ratio, m_P_ref*1000.0, m_eta_adj, T_db, P_amb, q_reject_est, m_dot_air, W_cool_par, P_cond, T_cond, f_hrsys );
			m_dot_makeup = 0.0;
			break;
		case 3:
			CSP::HybridHR( 1, m_P_cond_min, m_n_pl_inc, F_wc_tou, m_F_wcmax, m_F_wcmin, m_T_ITD_des, m_T_approach, m_dT_cw_ref, m_P_cond_ratio, m_P_ref*1000.0, m_eta_adj, T_db, T_wb, P_amb, q_reject_est, m_dot_makeup,
							W_cool_parhac, W_cool_parhwc, W_cool_par, P_cond, T_cond, f_hrsys );
			break;
		}

		// Set initial values
		double ADJ = 1.0;
		double err = 1.0;
		int qq = 0;

		// Do a quick check to see if there is actually a mass flow being supplied to the cycle
		// If not, go to the end
		if( abs(m_dot_ND) < 1.E-3 )
		{
			P_cycle = 0.0;
			Q_solar = 0.;
			Q_geo = 0.;
			eta = 0.0;
			T_cold = m_T_hot_ref;
			m_dot_demand = m_m_dot_ref;
			W_cool_par = 0.0;
			m_dot_makeup = 0.0;
			// Set the error to zero, since we don't want to iterate
			err = 0.0;
		}

		double P_dem_ND;
		
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
			double PND, QSND, QGND;
			if(! CycleMap(P_cond, T_hot - m_dT_hx_ref, m_dot_ND, PND, QSND, QGND) ){
				message("An error occurred while evaluating the geothermal power cycle performance.");
				return -1;
			}
			
			//Calculate the amount of geothermal
			Q_geo = m_q_geo_ref * QGND;
			Q_solar = m_q_solar_ref * QSND;
			//efficiency
			eta = P_cycle/(Q_geo + Q_solar);
			//mass flow of HTF
			m_dot_demand = max( m_dot_ND*m_m_dot_ref, 0.00001 );	//[kg/s]

			// Calculate the output values:
			P_cycle = PND * m_P_ref;
			/*water_TP( T_hot, m_P_boil_des*100.0, &wp );
			double h_hot = wp.H;
			double h_cold = h_hot - QSND*m_q_dot_st_ref/m_dot_st;
			do
			{
				water_PH( m_P_boil_des*100.0, h_cold, &wp );
				T_cold = wp.T;
				water_TP( T_cold, m_P_boil_des*100.0, &wp );
				if( abs(wp.H - h_cold)/h_cold < 0.01 )
				{					
					break;
				}
				h_cold*=0.999;
			} while( true );*/

			//Iteratively calculate cold HTF return temperature for solar field
			double c_htf = m_htfProps.Cp( T_hot + 273.15 );
			while(true){
				T_cold = T_hot - Q_solar/(c_htf * m_dot_demand );
				double c_htf_new =  m_htfProps.Cp( (T_cold + T_hot)/2. + 273.15 );
				if(abs(c_htf_new - c_htf)/c_htf < 0.005) break;
				c_htf = c_htf_new;
			}


			// Call the cooling tower model to update the condenser pressure
			double q_reject = (1.0 - eta)*(Q_geo + Q_solar)*1000.0;
		
			if( qq < 10 )
			{
				switch( m_CT )
				{
				case 1:
					CSP::evap_tower( 1, m_P_cond_min, m_n_pl_inc, m_dT_cw_ref, m_T_approach, m_P_ref*1000.0, m_eta_adj, T_db, T_wb, P_amb, q_reject, m_dot_makeup, W_cool_par, P_cond_guess, T_cond, f_hrsys );
					break;
				case 2:
					CSP::ACC( 1, m_P_cond_min, m_n_pl_inc, m_T_ITD_des, m_P_cond_ratio, m_P_ref*1000.0, m_eta_adj, T_db, P_amb, q_reject, m_dot_air, W_cool_par, P_cond_guess, T_cond, f_hrsys );
					break;
				case 3:
					CSP::HybridHR( 1, m_P_cond_min, m_n_pl_inc, F_wc_tou, m_F_wcmax, m_F_wcmin, m_T_ITD_des, m_T_approach, m_dT_cw_ref, m_P_cond_ratio, m_P_ref*1000.0, m_eta_adj, T_db, T_wb,
										P_amb, q_reject, m_dot_makeup, W_cool_parhac, W_cool_parhwc, W_cool_par, P_cond_guess, T_cond, f_hrsys );
					break;
				}
			}
			// Check to see if the calculated and demand values match
			// If they don't match, calculate the "ADJ" factor
			if( mode == 1 )
			{
				ADJ = ( demand_var - P_cycle ) / demand_var;		//MJW 10.31.2010 Adjustment factor
				err = abs(ADJ);										//MJW 10.31.2010 Take absolute value of the error...
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

			err = abs(err);


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

		

		// Finally, convert the values back to their original units
		m_dot_demand = m_dot_demand*3600.0;  //[kg/s]->[kg/hr]
		m_dot_htf = m_dot_htf*3600.0;          //![kg/s]->[kg/hr]

		return true;
	}


	virtual int call(double time, double step, int ncall){
		
		int mode = value( I_MODE );					//[-] Cycle part load control... from plant controller
		double T_hot = value( I_T_HOT );			//[C] Hot inlet temperature
		double m_dot_htf = value( I_M_DOT_HTF );		//[kg/s] Mass flow rate of HTF through the steam generator
		double T_wb = value( I_T_WB )+273.15;		//[K] Wet bulb temperature, convert from C
		double demand_var = value( I_DEMAND_VAR );	//[?] Control signal indicating operational mode - only used when mode == 1
		m_standby_control = value( I_STANDBY_CONTROL);	//[-] Control signal indicating standby mode
		double T_db = value( I_T_DB )+273.15;		//[K] Ambient dry bulb temperature, convert from C
		//double P_amb = value( I_P_AMB )*101325.0;	//[Pa] Ambient pressure, convert from bar
		double P_amb = value( I_P_AMB )*100.0;		//[Pa] Ambient pressure, convert from mbar
		//int tou = value( I_TOU );					//[-] Current Time-Of-Use period
		int tou = (int)value(I_TOU) - 1;			// control value between 1 & 9, have to change to 0-8 for array index
		double rh = value( I_RH )/100.0;			//[-] Relative humidity of the ambient air, convert from %

		double F_wc_tou = m_F_wc[tou];				//[-] Hybrid fraction at current Time-Of-Use period

		
		//*********************************************
		// Values for TRNSYS timestep comparison
		//*********************************************
		/*m_standby_control_prev = 1;
		m_time_su_prev = 0.0;
		m_E_su_prev = 0.0;*/

		double f_rec_su = 1.0;
		
		
		if( mode == 1)
			demand_var = demand_var*1000.0;			//[?] If the mode is to operate in power demand, convert from MW to KW

		
		// Set outputs to 0, let model solve for correct values
		double m_dot_st_bd = 0., P_turb_in;
		
		// Declare variables that will be solved by 'switch' options
		double P_cycle, q_solar, q_geo, eta, T_cold, m_dot_demand, m_dot_makeup, W_cool_par, f_hrsys, P_cond;

		switch (m_standby_control)
		{
		case 1:		// The cycle is in normal operation
		{
			GeoCSP_RankineCycle( T_db, T_wb, P_amb, T_hot, m_dot_htf, mode, demand_var, F_wc_tou, 
				P_cycle, q_solar, q_geo, eta, T_cold, m_dot_demand, m_dot_makeup, 
				W_cool_par, f_hrsys, P_cond);

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
			water_TP(T_hot, m_P_boil_des*100.0, &wp);
			double h_st_hot = wp.H;		//[kJ/kg]
			water_TP(m_T_cold_ref, m_P_boil_des*100.0, &wp);	
			double h_st_cold = wp.H;			//[kJ/kg]
			m_dot_st_bd = q_solar/max(1.e-6, (h_st_hot - h_st_cold)) * m_pb_bd_frac;
		}
			break;

		case 2:		// The cycle is in standby operation

		{
			double q_tot = m_P_ref/m_eta_ref;

			// Calculate the actual q_sby_needed from the reference flow
			double q_sby_needed = q_tot * m_q_sby_frac;

			// Now calculate the mass flow rate knowing the inlet temp of the steam and holding the outlet temperature at the reference outlet temp
			double c_htf = m_htfProps.Cp( (T_hot + m_T_cold_ref)/2.0 + 273.15 );
			double m_dot_sby = q_sby_needed/(c_htf * (T_hot - m_T_cold_ref))*3600.0;

			// Set other output values
			P_cycle = 0.0;
			eta = 0.0;
			T_cold = m_T_cold_ref;
			m_dot_demand = m_dot_sby;
			m_dot_makeup = 0.0;
			W_cool_par = 0.0;
			f_hrsys = 0.0;
			P_cond = 0.0;
			q_solar = 0.0;
			q_geo = 0.0;
		}

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
			q_solar = 0.0;
			q_geo = 0.0;
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
		value( O_Q_SOLAR, q_solar/1000.0);					//"Thermal load from solar",	"MWt"
		value(O_Q_GEO, q_geo/1000.0);						//"Thermal load from geothermal", "MWt"
		
		// Cycle thermal efficiency
		value( O_ETA, eta ); 
		// Heat transfer fluid outlet temp
		value( O_T_COLD, T_cold );
		// Wet cooling makeup water flow rate [kg/hr]
		value( O_M_DOT_MAKEUP, (m_dot_makeup + m_dot_st_bd)*3600.0 );
		// Heat transfer fluid demand flow rate [kg/hr]
		value( O_M_DOT_DEMAND, m_dot_demand );
		// Heat transfer fluid flow rate [kg/hr]
		value( O_M_DOT_OUT, m_dot_htf );
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

		return 0;
	}

	virtual int converged(double time)
	{

		m_standby_control_prev = m_standby_control;
		m_time_su_prev = m_time_su;
		m_E_su_prev = m_E_su;

		return 0;
	}
	
	
};

TCS_IMPLEMENT_TYPE( sam_geothermal_hybrid_pb, "Geomthermal Hybrid Power Cycle", "Mike Wagner", 1, sam_geothermal_hybrid_pb_vars, NULL, 1 );
