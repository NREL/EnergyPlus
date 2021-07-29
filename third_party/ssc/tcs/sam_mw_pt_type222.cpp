#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "htf_props.h"
#include "sam_csp_util.h"
#include "ngcc_powerblock.h"


enum{	//Parameters
		P_N_panels,
		P_D_rec,
		P_H_rec,
		P_THT,
		P_D_out,
		P_th_tu,
		P_mat_tube,
		P_field_fl,
		P_field_fl_props,
		P_Flow_type,
		P_epsilon,
		P_hl_ffact,
		P_T_htf_hot_des,
		P_T_htf_cold_des,
		P_f_rec_min,
		P_Q_rec_des,
		P_rec_su_delay,
		P_rec_qf_delay,
		P_m_dot_htf_max,
		P_A_sf,
		P_IS_DIRECT_ISCC,
		P_CYCLE_CONFIG,
//		P_fluxmap_angles,
//		P_fluxmap,       
		P_n_flux_x,
		P_n_flux_y,


		//Inputs
		I_azimuth,
		I_zenith,
		I_T_salt_hot,
		I_T_salt_cold,
		I_v_wind_10,
		I_P_amb,
		I_eta_pump,
		I_T_dp,
		I_I_bn,
		I_field_eff,
		I_T_db,
		I_night_recirc,
		I_hel_stow_deploy,
		I_flux_map,

		//Outputs
		O_m_dot_salt_tot,
		O_eta_therm,
		O_W_dot_pump,
		O_q_conv_sum,
		O_q_rad_sum,
		O_Q_thermal,
		O_T_salt_hot,
		//-not named-,
		O_field_eff_adj,
		O_q_solar_total,
		O_q_startup,
		O_dP_receiver,
		O_dP_total,
		O_vel_htf,
		O_T_salt_in,
		O_M_DOT_SS, 
		O_Q_DOT_SS,
		O_F_TIMESTEP,

		//N_MAX
		N_MAX};

tcsvarinfo sam_mw_pt_type222_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_NUMBER, P_N_panels,			"N_panels",			"Number of individual panels on the receiver",								"",			"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_D_rec,			"D_rec",			"The overall outer diameter of the receiver",								"m",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_H_rec,			"H_rec",			"The height of the receiver",												"m",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_THT,				"THT",				"The height of the tower (hel. pivot to rec equator)",						"m",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_D_out,			"d_tube_out",		"The outer diameter of an individual receiver tube",						"mm",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_th_tu,			"th_tube",			"The wall thickness of a single receiver tube",								"mm",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_mat_tube,			"mat_tube",         "The material name of the receiver tubes",									"",			"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_field_fl,			"rec_htf",			"The name of the HTF used in the receiver",									"",			"", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_field_fl_props,   "field_fl_props",   "User defined field fluid property data",                                   "-",        "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",        "",        ""},
	{TCS_PARAM, TCS_NUMBER, P_Flow_type,		"Flow_type",		"A flag indicating which flow pattern is used",								"",			"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_epsilon,			"epsilon",			"The emissivity of the receiver surface coating",							"",			"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_hl_ffact,			"hl_ffact",			"The heat loss factor (thermal loss fudge factor)",							"",			"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_T_htf_hot_des,	"T_htf_hot_des",	"Hot HTF outlet temperature at design conditions",							"C",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_T_htf_cold_des,	"T_htf_cold_des",	"Cold HTF inlet temperature at design conditions",							"C",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_f_rec_min,		"f_rec_min",		"Minimum receiver mass flow rate turn down fraction",						"",			"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_Q_rec_des,		"Q_rec_des",		"Design-point receiver thermal power output",								"MWt",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_rec_su_delay,		"rec_su_delay",		"Fixed startup delay time for the receiver",								"hr",		"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_rec_qf_delay,		"rec_qf_delay",		"Energy-based receiver startup delay (fraction of rated thermal power)",	"",			"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_m_dot_htf_max,	"m_dot_htf_max",	"Maximum receiver mass flow rate",											"kg/hr",	"", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_A_sf,				"A_sf",				"Solar Field Area",                                                         "m^2",      "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_IS_DIRECT_ISCC,   "is_direct_iscc",   "Is receiver directly connected to an iscc power block",                    "-",        "", "", "-999"},
	{TCS_PARAM, TCS_NUMBER, P_CYCLE_CONFIG,     "cycle_config",     "Configuration of ISCC power cycle",                                        "-",        "", "", "1"},
	{TCS_PARAM, TCS_NUMBER, P_n_flux_x,         "n_flux_x",         "Receiver flux map resolution - X",                                         "-",        "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_n_flux_y,         "n_flux_y",         "Receiver flux map resolution - Y",                                         "-",        "", "", ""},
	//{TCS_PARAM, TCS_MATRIX, P_fluxmap_angles,   "fluxmap_angles",   "Matrix containing zenith and azimuth angles for flux maps",                "-",        "2 columns - azimuth angle, zenith angle. number of rows must equal number of flux maps provided", "", "" },
	//{TCS_PARAM, TCS_MATRIX, P_fluxmap,          "fluxmap",          "Matrix containing flux map for various solar positions",                   "-",        "", "", "" },

	//INPUTS
	{TCS_INPUT, TCS_NUMBER, I_azimuth,			"azimuth",			"Solar azimuth angle",														"deg",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_zenith,			"zenith",			"Solar zenith angle",														"deg",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_T_salt_hot,		"T_salt_hot_target", "Desired HTF outlet temperature",											"C",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_T_salt_cold,		"T_salt_cold",		"Desired HTF inlet temperature",											"C",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_v_wind_10,		"V_wind_10",		"Ambient wind velocity, ground level",										"m/s",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_P_amb,			"P_amb",			"Ambient atmospheric pressure",												"mbar",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_eta_pump,			"eta_pump",			"Receiver HTF pump efficiency",												"",			"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_T_dp,				"T_dp",				"Ambient dew point temperature",											"C",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_I_bn,				"I_bn",				"Direct (beam) normal radiation",											"W/m^2-K",  "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_field_eff,		"field_eff",		"Heliostat field efficiency",												"",			"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_T_db,				"T_db",				"Ambient dry bulb temperature",												"C",		"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_night_recirc,		"night_recirc",		"Flag to indicate night recirculation through the rec.",					"",			"", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_hel_stow_deploy,	"hel_stow_deploy",	"Heliostat field stow/deploy solar angle",									"deg",		"", "", ""},
	{TCS_INPUT, TCS_MATRIX, I_flux_map,			"flux_map",			"Receiver flux map",														"-",		"", "", ""},

	//OUTPUTS
	{TCS_OUTPUT, TCS_NUMBER, O_m_dot_salt_tot,	"m_dot_salt_tot",	"Total HTF flow rate through the receiver",									"kg/hr",	"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_eta_therm,		"eta_therm",		"Receiver thermal efficiency",												"",			"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_dot_pump,		"W_dot_pump",		"Receiver pump power",														"MWe",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_q_conv_sum,		"q_conv_sum",		"Receiver convective losses",												"MWt",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_q_rad_sum,		"q_rad_sum",		"Receiver radiative losses",												"MWt",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_thermal,		"Q_thermal",		"Receiver thermal output",								                    "MWt",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_salt_hot,		"T_salt_hot",		"HTF outlet temperature",													"C",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_field_eff_adj,	"field_eff_adj",	"Adjusted heliostat field efficiency - includes overdesign adjustment",		"",			"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_q_solar_total,	"Q_solar_total",	"Total incident power on the receiver",										"MWt",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_q_startup,		"q_startup",		"Startup energy consumed during the current time step",						"MWt",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_dP_receiver,		"dP_receiver",		"Receiver HTF pressure drop",												"bar",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_dP_total,		"dP_total",			"Total receiver and tower pressure drop",									"bar",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_vel_htf,			"vel_htf",			"Heat transfer fluid average velocity",										"m/s",		"", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_salt_in,       "T_salt_cold",      "Inlet salt temperature",                                                   "C",        "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_M_DOT_SS,        "m_dot_ss",	        "Mass flow rate at steady state - does not derate for startup",             "kg/hr",    "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_DOT_SS,        "q_dot_ss",         "Thermal at steady state - does not derate for startup",                    "MW",       "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_F_TIMESTEP,      "f_timestep",       "Fraction of timestep that receiver is operational (not starting-up)",      "-",        "", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} 
} ;
	
	
class sam_mw_pt_type222 : public tcstypeinterface
{
private:
	HTFProperties field_htfProps;		// Instance of HTFProperties class for field HTF
	HTFProperties tube_material;		// Instance of HTFProperties class for receiver tube material
	HTFProperties ambient_air;			// Instance of HTFProperties class for ambient air

	ngcc_power_cycle cycle_calcs;

	int m_n_panels;	
	double m_d_rec;
	double m_h_rec;
	double m_h_tower;
	double m_od_tube;
	double m_th_tube;
	double m_epsilon;
	double m_hl_ffact;	
	double m_T_htf_hot_des;
	double m_T_htf_cold_des;
	double m_f_rec_min;
	double m_q_rec_des;
	double m_rec_su_delay;
	double m_rec_qf_delay;
	double m_m_dot_htf_max;
	double m_A_sf;

	double m_id_tube;
	double m_A_tube;
	int m_n_t;
	int m_n_flux_x;
	int m_n_flux_y;
	
	double m_A_rec_proj;
	double m_A_node;

	int m_itermode;
	double m_od_control;
	double m_tol_od;
	double m_m_dot_htf_des;
	double m_q_rec_min;
	
	/* declare storage variables here */
	int m_mode;
	int m_mode_prev;
	double m_E_su;
	double m_E_su_prev;
	double m_t_su;
	double m_t_su_prev;

	//util::matrix_t<double> m_fluxmap_angles;	// matrix for fluxmap solar positions
	//util::matrix_t<double> m_fluxmap;         // matrix for flux values
	//int m_num_sol_pos;	
	
	util::matrix_t<int> m_flow_pattern;
	int m_n_lines;
	
	util::matrix_t<double> m_flux_in;

	util::matrix_t<double> m_q_dot_inc;

	//T_sX(N_panels),T_panel_outX(N_panels),T_panel_inX(N_panels)
	util::matrix_t<double> m_T_s_guess;
	util::matrix_t<double> m_T_s;
	util::matrix_t<double> m_T_panel_out_guess;
	util::matrix_t<double> m_T_panel_out;
	util::matrix_t<double> m_T_panel_in_guess;
	util::matrix_t<double> m_T_panel_in;
	util::matrix_t<double> m_T_panel_ave;
	util::matrix_t<double> m_T_panel_ave_guess;
	util::matrix_t<double> m_T_film;
	util::matrix_t<double> m_q_dot_conv;
	util::matrix_t<double> m_q_dot_rad;
	util::matrix_t<double> m_q_dot_loss;
	util::matrix_t<double> m_q_dot_abs;

	double m_m_mixed;
	double m_LoverD;
	double m_RelRough;

	// ISCC-specific
	bool m_is_iscc;
	int m_cycle_config;
	double m_T_amb_low;
	double m_T_amb_high;
	double m_P_amb_low;
	double m_P_amb_high;
	double m_q_iscc_max;
	double *m_i_flux_map;


public:
	sam_mw_pt_type222( tcscontext *cst, tcstypeinfo *ti)
			: tcstypeinterface( cst, ti)
	{
		m_n_panels = -1;	
		m_d_rec = std::numeric_limits<double>::quiet_NaN();
		m_h_rec = std::numeric_limits<double>::quiet_NaN();
		m_h_tower = std::numeric_limits<double>::quiet_NaN();
		m_od_tube = std::numeric_limits<double>::quiet_NaN();
		m_th_tube = std::numeric_limits<double>::quiet_NaN();
		m_epsilon = std::numeric_limits<double>::quiet_NaN();
		m_hl_ffact = std::numeric_limits<double>::quiet_NaN();	
		m_T_htf_hot_des = std::numeric_limits<double>::quiet_NaN();
		m_T_htf_cold_des = std::numeric_limits<double>::quiet_NaN();
		m_f_rec_min = std::numeric_limits<double>::quiet_NaN();
		m_q_rec_des = std::numeric_limits<double>::quiet_NaN();
		m_rec_su_delay = std::numeric_limits<double>::quiet_NaN();
		m_rec_qf_delay = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_htf_max = std::numeric_limits<double>::quiet_NaN();
		m_A_sf = std::numeric_limits<double>::quiet_NaN();

		m_id_tube = std::numeric_limits<double>::quiet_NaN();
		m_A_tube = std::numeric_limits<double>::quiet_NaN();
		m_n_t = -1;
	
		m_A_rec_proj = std::numeric_limits<double>::quiet_NaN();
		m_A_node = std::numeric_limits<double>::quiet_NaN();

		m_itermode = -1;
		m_od_control = std::numeric_limits<double>::quiet_NaN();
		m_tol_od = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_htf_des = std::numeric_limits<double>::quiet_NaN();
		m_q_rec_min = std::numeric_limits<double>::quiet_NaN();
		m_mode = -1;
		m_mode_prev = -1;
		m_E_su = std::numeric_limits<double>::quiet_NaN();
		m_E_su_prev = std::numeric_limits<double>::quiet_NaN();
		m_t_su = std::numeric_limits<double>::quiet_NaN();
		m_t_su_prev = std::numeric_limits<double>::quiet_NaN();

		/*m_fluxmap_angles	= 0.0;
		m_fluxmap			= 0.0;
		m_num_sol_pos		= -1;*/
		
		m_flow_pattern		= 0.0;
		m_n_lines			= -1;

		/*m_flux_in.resize(12, 1);
		m_flux_in.fill(0.0);*/		 

		m_m_mixed = std::numeric_limits<double>::quiet_NaN();
		m_LoverD = std::numeric_limits<double>::quiet_NaN();
		m_RelRough = std::numeric_limits<double>::quiet_NaN();

		m_is_iscc = false;
		m_cycle_config = 1;

		m_T_amb_low = std::numeric_limits<double>::quiet_NaN(); 
		m_T_amb_high = std::numeric_limits<double>::quiet_NaN();
		m_P_amb_low = std::numeric_limits<double>::quiet_NaN(); 
		m_P_amb_high = std::numeric_limits<double>::quiet_NaN();

		m_q_iscc_max = std::numeric_limits<double>::quiet_NaN();

		m_n_flux_x = 0;
		m_n_flux_y = 0;

		
	}

	virtual ~sam_mw_pt_type222()
	{
	}

	virtual int init()
	{
		double dt = time_step();

		ambient_air.SetFluid( ambient_air.Air );
		// Declare instance of fluid class for FIELD fluid.
		int field_fl	= (int) value( P_field_fl );
		if( field_fl != HTFProperties::User_defined && field_fl < HTFProperties::End_Library_Fluids )
		{
			if( !field_htfProps.SetFluid( field_fl ) ) // field_fl should match up with the constants
			{
				message(TCS_ERROR, "Receiver HTF code is not recognized");
				return -1;
			}
		}
		else if( field_fl == HTFProperties::User_defined )
		{
			int nrows = 0, ncols = 0;
			double *fl_mat = value( P_field_fl_props, &nrows, &ncols );
			if ( fl_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<double> mat( nrows, ncols, 0.0 );
				for (int r=0;r<nrows;r++)
					for (int c=0;c<ncols;c++)
						mat.at(r,c) = TCS_MATRIX_INDEX( var( P_field_fl_props ), r, c );

				if ( !field_htfProps.SetUserDefinedFluid( mat ) )
				{
					//message( "user defined htf property table was invalid (rows=%d cols=%d)", nrows, ncols );
					message( TCS_ERROR, field_htfProps.UserFluidErrMessage(), nrows, ncols );
					return -1;
				}
			}
			else
			{
				message(TCS_ERROR, "The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", nrows, ncols);
				return -1;
			}
		}
		else
		{
			message(TCS_ERROR,  "Receiver HTF code is not recognized" );
			return -1;
		}

		// Declare instance of htf class for receiver tube material
		int mat_tube = (int) value( P_mat_tube );
		if( mat_tube == HTFProperties::Stainless_AISI316 || mat_tube == HTFProperties::T91_Steel )
		{
			tube_material.SetFluid( mat_tube );
		}
		else if( mat_tube == HTFProperties::User_defined )
		{
			message( TCS_ERROR, "Receiver material currently does not accept user defined properties" );
			return -1;
		}
		else
		{
			message( TCS_ERROR, "Receiver material code, %d, is not recognized", mat_tube );
			return -1;
		}


		m_n_panels	= (int) value( P_N_panels );	//[-] Number of panels in receiver
		m_d_rec		= value( P_D_rec );				//[m] Diameter of receiver
		m_h_rec		= value( P_H_rec );				//[m] Height of receiver
		m_h_tower	= value( P_THT );				//[m] Height of tower
		m_od_tube	= value( P_D_out )/1.E3;		//[m] Outer diameter of receiver tubes -> convert from mm
		m_th_tube	= value( P_th_tu )/1.E3;		//[m] Thickness of receiver tubes -> convert from mm

		int flowtype = (int) value( P_Flow_type );	//[-] Numerical code to designate receiver flow type

		m_epsilon			= value( P_epsilon );			//[-] Emissivity of receiver
		m_hl_ffact			= value( P_hl_ffact );			//[-] Heat Loss Fudge FACTor
		m_T_htf_hot_des		= value( P_T_htf_hot_des ) + 273.15;	 //[K] Design receiver outlet temperature -> convert from K
		m_T_htf_cold_des	= value( P_T_htf_cold_des ) + 273.15; //[K] Design receiver inlet temperature -> convert from C
		m_f_rec_min			= value( P_f_rec_min );			//[-] Minimum receiver mass flow rate turn down fraction
		m_q_rec_des			= value( P_Q_rec_des )*1.E6;	//[W] Design receiver thermal input -> convert from MW
		m_rec_su_delay		= value( P_rec_su_delay );		//[hr] Receiver startup time duration
		m_rec_qf_delay		= value( P_rec_qf_delay );		//[-] Energy-based receiver startup delay (fraction of rated thermal power)
		m_m_dot_htf_max		= value( P_m_dot_htf_max )/3600.0;	//[kg/s] Maximum mass flow rate through receiver -> convert from kg/hr
		m_A_sf				= value( P_A_sf );				//[m^2] Solar field area
		
		m_n_flux_x          = (int)value( P_n_flux_x );
		m_n_flux_y          = (int)value( P_n_flux_y );

		m_id_tube	= m_od_tube - 2*m_th_tube;			//[m] Inner diameter of receiver tube
		m_A_tube	= CSP::pi*m_od_tube/2.0*m_h_rec;	//[m^2] Outer surface area of each tube
		m_n_t		= (int) (CSP::pi*m_d_rec/(m_od_tube*m_n_panels));	// The number of tubes per panel, as a function of the number of panels and the desired diameter of the receiver
		
		int n_tubes		= m_n_t * m_n_panels;				//[-] Number of tubes in the system
		m_A_rec_proj	= m_od_tube*m_h_rec*n_tubes;		//[m^2] The projected area of the tubes on a plane parallel to the center lines of the tubes
		m_A_node		= CSP::pi*m_d_rec/m_n_panels*m_h_rec; //[m^2] The area associated with each node

		
		m_mode = 0;					//[-] 0 = requires startup, 1 = starting up, 2 = running
		m_itermode   = 1;			//[-] 1: Solve for design temp, 2: solve to match mass flow restriction
		m_od_control = 1.0;			//[-] Additional defocusing for over-design conditions
		m_tol_od     = 0.001;		//[-] Tolerance for over-design iteration

		double c_htf_des = field_htfProps.Cp( (m_T_htf_hot_des + m_T_htf_cold_des)/2.0 )*1000.0;		//[J/kg-K] Specific heat at design conditions
		m_m_dot_htf_des = m_q_rec_des/(c_htf_des*(m_T_htf_hot_des - m_T_htf_cold_des));					//[kg/s]
		m_q_rec_min = m_q_rec_des * m_f_rec_min;	//[W] Minimum receiver thermal power

		m_mode_prev = m_mode;
		m_E_su_prev = m_q_rec_des * m_rec_qf_delay;	//[W-hr] Startup energy
		m_t_su_prev = m_rec_su_delay;				//[hr] Startup time requirement

		//allocate the input array for the flux map
		m_i_flux_map = allocate( I_flux_map, m_n_flux_y, m_n_flux_x );

		//Get flow pattern
		//if(.not.allocated(Flow_pattern)) allocate(Flow_pattern(N_panels))
		//call flowPatterns(N_panels,flowtype,Flow_pattern,salt_out,nlines)
		//m_n_lines = 2;	// Need remaining flow patterns
		//m_flow_pattern.resize( m_n_lines, m_n_panels/m_n_lines );		// Try to use flow pattern in direct steam receiver
		std::string flow_msg;
		if(! CSP::flow_patterns( m_n_panels, flowtype, m_n_lines, m_flow_pattern, &flow_msg ) )
        {
            message(TCS_ERROR, flow_msg.c_str() );
            return -1;
        }
		
		// ****** Is this necessary ?!? ******************
		// Set outputs
		// OUT = 0.d0
		// OUT(7)=T_htf_cold_des  !mjw 3.29.11   565.0
		// ***********************************************

		m_q_dot_inc.resize(m_n_panels);
		m_q_dot_inc.fill(0.0);
		
		m_T_s_guess.resize(m_n_panels);
		m_T_s_guess.fill(0.0);
		m_T_s.resize(m_n_panels);
		m_T_s.fill(0.0);

		m_T_panel_out_guess.resize(m_n_panels);
		m_T_panel_out.resize(m_n_panels);
		m_T_panel_out_guess.fill(0.0);
		m_T_panel_out.fill(0.0);

		m_T_panel_in_guess.resize(m_n_panels);
		m_T_panel_in_guess.fill(0.0);
		m_T_panel_in.resize(m_n_panels);
		m_T_panel_in.fill(0.0);
		
		m_T_panel_ave.resize(m_n_panels);
		m_T_panel_ave.fill(0.0);
		m_T_panel_ave_guess.resize(m_n_panels);
		m_T_panel_ave_guess.fill(0.0);

		m_T_film.resize(m_n_panels);
		m_T_film.fill(0.0);

		m_q_dot_conv.resize(m_n_panels);
		m_q_dot_conv.fill(0.0);

		m_q_dot_rad.resize(m_n_panels);
		m_q_dot_rad.fill(0.0);

		m_q_dot_loss.resize(m_n_panels);
		m_q_dot_loss.fill(0.0);

		m_q_dot_abs.resize(m_n_panels);
		m_q_dot_abs.fill(0.0);

		m_m_mixed = 3.2;	//[-] Exponential for calculating mixed convection

		m_LoverD = m_h_rec/m_id_tube;
		m_RelRough = (4.5e-5)/m_id_tube;	//[-] Relative roughness of the tubes. http:www.efunda.com/formulae/fluids/roughness.cfm

		// Are we modeling a direct ISCC case?
		m_is_iscc = value(P_IS_DIRECT_ISCC) == 1;

		// If so, get cycle information
		if( m_is_iscc )
		{
			// Set cycle configuration in class
			m_cycle_config = value(P_CYCLE_CONFIG);
			cycle_calcs.set_cycle_config(m_cycle_config);

			// Get table limits
			cycle_calcs.get_table_range(m_T_amb_low, m_T_amb_high, m_P_amb_low, m_P_amb_high);
		}

		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{		
		//-----------------------------------------------------------------------------------------------------------------------
		// Read inputs and call the CRS subroutine here. This allows calls after all iterations and at the initial timestep
		//-----------------------------------------------------------------------------------------------------------------------

		double azimuth = value( I_azimuth );	//[deg] Solar azimuth angle 0 - 360, clockwise from due north, northern hemisphere
		double zenith = value( I_zenith );		//[deg] Solar zenith angle
		double T_salt_hot_target = value( I_T_salt_hot ) + 273.15;	//[K] Desired hot temp, convert from C
		double T_salt_cold_in = value( I_T_salt_cold ) + 273.15;	//[K] Cold salt inlet temp, convert from C
		double v_wind_10 = value( I_v_wind_10 );		//[m/s] Wind velocity
		double P_amb = value(I_P_amb)*100.0;	//[Pa] Ambient pressure, convert from mbar
		double eta_pump = value( I_eta_pump );	//[-] Receiver HTF pump efficiency
		double hour = time/3600.0;				//[hr] Hour of the year
		double hour_day = (int) hour%24;		//[hr] Hour of the day		
		double T_dp = value( I_T_dp ) + 273.15; //[K] Dewpoint temperature, convert from C
		double I_bn = value( I_I_bn );			//[W/m^2-K] Beam normal radiation
		double field_eff = value( I_field_eff ); //[-] Field efficiency value
		double T_amb = value( I_T_db ) + 273.15; //[K] Dry bulb temperature, convert from C
		int night_recirc = (int) value( I_night_recirc );	//[-] Night recirculation control 0 = empty receiver, 1 = recirculate
		double hel_stow_deploy = value( I_hel_stow_deploy );	//[deg] Solar elevation angle at which heliostats are stowed
		
		//get the flux map
		int n_flux_y, n_flux_x;
		m_i_flux_map = value(I_flux_map, &n_flux_y, &n_flux_x);
		
		if(n_flux_y > 1 ){
			message(TCS_WARNING, "The Molten Salt External Receiver (Type222) model does not currently support 2-dimensional "
				"flux maps. The flux profile in the vertical dimension will be averaged. NY=%d",n_flux_y);
		}
		m_flux_in.resize(n_flux_x);
		
		double T_sky = CSP::skytemp( T_amb, T_dp, hour );		

		// Set current timestep stored values to NaN so we know that code solved for them
		m_mode = -1;
		m_E_su = std::numeric_limits<double>::quiet_NaN();
		m_t_su = std::numeric_limits<double>::quiet_NaN();

		// ***************************************************************** 
		// Set storage variables ONLY for debugging purposes @ time == 36.00
		// *****************************************************************
		/*m_mode_prev = 2.0;
		m_E_su_prev = 0.0;
		m_t_su_prev = 0.0;

		m_itermode = 1;
		m_od_control = 1.0; */
		//******************************************************************
		//******************************************************************
		//******************************************************************

		m_itermode = 1;

		double v_wind = log( (m_h_tower+m_h_rec/2)/0.003)/log(10.0/0.003)*v_wind_10;

		double c_p_coolant, rho_coolant, f, u_coolant, q_conv_sum, q_rad_sum, q_dot_inc_sum;
		c_p_coolant = rho_coolant = f = u_coolant = q_conv_sum = q_rad_sum = q_dot_inc_sum = std::numeric_limits<double>::quiet_NaN();
		double eta_therm, m_dot_salt_tot, T_salt_hot_guess, m_dot_salt_tot_ss;
		eta_therm = m_dot_salt_tot = T_salt_hot_guess = m_dot_salt_tot_ss = std::numeric_limits<double>::quiet_NaN();
		bool rec_is_off = false;
		bool rec_is_defocusing = false;
		double field_eff_adj = 0.0;

		// ************* Outputs for ISCC model ****************
		double q_thermal_ss = 0.0;
		double f_rec_timestep = 1.0;
		// *****************************************************

		// Do an initial check to make sure the solar position called is valid
		// If it's not, return the output equal to zeros. Also check to make sure
		// the solar flux is at a certain level, otherwise the correlations aren't valid
		if( zenith>(90.0-hel_stow_deploy) || I_bn<=1.E-6 || (zenith==0.0 && azimuth==180.0) )
		{
			if( night_recirc == 1 )
			{
				I_bn = 0.0;
			}
			else
			{
				m_mode = 0.0;
				rec_is_off = true;
				// GOTO 900 - return zeros
			}
		}
		
		double T_coolant_prop = (T_salt_hot_target + T_salt_cold_in)/2.0;		//[K] The temperature at which the coolant properties are evaluated. Validated as constant (mjw)
		c_p_coolant = field_htfProps.Cp(T_coolant_prop)*1000.0;					//[kJ/kg-K] Specific heat of the coolant

		double m_dot_htf_max = m_m_dot_htf_max;
		if( m_is_iscc )
		{
			if( ncall == 0 )
			{
				double T_amb_C = max(m_P_amb_low, min(m_T_amb_high, T_amb - 273.15));
				double P_amb_bar = max(m_P_amb_low, min(m_P_amb_high, P_amb / 1.E5));
				m_q_iscc_max = cycle_calcs.get_ngcc_data(0.0, T_amb_C, P_amb_bar, ngcc_power_cycle::E_solar_heat_max)*1.E6;	// kWth, convert from MWth
			}

			double m_dot_iscc_max = m_q_iscc_max / (c_p_coolant*(T_salt_hot_target - T_salt_cold_in));		// [kg/s]
			m_dot_htf_max = min(m_m_dot_htf_max, m_dot_iscc_max);
		}

		double err_od = 999.0;	// Reset error before iteration
		
		// 15 continue -> TRNSYS command - replace
		do
		{
			if( rec_is_off )
				break;

			field_eff_adj = field_eff*m_od_control;

			// Get the values of the flux from the fluxmap and store them  as flux_in(col, row)
			if( I_bn > 1.0 )
			{
				for(int j=0; j<n_flux_x; j++){
					m_flux_in.at(j) = 0.;
					for(int i=0; i<n_flux_y; i++){
						m_flux_in.at(j) += m_i_flux_map[ j*n_flux_y + i ]
							*I_bn*field_eff_adj*m_A_sf/1000./(CSP::pi*m_h_rec*m_d_rec/(double)n_flux_x);	//[kW/m^2];
					}
				}
			}
			else
			{
				m_flux_in.fill(0.0);
			}
			
			double n_flux_x_d = (double)m_n_flux_x;
            double n_panels_d = (double)m_n_panels;

			if( m_n_panels >= m_n_flux_x )
			{
				// Translate to the number of panels, so each panel has its own linearly interpolated flux value
				for( int i = 0; i < m_n_panels; i++ )
				{
					double ppos = (n_flux_x_d/n_panels_d*i+n_flux_x_d*0.5/n_panels_d);
					int flo = (int)floor( ppos );
					int ceiling = (int)ceil( ppos );
					double ind = (int)( (ppos - flo)/max((double)(ceiling - flo),1.e-6) );
					if( ceiling > m_n_flux_x-1 ) ceiling = 0;
					
					double psp_field = (ind*(m_flux_in.at(ceiling)-m_flux_in.at(flo))+m_flux_in.at(flo));		//[kW/m^2] Average area-specific power for each node
					m_q_dot_inc.at(i) = m_A_node*psp_field;	//[kW] The power incident on each node
					
				}
			}
			else
			{
				/* 
                The number of panels is always even, therefore the receiver panels are symmetric about the N-S plane.

                The number of flux points may be even or odd. The distribution is assumed to be symmetric
                about North, therefore:
                    (a) A distribution with an odd number of points includes a center point (n_flux_x - 1)/2+1 
                        whose normal faces exactly north
                    (b) A distribution with an even number of points includes 2 points n_flux_x/2, n_flux_x/2+1 
                        which straddle the North vector. 
                In either scenario, two points straddle the South vector and no scenario allows a point to fall 
                directly on the South vector. Hence, the first and last flux points fall completely on the first
                and last panel, respectively.
                */
                
                double leftovers = 0.;
				int index_start = 0; int index_stop = 0;
				double q_flux_sum = 0.0;

                double panel_step = n_flux_x_d/n_panels_d;   //how many flux points are stepped over by each panel?

                for( int i=0; i<m_n_panels; i++)
                {
					double panel_pos = panel_step*(i+1);   //Where does the current panel end in the flux array?
    
                    index_start = (int)floor(panel_step*i);
                    index_stop = (int)floor(panel_pos);

                    q_flux_sum = 0.;
    
                    for(int j=index_start; j<index_stop+1; j++)
                    {
                        if(j==m_n_flux_x)
                        {
                            if(leftovers > 0.)
                                message(TCS_WARNING, "An error occurred during interpolation of the receiver flux map. The results may be inaccurate! Contact SAM support to resolve this issue.");

                            break;
                        }
                        if( j == 0 )
                        {
                            q_flux_sum = m_flux_in.at(j);
                            leftovers = 0.;
                        }
                        else if(j == index_start)
                        {
                            q_flux_sum += leftovers;
                            leftovers = 0.;
                        }
                        else if(j == index_stop)
                        {
                            double stop_mult = (panel_pos - floor(panel_pos));
                            q_flux_sum += stop_mult * m_flux_in.at(j);
                            leftovers = (1 - stop_mult)*m_flux_in.at(j);
                        }
                        else
                        {
                            q_flux_sum += m_flux_in[j];
                        }
                    }
                    m_q_dot_inc.at(i) = q_flux_sum * m_A_node/n_flux_x_d*n_panels_d ;
                }

			}			

			q_dot_inc_sum = 0.0;
			for( int i = 0; i < m_n_panels; i++ )
				q_dot_inc_sum += m_q_dot_inc.at(i);		//[kW] Total power absorbed by receiver

			// Set guess values
			if( night_recirc == 1 )
			{
				m_T_s_guess.fill(T_salt_hot_target);		//[K] Guess the temperature for the surface nodes
				m_T_panel_out_guess.fill( (T_salt_hot_target + T_salt_cold_in)/2.0);	//[K] Guess values for the fluid temp coming out of the control volume
				m_T_panel_in_guess.fill( (T_salt_hot_target + T_salt_cold_in)/2.0);	//[K] Guess values for the fluid temp coming into the control volume
			}
			else
			{
				m_T_s_guess.fill( T_salt_hot_target );		//[K] Guess the temperature for the surface nodes
				m_T_panel_out_guess.fill( T_salt_cold_in );	//[K] Guess values for the fluid temp coming out of the control volume
				m_T_panel_in_guess.fill( T_salt_cold_in );	//[K] Guess values for the fluid temp coming into the control volume
			}

			double c_guess = field_htfProps.Cp( (T_salt_hot_target + T_salt_cold_in)/2.0 );	//[kJ/kg-K] Estimate the specific heat of the fluid in receiver
			double m_dot_salt_guess = std::numeric_limits<double>::quiet_NaN();
			if( I_bn > 1.E-6 )
			{
				double q_guess = 0.5*q_dot_inc_sum;		//[kW] Estimate the thermal power produced by the receiver				
				m_dot_salt_guess = q_guess/(c_guess*(T_salt_hot_target - T_salt_cold_in)*m_n_lines);	//[kg/s] Mass flow rate for each flow path
			}
			else	// The tower recirculates at night (based on earlier conditions)
			{
				// Enter recirculation mode, where inlet/outlet temps switch
				T_salt_hot_target = T_salt_cold_in;
				T_salt_cold_in = m_T_s_guess.at(0);		//T_s_guess is set to T_salt_hot before, so this just completes 
				m_dot_salt_guess = -3500.0/(c_guess*(T_salt_hot_target - T_salt_cold_in)/2.0);
			}
			T_salt_hot_guess = 9999.9;		//[K] Initial guess value for error calculation
			double err = -999.9;					//[-] Relative outlet temperature error
			double tol = std::numeric_limits<double>::quiet_NaN();
			if( night_recirc == 1 )
				tol = 0.0057;
			else
				tol = 0.001;

			//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
			//                            ITERATION STARTS HERE
			//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
			int qq_max = 50;
			double m_dot_salt = std::numeric_limits<double>::quiet_NaN();
			int qq = 0;

			while( abs(err) > tol )
			{
				qq++;

				// if the problem fails to converge after 50 iterations, then the power is likely negligible and
				// ..the zero set can be returned
				if(qq > qq_max)
				{
					m_mode = 0.0;  // Set the startup mode
					rec_is_off = true;
					break;
					// goto 900
				}
			
				m_dot_salt = m_dot_salt_guess;
				
				for( int i = 0; i < m_n_panels; i++ )
				{
					m_T_s.at(i) = m_T_s_guess.at(i);
					m_T_panel_out.at(i) = m_T_panel_out_guess.at(i);
					m_T_panel_in.at(i) = m_T_panel_in_guess.at(i);
					// Now do the actual calculations
					m_T_panel_ave.at(i) = (m_T_panel_in.at(i)+m_T_panel_out.at(i))/2.0;		//[K] The average coolant temperature in each control volume
					//m_T_film.at(i,0) = (m_T_s.at(i,0) + m_T_panel_out.at(i,0))/2.0;					//[K] Film temperature
					m_T_film.at(i) = (m_T_s.at(i) + T_amb)/2.0;					//[K] Film temperature
				}

				// Calculate the average surface temperature
				double T_s_sum = 0.0;
				for( int i = 0; i < m_n_panels; i++ )
					T_s_sum += m_T_s.at(i);
				double T_s_ave = T_s_sum/m_n_panels;
				double T_film_ave = (T_amb + T_salt_hot_target)/2.0;

				// Convective coefficient for external forced convection using Siebers & Kraabel
				double k_film = ambient_air.cond( T_film_ave );				//[W/m-K] The conductivity of the ambient air
				double mu_film = ambient_air.visc( T_film_ave );			//[kg/m-s] Dynamic viscosity of the ambient air
				double rho_film = ambient_air.dens( T_film_ave, P_amb );	//[kg/m^3] Density of the ambient air
				double c_p_film = ambient_air.Cp( T_film_ave );				//[kJ/kg-K] Specific heat of the ambient air
				double Re_for = rho_film*v_wind*m_d_rec/mu_film;			//[-] Reynolds number
				double ksD = (m_od_tube/2.0)/m_d_rec;						//[-] The effective roughness of the cylinder [Siebers, Kraabel 1984]
				double Nusselt_for = CSP::Nusselt_FC( ksD, Re_for );		//[-] S&K
				double h_for = Nusselt_for*k_film/m_d_rec*m_hl_ffact;		//[W/m^2-K] Forced convection heat transfer coefficient

				// Convection coefficient for external natural convection using Siebers & Kraabel
				// Note: This relationship applies when the surrounding properties are evaluated at ambient conditions [S&K]
				double beta = 1.0/T_amb;												//[1/K] Volumetric expansion coefficient
				double nu_amb = ambient_air.visc(T_amb)/ambient_air.dens(T_amb, P_amb);	//[m^2/s] Kinematic viscosity		


			
				for( int i = 0; i < m_n_panels; i++ )
				{
					//for( int i = 0; i < m_n_panels/m_n_lines; i++ )
					//{
						// int i_fp = m_flow_pattern.at(j,i);
						int i_fp = i;
						// Natural convection
						double Gr_nat = max( 0.0, CSP::grav*beta*(m_T_s.at(i_fp) - T_amb)*pow(m_h_rec,3)/pow(nu_amb,2) );	//[-] Grashof Number at ambient conditions
						double Nusselt_nat = 0.098*pow(Gr_nat,(1.0/3.0))*pow( m_T_s.at(i_fp)/T_amb, -0.14 );					//[-] Nusselt number
						double h_nat = Nusselt_nat*ambient_air.cond( T_amb )/m_h_rec*m_hl_ffact;							//[W/m^-K] Natural convection coefficient
						// Mixed convection
						double h_mixed = pow( (pow(h_for,m_m_mixed) + pow(h_nat,m_m_mixed)), 1.0/m_m_mixed )*4.0;			//(4.0) is a correction factor to match convection losses at Solar II (correspondance with G. Kolb, SNL)
						m_q_dot_conv.at(i_fp) = h_mixed*m_A_node*(m_T_s.at(i_fp) - m_T_film.at(i_fp));							//[W] Convection losses per node
						// Radiation from the receiver - Calculate the radiation node by node
						m_q_dot_rad.at(i_fp) = 0.5*CSP::sigma*m_epsilon*m_A_node*(2.0*pow(m_T_s.at(i_fp),4) - pow(T_amb,4) - pow(T_sky,4))*m_hl_ffact;	//[W] Total radiation losses per node
						m_q_dot_loss.at(i_fp) = m_q_dot_rad.at(i_fp) + m_q_dot_conv.at(i_fp);			//[W] Total overall losses per node
						m_q_dot_abs.at(i_fp) = m_q_dot_inc.at(i_fp)*1000.0 - m_q_dot_loss.at(i_fp);	//[W] Absorbed flux at each node
						// Calculate the temperature drop across the receiver tube wall... assume a cylindrical thermal resistance
						double T_wall = (m_T_s.at(i_fp) + m_T_panel_ave.at(i_fp))/2.0;				//[K] The temperature at which the conductivity of the wall is evaluated
						double k_tube = tube_material.cond( T_wall );								//[W/m-K] The conductivity of the wall
						double R_tube_wall = m_th_tube/(k_tube*m_h_rec*m_d_rec*pow(CSP::pi,2)/2.0/(double)m_n_panels);	//[K/W] The thermal resistance of the wall
						// Calculations for the inside of the tube						
						double mu_coolant = field_htfProps.visc( T_coolant_prop );					//[kg/m-s] Absolute viscosity of the coolant
						double k_coolant = field_htfProps.cond( T_coolant_prop );					//[W/m-K] Conductivity of the coolant
						rho_coolant = field_htfProps.dens( T_coolant_prop, 1.0 );			//[kg/m^3] Density of the coolant

						u_coolant = m_dot_salt/(m_n_t*rho_coolant*pow((m_id_tube/2.0),2)*CSP::pi);	//[m/s] Average velocity of the coolant through the receiver tubes
						double Re_inner = rho_coolant*u_coolant*m_id_tube/mu_coolant;				//[-] Reynolds number of internal flow
						double Pr_inner = c_p_coolant*mu_coolant/k_coolant;							//[-] Prandtl number of internal flow
						double Nusselt_t;
						CSP::PipeFlow( Re_inner, Pr_inner, m_LoverD, m_RelRough, Nusselt_t, f );
						if( Nusselt_t <= 0.0 )
						{
							m_mode = 0.0;		// Set the startup mode
							rec_is_off = true;
							break;
							//GOTO 900
						}
						double h_inner = Nusselt_t*k_coolant/m_id_tube;								//[W/m^2-K] Convective coefficient between the inner tube wall and the coolant
						double R_conv_inner = 1.0/(h_inner*CSP::pi*m_id_tube/2.0*m_h_rec*m_n_t);	//[K/W] Thermal resistance associated with this value

						// Set up numerical flow grid
						//if( i == 0 )
						//	m_T_panel_in_guess.at(i_fp,0) = T_salt_cold_in;
						//else
						//	m_T_panel_in_guess.at(i_fp,0) = m_T_panel_out_guess.at(m_flow_pattern.at(j,i-1),0);

						// Set up numerical flow grid

						// Using flow pattern for direct steam receiver, which calls panels in flow order
						// The follow code reverts back to molten salt receiver convention, which calls panels in panel number order

						int j = -1;
						int i_comp = -1;
						bool found_loc = false;
						for( j = 0; j < 2; j++ )
						{
							for( int abc = 0; abc <  m_n_panels/m_n_lines && !found_loc; abc++ )
							{
								if( m_flow_pattern.at(j,abc) == i )
									found_loc = true;
								i_comp = abc - 1;								
							}
							if( found_loc )
								break;
						}
						if( i_comp == -1 )
							m_T_panel_in_guess.at(i_fp) = T_salt_cold_in;
						else
							m_T_panel_in_guess.at(i_fp) = m_T_panel_out.at(m_flow_pattern.at(j,i_comp));


						m_T_panel_out_guess.at(i_fp) = m_T_panel_in_guess.at(i_fp) + m_q_dot_abs.at(i_fp)/(m_dot_salt*c_p_coolant);	//[K] Energy balance for each node																																																
						m_T_panel_ave_guess.at(i_fp) = (m_T_panel_out_guess.at(i_fp) + m_T_panel_in_guess.at(i_fp))/2.0;				//[K] Panel average temperature
						m_T_s_guess.at(i_fp) = m_T_panel_ave_guess.at(i_fp) + m_q_dot_abs.at(i_fp)*(R_conv_inner+R_tube_wall);			//[K] Surface temperature based on the absorbed heat
					
						if( m_T_s_guess.at(i_fp) < 1.0 )
						{
							m_mode = 0.0;  // Set the startup mode
							rec_is_off = true;
							break;
						}
						
					//}	// End of panels in flow path
					//if( rec_is_off )
					//	break;
				}	// End flow paths in receiver

				if( rec_is_off )
					break;

				q_conv_sum = 0.0; q_rad_sum = 0.0; //q_inc_sum = 0.0;
				double q_abs_sum = 0.0;
				for( int i = 0; i < m_n_panels; i++ )
				{
					q_conv_sum += m_q_dot_conv.at(i);
					double blah = m_q_dot_conv.at(i);
					q_rad_sum += m_q_dot_rad.at(i);
					//q_inc_sum += m_q_dot_inc.at(i,0);
					q_abs_sum += m_q_dot_abs.at(i);
				}
			
				double T_salt_hot_guess_sum = 0.0;
				for( int j = 0; j < m_n_lines; j++ )
					T_salt_hot_guess_sum += m_T_panel_out_guess.at(m_flow_pattern.at(j,m_n_panels/m_n_lines-1));		//[K] Update the calculated hot salt outlet temp
				T_salt_hot_guess = T_salt_hot_guess_sum/(double)m_n_lines;
			
				if( q_dot_inc_sum > 0.0 )
					eta_therm = q_abs_sum / (q_dot_inc_sum*1000.0);
				else
					eta_therm = 0.0;

				err = (T_salt_hot_guess - T_salt_hot_target)/T_salt_hot_target;

				if( abs(err) > tol )
				{
					m_dot_salt_guess = q_abs_sum/(m_n_lines*c_p_coolant*(T_salt_hot_target - T_salt_cold_in));			//[kg/s]

					if( m_dot_salt_guess < 1.E-5 )
					{
						m_mode = 0.0;				//[-] Set the startup mode
						rec_is_off = true;
						// GOTO 900
					}
				}
			}

			if( rec_is_off )
				break;

			// Now we can calculate some of the parasitics associated with pumping the coolant fluid
			// Calculating the pressure drop across the receiver panels
			m_dot_salt_tot = m_dot_salt*m_n_lines;
			double m_dot_tube = m_dot_salt/(double)m_n_t;		//[kg/s] The mass flow through each individual tube

			// Limit the HTF mass flow rate to the maximum, if needed
			if( (m_dot_salt_tot > m_dot_htf_max) || m_itermode == 2 )
			{
				err_od = (m_dot_salt_tot - m_dot_htf_max)/m_dot_htf_max;
				if( err_od < m_tol_od )
				{
					m_itermode = 1;
					m_od_control = 1.0;
					rec_is_defocusing = false;
				}
				else
				{
					m_od_control = m_od_control*pow( (m_dot_htf_max/m_dot_salt_tot), 0.8);	//[-] Adjust the over-design defocus control by modifying the current value
					m_itermode = 2;
					rec_is_defocusing = true;
					// GOTO 15
				}
			}
		} while( rec_is_defocusing );

		double DELTAP, Pres_D, W_dot_pump, q_thermal, q_startup;
		DELTAP = Pres_D = W_dot_pump = q_thermal = q_startup = std::numeric_limits<double>::quiet_NaN();

		q_startup = 0.0;
		if( !rec_is_off )
		{			
			m_dot_salt_tot_ss = m_dot_salt_tot;
			if( m_E_su_prev > 0.0 || m_t_su_prev > 0.0 )
			{
				m_E_su = max( 0.0, m_E_su_prev - m_dot_salt_tot*c_p_coolant*(T_salt_hot_guess - T_salt_cold_in)*step/3600.0 );
				m_t_su = max( 0.0, m_t_su_prev - step/3600.0 );
				if( m_E_su + m_t_su > 0.0 )
				{
					m_mode = 1.0;		// If either are greater than 0, we're staring up but not finished
					q_startup = (m_E_su_prev - m_E_su)/(step/3600.0)*1.E-6;	
					rec_is_off = true;
					f_rec_timestep = 0.0;
					// GOTO 900
				}
				else
				{
					m_mode = 2.0;
					// Adjust the available mass flow to reflect startup
					m_dot_salt_tot = min( (1.0 - m_t_su_prev/(step/3600.0))*m_dot_salt_tot, m_dot_salt_tot - m_E_su_prev/((step/3600.0)*c_p_coolant*(T_salt_hot_guess-T_salt_cold_in)) );
					f_rec_timestep = max( 0.0, min( 1.0 - m_t_su_prev/(step/3600.0), 1.0 - m_E_su_prev/(m_dot_salt_tot*c_p_coolant*(T_salt_hot_guess - T_salt_cold_in)) ) );
				}
				q_startup = (m_E_su_prev - m_E_su)/(step/3600.0)*1.E-6;
			}
			else
			{
				m_E_su = m_E_su_prev;
				m_t_su = m_t_su_prev;
				m_mode = 2.0;		
				q_startup = 0.0;
			}

			// Pressure drop calculations
			double L_e_45 = 16.0;						// The equivalent length produced by the 45 degree bends in the tubes - Into to Fluid Mechanics, Fox et al.
			double L_e_90 = 30.0;						// The equivalent length produced by the 90 degree bends in the tubes
			double DELTAP_tube = rho_coolant*(f*m_h_rec/m_id_tube*pow(u_coolant,2)/2.0);	//[Pa] Pressure drop across the tube, straight length
			double DELTAP_45 = rho_coolant*(f*L_e_45*pow(u_coolant,2)/2.0);					//[Pa] Pressure drop across 45 degree bends
			double DELTAP_90 = rho_coolant*(f*L_e_90*pow(u_coolant,2)/2.0);					//[Pa] Pressure drop across 90 degree bends
			DELTAP = DELTAP_tube + 2*DELTAP_45 + 4*DELTAP_90;						//[Pa] Total pressure drop across the tube with (4) 90 degree bends, (2) 45 degree bends
			double DELTAP_h_tower = rho_coolant*m_h_tower*CSP::grav;						//[Pa] The pressure drop from pumping up to the receiver
			double DELTAP_net = DELTAP*m_n_panels/(double)m_n_lines + DELTAP_h_tower;		//[Pa] The new pressure drop across the receiver panels
			Pres_D = DELTAP_net*1.E-6;			//[MPa]
			double est_load = max( 0.25, m_dot_salt_tot/m_m_dot_htf_des )*100;		//[%] Relative pump load. Limit to 25%
			double eta_pump_adj = eta_pump*(-2.8825E-9*pow(est_load,4)+6.0231E-7*pow(est_load,3)-1.3867E-4*pow(est_load,2)+2.0683E-2*est_load);	//[-] Adjusted pump efficiency
			W_dot_pump = DELTAP_net*m_dot_salt_tot/rho_coolant/eta_pump_adj;
			q_thermal = m_dot_salt_tot*c_p_coolant*(T_salt_hot_guess - T_salt_cold_in);
			q_thermal_ss = m_dot_salt_tot_ss*c_p_coolant*(T_salt_hot_guess - T_salt_cold_in);

			// After convergence, determine whether the mass flow rate falls below the lower limit
			if( q_thermal < m_q_rec_min )
			{
				// GOTO 900
				rec_is_off = true;
			}
		}
		
		if( rec_is_off )
		{
			// 900 continue	// Receiver isn't producing usable energy
			m_dot_salt_tot = 0.0; eta_therm = 0.0; W_dot_pump = 0.0;
			q_conv_sum = 0.0; q_rad_sum = 0.0; m_T_s.fill(0.0); q_thermal = 0.0;
			// Set the receiver outlet temperature equal to the inlet design temperature
			T_salt_hot_guess = m_T_htf_cold_des;
			q_dot_inc_sum = 0.0;
			// Pressure drops
			DELTAP = 0.0; Pres_D = 0.0; u_coolant = 0.0;
			// Set receiver startup energy to 0
			// q_startup = 0.0;
			// ISCC outputs
			m_dot_salt_tot_ss = 0.0; f_rec_timestep = 0.0; q_thermal_ss = 0.0;
		}

		// Set the outputs from this model in sequential order and get out
		value( O_m_dot_salt_tot, m_dot_salt_tot*3600.0 );	//[kg/hr] convert from kg/s
		value( O_eta_therm, eta_therm );					//[-]
		value( O_W_dot_pump, W_dot_pump/1.E6 );				//[MW] convert from W
		value( O_q_conv_sum, q_conv_sum/1.E6 );				//[MW] convert from W
		value( O_q_rad_sum, q_rad_sum/1.E6 );				//[MW] convert from W
		value( O_Q_thermal, q_thermal/1.E6 );				//[MW] convert from W
		value( O_T_salt_hot, T_salt_hot_guess - 273.15 );	//[C] convert from K
		value( O_field_eff_adj, field_eff_adj );			//[-]
		value( O_q_solar_total, q_dot_inc_sum/1.E3 );			//[MW] convert from kW
		value( O_q_startup, q_startup );					//[MW]
		value( O_dP_receiver, DELTAP*m_n_panels/m_n_lines/1.E5 );	//[bar] receiver pressure drop, convert from Pa
		value( O_dP_total, Pres_D*10.0 );					//[bar] total pressure drop, convert from MPa
		value( O_vel_htf, u_coolant );						//[m/s]
		value( O_T_salt_in, T_salt_cold_in - 273.15 );		//[C]
		value( O_M_DOT_SS, m_dot_salt_tot_ss*3600.0 );		//[kg/hr] convert from kg/s
		value( O_Q_DOT_SS, q_thermal_ss/1.E6 );				//[MW] convert from W
		value( O_F_TIMESTEP, f_rec_timestep );				//[-]

		// OUT(8)=OUT(6)+abs(OUT(5)+OUT(4)) !Power before thermal  --- ?

		return 0;
	}

	virtual int converged( double time )
	{
		// Check HTF props?
		//!MJW 9.8.2010 :: Call the property range check subroutine with the inlet and outlet HTF temps to make sure they're in the valid range
		//call check_htf(Coolant,T_salt_hot)
		//call check_htf(Coolant,T_salt_cold)

		if( m_mode == 0 )
		{
			m_E_su = m_q_rec_des * m_rec_qf_delay;
			m_t_su = m_rec_su_delay;
		}

		m_mode_prev = m_mode;
		m_E_su_prev = m_E_su;
		m_t_su_prev = m_t_su;

		m_itermode = 1;
		m_od_control = 1.0;

		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_mw_pt_type222, "External Receiver/Tower", "Ty Neises", 1, sam_mw_pt_type222_variables, NULL, 1 )
