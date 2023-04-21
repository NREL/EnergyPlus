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
#include "htf_props.h"
#include "cavity_calcs.h"
#include "sam_csp_util.h"
//#include <shared/lib_util.h>
#include "lib_util.h"

/* Cavity Solar Central Receiver
Type 232
Authors: Lukas Feierabend & Soenke Teichel
Converted from Fortran to c++ November 2012 by Ty Neises  */

using namespace std;

void TranslateFluxArray( util::matrix_t<double> & fluxarray_2D, int &n_nodes, int &n_panels, util::matrix_t<double> & solarflux );
void PipeFlowCavity( double Re, double Pr, double LoverD, double relRough, double q_solar_total, int is_fd, double & Nusselt, double & f );
void FractionFunction( int n_nodes, int n_panels, int n_band, util::matrix_t<double> & T_s_guess_1D, util::matrix_t<double> & lambda_step_band, 
							util::matrix_t<double> & f_temp_band, util::matrix_t<double> & f_solar_band);

enum {
	P_rec_d_spec,     
	P_h_rec,          
	P_h_lip,          
	P_h_tower,        
	P_rec_angle,      
	P_d_tube_out,     
	P_th_tube,        
	P_eta_pump,       
	P_hel_stow,       
	P_flow_pattern,   
	P_HTF,            
	P_htf_props,      
	P_material,       
	P_hl_ffact,       
	P_T_htf_hot_des,  
	P_T_htf_cold_des, 
	P_f_rec_min,      
	P_q_rec_des,      
	P_rec_su_delay,   
	P_rec_qf_delay,   
	P_conv_model,     
	P_m_dot_htf_max,  
	P_eps_wavelength, 
	P_conv_coupled,   
	P_conv_forced,    
	P_h_wind_meas,    
	P_conv_wind_dir,  
	P_fluxmap_angles, 
	P_fluxmap,        
	
	I_azimuth,        
	I_zenith,         
	I_T_htf_hot,      
	I_T_htf_cold,            
	I_P_amb,                     
	I_T_dp,           
	I_I_bn,           
	I_eta_field,      
	I_T_amb,          
	I_u_wind,         
	I_deg_wind,              

	O_m_htf_total,                 
	O_eta_therm,    
	O_W_pump,       
	O_Q_conv_loss,  
	O_Q_rad_loss,   
	O_Q_thermal,    
	O_T_htf_hot,    
	O_Q_rec_abs,    
	O_field_eff_adj,
	O_Q_solar_total,
	O_Q_startup,    
	O_availability, 
	O_Q_rad_solar,  
	O_Q_rad_therm,  

	N_MAX };

tcsvarinfo sam_lf_st_pt_type232_variables[] = {
	// vartype    datatype   index              name             label                                                          units    meta   group   default_value
	// Parameters 
	{ TCS_PARAM, TCS_NUMBER, P_rec_d_spec,     "rec_d_spec",     "Receiver aperture width",                                     "m",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_h_rec,          "h_rec",          "Height of a receiver panel",                                  "m",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_h_lip,          "h_lip",          "Height of upper lip of cavity",                               "m",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_h_tower,        "h_tower",        "Total height of the solar tower",                             "m",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_rec_angle,      "rec_angle",      "Section of the cavity circle covered in panels",              "deg",   "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_d_tube_out,     "d_tube_out",     "Outer diameter of a single tube",                             "mm",    "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_th_tube,        "th_tube",        "Wall thickness of a single tube",                             "mm",    "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_eta_pump,       "eta_pump",       "Efficiency of HTF pump",                                      "-",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_hel_stow,       "hel_stow",       "Heliostat field stow/deploy solar angle",                     "deg",   "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_flow_pattern,   "flow_pattern",   "HTF flow scheme through receiver panels",                     "-",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_HTF,            "htf",            "Flag indicating heat transfer fluid",                         "-",     "",    "",  "" },
	{ TCS_PARAM, TCS_MATRIX, P_htf_props,      "field_fl_props", "User defined field fluid property data",                      "-",     "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",        "",        ""},
	{ TCS_PARAM, TCS_NUMBER, P_material,       "material",       "Receiver tube material",                                      "-",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_hl_ffact,       "hl_ffact",       "Heat loss factor (thermal loss fudge factor)",                "-",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_htf_hot_des,  "T_htf_hot_des",  "Hot HTF outlet temperature at design",                        "C",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_T_htf_cold_des, "T_htf_cold_des", "Cold HTF outlet temperature at design",                       "C",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_f_rec_min,      "f_rec_min",      "Minimum receiver mass flow rate turndown fraction",           "-",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_q_rec_des,      "q_rec_des",      "Design-point receiver thermal power output",                  "MWt",   "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_rec_su_delay,   "rec_su_delay",   "Fixed startup delay time for the receiver",                   "hr",    "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_rec_qf_delay,   "rec_qf_delay",   "Energy-based receiver startup delay (frac of rated power)",   "-",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_conv_model,     "conv_model",     "Type of convection model (1=Clausing, 2=Siebers/Kraabel)",    "-",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_m_dot_htf_max,  "m_dot_htf_max",  "Maximum receiver mass flow rate",                             "kg/hr", "",    "",  "" },
	{ TCS_PARAM, TCS_MATRIX, P_eps_wavelength, "eps_wavelength", "Matrix containing wavelengths, active & passive surface eps", "-",     "3 columns - band-end wavelength (end of final band should be entered but is assumed infinite), active surface emissivity, passive surface emissivity", "", "" },
	{ TCS_PARAM, TCS_NUMBER, P_conv_coupled,   "conv_coupled",   "1=coupled, 2=uncoupled",                                      "-",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_conv_forced,    "conv_forced",    "1=forced (use wind), 0=natural",                              "-",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_h_wind_meas,    "h_wind_meas",    "Height at which wind measurements are given",                 "m",     "",    "",  "" },
	{ TCS_PARAM, TCS_NUMBER, P_conv_wind_dir,  "conv_wind_dir",  "Wind direction dependent forced convection 1=on 0=off",       "-",     "",    "",  "" },
	{ TCS_PARAM, TCS_MATRIX, P_fluxmap_angles, "fluxmap_angles", "Matrix containing zenith and azimuth angles for flux maps",   "-",     "2 columns - azimuth angle, zenith angle. number of rows must equal number of flux maps provided", "", "" },
	{ TCS_PARAM, TCS_MATRIX, P_fluxmap,        "fluxmap",        "Matrix containing 10x12 flux map for various solar positions","-",     "",    "",  "" },

	{ TCS_INPUT, TCS_NUMBER, I_azimuth,        "azimuth",        "0 at due north, ranges clockwise from 0 to 360",              "deg",   "",    "",  "" },
	{ TCS_INPUT, TCS_NUMBER, I_zenith,         "zenith",         "solar zenith angle",                                          "deg",   "",    "",  "" },
	{ TCS_INPUT, TCS_NUMBER, I_T_htf_hot,      "T_htf_hot",      "Target hot outlet temperature of the working fluid",          "C",     "",    "",  "" },
	{ TCS_INPUT, TCS_NUMBER, I_T_htf_cold,     "T_htf_cold",     "Inlet temperature of the HTF",                                "C",     "",    "",  "" },
	{ TCS_INPUT, TCS_NUMBER, I_P_amb,          "P_amb",          "Ambient pressure",                                            "atm",   "",    "",  "" },
	{ TCS_INPUT, TCS_NUMBER, I_T_dp,           "T_dp",           "Dew point temperature",                                       "C",     "",    "",  "" },
	{ TCS_INPUT, TCS_NUMBER, I_I_bn,           "I_bn",           "Direct normal irradiation",                                   "W/m2",  "",    "",  "" },
	{ TCS_INPUT, TCS_NUMBER, I_eta_field,      "eta_field",      "Overall efficiency of heliostat field",                       "-",     "",    "",  "" },
	{ TCS_INPUT, TCS_NUMBER, I_T_amb,          "T_amb",          "Ambient temperature",                                         "C",     "",    "",  "" },
	{ TCS_INPUT, TCS_NUMBER, I_u_wind,         "u_wind",         "Wind velocity",                                               "m/s",   "",    "",  "" },
	{ TCS_INPUT, TCS_NUMBER, I_deg_wind,       "deg_wind",       "Wind direction",                                              "deg",   "",    "",  "" },

	{ TCS_OUTPUT, TCS_NUMBER, O_m_htf_total,   "m_htf_total",    "Total mass flow rate of the working fluid",                   "kg/hr", "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_eta_therm,     "eta_therm",      "Thermal efficiency of the receiver",                          "-",     "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_W_pump,        "W_pump",         "Estimated power for pumping the working fluid",               "MW",    "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_conv_loss,   "Q_conv_loss",    "Thermal convection losses from the receiver",                 "MW",    "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_rad_loss,    "Q_rad_loss",     "Radiation losses from the receiver",                          "MW",    "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_thermal,     "Q_thermal",      "Thermal energy absorbed by the heat transfer fluid",          "MW",    "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_T_htf_hot,     "T_htf_hot_out",  "Outlet temperature of the heat transfer fluid",               "C",     "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_rec_abs,     "Q_rec_abs",      "Receiver power prior to thermal losses",                      "MW",    "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_field_eff_adj, "field_eff_adj",  "Adjusted heliostat field efficiency - includes defocus",      "-",     "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_solar_total, "Q_solar_total",  "Total incident power on the receiver",                        "MW",    "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_startup,     "Q_startup",      "Startup energy consumed during the current time step",        "MW",    "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_availability,  "availability",   "Availability of the solar tower",                             "hr",    "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_rad_solar,   "Q_rad_solar",    "Solar radiation losses from the receiver",                    "MW",    "",    "",  "" },
	{ TCS_OUTPUT, TCS_NUMBER, O_Q_rad_therm,   "Q_therm_solar",  "Thermal radiation losses from the receiver",                  "MW",    "",    "",  "" },
	
	{ TCS_INVALID, TCS_INVALID,  N_MAX,       0,            0, 0, 0, 0, 0 }
};


class sam_lf_st_pt_type232 : public tcstypeinterface
{
private:
	
	// Class Instances
	HTFProperties rec_htf;
	HTFProperties tube_mat;
	Cavity_Calcs cavity;

	// Constant Data: not adjustable per notes by Lukas and Soenke?
	int m_n_nodes;	
	int m_n_panels;
	int m_n_coils;
	int m_night_recirc;	
	int m_n_45_bends;		
	int m_n_90_bends;		
	double m_L_e_45;		
	double m_L_e_90;		
	double m_eta_thermal_guess;	
	int m_n_rays;

	// Parameters
	double rec_d_spec;
	double h_rec;
	double h_lip;
	double h_tower; 
	double rec_angle; 
	double d_tube_out;   
	double th_tube;      
	double eta_pump;     
	double hel_stow;     
	int flow_pattern; 
	int htf;          
	int material;     
	double hl_ffact;     
	double T_htf_hot_des;
	double T_htf_cold_des;
	double f_rec_min;     
	double q_rec_des;     
	double rec_su_delay;  
	double rec_qf_delay;  
	int conv_model;    
	double m_dot_htf_max; 	
	int n_bands;
	int conv_coupled;
	int conv_forced;   
	double h_wind_meas;    
	int conv_wind_dir;
	util::matrix_t<double> fluxmap_angles;	// matrix for fluxmap solar positions
	util::matrix_t<double> fluxmap;         // matrix for flux values
	int num_sol_pos;

	// Arrays -- must be deleted in destructor 
	double * A_array; 

	// "matrix_t", "block"
	util::matrix_t<double> e_band_array;
	util::matrix_t<double> lambda_step_band;
	util::matrix_t<double> is_fd;
	util::block_t<double> F_hat;

	// Calculated Constants
	double r_rec;	
	double tol_od;	
	double m_dot_htf_des;
	double m_dot_htf_min;
	double A_node;
	double q_solar_critical;
	double A_tube;
	int n_tubes;
	double d_tube_in;
	double L_over_D;
	double L_over_D_p;
	double relRough;
	double L_tube_node;
	double max_eps_active;
	double A_f;
	double A_lip;
	double W_panel;
	double tolerance;

	// "Stored" Variabels
	int mode;
	double E_su_prev;
	double E_su;
	double t_su_prev;
	double t_su;

	int itermode;
	double od_control;

	// "matrix_t" from timestep call
	util::matrix_t<double> flux_array_2D;											
	util::matrix_t<double> solarflux;												
	util::matrix_t<double> q_solar;												
	util::matrix_t<double> q_solar_panel;											
	util::matrix_t<double> T_s;													
	util::matrix_t<double> T_s_guess;												
	util::matrix_t<double> T_htf_guess;											
	util::matrix_t<double> T_htf;													
	util::matrix_t<double> T_htf_ave;												
	util::matrix_t<double> T_htf_ave_guess;                                       
	util::matrix_t<double> m_htf_guess;											
	util::matrix_t<double> T_htf_hot_guess;                                       
	util::matrix_t<double> m_htf;													
	util::matrix_t<double> h_conv;												
	util::matrix_t<double> q_conv;													
	util::matrix_t<double> T_htf_guess_mid;                                       
	util::matrix_t<double> T_s_guess_mid_1D;                                      
	util::matrix_t<double> T_htf_ave_guess_mid;                                   
	util::matrix_t<double> flux_1D;												
	util::matrix_t<double> T_s_1D;												
	util::matrix_t<double> T_s_guess_1D;											
	util::matrix_t<double> h_rad_semi_gray_therm;                                 
	util::matrix_t<double> q_rad_solar;											
	util::matrix_t<double> q_rad_solar_net;                                       	
	util::matrix_t<double> UA_1DIM;												
	util::matrix_t<double> T_htf_ave_1D;											
	util::matrix_t<double> q_htf_1D;												
	util::matrix_t<double> f_temp_band;											
	util::matrix_t<double> f_solar_band;											
	util::matrix_t<double> q_rad_therm;											
	util::matrix_t<double> q_rad_semi_gray;                                       
	util::matrix_t<double> q_rad_therm_net;                                       
	util::matrix_t<double> q_rad_semi_gray_net;                                   
	util::matrix_t<double> q_htf;													
	util::matrix_t<double> q_htf_panel;											
	util::matrix_t<double> error_temp;											
	util::matrix_t<double> error_flow;											
	util::matrix_t<double> deltaP_node;											
	util::matrix_t<double> T_htf_ave_guess_1D;                                    
	util::matrix_t<double> rho_htf_p;												
	util::matrix_t<double> u_htf_p;												
	util::matrix_t<double> f_htf_p;												
	util::matrix_t<double> m_htf_p;												

	
public:
	sam_lf_st_pt_type232( tcscontext *cxt, tcstypeinfo *ti )
		: tcstypeinterface( cxt, ti )
	{
		// Constant hardcoded data
		m_n_nodes = 5;	
		m_n_panels = 4;
		m_n_coils = 6;
		m_night_recirc = 0;				// This mode is currently not working
		m_n_45_bends = 0;					// [-] Number of 45 degree bends in one tube
		m_n_90_bends = 4*m_n_coils;			// [-] Number of 90 degree bends in one tube
		m_L_e_45 = 16.0;					// [m] Equivalent length of a 45 degree bend in the tubing system
		m_L_e_90 = 30.0;					// [m] Equivalent length of a 90 degree bend in the tubing system
		m_eta_thermal_guess = 0.85;		// [-] Guess value for the thermal efficiency of the receiver
		m_n_rays = 300000;				// [-] Number of rays to use in Monte Carlo view factor routine

		// Parameter class data
		rec_d_spec	= std::numeric_limits<double>::quiet_NaN();
		h_rec		= std::numeric_limits<double>::quiet_NaN();
		h_lip		= std::numeric_limits<double>::quiet_NaN();
		h_tower		= std::numeric_limits<double>::quiet_NaN(); 
		rec_angle	= std::numeric_limits<double>::quiet_NaN();    
		d_tube_out	= std::numeric_limits<double>::quiet_NaN();
		th_tube		= std::numeric_limits<double>::quiet_NaN();
		eta_pump	= std::numeric_limits<double>::quiet_NaN();
		hel_stow	= std::numeric_limits<double>::quiet_NaN();
		flow_pattern = -1;
		htf			= -1;
		material	= -1;
		hl_ffact	= std::numeric_limits<double>::quiet_NaN();
		T_htf_hot_des	= std::numeric_limits<double>::quiet_NaN();
		T_htf_cold_des	= std::numeric_limits<double>::quiet_NaN();
		f_rec_min		= std::numeric_limits<double>::quiet_NaN();
		q_rec_des		= std::numeric_limits<double>::quiet_NaN();
		rec_su_delay	= std::numeric_limits<double>::quiet_NaN();
		rec_qf_delay	= std::numeric_limits<double>::quiet_NaN();
		conv_model	= -1;
		m_dot_htf_max	= std::numeric_limits<double>::quiet_NaN();
		n_bands			= -1;
		fluxmap_angles  = 0.0;
		fluxmap         = 0.0;
		num_sol_pos		= -1;
		conv_coupled	= -1;
		conv_forced		= -1;
		h_wind_meas		= std::numeric_limits<double>::quiet_NaN();   
		conv_wind_dir	= -1;

		// Arrays
		A_array = new double[m_n_nodes*m_n_panels+4];

		// matrix_t, block_t
		e_band_array = 0.0;
		lambda_step_band = 0.0;
		is_fd = 0.0;
		F_hat = 0.0;

		// Calculated Constants
		r_rec = std::numeric_limits<double>::quiet_NaN();		
		tol_od = std::numeric_limits<double>::quiet_NaN();		
		m_dot_htf_des = std::numeric_limits<double>::quiet_NaN();
		m_dot_htf_min = std::numeric_limits<double>::quiet_NaN();
		A_node = std::numeric_limits<double>::quiet_NaN();
		q_solar_critical = std::numeric_limits<double>::quiet_NaN();
		A_tube = std::numeric_limits<double>::quiet_NaN();
		n_tubes = -1;
		d_tube_in = std::numeric_limits<double>::quiet_NaN();
		L_over_D = std::numeric_limits<double>::quiet_NaN();
		L_over_D_p = std::numeric_limits<double>::quiet_NaN();
		relRough = std::numeric_limits<double>::quiet_NaN();
		L_tube_node = std::numeric_limits<double>::quiet_NaN();
		max_eps_active = std::numeric_limits<double>::quiet_NaN();
		A_f = std::numeric_limits<double>::quiet_NaN();
		A_lip = std::numeric_limits<double>::quiet_NaN();
		W_panel = std::numeric_limits<double>::quiet_NaN();
		tolerance = std::numeric_limits<double>::quiet_NaN();

		// "Storage" Variables
		mode = -1;
		E_su_prev = std::numeric_limits<double>::quiet_NaN(); 
		E_su = std::numeric_limits<double>::quiet_NaN(); 
		t_su_prev = std::numeric_limits<double>::quiet_NaN();
		t_su = std::numeric_limits<double>::quiet_NaN(); 	

		itermode = -1;
		od_control = std::numeric_limits<double>::quiet_NaN();

	}

	virtual ~sam_lf_st_pt_type232()
	{
		if(A_array) delete [] A_array;
	}

	virtual int init()
	{
		// Set parameter class data 
		rec_d_spec	= value( P_rec_d_spec );
		h_rec		= value( P_h_rec );
		h_lip		= value( P_h_lip );
		h_tower		= value( P_h_tower ); 
		rec_angle	= value( P_rec_angle ) * CSP::pi/180.0;	// Convert from degrees to radians    
		d_tube_out	= value( P_d_tube_out ) / 1000.0;		// Convert from mm to m
		th_tube		= value( P_th_tube ) / 1000.0;			// Convert from mm to m
		eta_pump	= value( P_eta_pump );
		hel_stow	= value( P_hel_stow );
		flow_pattern = (int) value( P_flow_pattern );
		htf			= (int) value( P_HTF );		

		// Declare instance of fluid class for STORAGE fluid.
		// Set fluid number or copy over fluid matrix, depending on specified fluid.
		if( htf != HTFProperties::User_defined )
		{
			if( !rec_htf.SetFluid( htf ) ) // store_fl should match up with the constants
			{
				message(TCS_ERROR, "Receiver HTF code is not recognized");
				return -1;
			}
		}
		else
		{
			int htf_rows = 0, htf_cols = 0;
			double *fl_mat = value( P_htf_props, &htf_rows, &htf_cols );
			if ( fl_mat != 0 && htf_rows > 2 && htf_cols == 7 )
			{
				util::matrix_t<double> mat( htf_rows, htf_cols, 0.0 );
				for (int r=0;r<htf_rows;r++)
					for (int c=0;c<htf_cols;c++)
						mat.at(r,c) = TCS_MATRIX_INDEX( var( P_htf_props ), r, c );

				if ( !rec_htf.SetUserDefinedFluid( mat ) )
				{
					message( TCS_ERROR, "user defined htf property table was invalid (rows=%d cols=%d)", htf_rows, htf_cols );
					return -1;
				}
			}
			else
			{
				message(TCS_ERROR, "The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", htf_rows, htf_cols);
				return -1;
			}
		}
		//********************************************************************************
		//********************************************************************************
		
		// Set HTF class for tubing material
		material		= (int) value( P_material );
		if( material != HTFProperties::User_defined )
		{
			tube_mat.SetFluid( material );
		}
		else
		{
			message(TCS_ERROR,  "user defined properties are not an option for tubing material. Select an available material number." );
			return -1;
		}
		//********************************************************************************
		//********************************************************************************

		// Continue setting parameter class data
		hl_ffact		= value( P_hl_ffact );
		T_htf_hot_des	= value( P_T_htf_hot_des )+273.15;	// Convert from C to K
		T_htf_cold_des	= value( P_T_htf_cold_des )+273.15;	// Convert from C to K
		f_rec_min		= value( P_f_rec_min );
		q_rec_des		= value( P_q_rec_des )*1.E6;			// Convert MWt to Wt
		rec_su_delay	= value( P_rec_su_delay );
		rec_qf_delay	= value( P_rec_qf_delay );
		conv_model		= (int) value( P_conv_model );
		m_dot_htf_max	= value( P_m_dot_htf_max ) / 3600.0;	// Convert from kg/hr to hg/s
		conv_coupled	= (int) value( P_conv_coupled );		// 1=coupled, 2=uncoupled
		conv_forced		= (int) value( P_conv_forced );
		h_wind_meas		= value( P_h_wind_meas );   
		conv_wind_dir	= (int) value( P_conv_wind_dir );
		
		// Set up matrix for epsilon - wavelength values. 3 columns: upper wavelength, active surface emissivity, passive surface emissivity
		int eps_rows = 0, eps_cols = 0;
		double *p_eps = value( P_eps_wavelength, &eps_rows, &eps_cols );
		util::matrix_t<double> eps_wavelength( eps_rows, eps_cols );
		if( p_eps != 0 && eps_rows > 0 && eps_cols == 3 )
		{
			for (int r=0; r<eps_rows; r++)
				for (int c=0; c<eps_cols; c++)
					eps_wavelength.at(r,c) = TCS_MATRIX_INDEX( var( P_eps_wavelength ), r, c );
		}
		//**********************************************************************

		n_bands = eps_rows;		// Set number of bands as class data
		// Find maximum absorptivity to use in initial calculations
		max_eps_active = 0.0;
		for( int i = 0; i < eps_rows; i++ )
			max_eps_active = max( max_eps_active, eps_wavelength.at(i,1) );

		// Create n_bands x 1 matrix containing the wavelength step for multi-band irradiance. The (n_band-1,0) value is not used.
		lambda_step_band.resize( n_bands, 1 );
		for( int l = 0; l < n_bands; l++ )
			lambda_step_band.at(l,0) = eps_wavelength.at(l,0);

		//**************************************************************************
		// Set up matrix for solar positions of flux map. 2 rows: azimuth, zenith -- a column for each solar position
		int angle_rows = 0, angle_cols = 0;
		double *p_angle = value( P_fluxmap_angles, &angle_rows, &angle_cols );
		fluxmap_angles.resize( angle_rows, angle_cols );
		if( p_angle != 0 && angle_rows == 2 && angle_cols > 3 )
		{
			for( int r = 0; r < angle_rows; r++ )
				for( int c = 0; c < angle_cols; c++ )
					fluxmap_angles.at(r,c) = TCS_MATRIX_INDEX( var( P_fluxmap_angles ), r, c );
		}
		else
		{
			message(TCS_ERROR,  "Flux map solar position input is incorrect P_fluxmap_angles: %d x %d", angle_rows, angle_cols );
			return -1;
		}
		num_sol_pos = angle_cols;
		// ****************************************************************************
		

		// ********************************************************************************
		// Set up matrix for solar flux. number of rows = num_sol_pos = number of solar positions, number of columns - 120 (10 height nodes x 12 circumferential nodes)
		int flux_rows = 0, flux_cols = 0;
		double *p_flux = value( P_fluxmap, &flux_rows, &flux_cols );
		if( flux_rows != num_sol_pos )
		{
			message(TCS_ERROR,  "Number of flux maps is not equal to number of solar positions" );
			return -1;
		}
		fluxmap.resize( flux_rows, flux_cols );
		if( p_flux != 0 && flux_cols == 120 )
		{
			for( int r = 0; r < flux_rows; r++ )
				for( int c = 0; c < flux_cols; c++ )
					fluxmap.at(r,c) = TCS_MATRIX_INDEX( var( P_fluxmap ), r, c );
		}
		else
		{
			message(TCS_ERROR,  "Flux map input is incorrect" );
			return -1;
		}
		// ********************************************************************************

		// Initial Calculations
		r_rec = rec_d_spec/2.0;			// [m]
		//double dt = time_step();		// [s]

		itermode = 1;		// 1 solve for design temp, 2 solve to match mass flow restriction
		tol_od	= .001;		// tolerance for over-design iteration
		od_control = 1.0;	// additional defocusing control for over-design conditions
		
		// Calculate the reference HTF mass flow rate, and the max and min values
		double c_htf_des = rec_htf.Cp( (T_htf_hot_des + T_htf_cold_des)/2.0 )*1000.0;	// [J/kg-K] Specific heat at design conditions		
		m_dot_htf_des = q_rec_des/(c_htf_des * (T_htf_hot_des - T_htf_cold_des));		// [kg/s]
		m_dot_htf_min = m_dot_htf_des * f_rec_min;										// [kg/s]

		// Critical level of incident irradiance under which the receiver is assumed off
		q_solar_critical = q_rec_des/m_eta_thermal_guess/max_eps_active*f_rec_min;	// [W]	

		// Set "storage" variables
		E_su_prev = q_rec_des * rec_qf_delay;	// [W-hr]  Startup energy E_su/E_su0
		t_su_prev = rec_su_delay;				// [hr] Startup time requirement t_su/t_su0
		
		// CALL THE VIEW FACTOR ROUTINES FOR CALCULATING VIEW FACTORS BETWEEN CAVITY SURFACES				
		cavity.Define_Cavity( m_n_rays, h_rec, r_rec, rec_angle, h_lip );

		double * F_AF = new double[m_n_nodes];
		double * F_BF = new double[m_n_nodes];
		double F_LCE = 0.0;
		double F_LF = 0.0;
		double F_OCE = 0.0;
		double F_OF = 0.0;
		double F_FCE = 0.0;
		cavity.OuterPanel_Floor( F_AF );
		cavity.InnerPanel_Floor( F_BF );
		cavity.Lip_Ceiling( F_LCE );
		cavity.Lip_Floor( F_LF );
		cavity.Opening_Ceiling( F_OCE );
		cavity.Opening_Floor( F_OF );
		
		util::matrix_t<double> F_AA( cavity.m_n_nodes, cavity.m_n_nodes, 0.0 );
		util::matrix_t<double> F_AB( cavity.m_n_nodes, cavity.m_n_nodes, 0.0 );
		util::matrix_t<double> F_AC( cavity.m_n_nodes, cavity.m_n_nodes, 0.0 );
		util::matrix_t<double> F_AD( cavity.m_n_nodes, cavity.m_n_nodes, 0.0 );
		
		double * F_AO = new double[m_n_nodes];
		double * F_AL = new double[m_n_nodes];
		double * F_BO = new double[m_n_nodes];
		double * F_BL = new double[m_n_nodes];

		cavity.PanelViewFactors( F_AB, F_AC, F_AD, F_AO, F_AL, F_BO, F_BL );
		
		// Get panel geometry from cavity class
		double h_node, alpha, W_aperture, z;
		cavity.GetGeometry( h_node, alpha, W_panel, W_aperture, z );

		A_node = W_panel*h_node;									// [m2] Area of a panel node
		A_f = 2.0*W_panel*r_rec*cos(alpha/2.0)+z*W_aperture;		// [m2] Floor surface area
		double A_ce = A_f;											// [m2] Ceiling surface area
		A_lip = h_lip*W_aperture;									// [m2] Lip surface area
		double A_o = (h_rec-h_lip)*W_aperture;						// [m2] Aperture surface area
		
		// Dimensions of the HTF tubes
		n_tubes = (int) (h_rec/(2.0*double(m_n_coils)*d_tube_out));	// [-] Number of tubes in each panel
		d_tube_in = d_tube_out - 2.0*th_tube;						// [m] Inner diameter of each receiver tube [m]
		A_tube = 0.25*CSP::pi*pow(d_tube_in,2);						// [m2] Cross-sectional area of one tube [m2]
		double L_tube = 2.0*m_n_coils*W_panel;		// [m] Entire tube length over the whole panel height [m]
		L_tube_node = L_tube/double(m_n_nodes);		// [m] Tube length in one panel node [m]
		L_over_D = L_tube_node/d_tube_in;			// [-] Ratio of tube length per node over the tube diameter for the pipe flow calculations
		L_over_D_p = L_tube/d_tube_in;				// [-] Ratio of tube length per panel over the tube diameter
		relRough = (4.5e-5)/d_tube_in;				// [-] Relative roughness of the tubes. http:www.efunda.com/formulae/fluids/roughness.cfm

		// Complete remaining view factors
		for( int j = 1; j < m_n_nodes; j++ )
		{
			for( int i = 0; i <= j; i++ )
			{
				F_AB.at(i,j)	= F_AB.at(j-i,0);
				F_AC.at(i,j)	= F_AC.at(j-i,0);
				F_AD.at(i,j)	= F_AD.at(j-i,0);
			}
		}

		for( int j = 1; j < m_n_nodes; j++ )
		{
			for( int i = j+1; i < m_n_nodes; i++ )
			{
				F_AB.at(i,j)	= F_AB.at(i-j,0);
				F_AC.at(i,j)	= F_AC.at(i-j,0);
				F_AD.at(i,j)	= F_AD.at(i-j,0);
			}
		}
		// **********************************


		double sum_AF = 0.0, sum_BF = 0.0;
		for( int i = 0; i < m_n_nodes; i++ )
		{
			sum_AF += F_AF[i];
			sum_BF += F_BF[i];
		}			
		F_FCE = 1.0 - ((sum_AF + sum_BF)*2.0*A_node + F_LF*A_lip + F_OF*A_o)/A_f;		
		
		util::matrix_t<double> F_L( m_n_nodes, m_n_panels, 0.0 );
		util::matrix_t<double> F_O( m_n_nodes, m_n_panels, 0.0 );
		util::matrix_t<double> F_F( m_n_nodes, m_n_panels, 0.0 );
		for( int i = 0; i < m_n_nodes; i++ )
		{
			F_L.at( i, 0 ) = F_AL[i];
			F_L.at( i, 1 ) = F_BL[i];
			F_L.at( i, 2 ) = F_BL[i];
			F_L.at( i, 3 ) = F_AL[i];

			F_O.at( i, 0 ) = F_AO[i];
			F_O.at( i, 1 ) = F_BO[i];
			F_O.at( i, 2 ) = F_BO[i];
			F_O.at( i, 3 ) = F_AO[i];

			F_F.at( i, 0 ) = F_AF[i];
			F_F.at( i, 1 ) = F_BF[i];
			F_F.at( i, 2 ) = F_BF[i];
			F_F.at( i, 3 ) = F_AF[i];
		}
			
		// ST: F_hat parameters are calculated according to "Heat Transfer; Nellis,Klein; Sec:(10.5.4) Eqn:(10-100)  

		/* For the calculation of the F_hat parameters, it is most convinient to organize the view factors in an two dimensional array.
		!Each row represents on of the isothermal surface segments of the cavity. 
		!(1,N_nodes) = Panel A ; (N_nodes+1,2*N_nodes) = Panel B ; (2*N_nodes+1,3*N_nodes) = Panel C ; (3*N_nodes+1,4*N_nodes) = Panel D ; 
		!(4*N_nodes+1) = Floor ; (4*N_nodes+2) = Ceiling ; (4*N_nodes+3) = Lip ; (4*N_nodes+4) = Opening  */

		//(N_nodes*N_panels+4,N_nodes*N_panels+4)
		util::matrix_t<double> F_view( m_n_nodes*m_n_panels+4, m_n_nodes*m_n_panels+4, 0.0 );

		// 
		for( int i = 0; i < m_n_nodes; i++ )
		{
			for( int j = 0; j < m_n_nodes; j++ )
			{
				// viewfactors surfaces of different panels
				F_view.at(i,j+m_n_nodes)   = F_AB.at(i,j);			
				F_view.at(i,j+2*m_n_nodes) = F_AC.at(i,j);				
				F_view.at(i,j+3*m_n_nodes) = F_AD.at(i,j);

				// relations due to symmetry
				F_view.at(i+m_n_nodes,j+2*m_n_nodes)	= F_view.at(i,j+m_n_nodes);	// F_B_C[i,j] = F_AB[i,j]
				F_view.at(i+2*m_n_nodes,j+3*m_n_nodes)	= F_view.at(i,j+m_n_nodes);	// F_C_D[i,j] = F_AB[i,j]
				F_view.at(i+m_n_nodes,j+3*m_n_nodes)	= F_view.at(i,j+2*m_n_nodes); // F_B_D[i,j] = F_A_C[i,j]
				
				// viewfactors surfaces of the same panel
				F_view.at(i,j)	= 0.0;						// F_A_A[1,1]
				F_view.at(i+m_n_nodes,j+m_n_nodes) = 0.0;		// F_A_A[1,1]
				F_view.at(i+2*m_n_nodes,j+2*m_n_nodes) = 0.0;	// F_A_A[1,1]
				F_view.at(i+3*m_n_nodes,j+3*m_n_nodes) = 0.0;	// F_A_A[1,1]
			}
		}

		// viewfactors surfaces of the same panel
		for( int i = 4*m_n_nodes; i < 4*m_n_nodes+4; i++ )
			F_view.at(i,i) = 0.0;

		// viewfactors panel surfaces to passive surfaces(Gloor,Ceiling,Lip,Opening)
		for( int i = 0; i < m_n_nodes; i++ )
		{
			F_view.at(i,m_n_panels*m_n_nodes) = F_AF[i];				// F_AF[i,1]
			F_view.at(i,m_n_panels*m_n_nodes+1) = F_AF[m_n_nodes-1-i];	// F_A_Ce[i,1]
			F_view.at(i,m_n_panels*m_n_nodes+2) = F_L.at(i,0);				// F_A_L[i]
			F_view.at(i,m_n_panels*m_n_nodes+3) = F_O.at(i,0);				// F_A_O[i]
		
			F_view.at(i+m_n_nodes,m_n_panels*m_n_nodes) = F_BF[i];				// F_BF[i,0]
			F_view.at(i+m_n_nodes,m_n_panels*m_n_nodes+1) = F_BF[m_n_nodes-1-i];	// F_B_CE[i,0]
			F_view.at(i+m_n_nodes,m_n_panels*m_n_nodes+2) = F_L.at(i,1);				// F_B_L[i]
			F_view.at(i+m_n_nodes,m_n_panels*m_n_nodes+3) = F_O.at(i,1);				// F_B_O[i]

			F_view.at(i+2*m_n_nodes,m_n_panels*m_n_nodes)	  = F_BF[i];		     // F_AF[i,1]
			F_view.at(i+2*m_n_nodes,m_n_panels*m_n_nodes+1) = F_BF[m_n_nodes-1-i];   // F_A_CE[i,1]
			F_view.at(i+2*m_n_nodes,m_n_panels*m_n_nodes+2) = F_L.at(i,2);            // F_C_L[i]
			F_view.at(i+2*m_n_nodes,m_n_panels*m_n_nodes+3) = F_O.at(i,2);            // F_C_O[i]

			F_view.at(i+3*m_n_nodes,m_n_panels*m_n_nodes)    = F_AF[i];             // F_AF[i,1]
			F_view.at(i+3*m_n_nodes,m_n_panels*m_n_nodes+1)  = F_AF[m_n_nodes-1-i];   // F_A_CE[i,1]
			F_view.at(i+3*m_n_nodes,m_n_panels*m_n_nodes+2)  = F_L.at(i,3);            // F_D_L[i]
			F_view.at(i+3*m_n_nodes,m_n_panels*m_n_nodes+3)  = F_O.at(i,3);            // F_D_O[i]
		}													

		// Reciprocity is used to find the corresponding view factors.
		for( int i = 0; i < m_n_panels*m_n_nodes; i++ )
		{
			A_array[i] = A_node;
		}

		A_array[m_n_panels*m_n_nodes]   = A_f;	// Floor surface area
		A_array[m_n_panels*m_n_nodes+1] = A_ce; // Ceiling surface area

		A_array[m_n_panels*m_n_nodes+2] = A_lip; // Lip surface area
		A_array[m_n_panels*m_n_nodes+3] = A_o;   // Opening surface area

		for( int i = 0; i < m_n_panels*m_n_nodes+4; i++ )
		{
			for( int j = 0; j < m_n_panels*m_n_nodes+4; j++ )
			{
				F_view.at(j,i) = (A_array[i]*F_view.at(i,j))/A_array[j];	// Reciprocity
			}
		}

		F_view.at(4*m_n_nodes+2, 4*m_n_nodes) = F_LF;	// F_L_F
		F_view.at(4*m_n_nodes, 4*m_n_nodes+2) = (A_array[4*m_n_nodes+2]*F_view.at(4*m_n_nodes+2, 4*m_n_nodes))/A_array[4*m_n_nodes];

		F_view.at(4*m_n_nodes+3, 4*m_n_nodes) = F_OF;	// F_O_F
		F_view.at(4*m_n_nodes, 4*m_n_nodes+3) = (A_array[4*m_n_nodes+3]*F_view.at(4*m_n_nodes+3, 4*m_n_nodes))/A_array[4*m_n_nodes];

		F_view.at(4*m_n_nodes+2, 4*m_n_nodes+1) = F_LCE;	// F_L_CE
		F_view.at(4*m_n_nodes+1, 4*m_n_nodes+2) = (A_array[4*m_n_nodes+2]*F_view.at(4*m_n_nodes+2, 4*m_n_nodes+1))/A_array[4*m_n_nodes+1];

		F_view.at(4*m_n_nodes+3, 4*m_n_nodes+1) = F_OCE;	// F_O_CE
		F_view.at(4*m_n_nodes+1, 4*m_n_nodes+3) = (A_array[4*m_n_nodes+3]*F_view.at(4*m_n_nodes+3, 4*m_n_nodes+1))/A_array[4*m_n_nodes+1];

		F_view.at(4*m_n_nodes, 4*m_n_nodes+1) = F_FCE;		// F_F_CE
		F_view.at(4*m_n_nodes+1, 4*m_n_nodes) = (A_array[4*m_n_nodes]*F_view.at(4*m_n_nodes, 4*m_n_nodes+1))/A_array[4*m_n_nodes+1];

		F_view.at(4*m_n_nodes+2, 4*m_n_nodes+3) = 0.0;		// F_L_O
		F_view.at(4*m_n_nodes+3, 4*m_n_nodes+2) = 0.0;		// F_O_L

		// *****************************************
		// ****** Multiband calculation ************
		// *****************************************

		e_band_array.resize( m_n_nodes*m_n_panels+4, n_bands );

		// Set emissivity for passive and active surfaces at each wavelength
		for( int j = 0; j < eps_rows; j++ )
		{
			for( int i = 0; i < m_n_panels*m_n_nodes; i++ )
			{
				e_band_array.at( i, j ) = eps_wavelength.at( j, 1 );
			}
			for( int i = m_n_panels*m_n_nodes; i < m_n_panels*m_n_nodes + 3; i++)
			{
				e_band_array.at( i, j ) = eps_wavelength.at( j, 2 );
			}
			e_band_array.at( m_n_panels*m_n_nodes+3, j ) = 1.0;		// Cavity opening is not reflective
		}
		// *******************************************************************

		// *******************************************************************
		// Set F hat values
		F_hat.resize( m_n_panels*m_n_nodes+4, m_n_panels*m_n_nodes+4, n_bands );
		util::block_t<double> F_hat_guess( m_n_panels*m_n_nodes+4, m_n_panels*m_n_nodes+4, eps_rows );
		util::matrix_t<double> error_array( m_n_panels*m_n_nodes+4, m_n_panels*m_n_nodes+4 );
		for( int k = 0; k < eps_rows; k++ )
		{
			for( int i = 0; i < m_n_panels*m_n_nodes+4; i++ )
			{
				for( int j = 0; j < m_n_panels*m_n_nodes+4; j++ )
				{
					F_hat.at( i, j, k ) = F_view.at( i, j );
				}
			}

			//Initial error
			double err_f_hat = 9999.0;
			int iter_f_hat = 0;

			while( err_f_hat > 1.E-30 && iter_f_hat < 100 )
			{
				for( int i = 0; i < m_n_panels*m_n_nodes+4; i++ )
				{
					for( int j = 0; j < m_n_panels*m_n_nodes; j++ )
					{
						double sum = 0.0;
						for( int l = 0; l < m_n_panels*m_n_nodes + 4; l++ )
						{
							sum		= (1.0 - e_band_array.at( l, k ))*F_view.at( i, l )*F_hat.at( l, j, k );
						}
						F_hat_guess.at( i, j, k )	= F_view.at( i, j ) + sum;
						error_array.at( i, j )		= fabs( F_hat.at( i, j, k) - F_hat_guess.at( i, j, k) );
						F_hat.at( i, j, k )			= F_hat_guess.at( i, j, k );
					}
				}
				for( int i = 0; i < m_n_panels*m_n_nodes+4; i++ )
				{
					err_f_hat = 0.0;
					for( int j = 0; j < m_n_panels*m_n_nodes+4; j++ )
					{
						err_f_hat = max( err_f_hat, error_array.at( i, j ));
					}
				}
			}
		}
		// **************************************************************************

		if( m_night_recirc )
			tolerance = 5.0E-4;
		else
			tolerance = 1.0E-8;

		is_fd.resize( m_n_nodes, m_n_panels );
		for(int i = 0; i < m_n_nodes; i++)
		{
			for(int j = 0; j < m_n_panels; j++)
			{
				switch(flow_pattern)
				{
				case 1:
					if( i == 0 )
						is_fd.at(i,j) = 1;
					else
						is_fd.at(i,j) = 0;
					break;

				case 2:
					if( i == m_n_nodes )
						is_fd.at(i,j) = 1;
					else
						is_fd.at(i,j) = 0;
					break;

				case 3:
					if( (i == m_n_nodes - 1 && j%2 == 1) || (i == 0 && (j+1)%2 == 1) )
						is_fd.at(i,j) = 1;
					else 
						is_fd.at(i,j) = 0;
					break;

				case 4:
					if( (i == 0 && j%2 == 1) || (i == m_n_nodes - 1 && (j+1)%2 == 1) )
						is_fd.at(i,j) = 1;
					else
						is_fd.at(i,j) = 0;
					break;

				case 5:
					if( (i == 0 && (j == 0||j==3)) || (i == m_n_nodes - 1 && (j == 1||j==2)) )
						is_fd.at(i,j) = 1;
					else
						is_fd.at(i,j) = 0;
					break;

				case 6:
					if( j < m_n_panels/2 )
					{
						if( i==0 && j%2==1 )
							is_fd.at(i,j) = 1;
						else if( i==m_n_nodes-1 && j%2==0 )
							is_fd.at(i,j) = 1;
						else
							is_fd.at(i,j) = 0;
					}
					else
					{
						if( i==0 && (m_n_panels-j-1)%2==1 )
							is_fd.at(i,j) = 1;
						else if( i==m_n_nodes-1 && (m_n_panels-j-1)%2==0 )
							is_fd.at(i,j) = 1;
						else
							is_fd.at(i,j) = 0;
					}
					break;

				case 7:
					if( (i == 0 && (j == 0||j==3)) || (i == m_n_nodes - 1 && (j == 1||j==2)) )
						is_fd.at(i,j) = 1;
					else
						is_fd.at(i,j) = 0;
					break;

				case 8:
					if( (i == 0 && (j == 1||j==2)) || (i == m_n_nodes - 1 && (j == 0||j==3)) )
						is_fd.at(i,j) = 1;
					else
						is_fd.at(i,j) = 0;
					break;

				}																								
			}
		}

		delete[] F_AF;
		delete[] F_BF;
		delete[] F_AO;
		delete[] F_AL;
		delete[] F_BO;
		delete[] F_BL;

		// 'matrix_t' from timestep 
		flux_array_2D.resize_fill( 10, 12, 0.0 );
		solarflux.resize_fill( m_n_nodes, m_n_panels, 0.0 );		
		q_solar.resize_fill( m_n_nodes, m_n_panels, 0.0 );
		q_solar_panel.resize_fill( m_n_panels, 1, 0.0 );
		T_s.resize_fill( m_n_nodes, m_n_panels, 0.0);
		T_s_guess.resize_fill( m_n_nodes, m_n_panels, 0.0);		
		T_htf_guess.resize_fill( m_n_nodes+1, m_n_panels, 0.0 );
		T_htf.resize_fill( m_n_nodes+1, m_n_panels, 0.0 );
		T_htf_ave.resize_fill( m_n_nodes, m_n_panels, 0.0 );				
		T_htf_ave_guess.resize_fill( m_n_nodes, m_n_panels, 0.0 );		
		m_htf_guess.resize_fill( m_n_panels, 1, 0.0 );		
		T_htf_hot_guess.resize_fill( m_n_panels, 1, 0.0 );
		m_htf.resize_fill( m_n_panels, 1, 0.0 );
		h_conv.resize_fill( m_n_nodes*m_n_panels+4, 1, 0.0 );
		q_conv.resize_fill( m_n_nodes*m_n_panels+4, 1, 0.0 );						
		T_htf_guess_mid.resize_fill( m_n_nodes+1, m_n_panels, 0.0 );
		T_s_guess_mid_1D.resize_fill( m_n_nodes*m_n_panels+4, 1, 0.0 );		 
		T_htf_ave_guess_mid.resize_fill( m_n_nodes, m_n_panels, 0.0);  
		flux_1D.resize_fill( m_n_nodes*m_n_panels + 4, 1, 0.0 );
		T_s_1D.resize_fill( m_n_nodes*m_n_panels + 4, 1, 0.0 );
		T_s_guess_1D.resize_fill( m_n_nodes*m_n_panels + 4, 1, 0.0 );
		h_rad_semi_gray_therm.resize_fill( m_n_nodes*m_n_panels+4, m_n_nodes*m_n_panels+4, 0.0 );
		q_rad_solar.resize_fill( m_n_nodes*m_n_panels+4, m_n_nodes*m_n_panels+4, 0.0 );
		q_rad_solar_net.resize_fill( m_n_nodes*m_n_panels+4, 1, 0.0 );						
		UA_1DIM.resize_fill( m_n_nodes*m_n_panels + 4, 1, 0.0 );		
		T_htf_ave_1D.resize_fill( m_n_nodes*m_n_panels + 4, 1, 0.0 );
		q_htf_1D.resize_fill( m_n_nodes*m_n_panels+4, 1, 0.0 );
		f_temp_band.resize_fill( m_n_nodes*m_n_panels+4, n_bands, 0.0 );
		f_solar_band.resize_fill( n_bands, 1, 0.0 );
		q_rad_therm.resize_fill( m_n_nodes*m_n_panels+4, m_n_nodes*m_n_panels+4, 0.0 );
		q_rad_semi_gray.resize_fill( m_n_nodes*m_n_panels+4, m_n_nodes*m_n_panels+4, 0.0 );
		q_rad_therm_net.resize_fill( m_n_nodes*m_n_panels+4, 1, 0.0 );
		q_rad_semi_gray_net.resize_fill( m_n_nodes*m_n_panels+4, 1, 0.0 );
		q_htf.resize_fill( m_n_nodes, m_n_panels, 0.0 );
		q_htf_panel.resize_fill( m_n_panels, 1, 0.0 );
		error_temp.resize_fill( m_n_panels, 1, 0.0 );
		error_flow.resize_fill( m_n_panels, 1, 0.0 );
		deltaP_node.resize_fill( m_n_nodes, m_n_panels, 0.0 );
		T_htf_ave_guess_1D.resize_fill( m_n_nodes*m_n_panels+4, 1, 0.0 );
		rho_htf_p.resize_fill( m_n_panels, 1, 0.0 );
		u_htf_p.resize_fill( m_n_panels, 1, 0.0 );
		f_htf_p.resize_fill( m_n_panels, 1, 0.0 );
		m_htf_p.resize_fill( m_n_panels, 1, 0.0 );		

		return 0;
	}

	virtual int call( double time, double step, int /*ncall*/ )
	{ 
		double azimuth		= value( I_azimuth );
		double zenith		= value( I_zenith ); 
		double T_htf_hot	= value( I_T_htf_hot )+ 273.15;		// [K] Convert from C
		double T_htf_cold	= value( I_T_htf_cold )+ 273.15;	// [K] Convert from C
		
		// 8.7.15 twn: hardcode coolant pressure - hard to see a case where this would be an input from a different model...
		double P_htf		= 1.0*1.E5;							// [Pa] Convert from bar
		
		double P_amb		= value( I_P_amb )*100.0;			// [Pa] Convert from atm
		double hour			= time/3600.0;
		double T_dp			= value( I_T_dp )+273.15;			// [K] Convert from C      
		double I_bn			= value( I_I_bn );					// [W/m2]
		double eta_field	= value( I_eta_field );
		double T_amb		= value( I_T_amb )+273.15;			// [K] Convert from C					
		//double u_wind		= value( I_u_wind );
		//double deg_wind		= value( I_deg_wind );
		
		double T_sky = CSP::skytemp( T_amb, T_dp, hour );

		

		// Variables set in code		
		/*
		util::matrix_t<double> flux_array_2D( 10, 12, 0.0 );
		util::matrix_t<double> solarflux( m_n_nodes, m_n_panels, 0.0 );		
		util::matrix_t<double> q_solar( m_n_nodes, m_n_panels, 0.0 );
		util::matrix_t<double> q_solar_panel( m_n_panels, 1, 0.0 );
		util::matrix_t<double> T_s( m_n_nodes, m_n_panels, 0.0);
		util::matrix_t<double> T_s_guess( m_n_nodes, m_n_panels, 0.0);		
		util::matrix_t<double> T_htf_guess( m_n_nodes+1, m_n_panels, 0.0 );
		util::matrix_t<double> T_htf( m_n_nodes+1, m_n_panels, 0.0 );
		util::matrix_t<double> T_htf_ave( m_n_nodes, m_n_panels, 0.0 );				
		util::matrix_t<double> T_htf_ave_guess( m_n_nodes, m_n_panels, 0.0 );		
		util::matrix_t<double> m_htf_guess( m_n_panels, 1, 0.0 );		
		util::matrix_t<double> T_htf_hot_guess( m_n_panels, 1, 0.0 );
		util::matrix_t<double> m_htf( m_n_panels, 1, 0.0 );
		util::matrix_t<double> h_conv( m_n_nodes*m_n_panels+4, 1, 0.0 );
		util::matrix_t<double> q_conv( m_n_nodes*m_n_panels+4, 1, 0.0 );							
		util::matrix_t<double> T_htf_guess_mid( m_n_nodes+1, m_n_panels, 0.0 );
		util::matrix_t<double> T_s_guess_mid_1D( m_n_nodes*m_n_panels+4, 1, 0.0 );		 
		util::matrix_t<double> T_htf_ave_guess_mid( m_n_nodes, m_n_panels, 0.0);  
		util::matrix_t<double> flux_1D( m_n_nodes*m_n_panels + 4, 1, 0.0 );
		util::matrix_t<double> T_s_1D( m_n_nodes*m_n_panels + 4, 1, 0.0 );
		util::matrix_t<double> T_s_guess_1D( m_n_nodes*m_n_panels + 4, 1, 0.0 );
		util::matrix_t<double> h_rad_semi_gray_therm( m_n_nodes*m_n_panels+4, m_n_nodes*m_n_panels+4, 0.0 );
		util::matrix_t<double> q_rad_solar( m_n_nodes*m_n_panels+4, m_n_nodes*m_n_panels+4, 0.0 );
		util::matrix_t<double> q_rad_solar_net( m_n_nodes*m_n_panels+4, 1, 0.0 );								
		util::matrix_t<double> UA_1DIM( m_n_nodes*m_n_panels + 4, 1, 0.0 );		
		util::matrix_t<double> T_htf_ave_1D( m_n_nodes*m_n_panels + 4, 1, 0.0 );
		util::matrix_t<double> q_htf_1D( m_n_nodes*m_n_panels+4, 1, 0.0 );
		util::matrix_t<double> f_temp_band( m_n_nodes*m_n_panels+4, n_bands, 0.0 );
		util::matrix_t<double> f_solar_band( n_bands, 1, 0.0 );
		util::matrix_t<double> q_rad_therm( m_n_nodes*m_n_panels+4, m_n_nodes*m_n_panels+4, 0.0 );
		util::matrix_t<double> q_rad_semi_gray( m_n_nodes*m_n_panels+4, m_n_nodes*m_n_panels+4, 0.0 );
		util::matrix_t<double> q_rad_therm_net( m_n_nodes*m_n_panels+4, 1, 0.0 );
		util::matrix_t<double> q_rad_semi_gray_net( m_n_nodes*m_n_panels+4, 1, 0.0 );
		util::matrix_t<double> q_htf( m_n_nodes, m_n_panels, 0.0 );
		util::matrix_t<double> q_htf_panel( m_n_panels, 1, 0.0 );
		util::matrix_t<double> error_temp( m_n_panels, 1, 0.0 );
		util::matrix_t<double> error_flow( m_n_panels, 1, 0.0 );
		util::matrix_t<double> deltaP_node( m_n_nodes, m_n_panels, 0.0 );
		util::matrix_t<double> T_htf_ave_guess_1D( m_n_nodes*m_n_panels+4, 1, 0.0 );
		util::matrix_t<double> rho_htf_p( m_n_panels, 1, 0.0 );
		util::matrix_t<double> u_htf_p( m_n_panels, 1, 0.0 );
		util::matrix_t<double> f_htf_p( m_n_panels, 1, 0.0 );
		util::matrix_t<double> m_htf_p( m_n_panels, 1, 0.0 );	*/	


		double err_coupled_conv, q_convection_guess, q_convection;
		int iter_coupled_conv;
		double deltaT_htfX;
		double err_defocus, field_eff_adj;
		double q_solar_total, T_O, T_F_guess, T_CE_guess, T_L_guess, T_F, T_CE, T_L;
		double T_htf_average, rho_htf, k_htf, mu_htf, c_htf, Pr_htf;
		double gamma;
		int gamma_count;
		double gamma_calc_array[7] = {1, 0.8, 0.6, 0.4, 0.2, 0.1, 0.05};												
		double q_losses_therm_guess, q_losses_rad_guess, q_outX, h_FX, h_avgX, h_stagX, T_stagX;
		int SX;
		double T_bulk;		
		double q_htf_total;								
		double errorsum_temp, errorsum_flow;
		bool cycle_out_loop;
		double Q_radiation_loss, Q_radiation_loss_solar, Q_radiation_loss_therm, Q_radiation_loss_semi_sum;
		double T_s_ave, A_cavity, h_clausing1983, h_clausing1987;
		double q_convection_Clausing1983, q_convection_Clausing1987, h_F, h_ave, h_stag, T_stag;
		int S;
		double h_SK_forced;
		double m_htf_total, err_od;
		bool defocus_rec;
		double q_startup, eta_thermal;		
		double deltaP_ave;														

		mode = -1;		// Mode should be set every time
		E_su = std::numeric_limits<double>::quiet_NaN(); 
		t_su = std::numeric_limits<double>::quiet_NaN();  

		// do an initial check to make sure the solar position called is valid.
		// if its not, return the output equal to zeros. Also check to make sure
		// the solar flux is at a certain level, otherwise the correlations aren't valid
		if( ((zenith > 90.0 - hel_stow) || (I_bn <= 1.0)) || ((zenith == 0.0) && (azimuth == 180.0)) )
		{
			if( m_night_recirc == 1 )		I_bn = 0.0;
			else
			{
				// Set nightime outputs
				mode = 0;
				NullOutputs();
				return 0;
			}
		}

		err_defocus = 999.9;	// Always reset before iteration
		defocus_rec = true;		// Allows entry into loop, and is set to false immediately afterwards. If defocus is required, will be set to true and loop will repeat

		// 15 continue !mjw 3.29.11 return point for over-design iteration
		while( defocus_rec )
		{
			defocus_rec = false;	// Set so that loop is not repeated unless defocus is required
			field_eff_adj = eta_field*od_control;	// mjw 3.29.11
				
			if( I_bn > 1.0 )
			{
				double * flux_array_1D = new double[120];
				// Get flux array: f(azimuth, zenith, flux_map, flux_array_1D)
				double hold = 1.e6;
				int p1;
				for( int i = 0; i < num_sol_pos; i++ )
				{
					double azi_i = fluxmap_angles.at(0,i);
					double zen_i = fluxmap_angles.at(1,i);
					double xdist = sqrt( pow( azi_i - azimuth, 2 ) + pow( zen_i - zenith, 2 ) );
					if( xdist <= hold )
					{
						hold = xdist;
						p1 = i;
					}
				}
				for( int i = 0; i < 120; i++ )
				{
					flux_array_1D[i] = fluxmap.at( p1, i );
				}
				for( int i = 0; i < 120; i++)
				{
					int i_1 = (i)/12;
					int j_1 = (i)%12;
					flux_array_2D.at(i_1, j_1) = flux_array_1D[i]/(950.0)*I_bn*field_eff_adj;	// [kW/m2]
				}
				delete [] flux_array_1D;
			}
			else
			{
				for( int i = 0; i < 120; i++ )
				{
					int i_1 = (i+1)/12;
					int j_1 = (i+1)%12;
					flux_array_2D.at(i_1, j_1) = 0.0;
				}
			}

			// Translate the 2D array to the number of vertical nodes and the number of panels, so each node has its own averaged flux value		
			TranslateFluxArray( flux_array_2D, m_n_nodes, m_n_panels, solarflux );

			// Solar irradiance on each node [W]
			for( int i = 0; i < m_n_nodes; i++ )
				for( int j = 0; j < m_n_panels; j++ )
					q_solar.at(i,j) = 1000.0 * A_node * solarflux.at(i,j);		// [W]
		
			// Solar irradiance on each panel [W]
			for( int i = 0; i < m_n_panels; i++ )
			{	
				double sum_panel = 0.0;
				for( int j = 0; j < m_n_nodes; j++ )
				{
					sum_panel += q_solar.at(j,i);
				}
				q_solar_panel.at(i,0) = sum_panel;
			}

			// Solar irradiance on receiver
			q_solar_total = 0.0;
			for( int i = 0; i < m_n_panels; i++ )
				q_solar_total += q_solar_panel.at(i,0);

			// 'q_solar_critical' calculation is in initial calculation section
			if( q_solar_total < q_solar_critical )
			{
				// Set nightime outputs and get out
				mode = 0;
				NullOutputs();
				return 0;
			}

			// Aperture (Opening) surface is set to the sky temperature
			T_O = T_sky;
			// Guess values for the receiver surface temperatures	
			T_F_guess = T_htf_hot;
			T_CE_guess = T_htf_hot;
			T_L_guess = T_htf_hot;
			T_s_guess.fill( T_htf_hot );

			// Guess values for the surface and HTF temperature		
			if(m_night_recirc == 1)
			{
				T_htf_guess.fill( (T_htf_hot+T_htf_cold)/2.0 );
				T_htf_ave_guess.fill( (T_htf_hot+T_htf_cold)/2.0 );
			}
			else
			{
				T_htf_guess.fill( T_htf_cold );
				T_htf_ave_guess.fill( T_htf_cold );
			}

			// HTF fluid properties at the average inlet and outlet temperatures
			T_htf_average = (T_htf_cold + T_htf_hot)/2.0;		// [K]
			rho_htf = rec_htf.dens( T_htf_average, P_htf );		// [kg/m3]
			k_htf = rec_htf.cond( T_htf_average );				// [W/m-K]
			mu_htf = rec_htf.visc( T_htf_average );				// [Pa-s]
			c_htf = rec_htf.Cp( T_htf_average )*1000.0;			// [J/kg-K]
			Pr_htf = c_htf * mu_htf / k_htf;					// [-]

			//***************************************************************************************
			// Assign guess values for temperatures and mass flow rates depending on the flow pattern
			//***************************************************************************************

			if( I_bn > 1.0 )		// Have already checked that I_bn > 1.0 ...
			{
				// Estimate the thermal power produced by the receiver
				// and calculate a HTF mass flow rate guess value, then assign guess values to the
				// average heat transfer fluid temperature for every node

				switch(flow_pattern)
				{
				case 1:
					for( int j = 0; j < m_n_panels; j++ )
					{
						m_htf_guess.at(j,0) = m_eta_thermal_guess * q_solar_panel.at(j,0)/(c_htf*(T_htf_hot - T_htf_cold));
						for( int i = 0; i < m_n_nodes; i++ )
						{
							// The heat transferred to the fluid increases the HTF temperature
							T_htf_guess.at(i+1,j) = T_htf_guess.at(i,j) + m_eta_thermal_guess*q_solar.at(i,j)/(m_htf_guess.at(j,0)*c_htf);

							// Average node heat transfer temperature [K]
							T_htf_ave_guess.at(i,j) = (T_htf_guess.at(i,j) + T_htf_guess.at(i+1,0))/2.0;
						}
					}

					break; // End flow_pattern 1

				case 2:
					for( int j = 0; j < m_n_panels; j++ )
					{
						m_htf_guess.at(j,0) = m_eta_thermal_guess * q_solar_panel.at(j,0)/(c_htf*(T_htf_hot - T_htf_cold));
						for( int i = 0; i < m_n_nodes; i++ )
						{
							// The heat transferred to the fluid increases the HTF temperature
							T_htf_guess.at( m_n_nodes-1-i, j ) = T_htf_guess.at( m_n_nodes-i, j ) + m_eta_thermal_guess*q_solar.at( m_n_nodes-1-i, j )/(m_htf_guess.at(j,0)*c_htf);

							// Average node heat transfer temperature [K]
							T_htf_ave_guess.at( m_n_nodes-1-i, j ) = ( T_htf_guess.at( m_n_nodes-i, j ) + T_htf_guess.at(m_n_nodes-1-i, j) )/2.0;
						}
					}

					break; // End flow_pattern 2

				case 3:
					// Guess the value for the total mass flow rate through the receiver
					m_htf_guess.fill(m_eta_thermal_guess*q_solar_total/(c_htf*(T_htf_hot-T_htf_cold)));

					// Calculate the average HTF temperature for each node
					for( int j = 0; j < m_n_panels; j++ )
						for( int i = 0; i < m_n_nodes; i++ )
						{
							if( j%2 != 0 )
							{
								if( i == 0 )
								{
									// Set inlet temperature - flow from top to bottom
									T_htf_guess.at( m_n_nodes, j ) = T_htf_guess.at( m_n_nodes, j-1);									
								}
								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at( m_n_nodes-1-i, j ) = T_htf_guess.at( m_n_nodes-i, j ) + m_eta_thermal_guess*q_solar.at( m_n_nodes-1-i, j )/(m_htf_guess.at(j,0)*c_htf);
								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at( m_n_nodes-1-i, j ) = (T_htf_guess.at( m_n_nodes-1-i, j )+T_htf_guess.at( m_n_nodes-i, j ))/2.0;
							}
							else
							{
								if( i == 0 && j!=0 )
								{
									// Set inlet temperature - flow from bottom to top
									T_htf_guess.at( 0, j ) = T_htf_guess.at( 0, j-1 );
								}
								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at(i+1,j) = T_htf_guess.at(i,j) + m_eta_thermal_guess*q_solar.at(i,j)/(m_htf_guess.at(j,0)*c_htf);
								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at(i,j) = (T_htf_guess.at(i,j) + T_htf_guess.at(i+1,j))/2.0;
							}
						}
					break;	// End flow_pattern = 3

				case 4:
					// Guess the value for the total mass flow rate through the receiver
					m_htf_guess.fill(m_eta_thermal_guess*q_solar_total/(c_htf*(T_htf_hot-T_htf_cold)));
					
					// Calculate the average HTF temperature for each node
					for( int j = 0; j < m_n_panels; j++ )
					{
						for( int i = 0; i < m_n_nodes; i++ )
						{
							if( (j&2) != 0 )	// flow from bottom to top
							{
								if( i == 0 )
								{
									// Set inlet temperature
									T_htf_guess.at( 0, j ) = T_htf_guess.at( 0, j-1 );							
								}
								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at(i+1,j) = T_htf_guess.at(i,j) + m_eta_thermal_guess*q_solar.at(i,j)/(m_htf_guess.at(j,0)*c_htf);
								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at(i,j) = (T_htf_guess.at(i,j) + T_htf_guess.at(i+1,j))/2.0;
							}
							else		// flow from top to bottom
							{
								if( i == 0 && j != 0 )
								{
									// Set inlet temperature - flow from top to bottom
									T_htf_guess.at( m_n_nodes, j ) = T_htf_guess.at( m_n_nodes, j-1);
								}
								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at( m_n_nodes-1-i, j ) = T_htf_guess.at( m_n_nodes-i, j ) + m_eta_thermal_guess*q_solar.at( m_n_nodes-1-i, j )/(m_htf_guess.at(j,0)*c_htf);
								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at( m_n_nodes-1-i, j ) = (T_htf_guess.at( m_n_nodes-1-i, j )+T_htf_guess.at( m_n_nodes-i, j ))/2.0;
							}
						}
					}
					break;	// End flow_pattern = 4

				case 5:

					// Guess value for the total mass flow rate through the receiver
					m_htf_guess.fill( 0.0 );
					{
						double q_sum_0 = 0.0;
						double q_sum_1 = 0.0;
						for( int j = 0; j < m_n_panels/2; j++ )
						{
							q_sum_0 += q_solar_panel.at(j,0);
							q_sum_1 += q_solar_panel.at(m_n_panels-1-j,0);
						}
						m_htf_guess.at(0,0) = m_eta_thermal_guess*q_sum_0/(c_htf*(T_htf_hot - T_htf_cold));
						m_htf_guess.at(1,0) = m_eta_thermal_guess*q_sum_1/(c_htf*(T_htf_hot - T_htf_cold));
					}

					for( int j = 0; j < m_n_panels/2; j++ )
					{
						for( int i = 0; i < m_n_nodes; i++ )
						{
							if( j == 0 )
							{
								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at(i+1,j) = T_htf_guess.at(i,j) + m_eta_thermal_guess*q_solar.at(i,j)/(m_htf_guess.at(0,0)*c_htf);
								T_htf_guess.at(i+1,m_n_panels-1-j) = T_htf_guess.at(i,m_n_panels-1-j) + m_eta_thermal_guess*q_solar.at(i,m_n_panels-1-j)/(m_htf_guess.at(1,0)*c_htf);
								
								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at(i,j) = (T_htf_guess.at(i,j) + T_htf_guess.at(i+1,j))/2.0;
								T_htf_ave_guess.at(i,m_n_panels-1-j) = (T_htf_guess.at(i,m_n_panels-1-j) + T_htf_guess.at(i+1,m_n_panels-1-j))/2.0;
							}
							else
							{
								if( i == 0 )
								{
									// Set inlet temperature
									T_htf_guess.at(m_n_nodes,j) = T_htf_guess.at(m_n_nodes,j-1);
									T_htf_guess.at(m_n_nodes,m_n_panels-1-j) = T_htf_guess.at(m_n_nodes,m_n_panels-1-j+1);
								}
								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at( m_n_nodes-1-i, j ) = T_htf_guess.at( m_n_nodes-i, j ) + m_eta_thermal_guess*q_solar.at( m_n_nodes-1-i, j )/(m_htf_guess.at(0,0)*c_htf);
								T_htf_guess.at( m_n_nodes-1-i, m_n_panels-1-j ) = T_htf_guess.at( m_n_nodes-i, m_n_panels-1-j ) + m_eta_thermal_guess*q_solar.at( m_n_nodes-1-i, m_n_panels-1-j )/(m_htf_guess.at(1,0)*c_htf);

								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at( m_n_nodes-1-i, j ) = (T_htf_guess.at( m_n_nodes-1-i, j )+T_htf_guess.at( m_n_nodes-i, j ))/2.0;
								T_htf_ave_guess.at( m_n_nodes-1-i, m_n_panels-1-j ) = (T_htf_guess.at( m_n_nodes-1-i, m_n_panels-1-j )+T_htf_guess.at( m_n_nodes-i, m_n_panels-1-j ))/2.0;
							}
						}
					}

					break;	// End flow_pattern = 5

				case 6:
					m_htf_guess.fill( 0.0 );
					
					{
						double sum_path = 0.0;
						for( int i = 0; i < m_n_panels/2; i++ )
							sum_path += q_solar_panel.at(i,0);
						m_htf_guess.at(0,0) = m_eta_thermal_guess * sum_path / (c_htf*(T_htf_hot - T_htf_cold));		// [kg/s]
					
						sum_path = 0.0;
						for( int i = m_n_panels/2; i < m_n_panels; i++ )
							sum_path += q_solar_panel.at(i,0);
						m_htf_guess.at(1,0) = m_eta_thermal_guess * sum_path / (c_htf*(T_htf_hot - T_htf_cold));		// [kg/s]
					}

					// Average temperature rise in each node
					deltaT_htfX = (T_htf_hot-T_htf_cold)/(double)(m_n_nodes*m_n_panels*0.5);		// [K]

					for( int j = 0; j < m_n_panels/2; j++ )
						for( int i = 0; i < m_n_nodes; i++ )
						{
							if( j==0 )		// If first panel
							{
								// The heat transferred to the fluid increases the HTF temperature
								// Outer left panel
								T_htf_guess.at(m_n_nodes-1-i,j) = T_htf_guess.at(m_n_nodes-i,j) + m_eta_thermal_guess*q_solar.at(m_n_nodes-1-i,j)/(m_htf_guess.at(0,0)*c_htf);

								// Outer right panel
								T_htf_guess.at(m_n_nodes-1-i,m_n_panels-1-j) = T_htf_guess.at(m_n_nodes-i,m_n_panels-1-j) + m_eta_thermal_guess*q_solar.at(m_n_nodes-1-i,m_n_panels-1-j)/(m_htf_guess.at(1,0)*c_htf);
														
								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at(m_n_nodes-1-i,j) = (T_htf_guess.at(m_n_nodes-i,j) + T_htf_guess.at(m_n_nodes-1-i,j))/2.0;
								T_htf_ave_guess.at(m_n_nodes-1-i,m_n_panels-1-j) = (T_htf_guess.at(m_n_nodes-i,m_n_panels-1-j) + T_htf_guess.at(m_n_nodes-1-i,m_n_panels-1-j))/2.0;							
							}
							else if( (int) ( (double) j/2.0 ) < (double) j/2.0 )
							{
								if( i == 0 )	// HTF flow from bottom to top
								{
									// entrance to next panel equals outlet of previous panel
									T_htf_guess.at(i,j) = T_htf_guess.at(i,j-1);	
									T_htf_guess.at(i,m_n_panels-1-j) = T_htf_guess.at(i,m_n_panels-j);	
								}							
								// outlet temp of first node
								T_htf_guess.at(i+1,j) = T_htf_guess.at(i,j) + m_eta_thermal_guess*q_solar.at(i,j)/(m_htf_guess.at(0,0)*c_htf);
								T_htf_guess.at(i+1,m_n_panels-1-j) = T_htf_guess.at(i,m_n_panels-1-j) + m_eta_thermal_guess*q_solar.at(i,m_n_panels-1-j)/(m_htf_guess.at(1,0)*c_htf);
							
								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at(i,j) = (T_htf_guess.at(i+1,j) + T_htf_guess.at(i,j))/2.0;
								T_htf_ave_guess.at(i,m_n_panels-1-j) = (T_htf_guess.at(i+1,m_n_panels-1-j) + T_htf_guess.at(i,m_n_panels-1-j))/2.0;
							}
						}
					break;	// End FlowPattern 6

				case 7:

					// Guess value for the total mass flow rate through the receiver
					m_htf_guess.fill( 0.0 );
					{
						double q_sum_0 = 0.0;
						double q_sum_1 = 0.0;
						for( int j = 0; j < m_n_panels/2; j++ )
						{
							q_sum_0 += q_solar_panel.at(j,0);
							q_sum_1 += q_solar_panel.at(m_n_panels-1-j,0);
						}
						m_htf_guess.at(0,0) = m_eta_thermal_guess*q_sum_0/(c_htf*(T_htf_hot - T_htf_cold));
						m_htf_guess.at(1,0) = m_eta_thermal_guess*q_sum_1/(c_htf*(T_htf_hot - T_htf_cold));
					}

					for( int j = 0; j < m_n_panels/2; j++ )
					{
						for( int i = 0; i < m_n_nodes; i++ )
						{
							if( j == 0 )
							{
								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j ) = T_htf_guess.at( m_n_nodes-i, m_n_panels/2-1-j ) + m_eta_thermal_guess*q_solar.at( m_n_nodes-1-i, m_n_panels/2-1-j )/(m_htf_guess.at(0,0)*c_htf);
								T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2+j ) = T_htf_guess.at( m_n_nodes-i, m_n_panels/2+j ) + m_eta_thermal_guess*q_solar.at( m_n_nodes-1-i, m_n_panels/2+j )/(m_htf_guess.at(1,0)*c_htf);

								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j ) = (T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j )+T_htf_guess.at( m_n_nodes-i, m_n_panels/2-1-j ))/2.0;
								T_htf_ave_guess.at( m_n_nodes-1-i, m_n_panels/2+j ) = (T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2+j )+T_htf_guess.at( m_n_nodes-i, m_n_panels/2+j ))/2.0;
							}
							else
							{
								if( i == 0 )
								{
									T_htf_guess.at( 0, m_n_panels/2-1-j ) = T_htf_guess.at( 0, m_n_panels/2-1-j+1 );
									T_htf_guess.at( 0, m_n_panels/2+j ) = T_htf_guess.at( 0, m_n_panels/2+j-1 );
								}
								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at(i+1,m_n_panels/2-1-j) = T_htf_guess.at(i,m_n_panels/2-1-j) + m_eta_thermal_guess*q_solar.at(i,m_n_panels/2-1-j)/(m_htf_guess.at(0,0)*c_htf);
								T_htf_guess.at(i+1,m_n_panels/2+j) = T_htf_guess.at(i,m_n_panels/2+j) + m_eta_thermal_guess*q_solar.at(i,m_n_panels/2+j)/(m_htf_guess.at(1,0)*c_htf);
								
								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at(i,m_n_panels/2-1-j) = (T_htf_guess.at(i,m_n_panels/2-1-j) + T_htf_guess.at(i+1,m_n_panels/2-1-j))/2.0;
								T_htf_ave_guess.at(i,m_n_panels/2+j) = (T_htf_guess.at(i,m_n_panels/2+j) + T_htf_guess.at(i+1,m_n_panels/2+j))/2.0;
							}
						}
					}

					break;	// End FlowPattern 7

				case 8:
					
					// Guess value for the total mass flow rate through the receiver
					m_htf_guess.fill( 0.0 );
					{
						double q_sum_0 = 0.0;
						double q_sum_1 = 0.0;
						for( int j = 0; j < m_n_panels/2; j++ )
						{
							q_sum_0 += q_solar_panel.at(j,0);
							q_sum_1 += q_solar_panel.at(m_n_panels-1-j,0);
						}
						m_htf_guess.at(0,0) = m_eta_thermal_guess*q_sum_0/(c_htf*(T_htf_hot - T_htf_cold));
						m_htf_guess.at(1,0) = m_eta_thermal_guess*q_sum_1/(c_htf*(T_htf_hot - T_htf_cold));
					}

					for( int j = 0; j < m_n_panels/2; j++ )
					{
						for( int i = 0; i < m_n_nodes; i++ )
						{
							if( j == 0 )
							{
								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at(i+1,m_n_panels/2-1-j) = T_htf_guess.at(i,m_n_panels/2-1-j) + m_eta_thermal_guess*q_solar.at(i,m_n_panels/2-1-j)/(m_htf_guess.at(0,0)*c_htf);
								T_htf_guess.at(i+1,m_n_panels/2+j) = T_htf_guess.at(i,m_n_panels/2+j) + m_eta_thermal_guess*q_solar.at(i,m_n_panels/2+j)/(m_htf_guess.at(1,0)*c_htf);
								
								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at(i,m_n_panels/2-1-j) = (T_htf_guess.at(i,m_n_panels/2-1-j) + T_htf_guess.at(i+1,m_n_panels/2-1-j))/2.0;
								T_htf_ave_guess.at(i,m_n_panels/2+j) = (T_htf_guess.at(i,m_n_panels/2+j) + T_htf_guess.at(i+1,m_n_panels/2+j))/2.0;
							}
							else
							{
								if( i == 0 )
								{
									T_htf_guess.at( m_n_nodes, m_n_panels/2-1-j ) = T_htf_guess.at( m_n_nodes, m_n_panels/2-1-j+1 );
									T_htf_guess.at( m_n_nodes, m_n_panels/2+j ) = T_htf_guess.at( m_n_nodes, m_n_panels/2+j-1 );
								}

								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j ) = T_htf_guess.at( m_n_nodes-i, m_n_panels/2-1-j ) + m_eta_thermal_guess*q_solar.at( m_n_nodes-1-i, m_n_panels/2-1-j )/(m_htf_guess.at(0,0)*c_htf);
								T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2+j ) = T_htf_guess.at( m_n_nodes-i, m_n_panels/2+j ) + m_eta_thermal_guess*q_solar.at( m_n_nodes-1-i, m_n_panels/2+j )/(m_htf_guess.at(1,0)*c_htf);
								
								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j ) = (T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j )+T_htf_guess.at( m_n_nodes-i, m_n_panels/2-1-j ))/2.0;
								T_htf_ave_guess.at( m_n_nodes-1-i, m_n_panels/2+j ) = (T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2+j )+T_htf_guess.at( m_n_nodes-i, m_n_panels/2+j ))/2.0;
							}
						}
					}

					break;	// End FlowPattern 8

				}
			}												

			// Initial values for error calculations
			T_htf_hot_guess.fill( 9999.9 );
			m_htf.fill( 9999.9 );
			err_coupled_conv = 9999.9;
			iter_coupled_conv = 0;
			gamma = 1.0;
			gamma_count = 1;
				// Do these need to be reset when defocusing?
			h_conv.fill( 0.0 );
			q_conv.fill( 0.0 );			
			q_convection_guess = 0.0;
			q_convection = 1.0;			
			// ******************************

			// Set passive panels and opening to 0
			flux_1D.fill( 0.0 );
			UA_1DIM.fill( 0.0 );
			T_htf_ave_guess_1D.fill( 0.0 );
			// Initialize one dimensional arrays
			for( int i = 0; i < m_n_panels; i++ )
				for( int j = 0; j < m_n_nodes; j++ )
				{
					flux_1D.at(j+i*m_n_nodes,0) = solarflux.at(j,i)*1000.0;							// [W/m2]
					T_s_guess_1D.at(j+(i*m_n_nodes),0) =  T_s_guess.at(j,i);							// [K]
					T_htf_ave_guess_1D.at(j+(i*m_n_nodes),0) = T_htf_ave_guess.at(j,i);				// [K]
				}
			T_s_guess_1D.at( m_n_nodes*m_n_panels, 0 ) = T_F_guess;
			T_s_guess_1D.at( m_n_nodes*m_n_panels+1, 0 ) = T_CE_guess;
			T_s_guess_1D.at( m_n_nodes*m_n_panels+2, 0 ) = T_L_guess;
			T_s_guess_1D.at( m_n_nodes*m_n_panels+3, 0 ) = T_O;
			// ******************************************************************************************

			/*ofstream out_file;
			out_file.open("fish.txt");
			for( int j = 0; j < n_nodes*n_panels; j++ )
				out_file << flux_1D.at(j,0) << " " << T_s_guess_1D.at(j,0) << " " << T_htf_ave_guess_1D.at(j,0) << endl; */

			// Initialize "old" copies used in iterative loop
			for( int i = 0; i < m_n_nodes*m_n_panels+4; i++ )
				T_s_1D.at(i,0) = T_s_guess_1D.at(i,0);
			for( int i = 0; i < m_n_panels; i++ )
			{
				for( int j = 0; j < m_n_nodes; j++ )
				{
					T_htf_ave.at(j,i) = T_htf_ave_guess.at(j,i);
					T_htf.at(j,i) = T_htf_guess.at(j,i);
				}
				T_htf.at(m_n_nodes,i) = T_htf_guess.at(m_n_nodes,i); 
			}		

			// Call convection correlation to get guess value for T_bulk
			q_losses_therm_guess = q_solar_total*max_eps_active*(1.0 - m_eta_thermal_guess);
			q_losses_rad_guess = q_losses_therm_guess / 2.0;
			cavity.ConvectionClausing1983(m_n_panels, T_s_guess, T_F_guess, T_CE_guess, T_L_guess, T_amb, P_amb, A_node, q_losses_rad_guess,
											q_outX, h_FX, h_avgX, h_stagX, T_stagX, T_bulk, SX);									
			// ***********************************************************						

			cycle_out_loop = false;		// Logic to break out of qq loop and reset gamma. Should default to false unless switch in qq loop

			while( fabs(err_coupled_conv) > 1.E-8 && iter_coupled_conv < 100 )
			{
				// 222 continue
				//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
				// ITERATION STARTS HERE
				//>>>>><<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
				int qq_max = 200;
				for( int qq = 0; qq < 200; qq++ ) 
				{
																														
					//------------------------------------------------------------------------
					// Flow and convective heat transfer characteristics
					//------------------------------------------------------------------------
					for( int j = 0; j < m_n_panels; j++ )
					{
						double T_htf_ave_guess_p = 0.0;
						for( int i = 0; i < m_n_nodes; i++ )
							T_htf_ave_guess_p += (T_htf_ave_guess.at(i,j)/m_n_nodes);		// [K]

						rho_htf_p.at(j,0) = rec_htf.dens( T_htf_ave_guess_p, P_htf );	// [kg/m3]
						double mu_htf_p = rec_htf.visc( T_htf_ave_guess_p );			// [Pa-s]
						double k_htf_p = rec_htf.cond( T_htf_ave_guess_p );				// [W/m-K]
						double c_htf_p = rec_htf.Cp( T_htf_ave_guess_p );				// [J/kg-K]
						double Pr_htf_p = c_htf_p * mu_htf_p / k_htf_p;					// [-]

						// Fluid velocity in the tubes [m/s]
						// select case(FlowPattern)
						if( flow_pattern < 5 )
						{
							u_htf_p.at(j,0) = m_htf_guess.at(j,0)/(A_tube*rho_htf_p.at(j,0)*(double)n_tubes);
							m_htf_p.at(j,0) = m_htf_guess.at(j,0);
						}
						else
						{
							if( j < m_n_panels/2 )
							{
								u_htf_p.at(j,0) = m_htf_guess.at(0,0)/(A_tube*rho_htf_p.at(j,0)*(double)n_tubes);
								m_htf_p.at(j,0) = m_htf_guess.at(0,0);
							}
							else
							{
								u_htf_p.at(j,0) = m_htf_guess.at(1,0)/(A_tube*rho_htf_p.at(j,0)*(double)n_tubes);
								m_htf_p.at(j,0) = m_htf_guess.at(1,0);
							}
						}
                
						double Re_htf_p = u_htf_p.at(j,0)*rho_htf_p.at(j,0)*d_tube_in/mu_htf_p;
						double Nu_htf_dummy;
						PipeFlowCavity( Re_htf_p, Pr_htf_p, L_over_D_p, relRough, q_solar_panel.at(j,0), 1, Nu_htf_dummy, f_htf_p.at(j,0) );
						if( m_htf_guess.at(j,0) == 0.0 )
							f_htf_p.at(j,0) = 0.0;

						for( int i = 0; i < m_n_nodes; i++ )
						{	
							double T_htf_ave_ij = (T_htf_guess.at(i,j) + T_htf_guess.at(i+1,j))/2.0;
							// ------------------------------------------------------------------------ !ST R_conv caclulated for every node
							double mu_htf_node = rec_htf.visc( T_htf_ave_ij );							// [Pa-s]
							double Re_htf_node = m_htf_p.at(j,0)*d_tube_in/(A_tube*mu_htf_node*(double)n_tubes);	// [-]
							double c_htf_node = rec_htf.Cp( T_htf_ave_ij )*1000.0;						// [J/kg-K]
							double k_htf_node = rec_htf.cond( T_htf_ave_ij );							// [W/m-K]
							double Pr_htf_node = c_htf_node*mu_htf_node/k_htf_node;						// [-]
							
							double f_htf_dummy, Nu_htf_node;
							PipeFlowCavity(Re_htf_node, Pr_htf_node, L_over_D, relRough, q_solar.at(i, j), (int)is_fd.at(i, j), Nu_htf_node, f_htf_dummy);
				        
							// The heat transfer coefficient for thermal energy transfer from the tube walls to the HTF
							double h_htf_node = Nu_htf_node*k_htf_node/d_tube_in;
							// Calculate the resistance to convection from the tube walls to the HTF [K/W] - all tubes in the node
							double R_conv_node = 2.0/(h_htf_node*L_tube_node*d_tube_in*CSP::pi*(double)n_tubes); 

							// Wall conductivity [W/m-K]
							double k_tube = tube_mat.cond( (T_s_guess.at(i,j) + T_htf_ave_ij)/2.0 );			
							// The resistance to conduction of thermal energy through the tube walls [K/W] - all tubes in the node
							double R_cond = log(d_tube_out/d_tube_in)/(CSP::pi*L_tube_node*k_tube*(double)n_tubes);	// Removed "2.0*" in denominator per discussion with Lukas 10.5.2010
							// Receiver panel conductance, the inverse of the sum of the resistances to conduction and convection [W/K]				      
							UA_1DIM.at(i+j*m_n_nodes,0) = 1.0/(R_cond+R_conv_node);	
						}			
					}																				
				
					//-----------------------------------------------------------------------------------------------
					// Calculate the radiation heat transfer coefficients based on the calculated surface temperatures
					//-----------------------------------------------------------------------------------------------																																									

					// Band-mode = 2: always allow multi-node
					FractionFunction( m_n_nodes, m_n_panels, n_bands, T_s_guess_1D, lambda_step_band, f_temp_band, f_solar_band );

					for( int i = 0; i < m_n_nodes*m_n_panels+4; i++ )
						for( int j = 0; j < m_n_nodes*m_n_panels+4; j++ )
						{
							double sum_solar = 0.0;
							// h_rad_semi_gray_therm represents thermal (temperature driven) radiation heat transfer between all surfaces in the cavity
							if( T_s_guess_1D.at(i,0) == T_s_guess_1D.at(j,0) )
								h_rad_semi_gray_therm.at(i,j) = 1.E-6;
							else
							{
								double sum_therm = 0.0;							
								for( int l = 0; l < n_bands; l++ )
								{
									sum_therm += f_temp_band.at(i,l)*e_band_array.at(i,l)*A_array[i]*pow(T_s_guess_1D.at(i,0),4)*F_hat.at(i,j,l)*e_band_array.at(j,l) - f_temp_band.at(j,l)*e_band_array.at(j,l)*A_array[j]*pow(T_s_guess_1D.at(j,0),4)*F_hat.at(j,i,l)*e_band_array.at(i,l);
									sum_solar += f_solar_band.at(l,0)*F_hat.at(i,j,l)*A_array[i]*( (flux_1D.at(i,0)*(1.0-e_band_array.at(i,l))*e_band_array.at(j,l)) - (flux_1D.at(j,0)*(1.0-e_band_array.at(j,l))*e_band_array.at(i,l)) );
								}
								// Again, only multi-band model
								h_rad_semi_gray_therm.at(i,j) = CSP::sigma/(A_array[i]*(T_s_guess_1D.at(i,0) - T_s_guess_1D.at(j,0)))*sum_therm;
							}
							q_rad_solar.at(i,j) = sum_solar;
						}

					// Net solar radiation heat transfer transfer from each surface (positive if more energy leaves than is absorbed
					for( int i = 0; i < m_n_nodes*m_n_panels+4; i++ )
					{
						double sum_solar_net = 0.0;
						for( int j = 0; j < m_n_nodes*m_n_panels+4; j++ )
							sum_solar_net += q_rad_solar.at(i,j);
						q_rad_solar_net.at(i,0) = sum_solar_net;
					}

					// Calculation active heat exchanger surfaces  - APPLICABLE FOR ALL SURFACE (i=1,N_panels*N_nodes+4) IF CONVECTION AT ALL SURFACES IS DEPENDENT ON THE SURFACE TEMPERATURE
					for( int i = 0; i < m_n_nodes*m_n_panels+3; i++ ) 
					{
						double sum_T1 = 0.0;
						double sum_T2 = 0.0;
						for( int j = 0; j < m_n_nodes*m_n_panels+4; j++ )
						{
							sum_T1 += h_rad_semi_gray_therm.at(i,j) * T_s_guess_1D.at(j,0);
							sum_T2 += h_rad_semi_gray_therm.at(i,j);
						}
						// Energy balance active surfaces
						T_s_guess_1D.at(i,0) = (flux_1D.at(i,0)*A_array[i] + UA_1DIM.at(i,0)*T_htf_ave_guess_1D.at(i,0) + A_array[i]*sum_T1 - q_rad_solar_net.at(i,0) + A_array[i]*h_conv.at(i,0)*T_bulk)/( UA_1DIM.at(i,0) + A_array[i]*sum_T2 + A_array[i]*h_conv.at(i,0) );
						q_htf_1D.at(i,0) = UA_1DIM.at(i,0)*(T_s_guess_1D.at(i,0) - T_htf_ave_guess_1D.at(i,0));
					}

					for( int i = m_n_nodes*m_n_panels; i < m_n_nodes*m_n_panels + 4; i++ )
						q_htf_1D.at(i,0) = 0.0;

					// Calculation of relaxed temperature for next iteration step
					for( int i = 0; i < m_n_panels*m_n_nodes+4; i++ )
						T_s_guess_mid_1D.at(i,0) = gamma*T_s_guess_1D.at(i,0)+(1.0-gamma)*T_s_1D.at(i,0);

					for( int k = 0; k < m_n_panels; k++ )
						for( int j = 0; j < m_n_nodes; j++ )
						{
							q_htf.at(j,k) = q_htf_1D.at((size_t)j+k*m_n_nodes,(size_t)0.0);
						}

					// This must be edited for FLOWPATTERNS 1 and 2   !ST ??? I did not change anything about the flow patterns - should probably be reviewed
					// Absorbed thermal energy by the HTF [W]
					q_htf_total = 0.0;
					for( int i = 0; i < m_n_nodes; i++ )
						for( int j = 0; j < m_n_panels; j++ )
							q_htf_total += q_htf.at(i,j);

					//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
					// Calculate the increase of temperature in the working fluid with regards to the different
					// flow patterns 1 through 8.
					//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

					switch(flow_pattern)
					{
					case 1:
						for( int j = 0; j < m_n_panels; j++ )
						{
							// Absorbed thermal energy by the HTF in each panel [W]
							double sum_q_htf = 0.0;
							for( int i = 0; i < m_n_nodes; i++ )
								sum_q_htf += q_htf.at(i,j);
							q_htf_panel.at(j,0) = sum_q_htf;

							// Calculate the mass flow rates [kg/s]
							m_htf_guess.at(j,0) = q_htf_panel.at(j,0)/(c_htf*(T_htf_hot - T_htf_cold));

							for( int i = 0; i < m_n_nodes; i++ )
							{
								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at(i+1,j) = T_htf_guess.at(i,j) + q_htf.at(i,j)/(m_htf_guess.at(j,0)*c_htf);

								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at(i,j) =(T_htf_guess.at(i+1,j) + T_htf_guess.at(i,j))/2.0;
							}

							// Update the panel outlet tmeperature [K]
							T_htf_hot_guess.at(j,0) = T_htf_guess.at(m_n_nodes,j);
						}

						break;	// flow_pattern = 1

					case 2:
						for( int j = 0; j < m_n_panels; j++ )
						{
							// Absorbed thermal energy by the HTF in each panel [W]
							double sum_q_htf = 0.0;
							for( int i = 0; i < m_n_nodes; i++ )
								sum_q_htf += q_htf.at(i,j);
							q_htf_panel.at(j,0) = sum_q_htf;
							
							// Calculate the mass flow rates [kg/s]
							m_htf_guess.at(j,0) = q_htf_panel.at(j,0)/(c_htf*(T_htf_hot - T_htf_cold));

							// Calculate the average HTF temperature for each node
							for( int i = 0; i < m_n_nodes; i++ )
							{
								// The heat transferred to the fluid increases the HTF temperature
								T_htf_guess.at( m_n_nodes-1-i, j ) = T_htf_guess.at( m_n_nodes-i, j ) + q_htf.at( m_n_nodes-1-i, j )/(m_htf_guess.at(j,0)*c_htf);

								// Average node heat transfer temperature [K]
								T_htf_ave_guess.at( m_n_nodes-1-i, j ) = ( T_htf_guess.at( m_n_nodes-i, j ) + T_htf_guess.at(m_n_nodes-1-i, j) )/2.0;
							}

							// Update the panel outlet temperature [K]
							T_htf_hot_guess.at(j,0) = T_htf_guess.at(0,j);
						}

						break;	// End flow_pattern 2
					
					case 3:
						// Calculate the mass flow rates [kg/s]
						m_htf_guess.fill( q_htf_total/(c_htf*(T_htf_hot - T_htf_cold)) );

						// Calculate the average HTF temperature for each node
						for( int j = 0; j < m_n_panels; j++ )
						{
							for( int i = 0; i < m_n_nodes; i++ )
							{
								if( j%2 != 0 )		// flow from top to bottom
								{
									if( i == 0 )
									{
										// Set inlet temperature
										T_htf_guess.at( m_n_nodes, j ) = T_htf_guess.at( m_n_nodes, j-1);
									}
									// The heat transferred to the fluid increases the HTF temperature
									T_htf_guess.at( m_n_nodes-1-i, j ) = T_htf_guess.at( m_n_nodes-i, j ) + q_htf.at( m_n_nodes-1-i, j )/(m_htf_guess.at(j,0)*c_htf);
									// Average node heat transfer temperature [K]
									T_htf_ave_guess.at( m_n_nodes-1-i, j ) = (T_htf_guess.at( m_n_nodes-1-i, j )+T_htf_guess.at( m_n_nodes-i, j ))/2.0;
								}
								else
								{
									if( i == 0 && j != 0 )		// Flow from bottom to top
									{
										// Set inlet temperature 
										T_htf_guess.at( 0, j ) = T_htf_guess.at( 0, j-1 );
									}
									// The heat transferred to the fluid increases the HTF temperature
									T_htf_guess.at(i+1,j) = T_htf_guess.at(i,j) + q_htf.at(i,j)/(m_htf_guess.at(j,0)*c_htf);
									// Average node heat transfer temperature [K]
									T_htf_ave_guess.at(i,j) = (T_htf_guess.at(i,j) + T_htf_guess.at(i+1,0))/2.0;
								}
							}
						}

						T_htf_hot_guess.fill( T_htf_guess.at( 0, m_n_panels-1 ) );

						break;	// End flow_pattern = 3

					case 4:
						// Calculate the mass flow rate [kg/s]
						m_htf_guess.fill( q_htf_total/(c_htf*(T_htf_hot - T_htf_cold)) );

						// Calculate the average HTF temperature for each node
						for( int j = 0; j < m_n_panels; j++ )
						{
							for( int i = 0; i < m_n_nodes; i++ )
							{
								if( (j&2) != 0 )	// flow from bottom to top
								{
									if( i == 0 )
									{
										// Set inlet temperature
										T_htf_guess.at( 0, j ) = T_htf_guess.at( 0, j-1 );							
									}
									// The heat transferred to the fluid increases the HTF temperature
									T_htf_guess.at(i+1,j) = T_htf_guess.at(i,j) + q_htf.at(i,j)/(m_htf_guess.at(j,0)*c_htf);
									// Average node heat transfer temperature [K]
									T_htf_ave_guess.at(i,j) = (T_htf_guess.at(i,j) + T_htf_guess.at(i+1,j))/2.0;
								}
								else		// flow from top to bottom
								{
									if( i == 0 && j != 0 )
									{
										// Set inlet temperature - flow from top to bottom
										T_htf_guess.at( m_n_nodes, j ) = T_htf_guess.at( m_n_nodes, j-1);
									}
									// The heat transferred to the fluid increases the HTF temperature
									T_htf_guess.at( m_n_nodes-1-i, j ) = T_htf_guess.at( m_n_nodes-i, j ) + q_htf.at( m_n_nodes-1-i, j )/(m_htf_guess.at(j,0)*c_htf);
									// Average node heat transfer temperature [K]
									T_htf_ave_guess.at( m_n_nodes-1-i, j ) = (T_htf_guess.at( m_n_nodes-1-i, j )+T_htf_guess.at( m_n_nodes-i, j ))/2.0;
								}
							}
						}
						T_htf_hot_guess.fill( T_htf_guess.at( m_n_nodes, m_n_panels - 1 ) );

						break;	// End flow_pattern = 4

					case 5:
						
						for( int j = 0; j < m_n_panels; j++ )
						{
							q_htf_panel.at(j,0) = 0.0;
							for( int i = 0; i < m_n_nodes; i++ )
								q_htf_panel.at(j,0) += q_htf.at(i,j);
						}

						{
							double sum_path = 0.0;
							for( int i = 0; i < m_n_panels/2; i++ )
								sum_path += q_htf_panel.at(i,0);
							m_htf_guess.at(0,0) = sum_path / (c_htf*(T_htf_hot - T_htf_cold));
						
							sum_path = 0.0;
							for( int i = m_n_panels/2; i < m_n_panels; i++ )
								sum_path += q_htf_panel.at(i,0);
							m_htf_guess.at(1,0) = sum_path / (c_htf*(T_htf_hot - T_htf_cold));
						}
					
						for( int j = 0; j < m_n_panels/2; j++ )
						{
							for( int i = 0; i < m_n_nodes; i++ )
							{
								if( j == 0 )
								{
									// The heat transferred to the fluid increases the HTF temperature
									T_htf_guess.at(i+1,j) = T_htf_guess.at(i,j) + q_htf.at(i,j)/(m_htf_guess.at(0,0)*c_htf);
									T_htf_guess.at(i+1,m_n_panels-1-j) = T_htf_guess.at(i,m_n_panels-1-j) + q_htf.at(i,m_n_panels-1-j)/(m_htf_guess.at(1,0)*c_htf);
								
									// Average node heat transfer temperature [K]
									T_htf_ave_guess.at(i,j) = (T_htf_guess.at(i,j) + T_htf_guess.at(i+1,j))/2.0;
									T_htf_ave_guess.at(i,m_n_panels-1-j) = (T_htf_guess.at(i,m_n_panels-1-j) + T_htf_guess.at(i+1,m_n_panels-1-j))/2.0;
								}
								else
								{
									if( i == 0 )
									{
										// Set inlet temperature
										T_htf_guess.at(m_n_nodes,j) = T_htf_guess.at(m_n_nodes,j-1);
										T_htf_guess.at(m_n_nodes,m_n_panels-1-j) = T_htf_guess.at(m_n_nodes,m_n_panels-1-j+1);
									}
									// The heat transferred to the fluid increases the HTF temperature
									T_htf_guess.at( m_n_nodes-1-i, j ) = T_htf_guess.at( m_n_nodes-i, j ) + q_htf.at( m_n_nodes-1-i, j )/(m_htf_guess.at(0,0)*c_htf);
									T_htf_guess.at( m_n_nodes-1-i, m_n_panels-1-j ) = T_htf_guess.at( m_n_nodes-i, m_n_panels-1-j ) + q_htf.at( m_n_nodes-1-i, m_n_panels-1-j )/(m_htf_guess.at(1,0)*c_htf);
					
									// Average node heat transfer temperature [K]
									T_htf_ave_guess.at( m_n_nodes-1-i, j ) = (T_htf_guess.at( m_n_nodes-1-i, j )+T_htf_guess.at( m_n_nodes-i, j ))/2.0;
									T_htf_ave_guess.at( m_n_nodes-1-i, m_n_panels-1-j ) = (T_htf_guess.at( m_n_nodes-1-i, m_n_panels-1-j )+T_htf_guess.at( m_n_nodes-i, m_n_panels-1-j ))/2.0;
								}
							}
						}
					
						T_htf_hot_guess.at(0,0) = T_htf_guess.at(0,1);
						T_htf_hot_guess.at(1,0) = T_htf_guess.at(0,2);

						break;	// End flow_pattern = 5

					case 6:
						for( int j = 0; j < m_n_panels; j++ )
						{
							q_htf_panel.at(j,0) = 0.0;
							for( int i = 0; i < m_n_nodes; i++ )
								q_htf_panel.at(j,0) += q_htf.at(i,j);
						}

						{
							double sum_path = 0.0;
							for( int i = 0; i < m_n_panels/2; i++ )
								sum_path += q_htf_panel.at(i,0);
							m_htf_guess.at(0,0) = sum_path / (c_htf*(T_htf_hot - T_htf_cold));
						
							sum_path = 0.0;
							for( int i = m_n_panels/2; i < m_n_panels; i++ )
								sum_path += q_htf_panel.at(i,0);
							m_htf_guess.at(1,0) = sum_path / (c_htf*(T_htf_hot - T_htf_cold));
						}

						for( int j = 0; j < m_n_panels/2; j++ )
							for( int i = 0; i < m_n_nodes; i++ )
							{
								if(j==0)		// If first panel
								{
									// The heat transferred to the fluid increases the HTF temperature
									// Outer left panel
									T_htf_guess.at(m_n_nodes-1-i,j) = T_htf_guess.at(m_n_nodes-i,j) + q_htf.at(m_n_nodes-1-i,j)/(m_htf_guess.at(0,0)*c_htf);
								
									// Outer right panel
									T_htf_guess.at(m_n_nodes-1-i,m_n_panels-1-j) = T_htf_guess.at(m_n_nodes-i,m_n_panels-1-j) + q_htf.at(m_n_nodes-1-i,m_n_panels-1-j)/(m_htf_guess.at(1,0)*c_htf);							

									// Average node heat transfer temperature [K]
									T_htf_ave_guess.at(m_n_nodes-1-i,j) = (T_htf_guess.at(m_n_nodes-i,j) + T_htf_guess.at(m_n_nodes-1-i,j))/2.0;
									T_htf_ave_guess.at(m_n_nodes-1-i,m_n_panels-1-j) = (T_htf_guess.at(m_n_nodes-i,m_n_panels-1-j) + T_htf_guess.at(m_n_nodes-1-i,m_n_panels-1-j))/2.0;
								}
								else if( (int) ( (double) j / 2.0 ) < (double) j / 2.0 )
								{
									if( i == 0 )	// HTF flow from bottom to top
									{
										// entrance to next panel equals outlet of previous panel
										T_htf_guess.at(i,j) = T_htf_guess.at(i,j-1);
										T_htf_guess.at(i,m_n_panels-1-j) = T_htf_guess.at(i,m_n_panels-j);
									}
									// outlet temperature
									T_htf_guess.at(i+1,j) = T_htf_guess.at(i,j) + q_htf.at(i,j)/(m_htf_guess.at(0,0)*c_htf);
									T_htf_guess.at(i+1,m_n_panels-1-j) = T_htf_guess.at(i,m_n_panels-1-j) + q_htf.at(i,m_n_panels-1-j)/(m_htf_guess.at(1,0)*c_htf);

									// Average node heat transfer temperature
									T_htf_ave_guess.at(i,j) = (T_htf_guess.at(i+1,j) + T_htf_guess.at(i,j))/2.0;
									T_htf_ave_guess.at(i,m_n_panels-1-j) = (T_htf_guess.at(i+1,m_n_panels-1-j) + T_htf_guess.at(i,m_n_panels-1-j))/2.0;
								}
							}
					
						// Hot coolant outlet temperature
						if( (int) ( (double) m_n_panels / 2.0 / 2.0 ) < (double) m_n_panels /2.0 /2.0 )
						{
							T_htf_hot_guess.at(0,0); // need to add equations for n_panels != 4
						}
						else
						{
							T_htf_hot_guess.at(0,0) = T_htf_guess.at( m_n_nodes, m_n_panels/2 - 1 );
							T_htf_hot_guess.at(1,0) = T_htf_guess.at( m_n_nodes, m_n_panels/2 );
						}	

						break;	// flow_pattern = 6

					case 7:

						for( int j = 0; j < m_n_panels; j++ )
						{
							q_htf_panel.at(j,0) = 0.0;
							for( int i = 0; i < m_n_nodes; i++ )
								q_htf_panel.at(j,0) += q_htf.at(i,j);
						}
						
						{
							double sum_path = 0.0;
							for( int i = 0; i < m_n_panels/2; i++ )
								sum_path += q_htf_panel.at(i,0);
							m_htf_guess.at(0,0) = sum_path / (c_htf*(T_htf_hot - T_htf_cold));
						
							sum_path = 0.0;
							for( int i = m_n_panels/2; i < m_n_panels; i++ )
								sum_path += q_htf_panel.at(i,0);
							m_htf_guess.at(1,0) = sum_path / (c_htf*(T_htf_hot - T_htf_cold));
						}

						for( int j = 0; j < m_n_panels/2; j++ )
						{
							for( int i = 0; i < m_n_nodes; i++ )
							{
								if( j == 0 )
								{
									// The heat transferred to the fluid increases the HTF temperature
									T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j ) = T_htf_guess.at( m_n_nodes-i, m_n_panels/2-1-j ) + q_htf.at( m_n_nodes-1-i, m_n_panels/2-1-j )/(m_htf_guess.at(0,0)*c_htf);
									T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2+j ) = T_htf_guess.at( m_n_nodes-i, m_n_panels/2+j ) + q_htf.at( m_n_nodes-1-i, m_n_panels/2+j )/(m_htf_guess.at(1,0)*c_htf);
						
									// Average node heat transfer temperature [K]
									T_htf_ave_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j ) = (T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j )+T_htf_guess.at( m_n_nodes-i, m_n_panels/2-1-j ))/2.0;
									T_htf_ave_guess.at( m_n_nodes-1-i, m_n_panels/2+j ) = (T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2+j )+T_htf_guess.at( m_n_nodes-i, m_n_panels/2+j ))/2.0;
								}
								else
								{
									if( i == 0 )
									{
										T_htf_guess.at( 0, m_n_panels/2-1-j ) = T_htf_guess.at( 0, m_n_panels/2-1-j+1 );
										T_htf_guess.at( 0, m_n_panels/2+j ) = T_htf_guess.at( 0, m_n_panels/2+j-1 );
									}
									// The heat transferred to the fluid increases the HTF temperature
									T_htf_guess.at(i+1,m_n_panels/2-1-j) = T_htf_guess.at(i,m_n_panels/2-1-j) + q_htf.at(i,m_n_panels/2-1-j)/(m_htf_guess.at(0,0)*c_htf);
									T_htf_guess.at(i+1,m_n_panels/2+j) = T_htf_guess.at(i,m_n_panels/2+j) + q_htf.at(i,m_n_panels/2+j)/(m_htf_guess.at(1,0)*c_htf);
									
									// Average node heat transfer temperature [K]
									T_htf_ave_guess.at(i,m_n_panels/2-1-j) = (T_htf_guess.at(i,m_n_panels/2-1-j) + T_htf_guess.at(i+1,m_n_panels/2-1-j))/2.0;
									T_htf_ave_guess.at(i,m_n_panels/2+j) = (T_htf_guess.at(i,m_n_panels/2+j) + T_htf_guess.at(i+1,m_n_panels/2+j))/2.0;
								}
							}
						}

						T_htf_hot_guess.at(0,0) = T_htf_guess.at(m_n_nodes,0);
						T_htf_hot_guess.at(1,0) = T_htf_guess.at(m_n_nodes,m_n_panels-1);

						break;	// flow_pattern = 7

					case 8:

						for( int j = 0; j < m_n_panels; j++ )
						{
							q_htf_panel.at(j,0) = 0.0;
							for( int i = 0; i < m_n_nodes; i++ )
								q_htf_panel.at(j,0) += q_htf.at(i,j);
						}
						
						{
							double sum_path = 0.0;
							for( int i = 0; i < m_n_panels/2; i++ )
								sum_path += q_htf_panel.at(i,0);
							m_htf_guess.at(0,0) = sum_path / (c_htf*(T_htf_hot - T_htf_cold));
						
							sum_path = 0.0;
							for( int i = m_n_panels/2; i < m_n_panels; i++ )
								sum_path += q_htf_panel.at(i,0);
							m_htf_guess.at(1,0) = sum_path / (c_htf*(T_htf_hot - T_htf_cold));
						}

						for( int j = 0; j < m_n_panels/2; j++ )
						{
							for( int i = 0; i < m_n_nodes; i++ )
							{
								if( j == 0 )
								{
									// The heat transferred to the fluid increases the HTF temperature
									T_htf_guess.at(i+1,m_n_panels/2-1-j) = T_htf_guess.at(i,m_n_panels/2-1-j) + q_htf.at(i,m_n_panels/2-1-j)/(m_htf_guess.at(0,0)*c_htf);
									T_htf_guess.at(i+1,m_n_panels/2+j) = T_htf_guess.at(i,m_n_panels/2+j) + q_htf.at(i,m_n_panels/2+j)/(m_htf_guess.at(1,0)*c_htf);
									
									// Average node heat transfer temperature [K]
									T_htf_ave_guess.at(i,m_n_panels/2-1-j) = (T_htf_guess.at(i,m_n_panels/2-1-j) + T_htf_guess.at(i+1,m_n_panels/2-1-j))/2.0;
									T_htf_ave_guess.at(i,m_n_panels/2+j) = (T_htf_guess.at(i,m_n_panels/2+j) + T_htf_guess.at(i+1,m_n_panels/2+j))/2.0;
								}
								else
								{
									if( i == 0 )
									{
										T_htf_guess.at( m_n_nodes, m_n_panels/2-1-j ) = T_htf_guess.at( m_n_nodes, m_n_panels/2-1-j+1 );
										T_htf_guess.at( m_n_nodes, m_n_panels/2+j ) = T_htf_guess.at( m_n_nodes, m_n_panels/2+j-1 );
									}
						
									// The heat transferred to the fluid increases the HTF temperature
									T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j ) = T_htf_guess.at( m_n_nodes-i, m_n_panels/2-1-j ) + q_htf.at( m_n_nodes-1-i, m_n_panels/2-1-j )/(m_htf_guess.at(0,0)*c_htf);
									T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2+j ) = T_htf_guess.at( m_n_nodes-i, m_n_panels/2+j ) + q_htf.at( m_n_nodes-1-i, m_n_panels/2+j )/(m_htf_guess.at(1,0)*c_htf);
									
									// Average node heat transfer temperature [K]
									T_htf_ave_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j ) = (T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2-1-j )+T_htf_guess.at( m_n_nodes-i, m_n_panels/2-1-j ))/2.0;
									T_htf_ave_guess.at( m_n_nodes-1-i, m_n_panels/2+j ) = (T_htf_guess.at( m_n_nodes-1-i, m_n_panels/2+j )+T_htf_guess.at( m_n_nodes-i, m_n_panels/2+j ))/2.0;
								}
							}
						}

						T_htf_hot_guess.at(0,0) = T_htf_guess.at(0,0);
						T_htf_hot_guess.at(1,0) = T_htf_guess.at(0,3);

						break;	// flow_pattern = 8
					}

					for( int k = 0; k < m_n_panels; k++ )
						for( int j = 0; j < m_n_nodes+1; j++ )
							T_htf_guess_mid.at(j,k) = gamma*T_htf_guess.at(j,k) + (1.0-gamma)*T_htf.at(j,k);

					for( int k = 0; k < m_n_panels; k++ )
						for( int j = 0; j < m_n_nodes; j++ )
							T_htf_ave_guess_mid.at(j,k) = (T_htf_guess_mid.at(j,k) + T_htf_guess_mid.at(j+1,k))/2.0;

					// twn: these are find the max, rather than sum -> poorly named?
					errorsum_temp = 0.0;
					errorsum_flow = 0.0;		
					switch(flow_pattern)
					{
					case 1:
					case 2:
						
						for( int j = 0; j < m_n_panels; j++ )
						{
							// Check if panel has a significant mass flow rate
							if( m_htf_guess.at(j,0) == 0.0 )
							{
								// Don't include the inactive panels in the convergence calculation
								error_temp.at(j,0) = 0.0;
								error_flow.at(j,0) = 0.0;
							}
							else
							{
								// Temperature error for each flow path
								error_temp.at(j,0) = fabs( (T_htf_hot_guess.at(j,0) - T_htf_hot)/T_htf_hot );
								error_flow.at(j,0) = fabs( (m_htf.at(j,0) - m_htf_guess.at(j,0))/m_htf_guess.at(j,0) );
							}
							errorsum_temp = max( errorsum_temp, error_temp.at(j,0) );
							errorsum_flow = max( errorsum_flow, error_flow.at(j,0) );
						}
						

						break;	// flow_pattern = 1 & 2

					case 3:
					case 4:
						errorsum_temp = fabs( (T_htf_hot_guess.at(0,0) - T_htf_hot)/T_htf_hot );
						errorsum_flow = fabs( (m_htf.at(0,0) - m_htf_guess.at(0,0))/m_htf_guess.at(0,0) );

						break;	// flow_pattern = 3 & 4

					case 5:
					case 6:
					case 7:
					case 8:
						for( int j = 0; j < 2; j++ )
						{
							errorsum_temp = max( errorsum_temp, fabs((T_htf_hot_guess.at(j,0) - T_htf_hot)/T_htf_hot) );
							errorsum_flow = max( errorsum_flow, fabs( fabs(m_htf.at(j,0) - m_htf_guess.at(j,0))/m_htf_guess.at(j,0) ) );
						}
						break;	// flow_pattern = 6
					}
					// **************************************************************																				

					// Set the variables equal to their newly calculated values
					for( int i = 0; i < m_n_panels*m_n_nodes+4; i++ )
						T_s_1D.at(i,0) = T_s_guess_1D.at(i,0);

					for( int i = 0; i < m_n_panels; i++ )
						for( int j = 0; j < m_n_nodes; j++ )
							T_s.at(j,i) = T_s_guess.at(j,i);

					for( int i = 0; i < m_n_nodes+1; i++ )
						for( int j = 0; j < m_n_panels; j++ )
							T_htf.at(i,j) = T_htf_guess.at(i,j);

					for( int i = 0; i < m_n_nodes; i++ )
						for( int j = 0; j < m_n_panels; j++ )
							T_htf_ave.at(i,j) = T_htf_ave_guess.at(i,j);

					for( int i = 0; i < m_n_panels; i++ )
						m_htf.at(i,0) = m_htf_guess.at(i,0);

					for( int i = 0; i < m_n_panels; i++ )
						for( int j = 0; j < m_n_nodes; j++ )
							T_s_guess.at(j,i) = T_s_guess_mid_1D.at(j+(i*m_n_nodes),0);

					for( int j = 0; j < m_n_panels*m_n_nodes+4; j++ )
						T_s_guess_1D.at(j,0) = T_s_guess_mid_1D.at(j,0);	//ST: mid temperature - successive relaxation iteration method

					for( int i = 0; i < m_n_nodes; i++ )
						for( int j = 0; j < m_n_panels; j++ )
							T_htf_ave_guess.at(i,j) = T_htf_ave_guess_mid.at(i,j);

					for( int i = 0; i < m_n_nodes+1; i++ )
						for( int j = 0; j < m_n_panels; j++ )
							T_htf_guess.at(i,j) = T_htf_guess_mid.at(i,j);
	
					for( int k = 0; k < m_n_panels; k++ )
						for( int j = 0; j < m_n_nodes; j++ )												
							T_htf_ave_guess_1D.at(j+k*m_n_nodes,0) = T_htf_ave_guess.at(j,k);

					// Total convergence criterium
					if(errorsum_temp < tolerance && errorsum_flow < tolerance)  break;

					// if the problem fails to converge after the maximum number of iterations, 
					// the relaxation factor is reduced, the calculation restarted to obtain convergence
				
					// if the program fails to convege for the smallest convergence factor
					// then the total power on the receiver is likely to be negligible
					// and the mass flow rate and power outputs will be set to zero. The temperatures are set to the cold HTF temp
								
					if( (qq > qq_max) && (gamma<0.1) )
					{
						// Set null outputs
						mode = 0;
						NullOutputs();
						return 0;
					}
								
					if(qq > qq_max)
					{
						gamma_count = gamma_count+1;  
						gamma = gamma_calc_array[gamma_count];
						cycle_out_loop = true;				// logic to restart the qq-loop with a smaller relaxation factor
						break;	
					}   																																																																						
				}	// End of 'qq' iteration loop
																																													
				if( cycle_out_loop )	// If qq loop determined that gamma should be reset, cycle back (continue) to beginning of outer loop
				{
					cycle_out_loop = false;
					continue;
				}

				T_F = T_s_1D.at((size_t)m_n_panels*m_n_nodes, (size_t)0.0);
				T_CE = T_s_1D.at((size_t)m_n_panels*m_n_nodes + 1, (size_t)0.0);
				T_L = T_s_1D.at((size_t)m_n_panels*m_n_nodes + 2, (size_t)0.0);
																												
				// Thermal radiation losses are calculated using the NEW surface temperatures
				for( int i = 0; i < m_n_nodes*m_n_panels+4; i++ )
					for( int j = 0; j < m_n_nodes*m_n_panels+4; j++ )
					{						        
						// Multi band - model
						double sum_rad_therm = 0.0;
						for( int l = 0; l < n_bands; l++ )
							sum_rad_therm += f_temp_band.at(i,l)*e_band_array.at(i,l)*A_array[i]*pow(T_s_1D.at(i,0),4)*F_hat.at(i,j,l)*e_band_array.at(j,l) - f_temp_band.at(j,l)*e_band_array.at(j,l)*A_array[j]*pow(T_s_1D.at(j,0),4)*F_hat.at(j,i,l)*e_band_array.at(i,l);
						q_rad_therm.at(i,j) = CSP::sigma*sum_rad_therm;
						q_rad_semi_gray.at(i,j) = q_rad_therm.at(i,j) + q_rad_solar.at(i,j);
					}

				for( int i = 0; i < m_n_panels*m_n_nodes+4; i++ )
				{
					q_rad_therm_net.at(i,0) = 0.0;
					for( int j = 0; j < m_n_panels*m_n_nodes+4; j++ )
						q_rad_therm_net.at(i,0) += q_rad_therm.at(i,j);
					q_rad_semi_gray_net.at(i,0) = q_rad_therm_net.at(i,0) + q_rad_solar_net.at(i,0);
				}

				// Convective loss from each surface
				for( int i = 0; i < m_n_panels*m_n_nodes+3; i++ )
					q_conv.at(i,0) = A_array[i] * h_conv.at(i,0)*(T_s_1D.at(i,0) - T_bulk);

				// Calculate the total radiation losses out of the aperature
				// twn: only Q_radiation_loss is used in this loop, the rest of these calculations could be moved to after loop
				Q_radiation_loss = fabs(q_rad_semi_gray_net.at(m_n_panels*m_n_nodes+3,0));
				Q_radiation_loss_solar = fabs(q_rad_solar_net.at(m_n_panels*m_n_nodes+3,0));
				Q_radiation_loss_therm = fabs(q_rad_therm_net.at(m_n_panels*m_n_nodes+3,0));
				Q_radiation_loss_semi_sum = Q_radiation_loss_therm + Q_radiation_loss_solar;

				// ST: area averaged surface temperature determined
				double sum_Ts_a = 0.0;
				for( int i = 0; i < m_n_nodes; i++ )
					for( int j = 0; j < m_n_panels; j++ )
						sum_Ts_a += T_s.at(i,j) * A_node;
				T_s_ave = (sum_Ts_a + T_F*A_f + T_CE*A_f + T_L*A_lip)/(2.0*A_f + A_lip + m_n_nodes*m_n_panels*A_node);
				A_cavity = m_n_nodes*m_n_panels*A_node + 2.0*A_f + A_lip;

				q_convection_guess = q_convection;		//ST coupled convection

				//ST Clausing1983 is called to determine T_bulk
				cavity.ConvectionClausing1983( m_n_panels, T_s, T_F, T_CE, T_L, T_amb, P_amb, A_node, Q_radiation_loss, q_convection_Clausing1983, h_F, h_ave, h_stag, T_stag, T_bulk, S );

				// Calculate the total natural convection heat losses out of the aperture [W]
				if( conv_model == 1)	// Clausing 1983
				{
					q_convection = q_convection_Clausing1983;
					h_clausing1983 = q_convection_Clausing1983/(A_cavity*(T_s_ave - T_amb));
				}
				else
				{
					cavity.ConvectionClausing1987( m_n_panels, T_s, T_F, T_amb, P_amb, q_convection_Clausing1987 );
					q_convection = q_convection_Clausing1987;
					h_clausing1987 = q_convection_Clausing1987/(A_cavity*(T_s_ave - T_amb));
				}

				if( conv_forced == 1)
				{
					// Add forced convection model
				}
				else
				{
					h_SK_forced = 0.0;
				}

				if( conv_coupled == 1 )
				{

				}
				else if( conv_coupled == 2 )
				{
					// uncoupled convection- no local convection heat transfer coefficients are determined; 
					// convection loss is considered as reduction of useful energy gain of the HTF
				
					h_conv.fill( 0.0 );
				
					// Sum of natural and forced convection is calculated
					q_convection = q_convection + ( h_SK_forced*(h_rec-h_lip)*4*W_panel*(T_s_ave-T_amb) );
				
					// Substract the convection losses from the total energy gain in the HTF
					q_htf_total = q_htf_total - q_convection*hl_ffact;			// MJW 9.8.2010:: Add convection heat loss multiplier into eqn. (hl_ffact)

					err_coupled_conv = 0.0;			// 11.9.12 twn: get out of loop if not coupled!
				}				
			}
			// Total mass flow rate is adjusted accounting for the convection losses
			m_htf_total = q_htf_total / (c_htf*(T_htf_hot - T_htf_cold));

			// In case the code produced unrealistic negative numbers because of convergence issues, all outputs are set to 0.
			if( (m_htf_total < 0.0) || (q_htf_total < 0.0) )
			{
				// Set nightime outputs
				mode = 0;
				NullOutputs();
				return 0;
			}
			
			// mjw 3.29.11 Limit the HTF mass flow rate to the maximum, if needed. 
			if( (m_htf_total > m_dot_htf_max) || (itermode == 2) )
			{
				err_od = (m_htf_total - m_dot_htf_max)/m_dot_htf_max;
				if( err_od < tol_od )
				{
					itermode = 1;    
					od_control = 1.0;
					defocus_rec = false;
				}
				else
				{
					od_control = od_control*pow( (m_dot_htf_max/m_htf_total), 0.8 );  // adjust the over-design defocus control by modifying the current value
					itermode = 2;
					defocus_rec = true;
				}
			}									
		}

		// After convergence, determine whether the mass flow rate falls below the lower limit
		if( m_htf_total < m_dot_htf_min )
		{
			// Set outputs to 0 and get out
			mode = 0;
			NullOutputs();
			return 0;
		}

		// ----Startup
		if( (E_su_prev > 0.) || (t_su_prev > 0.) )		// mjw 3.10.11
		{
		    E_su = max(0.0, E_su_prev - m_htf_total*c_htf*(T_htf_hot - T_htf_cold)*(step/3600.0));
		    t_su = max(0.0, t_su_prev - step/3600.0);
		    if(E_su + t_su > 0.0)
			{
		        mode = 1;		// If either are greater than 0, we're starting up but not finished
		        q_startup = (E_su_prev - E_su)/(step/3600.0)*1.0E-6;			// mjw 3.10.11
		        // goto 900  !mjw 3.10.11
				// Set nightime outputs
				NullOutputs();
				return 0;
			}
		    else				// Only part of the timestep/energy was needed to startup.  
			{
		        mode= 2;
		        // Adjust the available mass flow to reflect startup
		        m_htf_total = min( (1.0-t_su_prev/(step/3600.0))*m_htf_total, m_htf_total - E_su_prev/(step/3600.0*c_htf*(T_htf_hot - T_htf_cold)) );
			}
		}
		else
		{
			E_su = 0.0;
			t_su = 0.0;
		}
		q_startup = (E_su_prev - E_su)/(step/3600.0)*1.E-6;       // Convert W-hr to MW  mjw 3.10.11
		
		// Thermal receiver efficiency
		eta_thermal = q_htf_total/q_solar_total;

		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		// Calculation of the required mechanical energy to move the working fluid
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		//Pressure drop in the tubes in every node
		for( int j = 0; j < m_n_panels; j++ )
			for( int i = 0; i < m_n_nodes; i++ )
				deltaP_node.at(i,j) = rho_htf_p.at(j,0)*f_htf_p.at(j,0)*pow(u_htf_p.at(j,0),2)*0.5*(L_tube_node/d_tube_in + (double)m_n_45_bends/(double)m_n_nodes*m_L_e_45 + (double)m_n_90_bends/(double)m_n_nodes*m_L_e_90);
		
		// Mass flow rate weighted averaged pressure drop in a tube [Pa]
		switch( flow_pattern )
		{
		case 1:
		case 2:
			{
				double * deltaP_x_mhtf = new double [m_n_panels];
				for( int j = 0; j < m_n_panels; j++ )
				{
					deltaP_x_mhtf[j] = 0.0;
					for( int i = 0; i < m_n_nodes; i++ )
						deltaP_x_mhtf[j] += deltaP_node.at(i,j) * m_htf.at(j,0);
				}
			
			
				double deltaP_sum = 0.0;
				double m_dot_sum = 0.0;

				for( int j = 0; j < m_n_panels; j++ )
				{
					deltaP_sum += deltaP_x_mhtf[j];
					m_dot_sum += m_htf.at(j,0);
				}

				delete [] deltaP_x_mhtf;

				deltaP_ave = deltaP_sum/m_dot_sum;
			}

			break;	// flow_pattern = 1 & 2

		case 3:
		case 4:

			deltaP_ave = 0.0;
			for( int j = 0; j < m_n_panels; j++ )
			{
				for( int i = 0; i < m_n_nodes; i++ )
					deltaP_ave += deltaP_node.at(i,j);
			}
			break;	// flow_pattern = 3 & 4

		case 5:
		case 6:
		case 7:
		case 8:
			double * deltaP_x_mhtf = new double[m_n_panels];
			for( int j = 0; j < m_n_panels/2; j++ )
			{
				deltaP_x_mhtf[j] = 0.0;
				for( int i = 0; i < m_n_nodes; i++ )
					deltaP_x_mhtf[j] += deltaP_node.at(i,j) * m_htf.at(0,0);
			}
			for( int j = m_n_panels/2; j < m_n_panels; j++ )
			{
				deltaP_x_mhtf[j] = 0.0;
				for( int i = 0; i < m_n_nodes; i++ )
					deltaP_x_mhtf[j] += deltaP_node.at(i,j) * m_htf.at(1,0);
			}
			deltaP_ave = 0.0;
			for( int j = 0; j < m_n_panels; j++ )
				deltaP_ave += deltaP_x_mhtf[j]/(m_htf.at(0,0) + m_htf.at(1,0));
			delete [] deltaP_x_mhtf;

			break;	// flow_pattern = 6
		}
		
		// Density of the cold working fluid [kg/m3]
		double rho_htf_cold = rec_htf.dens( T_htf_cold, P_htf );
		// Pressure loss over the tower height [Pa] 
		double deltaP_THT = rho_htf_cold*h_tower*CSP::grav;
		//Pump power calculation
		double est_load = max(0.25, m_htf_total/m_dot_htf_des)*100.0;                 // MJW 8.26.2010. Calculate the relative pump load. Limit to 25%
		double eta_pump_adj = eta_pump*(-2.8825E-09*pow(est_load,4) + 6.0231E-07*pow(est_load,3) - 1.3867E-04*pow(est_load,2) + 2.0683E-02*est_load);  // Calculate the adjusted pump efficiency
		double W_pump = m_htf_total*(deltaP_THT/rho_htf_cold+deltaP_ave/rho_htf)/eta_pump_adj;	// source: Fox et al, pp354

		// Energy absorbed by the coolant [W]
		double Q_thermal = q_htf_total;
		// Convective heat losses from the receiver [W]

		double Q_convection_loss;
		if( conv_model == 1)	
		{
			// Add equations for coupled convection
		}
		else
		{
			Q_convection_loss = q_convection*hl_ffact;		
		}
		// Hot outlet temperature [K]
		double T_htf_hot_out = T_htf_hot;
	
		double flux_ave = 0.0;
		for( int i = 0; i < m_n_panels*m_n_nodes + 4; i++ )
			flux_ave += flux_1D.at(i,0)/1000.0/(m_n_panels*m_n_nodes);																		
		
		double availability;
		if( Q_thermal == 0 )
			availability = 0;
		else
			availability = 1;

		value( O_m_htf_total, m_htf_total*3600.0 );  
		value( O_eta_therm, eta_thermal );    
		value( O_W_pump, W_pump/1.0E6 );       
		value( O_Q_conv_loss, Q_convection_loss/1.0E6 );  
		value( O_Q_rad_loss, Q_radiation_loss/1.0E6 );   
		value( O_Q_thermal, Q_thermal/1.0E6 );    
		value( O_T_htf_hot, T_htf_hot_out - 273.15 );    
		value( O_Q_rec_abs, value( O_Q_thermal ) + fabs( value( O_Q_rad_loss ) + value( O_Q_conv_loss ) ) );    
		value( O_field_eff_adj, field_eff_adj );
		value( O_Q_solar_total, q_solar_total/1.0E6 );				// 1.10.14, twn: Change to pre-defocus?
		value( O_Q_startup, q_startup );   
		value( O_availability, availability ); 
		value( O_Q_rad_solar, Q_radiation_loss_solar/1.0E6 );  
		value( O_Q_rad_therm, Q_radiation_loss_therm/1.0E6 );

		/* ofstream out_file;
		out_file.open("fish.txt");
		for( int j = 0; j < n_nodes; j++ )
			for( int i = 0; i < n_panels; i++ )
				out_file << UA.at(j,i) << endl; */

		return 0;
	}

	virtual int converged( double /*time*/ )
	{
		if( mode == 0 )
		{
			E_su = q_rec_des * rec_qf_delay;
			t_su = rec_su_delay;
		}

		// Only for test, set to TRNSYS values at time==36 
		// ***********************************************
		// E_su = 0.0;
		// t_su = 0.0;
		// ***********************************************
		// ***********************************************
		
		E_su_prev = E_su;
		t_su_prev = t_su;

		itermode = 1;
		od_control = 1;

		return 0;
	}

	void NullOutputs()
	{
		value( O_m_htf_total, 0.0 );  
		value( O_eta_therm, 0.0 );    
		value( O_W_pump, 0.0 );       
		value( O_Q_conv_loss, 0.0 );  
		value( O_Q_rad_loss, 0.0 );   
		value( O_Q_thermal, 0.0 );   
		//******* Fix this!!! ******
		//value( O_T_htf_hot, T_htf_cold_des - 273.15 );    
		value( O_T_htf_hot, 290.0 );
		//******************************************
		value( O_Q_rec_abs, 0.0 );    
		value( O_field_eff_adj, 0.0 );
		value( O_Q_solar_total, 0.0 );
		value( O_Q_startup, 0.0 );   
		value( O_availability, 0.0 ); 
		value( O_Q_rad_solar, 0.0 );  
		value( O_Q_rad_therm, 0.0 );
		return;
	}

};

void TranslateFluxArray( util::matrix_t<double> & fluxarray_2D, int &n_nodes, int &n_panels, util::matrix_t<double> & solar_flux )
{
	util::matrix_t<double> fluxarray1( 10, n_panels, 0.0 );

	if( n_panels == 4 )
	{
		for( int i = 0; i < 10; i++ )
		{
			fluxarray1.at(i,0) = (fluxarray_2D.at(i,0) + fluxarray_2D.at(i,1) + fluxarray_2D.at(i,2))/3.0;
			fluxarray1.at(i,1) = (fluxarray_2D.at(i,3) + fluxarray_2D.at(i,4) + fluxarray_2D.at(i,5))/3.0;
			fluxarray1.at(i,2) = (fluxarray_2D.at(i,6) + fluxarray_2D.at(i,7) + fluxarray_2D.at(i,8))/3.0;
			fluxarray1.at(i,3) = (fluxarray_2D.at(i,9) + fluxarray_2D.at(i,10) + fluxarray_2D.at(i,11))/3.0;
		}
	}

	if( n_nodes == 5) 
	{
		for( int i = 0; i < n_panels; i++ )
		{
			solar_flux.at(0,i) = (fluxarray1.at(0,i) + fluxarray1.at(1,i))/2.0;
			solar_flux.at(1,i) = (fluxarray1.at(2,i) + fluxarray1.at(3,i))/2.0;
			solar_flux.at(2,i) = (fluxarray1.at(4,i) + fluxarray1.at(5,i))/2.0;
			solar_flux.at(3,i) = (fluxarray1.at(6,i) + fluxarray1.at(7,i))/2.0;
			solar_flux.at(4,i) = (fluxarray1.at(8,i) + fluxarray1.at(9,i))/2.0;
		}		
	}

	return;
}

void PipeFlowCavity( double Re, double Pr, double LoverD, double relRough, double q_solar_total, int is_fd, double & Nusselt, double & f )
{
	/* *********************************************************************
	!* PipeFlow_turbulent:              *
	!* This procedure calculates the average Nusselt number and friction *
	!* factor for turbulent flow in a pipe given Reynolds number (Re),   *
	!* Prandtl number (Pr), the pipe length diameter ratio (LoverD) and  *
	!* the relative roughness}             *
	!********************************************************************* */

	// REGRESSION MODELS FOR ESTIMATING THE REYNOLDS NUMBERS BASED ON THE INCOMING SOLAR RADIATION
	// SERVES AS GUESS VALUES IN case A NEGATIVE REYNOLDS NUMBERS IS PROVIDED
	if (Re < 0.0)
	{
	    if(q_solar_total > 2.0E+7)
	        Re = -5979.08 + 0.00266426*q_solar_total;
	    else if(q_solar_total > 3.69E+06)
	        Re = -14267.6 + 0.00410787*q_solar_total - 6.40334E-11*pow(q_solar_total,2);
	    else
	        Re = 0.001174*q_solar_total;
	}

	// GUESS VALUE FOR THE PRANDTL NUMBER IN case NEGATIVE NUMBERS IS PROVIDED
	if (Pr < 0.0)
	    Pr = 5.0;
	
	// Correlation for laminar flow.. Note that no transitional effects are considered
	if (Re < 2300.)
	{
	    // This procedure calculates the average Nusselt number and friction factor for laminar flow in a pipe
	    // given Reynolds number (Re), Prandtl number (Pr), the pipe length diameter ratio (LoverD)
	    // and the relative roughness
	    double Gz = Re*Pr/LoverD;	// Eq. 5-79 Nellis and Klein
	    //double x = LoverD/Re;     // Eq. 5-58 Nellis and Klein
	    //double fR = 3.44/sqrt(x)+(1.25/(4*x)+16-3.44/sqrt(x))/(1+0.00021*pow(x,-2));	// Eq. 5-57 () Nellis and Klein
	    //double f = 4.0*fR/Re;		// Eq. 5-57 Nellis and Klein
	    double Nusselt_T = 3.66+((0.049+0.02/Pr)*pow(Gz,1.12))/(1.0+0.065*pow(Gz,0.7));	// Eq. 5-80 Nellis and Klein
		//double Nusselt_H = 4.36+((0.1156 +0.08569/pow(Pr,0.4)*Gz)/(1.0+0.1158*pow(Gz,0.6)));	// Eq. 5-81 Nellis and Klein
	    Nusselt = Nusselt_T;		// Constant temperature Nu is better approximation
	}  
	else
	{
		// Correlation for turbulent flow
		double f_fd, Nusselt_L;	    	    
	    if (relRough > 1e-5)	// Duct with surface roughness
		{
	        f_fd = pow((-2.0*log10(2.0*relRough/7.4-5.02*log10(2.0*relRough/7.4+13/Re)/Re)),-2);	// Eq. 5-65
		}
		else	// Aerodynamically smooth duct
		{
			f_fd = pow((0.79*log(Re)-1.64),-2);			// Eq. 5-63 Nellis and Klein
		}
		Nusselt_L= ((f_fd/8.)*(Re-1000.0)*Pr)/(1.0+12.7*sqrt(f_fd/8.)*(pow(Pr,(2.0/3.0))-1.0));	// Eq. 5-84
		if(is_fd == 0)	// Flow is fully developed, don't account for developing flow
		{
			f = f_fd;
			Nusselt = Nusselt_L;
		}
		else
		{
			f = f_fd*(1.0+pow((1.0/LoverD),0.7));		// Eq. 5-66 Nellis and Klein: account for developing flow
			Nusselt = Nusselt_L*(1.0+pow((1.0/LoverD),0.7));	// !account for developing flow
		}
	}

	return;
}

void FractionFunction( int n_nodes, int n_panels, int n_band, util::matrix_t<double> & T_s_guess_1D, util::matrix_t<double> & lambda_step_band, 
							util::matrix_t<double> & f_temp_band, util::matrix_t<double> & f_solar_band)
{
    // This subroutine calculates the total convective heat losses from the receiver
    // with the correlation presented in Petukhov and Popov (1963)  !ST ( Heat Exchanger Design Handbook 2008 G.F. Hewitt Section 2.5 II b) )
    // The inputs are:
    //   - lambda_step - emissivity step wavelength [micron]
    //   - T_sX_array - surface temperatures
    //  The outputs are:
    //   - F_Thermal - fraction of blackbody radiaiton at 800K in the wavelength band from 0-lambda_step
    //   - f_solar_band - fraction of blackbody radiaiton at 5800K in the wavelength band from 0-lambda_step
    
	/* !integer,parameter,intent(IN)::N_nodes,N_panels
    integer::N_nodes,N_panels,N_band
    real(8),dimension(N_band-1),intent(IN)::lambda_step_band
    real(8),dimension(N_nodes*N_panels+4),intent(IN)::T_sX_array
    
    !real(8),dimension(N_nodes*N_panels+4)::
    real(8),dimension(N_nodes*N_panels+5,N_band)::f_uni,gamma_array
    real(8),dimension(10)::n
    
    real(8),dimension(N_nodes*N_panels+4,N_band),intent(OUT):: f_temp_band
    real(8),dimension(N_band),intent(OUT):: f_solar_band
    
    real(8)::i,k,T_sun,l
    
    real(8),parameter::pi=3.14159265, C_2=14387.69 */
    
	util::matrix_t<double> gamma_array( n_nodes*n_panels+5, n_band, 0.0 );
	util::matrix_t<double> f_uni( n_nodes*n_panels+5, n_band, 0.0 );

	double T_sun = 5800.0;		// [K] Sun temperature
	double C_2 = 14387.69;

	int n[10];
	for( int i = 0; i < 10; i++ )
		n[i] = i+1;
		
	for( int l = 0; l < n_band - 1; l++ )
	{ 
		// gamma value for temperature dependent fraction
		for( int k = 0; k < n_nodes*n_panels+4; k++ )
			gamma_array.at(k,l) = C_2 / ( lambda_step_band.at(l,0) * T_s_guess_1D.at(k,0) );
		
		// opening has fraction of UNITY
		gamma_array.at( n_nodes*n_panels+4, l ) = C_2 / ( lambda_step_band.at(l,0) * T_sun );

		for( int k = 0; k < n_nodes*n_panels+5; k++ )
		{
			double sum = 0.0;
			for( int i = 0; i < 10; i++ )
				sum += exp(-n[i]*gamma_array.at(k,l))/n[i]*( pow( gamma_array.at(k,l), 3) + (3.0*pow( gamma_array.at(k,l), 2))/n[i] + (6.0*gamma_array.at(k,l))/pow(double(n[i]),2.0) + 6.0/pow(double(n[i]),3) );
						//exp(-n(1:10)*gamma_array(k,l))/n(1:10)*(  gamma_array(k,l)**3+ (3*gamma_array(k,l)**2)/n(1:10) + (6*gamma_array(k,l))/n(1:10)**2 + 6./n(1:10)**3  )
			f_uni.at(k,l) = 15.0/pow(CSP::pi,4) * sum;
		}
	}

	for( int l = 0; l < n_band; l++ )
	{
		if( l == 0 )
		{
			for( int k = 0; k < n_nodes*n_panels+4; k++ )
				f_temp_band.at(k,l) = f_uni.at(k,l);

			f_solar_band.at(l,0) = f_uni.at(n_nodes*n_panels+4,l);
		}

		else if( l == n_band - 1)
		{
			for( int k = 0; k < n_nodes*n_panels+4; k++ )
				f_temp_band.at(k,l) = 1 - f_uni.at(k,l-1);

			f_solar_band.at(l,0) = 1.0 - f_uni.at( n_nodes*n_panels+4,l-1);
		}
		else
		{
			for( int k = 0; k < n_nodes*n_panels+4; k++ )
				f_temp_band.at(k,l) = f_uni.at(k,l) - f_uni.at(k,l-1);

			f_solar_band.at(l,0) = f_uni.at(n_nodes*n_panels+4,l) - f_uni.at(n_nodes*n_panels+4,l-1);
		}
	}
	return;
}

TCS_IMPLEMENT_TYPE( sam_lf_st_pt_type232, "Cavity Receiver Model", "Ty Neises", 1, sam_lf_st_pt_type232_variables, NULL, 1 )

