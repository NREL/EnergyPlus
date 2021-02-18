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
#include "sam_csp_util.h"

using namespace std;

enum{	//Parameters
		P_D_AP,           
		P_RHO,            
		P_N_NS,           
		P_N_EW,           
		P_NS_DISH_SEP,    
		P_EW_DISH_SEP,    
		P_SLOPE_NS,       
		P_SLOPE_EW,       
		P_W_SLOT_GAP,     
		P_H_SLOT_GAP,     
		P_MANUFACTURER,   
		P_WIND_STOW_SPEED,
		P_A_PROJ,         
		P_I_CUT_IN,       
		P_D_AP_TEST,      
		P_TEST_IF,        
		P_TEST_L_FOCAL,   
		P_A_TOTAL,        

		//Inputs
		I_I_BEAM,    
		I_T_AMB,     
		I_WIND_SPEED,
		I_ZENITH,    
		I_P_ATM,     
		I_AZIMUTH,   

		//Outputs
		O_POWER_OUT_COL,       
		O_COLLECTOR_LOSSES,    
		O_ETA_COLLECTOR,       
		O_NUMBER_OF_COLLECTORS,               
		O_I_CUT_IN,            
		O_POWER_IN_REC,        
		O_INTERCEPT_FACTOR,  
		O_D_AP,
		O_POWER_IN_COLLECTOR,  
		O_PHI_SHADE,           

		//N_MAX
		N_MAX};

tcsvarinfo sam_pf_dish_collector_type295_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_NUMBER, P_D_AP,             "d_ap",               "Dish aperture diameter",                       "m",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_RHO,              "rho",                "Mirror surface reflectivity",                  "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_N_NS,             "n_ns",               "Number of collectors North-South",             "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_N_EW,             "n_ew",               "Number of collectors East-West",               "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_NS_DISH_SEP,      "ns_dish_sep",        "Collector separation North-South",             "m",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_EW_DISH_SEP,      "ew_dish_sep",        "Collector separation East-West",               "m",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_SLOPE_NS,         "slope_ns",           "North-South ground slope",                     "%",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_SLOPE_EW,         "slope_ew",           "East-West ground slope",                       "%",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_W_SLOT_GAP,       "w_slot_gap",         "Slot gap width",                               "m",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_H_SLOT_GAP,       "h_slot_gap",         "Slot gap height",                              "m",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_MANUFACTURER,     "manufacturer",       "Dish manufacturer (fixed as 5 = other)",       "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_WIND_STOW_SPEED,	"wind_stow_speed",    "Wind stow speed",                              "m/s", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_A_PROJ,           "A_proj",             "Projected mirror area",                        "m^2", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_I_CUT_IN,         "I_cut_in",           "Insolation cut in value",                      "W/m^2", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_D_AP_TEST,        "d_ap_test",          "Receiver aperture diameter during test",       "m",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TEST_IF,          "test_if",            "Test intercept factor",                        "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TEST_L_FOCAL,     "test_L_focal",       "Focal length of mirror system",                "m",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_A_TOTAL,          "A_total",            "Total Area",                                   "m^2", "", "", ""},

	// INPUTS
	{TCS_INPUT, TCS_NUMBER, I_I_BEAM,           "I_beam",             "Direct normal radiation",                      "kJ/hr-m2", "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_T_AMB,            "T_amb",              "Dry bulb temperature",                         "C",        "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_WIND_SPEED,       "wind_speed",         "Wind velocity",                                "m/s",      "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_ZENITH,           "zenith",             "Solar zenith angle",                           "deg",      "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_P_ATM,            "P_atm",              "Atmospheric pressure",                         "Pa",       "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_AZIMUTH,          "azimuth",            "Solar azimuth angle",                          "deg",      "", "", ""},

	// OUTPUTS
	{TCS_OUTPUT, TCS_NUMBER, O_POWER_OUT_COL,       "Power_out_col",           "Total power from the collector dish",          "kW",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_COLLECTOR_LOSSES,    "Collector_Losses",        "Total collector losses (Incident - P_out)",    "kW",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_COLLECTOR,       "eta_collector",           "Collector efficiency",                         "-",    "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_NUMBER_OF_COLLECTORS,"Number_of_collectors",    "Total number of collectors (n_es*n_ns)",       "-",    "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_I_CUT_IN,            "I_cut_in",                "The cut-in DNI value used in the simulation",  "W/m^2","", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_POWER_IN_REC,        "Power_in_rec",            "Power entering the receiver from the collector","kW",  "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_INTERCEPT_FACTOR,    "Intercept_factor",        "The receiver intercept factor",                 "-",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_D_AP,                "d_ap_out",                "Dish aperture diameter",                        "m",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_POWER_IN_COLLECTOR,  "Power_in_collector",      "Power incident on the collector",               "kW",  "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_PHI_SHADE,           "Phi_shade",               "Dish-to-dish shading performance factor",       "-",   "", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	
	
class sam_pf_dish_collector_type295 : public tcstypeinterface
{
private:
	// Class Instances

	//Parameters
	double m_d_ap;
	double m_rho;
	double m_n_ns;
	double m_n_ew;
	double m_ns_dish_sep;
	double m_ew_dish_sep;
	double m_slope_ns;
	double m_slope_ew;
	double m_w_slot_gap;
	double m_h_slot_gap;
	int m_manufacturer;
	double m_wind_stow_speed;
    double m_A_proj;
    double m_I_cut_in;
	double m_d_ap_test;
	double m_test_if;
	double m_test_L_focal;
	double m_A_total;

	//Stored Variables

	// Calculated
	double m_d_collector;
	double m_x_mirror_gap;
	double m_H_mirror_gap;
	double m_intercept_factor;

public:
	sam_pf_dish_collector_type295( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		m_d_ap = std::numeric_limits<double>::quiet_NaN();
		m_rho = std::numeric_limits<double>::quiet_NaN();
		m_n_ns = std::numeric_limits<double>::quiet_NaN();
		m_n_ew = std::numeric_limits<double>::quiet_NaN();
		m_ns_dish_sep = std::numeric_limits<double>::quiet_NaN();
		m_ew_dish_sep = std::numeric_limits<double>::quiet_NaN();
		m_slope_ns = std::numeric_limits<double>::quiet_NaN();
		m_slope_ew = std::numeric_limits<double>::quiet_NaN();
		m_w_slot_gap = std::numeric_limits<double>::quiet_NaN();
		m_h_slot_gap = std::numeric_limits<double>::quiet_NaN();
		m_manufacturer = -1;
		m_wind_stow_speed = std::numeric_limits<double>::quiet_NaN();
		m_A_proj = std::numeric_limits<double>::quiet_NaN();
		m_I_cut_in = std::numeric_limits<double>::quiet_NaN();
		m_d_ap_test = std::numeric_limits<double>::quiet_NaN();
		m_test_if = std::numeric_limits<double>::quiet_NaN();
		m_test_L_focal = std::numeric_limits<double>::quiet_NaN();
		m_A_total = std::numeric_limits<double>::quiet_NaN();
		m_x_mirror_gap = std::numeric_limits<double>::quiet_NaN();
		m_H_mirror_gap = std::numeric_limits<double>::quiet_NaN();
		m_d_collector = std::numeric_limits<double>::quiet_NaN();
		m_intercept_factor = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_pf_dish_collector_type295()
	{
	}

	virtual int init()
	{
		// Read in parameters
		m_d_ap = value( P_D_AP );
		m_rho = value( P_RHO );
		m_n_ns = value( P_N_NS );
		m_n_ew = value( P_N_EW );
		m_ns_dish_sep = value( P_NS_DISH_SEP );
		m_ew_dish_sep = value( P_EW_DISH_SEP );
		m_slope_ns = value( P_SLOPE_NS );
		m_slope_ew = value( P_SLOPE_EW );
		m_w_slot_gap = value( P_W_SLOT_GAP );
		m_h_slot_gap = value( P_H_SLOT_GAP );
		m_manufacturer = (int)value( P_MANUFACTURER );

		switch( m_manufacturer )
		{
		case 1:				// SES System = 1
			m_A_proj			= 87.7;
			m_wind_stow_speed	= 16.0;
			m_I_cut_in			= 200.0;
			m_d_ap_test			= 0.184;
			m_test_if			= 0.995;
			m_test_L_focal		= 7.45;
			m_A_total			= 91.0;
			break;
			
		case 2:				// WGA System = 2
			m_A_proj			= 41.2;
			m_wind_stow_speed	= 16;
			m_I_cut_in			= 275;
			m_d_ap_test			= 0.14;
			m_test_if			= 0.998;
			m_test_L_focal		= 5.45;
			m_A_total			= 42.9;
			break;

		case 3:				// SBP System = 3	
			m_A_proj			= 56.7;
			m_wind_stow_speed	= 16.0;
			m_I_cut_in			= 250;
			m_d_ap_test			= 0.15;
			m_test_if			= 0.93;
			m_test_L_focal		= 4.5;
			m_A_total			= 60.0;
			break;

		case 4:				// SAIC System = 4
			m_A_proj			= 113.5;
			m_wind_stow_speed	= 16.0;
			m_I_cut_in			= 375.0;
			m_d_ap_test			= 0.38;
			m_test_if			= 0.90;
			m_test_L_focal		= 12.0;
			m_A_total			= 117.2;
			break;

		case 5:				// User input values = 5
			m_wind_stow_speed = value( P_WIND_STOW_SPEED );
			m_A_proj = value( P_A_PROJ );
			m_I_cut_in = value( P_I_CUT_IN );
			m_d_ap_test = value( P_D_AP_TEST );
			m_test_if = value( P_TEST_IF );
			m_test_L_focal = value( P_TEST_L_FOCAL );
			m_A_total = value( P_A_TOTAL );
			break;
		}


		/*--------------------------------------
		Adding code to solve for intercept factor here to reduce time
		The intercept factor only needs to be solved for the first time step
		Theory comes from Stine and Harrigan (1985)
 		-------------------------------*/

		// Constants used to solve for gamma below
		//double pipipi = 3.14159;
		double r = 0.2316419;
		double b1 = 0.319381530;
		double b2 = -0.356563782;
		double b3 = 1.781477937;
		double b4 = -1.82125978;
		double b5 = 1.330274429;

		// Collector diameter
		m_d_collector = 2.0*pow( m_A_total/(CSP::pi+0.0000001), 0.5 );
			
		// Height of parabola for a parabolic concentrator
		double h = pow( m_d_collector, 2 ) / (16.0*m_test_L_focal+0.0000001);

		// Initial guess value for the collector tracking and manufacturer error [mrad]
		double sigma_tot_guess = 0.001;

		// Initial value for intercept_factor_solve
		double intercept_factor_solve = 1.001;
		// beam insolation value used for intercept factor....value doesn't matter
		double Ib = 1000;
		// step used for incrementing the rim angle (differential ring)
		double d_psi = 0.001;	//radians
		// initializing intial power from collector
		double Power_tot = 0.0;
	    double Power_int_tot = 0.0;

		// 1) solve for rim angle
		double psi_rim = atan( 1/(0.0000001+(m_d_collector/(8.0*h+ 0.0000001) - (2.0*h/(m_d_collector+0.0000001)))) );

		// 2) Perform while loop to solve for collector error until appropriate intercept value is achieved
		if( intercept_factor_solve >= m_test_if )
		{
			// 3) Perform do (for) loop to solve for values for each rim angle and sum them
			sigma_tot_guess += 0.000015;
			Power_tot = 0.0;				// need to re-initialize to 0
			Power_int_tot = 0.0;			// need to re-initialize to 0

			for( double psi = 0.0; psi <= psi_rim; psi+=d_psi )
			{
				// 4) Solve for n_std_dev (# or standard deviations) for a specific rim angle
				double omega_n_accurate = m_d_ap_test;

				// p is the distance from point on parabolic reflector to focal point, p.184 Stine
				double DELTAr_accurate = omega_n_accurate * cos(psi);
				double p = 2.0*m_test_L_focal/(1+cos(psi)+0.0000001);
				double n_std_dev = (2.0/sigma_tot_guess) * atan( DELTAr_accurate/(2.0*p+0.0000001));

				// 5) Solve for gamma (intercept factor) for a specific rim angle
				//    gamma is highest at the mirror vertex and decreases near the collector perimeter
				double x = n_std_dev/2.0;
				double t1 = 1.0/(1.0+r*x+0.0000001);
				double fx = 1.0/(pow(2.0*CSP::pi,0.5)+0.0000001)*exp(-(x*x/2.0)+ 0.0000001);
				double Q = fx*(b1*t1 + b2*t1*t1 + b3*pow(t1,3) + b4*pow(t1,4) + b5*pow(t1,5));
				double gamma = 1.0 - 2.0*Q;

				// 6) Solve for d_Power (total power reflected from differential ring) for spec rim angle
				double d_power = (8.0*CSP::pi*Ib*pow(m_test_L_focal,2)*sin(psi)*d_psi)/pow( (1.0+cos(psi)+0.0000001), 2 );

				// 7) Sovle for d_Power_intercept (total power from diff ring intercepted by receiver) for spec rim angle
				double d_power_intercept = d_power*pow(gamma,4);

				// 8) Sum up d_power and d_power_intercept until rim angle is reached
				Power_tot = Power_tot + d_power;
				Power_int_tot = Power_int_tot + d_power_intercept;
			}

			// 9) Divide sum of d_power_intercept by d_power to obtain intercept_factor_solve
			intercept_factor_solve = Power_int_tot / (Power_tot + 0.0000001);
		}

		//==============================================================================
		// Solve for intercept factor when changing the aperture diameter
		//==============================================================================


		double sigma_tot = sigma_tot_guess;		// error in collector solved above in loops
	
		// re-initialize values
		Power_tot = 0.0;
		Power_int_tot = 0.0;
		
		m_intercept_factor = 1.0;
		if(m_d_ap == m_d_ap_test)			
			m_intercept_factor = m_test_if;
		else
		{
			// 2) Perform while loop to solve for collector error until appropriate intercept value is achieved

			// 3) Perform do (for) loop to solve for values for each rim angle and sum them

			for( double psi = 0.0; psi <= psi_rim; psi+=d_psi )
			{
				// 4) solve for n_std_dev (# of standard deviations) for a specific rim angle 
				double omega_n_accurate = m_d_ap;

				// p is the distance from point on parabolic reflector to focal point p.184 Stine            	
				double DELTAr_accurate = omega_n_accurate * cos(psi);
				double p = 2.0*m_test_L_focal/(1.0+cos(psi)+0.0000001);
				double n_std_dev=(2/sigma_tot) * atan(DELTAr_accurate/(2.0*p+0.0000001));

				// 5) solve for gamma (intercept factor) for a specific rim angle
				//    gamma is highest at the mirror vertex and decreases near the collector perimeter
		
				double x = n_std_dev/2.0; 
				double t1 = 1.0/(1.0+r*x+0.0000001);
				double fx = 1.0/(pow((2*CSP::pi),0.5)+0.0000001)*exp(-(x*x/2.0)+0.0000001);
				double Q = fx*(b1*t1 + b2*pow(t1,2) + b3*pow(t1,3) + b4*pow(t1,4) + b5*pow(t1,5));
				double gamma = 1.0 - 2.0*Q;  

				// 6) solve for d_Power (total power reflected from differential ring) for spec rim angle
				double d_power=(8.0*CSP::pi*Ib*pow(m_test_L_focal,2)*sin(psi)*d_psi)/pow( (1.0+cos(psi)+ 0.0000001), 2 );
	
				// 7) solve for d_Power_intercept (total power from diff ring intercepted by receiver) for spec rim angle
				// !d_Power_intercept may be more accurate raising gamma to power of 2-->4
				double d_power_intercept = d_power * pow( gamma, 4 );  
	
				// 8) sum up d_Power & d_Power_intercept until rim angle reached

				Power_tot = Power_tot + d_power;
				Power_int_tot = Power_int_tot + d_power_intercept;
			}

			// Divide sum of d_Power_intercept by d_Power to obtain intercept_factor_solve
			m_intercept_factor = Power_int_tot / (Power_tot+ 0.0000001);
		}
	
		return 0;
	}

	virtual int call( double /*time*/, double /*step*/, int /*ncall*/ )
	{						
		double I_beam_in = value( I_I_BEAM );			//[W/m^2]
		//double T_amb_in = value( I_T_AMB );
		double wind_speed = value( I_WIND_SPEED );
		double sun_angle_in = value( I_ZENITH );
		//double P_atm_in = value( I_P_ATM )*100.0;			//[Pa] Ambient pressure, convert from mbar
		double solar_azimuth = value( I_AZIMUTH ) - 180.0;	//[deg] Convert to TRNSYS convention

		m_x_mirror_gap = m_w_slot_gap;
		m_H_mirror_gap = m_h_slot_gap;

		double DNI = I_beam_in;

		//==============================================================================
		// Paul Fraser Collector Array Shading Code 2008
		//==============================================================================

		// elevation angle referenced from the horizon
		double elevation_angle = (90.0-sun_angle_in)*2.0*3.142/360;		// convert to radians

		// azimuth_angle   referenced from the South
		double azimuth_angle = ((solar_azimuth)*2.0*3.142/360);			// convert to radians
           
		//!d_collector:  above	
		//NNS  number dishes North-South
		//NEW  number of dishes East-West
	
		double zero = 0.0;
		double Number_of_Collectors = m_n_ew*m_n_ns;
		double N_rect = 99;								//[-] number of rectangles dish is broken up into
		double phi_A = azimuth_angle;					//[rad] azimuth angle
		double phi_E = elevation_angle;					//[rad] elevation angle

		double L_NS = m_ns_dish_sep;					//"dish separation distance N-S"
		double L_EW = m_ew_dish_sep;
		double D_dish = m_d_collector;					//diameter of dish
		double r_dish = D_dish / 2.0;					//radius of dish
		double w_rect = D_dish / N_rect;				//width of differential rectangle for shading

		double rise_NS = 0.01*m_slope_ns*L_NS;			//distance the dish moves vertically based on slope
		double rise_EW = 0.0;
		if (phi_A < 0.0)
			rise_EW = 0.01*m_slope_ew*L_EW;
		else
			rise_EW = -0.01*m_slope_ew*L_EW;

		double slope_diag = 0.0;
		if (phi_A <= 0.0)
			slope_diag = (rise_NS + rise_EW) / pow( (pow(L_NS,2)+pow(L_EW,2)) , 0.5 );
		else
			slope_diag = (rise_NS - rise_EW) / pow( (pow(L_NS,2)+pow(L_EW,2)) , 0.5 );

		double rise_diag = 0.01*slope_diag*pow( (pow(L_NS,2)+pow(L_EW,2)) , 0.5 );
		//double phi_slope_diag = atan(rise_diag/pow( (pow(L_NS,2)+pow(L_EW,2)) , 0.5 ));
	
		// N-S shade
		double x_A = sin(phi_A)*L_NS;		// distance shading line from south dish is offset from center of north dish x-direction
		double y_B = pow( pow(L_NS,2) - pow(x_A,2) , 0.5);		// distance shading line from south dish is offset from center of north dish y-direction

		// E-W shade 
		double x_A_EW = sin( CSP::pi/2.0 - phi_A )*L_EW;		// distance shading line from south dish is offset from center of north dish x-direction
		double y_B_EW = pow( pow(L_EW,2) - pow(x_A_EW,2) , 0.5);		// distance shading line from south dish is offset from center of north dish y-direction

		// Diagonal shade
		double phi_diag = atan(L_NS/L_EW);
		double phi_diag_pt = CSP::pi/2.0 - fabs(phi_A) - phi_diag;
		double x_A_diag = sin(phi_diag_pt) * pow( pow(L_EW,2)+pow(L_NS,2), 0.5 );
		double y_B_diag = pow( pow(L_EW,2)+pow(L_NS,2) - pow(x_A_diag,2), 0.5 );   

		// ==================="
		double N_loop_1 = (-N_rect+1)/2.0;				// # of rectangles to the left of center......loop starts at center of differential shading rectangle at the left of the first dish
		double N_loop_2 = (N_rect-1)/2.0;				// # of rectangles to the right of center......loop ends at center of differential shading rectangle at the right of the first dish
		double one = 1.0;
		double A_shade_NS = 0.0;				// initialize N-S shading
		double A_shade_EW = 0.0;				// initialize E-W shading
		double A_shade_diag = 0.0;
		double NS_shade = 0.0;					// initialize
		double EW_shade = 0.0;					// initialize
		double diag_shade = 0.0;

		for( int N_loop = (int)N_loop_1; N_loop <= (int)N_loop_2; N_loop+=(int)one )
		{
			double x_dish_1 = N_loop * w_rect;			// position of differential rectangle on x-axis of dish 1 (south)
			double x_dish_2 = N_loop * w_rect - x_A;	// position of differential rectangle on x-axis of dish 2 (north) that the center of dish 1 projects onto"
			double x_dish_2_EW = N_loop * w_rect - x_A_EW;	// point on dish 2 that the pt x_dish_1 on dish 1 projects to
			double x_dish_2_diag = N_loop * w_rect - x_A_diag;
			
			// =========================
			
			if (x_dish_2 < -r_dish)
				NS_shade = 0.0;			// [m^2]
			
			if (x_dish_2 > r_dish)
				NS_shade = 0.0;			// [m^2]
			
			if (x_dish_2_EW < -r_dish)
				EW_shade = 0.0;			// [m^2]
			
			if (x_dish_2_EW > r_dish)
				EW_shade = 0.0;			// [m^2]
			
			if (x_dish_2_diag < -r_dish)
				diag_shade = 0.0;		// [m^2]
			
			if (x_dish_2_diag > r_dish)
				diag_shade = 0.0;		// [m^2]
			
			// =========================

			// Majority of dish outside of center
			
			// N-S shade
			double H_dish_1 = pow( pow(r_dish,2) - pow(x_dish_1,2), 0.5);				// height (radius) between center of each differential rectangle on dish 1 and perimeter of dish 1
			if (x_dish_2 >= -r_dish)
			{
				if (x_dish_2 <=  r_dish)
				{
					double x_shade = w_rect;
					
					double H_dish_2 = pow( pow(r_dish,2) - pow(x_dish_2,2), 0.5);		// "height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2
			
					double y_shade = -(tan(phi_E)*y_B) + H_dish_1 + H_dish_2-rise_NS;
					if (y_shade <= 0)
						y_shade = 0.0;
					if (y_shade > 2.0*H_dish_2)
						y_shade = 2.0*H_dish_2;
					
					NS_shade = x_shade * y_shade;
				}
			}

			// E-W shade
			if (x_dish_2_EW >= -r_dish)
			{
				if (x_dish_2_EW <= r_dish)
				{
					double H_dish_2_EW = pow( pow(r_dish,2) - pow(x_dish_2_EW,2), 0.5 );		// height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2

					double y_shade_EW = -(tan(phi_E)*y_B_EW) + H_dish_1 + H_dish_2_EW-rise_EW;
					double x_shade = w_rect;
					if (y_shade_EW <= 0)
						y_shade_EW = 0.0;
				
					if (y_shade_EW > 2.0*H_dish_2_EW)
						y_shade_EW = 2*H_dish_2_EW;
				
					EW_shade = x_shade * y_shade_EW;
				}
			}

			// Diagional shade
			if (x_dish_2_diag >= -r_dish)
			{
				if (x_dish_2_diag <= r_dish)
				{
					double H_dish_2_diag = pow( pow(r_dish,2) - pow(x_dish_2_diag,2), 0.5 );	// height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2
					
					double y_shade_diag=-(tan(phi_E)*y_B_diag)+H_dish_1+H_dish_2_diag-rise_diag;
					double x_shade = w_rect	;
					if (y_shade_diag <= 0.0)
						y_shade_diag = 0.0;
				
					if (y_shade_diag > 2.0*H_dish_2_diag)
						y_shade_diag = 2*H_dish_2_diag;
				
					diag_shade = x_shade * y_shade_diag;
				}
			}

			// =========================
			// Dish center

			// N-S shade
			if (x_dish_2 >= (-m_x_mirror_gap/2))
			{
				if (x_dish_2 <= (m_x_mirror_gap/2))
				{
					double x_shade = w_rect;
					double H_dish_1 = pow( pow(r_dish,2) - pow(x_dish_1,2), 0.5 );		// height (radius) between center of each differential rectangle on dish 1 and perimeter of dish 1
					double H_dish_2 = pow( pow(r_dish,2) - pow(x_dish_2,2), 0.5 );		// height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2
					double y_shade=-(tan(phi_E)*y_B)+H_dish_1+H_dish_2-m_H_mirror_gap-rise_NS;
					if (y_shade <= 0)
						y_shade = 0.0;
				
					if (y_shade > 2.0*H_dish_2)
						y_shade = 2.0*H_dish_2;
				
					NS_shade = x_shade * y_shade;
				}
			}

			// "E-W shade"
			if (x_dish_2_EW >= (-m_x_mirror_gap/2))
			{
				if (x_dish_2_EW <= (m_x_mirror_gap/2))
				{
					double x_shade = w_rect;
					double H_dish_2_EW = pow( pow(r_dish,2) - pow(x_dish_2_EW,2), 0.5);		// height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2"
					double y_shade_EW=-(tan(phi_E)*y_B_EW)+H_dish_1+H_dish_2_EW-m_H_mirror_gap-rise_EW;
					if (y_shade_EW <= 0)
						y_shade_EW = 0.0;
				
					if (y_shade_EW > 2.0*H_dish_2_EW)
						y_shade_EW = 2.0*H_dish_2_EW;
				
					EW_shade = x_shade * y_shade_EW;
				}
			}
			
			// Diagonal shading

			if (x_dish_2_diag >= (-m_x_mirror_gap/2))
			{
				if (x_dish_2_diag <= (m_x_mirror_gap/2))
				{
					double x_shade = w_rect;
					double H_dish_2_diag = pow( pow(r_dish,2) - pow(x_dish_2_diag,2), 0.5 );	// height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2
					double y_shade_diag = -(tan(phi_E)*y_B_diag) + H_dish_1 + H_dish_2_diag-m_H_mirror_gap - rise_diag;

					if (y_shade_diag <= 0)
						y_shade_diag = 0.0;

					if (y_shade_diag > 2*H_dish_2_diag)
						y_shade_diag = 2*H_dish_2_diag;
				
					diag_shade = x_shade * y_shade_diag;
				}
			}

			A_shade_NS = A_shade_NS + NS_shade;			// sum up total NS shading over dish
			A_shade_EW = A_shade_EW + EW_shade;			// sum up total EW shading over dish
			A_shade_diag = A_shade_diag + diag_shade;
		}

		// ==========
		if (phi_E < 0)
		{
			A_shade_NS = 0.0;
			A_shade_EW = 0.0;
			A_shade_diag = 0.0;
		}

		double A_shade_combined = A_shade_NS + A_shade_EW + A_shade_diag;
		double A_shade_interior = (m_n_ns-1)*(m_n_ew-1)*A_shade_combined;

		double A_shade_exterior;
		if (solar_azimuth <= -90.0)		// Northeast quadrant
			A_shade_exterior = (m_n_ns-1.0)*A_shade_NS + (m_n_ew-1.0)*A_shade_EW;

		if (solar_azimuth >= 0.0)		// Southwest quadrant
		{
			if (solar_azimuth <= 90.0)
				A_shade_exterior = (m_n_ns-1.0)*A_shade_NS + (m_n_ew-1.0)*A_shade_EW;
		}

		if (solar_azimuth >= 90.0)		// Northwest quadrant
			A_shade_exterior = (m_n_ns-1.0)*A_shade_EW + (m_n_ew-1.0)*A_shade_NS;
		

		if (solar_azimuth > -90.0)		// Southeast quadrant
		{
			if (solar_azimuth < 0.0)
				A_shade_exterior = (m_n_ns-1.0)*A_shade_EW + (m_n_ns-1.0)*A_shade_NS;
		}

		double A_shade_tot = A_shade_interior + A_shade_exterior;

		double Shade_AVG=max(zero,(A_shade_tot)/(m_n_ns*m_n_ew));  


		// ===================================================
		// Correct projected area of dish for shading

		// Projected_Area = MAX(zero,Projected_Area - Shade_AVG)
		// performance fraction reduced due to shading
		double phi_shade = max(zero,(m_A_proj-Shade_AVG)/(m_A_proj+0.0000001));
		// ===================================================

		// *************************************************
		//  O U T P U T S
		// *************************************************

		
		// Output: Power_out_col  (total power from collector dish)
		// Turn off Stirling energy production if wind speed past allowable
		// Turn off SE if I_beam_in is less than the system cut in insolation
		if(wind_speed <= m_wind_stow_speed )
		{
			if(DNI >= m_I_cut_in)
			{
				value( O_POWER_OUT_COL, DNI*m_rho*m_A_proj*phi_shade/1000.0 );
				value( O_COLLECTOR_LOSSES, DNI/1000.0*m_A_proj - value( O_POWER_OUT_COL ) );
				value( O_ETA_COLLECTOR, value( O_POWER_OUT_COL) / ( value( O_POWER_OUT_COL ) + value( O_COLLECTOR_LOSSES ) ) );
			}
			else
			{
				value( O_POWER_OUT_COL, 0.0 );
				value( O_COLLECTOR_LOSSES, 0.0 );
				value( O_ETA_COLLECTOR, 0.0 );
			}
		}
		else
		{
			value( O_POWER_OUT_COL, 0.0 );
			value( O_COLLECTOR_LOSSES, 0.0 );
			value( O_ETA_COLLECTOR, 0.0 );
		}

		// Number of Collectors
		value( O_NUMBER_OF_COLLECTORS, Number_of_Collectors );

		value( O_I_CUT_IN, m_I_cut_in );
		
		value( O_POWER_IN_REC, value( O_POWER_OUT_COL ) * m_intercept_factor );

		value( O_INTERCEPT_FACTOR, m_intercept_factor );

		value( O_D_AP, m_d_ap );

		value( O_POWER_IN_COLLECTOR, DNI*m_A_proj/1000.0 );

		value( O_PHI_SHADE, phi_shade );

		return 0;
	}

	virtual int converged( double /*time*/ )
	{
		
		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_pf_dish_collector_type295, "Collector Dish", "Ty Neises", 1, sam_pf_dish_collector_type295_variables, NULL, 1 )

