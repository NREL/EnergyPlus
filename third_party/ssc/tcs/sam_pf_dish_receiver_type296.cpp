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
		P_REC_TYPE,           
		P_TRANSMITTANCE_COVER,
		P_MANUFACTURER,       
		P_ALPHA_ABSORBER,     
		P_A_ABSORBER,         
		P_ALPHA_WALL,         
		P_A_WALL,             
		P_L_INSULATION,       
		P_K_INSULATION,       
		P_D_CAV,              
		P_P_CAV,              
		P_L_CAV,              
		P_DELTA_T_DIR,        
		P_DELTA_T_REFLUX,  
		P_T_HEATER_HEAD_HIGH,
		P_T_HEATER_HEAD_LOW, 

		//Inputs
		I_POWER_IN_REC,      
		I_T_AMB,             
		I_P_ATM,             
		I_WIND_SPEED,        
		I_SUN_ANGLE,           
		I_N_COLLECTORS,      
		I_DNI,               
		I_I_CUT_IN,          
		I_D_AP,              

		//Outputs        
		O_P_OUT_REC,          
		O_Q_REC_LOSSES,       
		O_ETA_REC,            
		O_T_HEATER_HEAD_OPERATE,
		O_Q_RAD_REFLECTION,   
		O_Q_RAD_EMISSION,     
		O_Q_CONV,             
		O_Q_COND,             

		//N_MAX
		N_MAX};

tcsvarinfo sam_pf_dish_receiver_type296_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_NUMBER, P_REC_TYPE,            "rec_type",           "Receiver type (always = 1)",                   "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_TRANSMITTANCE_COVER, "transmittance_cover","Transmittance cover (always = 1)",             "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_MANUFACTURER,        "manufacturer",       "Manufacturer (always=5)",                      "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_ALPHA_ABSORBER,      "alpha_absorber",     "Absorber absorptance",                         "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_A_ABSORBER,          "A_absorber",         "Absorber surface area",                        "m^2", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_ALPHA_WALL,          "alpha_wall",         "Cavity absorptance",                           "-",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_A_WALL,              "A_wall",             "Cavity surface area",                          "m^2", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_L_INSULATION,        "L_insulation",       "Insulation thickness",                         "m",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_K_INSULATION,        "k_insulation",       "Insulation thermal conductivity",              "W/m-K", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_D_CAV,               "d_cav",              "Internal diameter of cavity perp to aperture", "m",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_P_CAV,               "P_cav",              "Internal cavity pressure with aperture covered","kPa", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_L_CAV,               "L_cav",              "Internal depth of cavity perp to aperture",    "m",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_DELTA_T_DIR,         "DELTA_T_DIR",        "Delta temperature for DIR receiver",           "K",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_DELTA_T_REFLUX,      "DELTA_T_REFLUX",     "Delta temp for REFLUX receiver (always = 40)", "K",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_T_HEATER_HEAD_HIGH,  "T_heater_head_high",	"Heater Head Set Temperature",               "K", "", "", ""},   
	{TCS_PARAM, TCS_NUMBER, P_T_HEATER_HEAD_LOW,   "T_heater_head_low",		"Header Head Lowest Temperature",            "K", "", "", ""},

	// INPUTS
	{TCS_INPUT, TCS_NUMBER, I_POWER_IN_REC,        "Power_in_rec",          "Power entering the receiver from the collector", "kW", "", "", ""},    
	{TCS_INPUT, TCS_NUMBER, I_T_AMB,               "T_amb",					"Ambient temperature in Kelvin",                  "K", "", "", ""},   
	{TCS_INPUT, TCS_NUMBER, I_P_ATM,               "P_atm",					"Atmospheric pressure",                           "Pa", "", "", ""},   
	{TCS_INPUT, TCS_NUMBER, I_WIND_SPEED,          "wind_speed",			"Wind velocity",                                  "m/s", "", "", ""},   
	{TCS_INPUT, TCS_NUMBER, I_SUN_ANGLE,           "sun_angle",				"Solar altitude angle",                           "deg", "", "", ""},   
	{TCS_INPUT, TCS_NUMBER, I_N_COLLECTORS,        "n_collectors",			"Total number of collectors (Num N-S x Num E-W)", "-", "", "", ""},    
	{TCS_INPUT, TCS_NUMBER, I_DNI,                 "DNI",					"Direct normal radiation",                        "W/m^2", "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_I_CUT_IN,            "I_cut_in",				"The cut-in DNI value used in the simulation",    "W/m^2", "", "", ""},    
	{TCS_INPUT, TCS_NUMBER, I_D_AP,                "d_ap",					"The aperture diameter used in the simulation",   "m", "", "", ""},     

	// OUTPUTS
	{TCS_OUTPUT, TCS_NUMBER, O_P_OUT_REC,          "P_out_rec",                  "Receiver output power",                     "kW", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_REC_LOSSES,       "Q_rec_losses",				 "Receiver thermal losses",                   "kW", "", "", ""},         
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_REC,            "eta_rec",					 "Receiver efficiency",                       "-", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_HEATER_HEAD_OPERATE, "T_heater_head_operate",	 "Receiver head operating temperature",       "K", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_RAD_REFLECTION,   "rad_reflection",			 "Reflected radiation",                       "kW", "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_Q_RAD_EMISSION,     "rad_emission",				 "Emitted radiation",                         "kW", "", "", ""},         
	{TCS_OUTPUT, TCS_NUMBER, O_Q_CONV,             "q_conv",					 "Total convection losses",                   "kW", "", "", ""},         
	{TCS_OUTPUT, TCS_NUMBER, O_Q_COND,             "q_cond",					 "Conduction losses",                         "kW", "", "", ""},    

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	
	
class sam_pf_dish_receiver_type296 : public tcstypeinterface
{
private:
	// Class Instances

	//Parameters
	double m_receiver_type;
	double m_transmittance_cover;
	int m_manufacturer;
	double m_alpha_absorber;
	double m_A_absorber;
	double m_alpha_wall;
	double m_A_wall;
	double m_L_insulation;
	double m_k_insulation;
	double m_d_cav;
	double m_P_cav;
	double m_L_cav;
	double m_delta_T_DIR;
	double m_delta_T_reflux;
	double m_T_heater_head_high;
	double m_T_heater_head_low;

	//Stored Variables

	// Calculated

public:
	sam_pf_dish_receiver_type296( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		m_receiver_type = std::numeric_limits<double>::quiet_NaN();
		m_transmittance_cover = std::numeric_limits<double>::quiet_NaN();
		m_manufacturer = -1;
		m_alpha_absorber = std::numeric_limits<double>::quiet_NaN();
		m_A_absorber = std::numeric_limits<double>::quiet_NaN();
		m_alpha_wall = std::numeric_limits<double>::quiet_NaN();
		m_A_wall = std::numeric_limits<double>::quiet_NaN();
		m_L_insulation = std::numeric_limits<double>::quiet_NaN();
		m_k_insulation = std::numeric_limits<double>::quiet_NaN();
		m_d_cav = std::numeric_limits<double>::quiet_NaN();
		m_P_cav = std::numeric_limits<double>::quiet_NaN();
		m_L_cav = std::numeric_limits<double>::quiet_NaN();
		m_delta_T_DIR = std::numeric_limits<double>::quiet_NaN();
		m_delta_T_reflux = std::numeric_limits<double>::quiet_NaN();
		m_T_heater_head_high = std::numeric_limits<double>::quiet_NaN();
		m_T_heater_head_low = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_pf_dish_receiver_type296()
	{
	}

	virtual int init()
	{
		m_receiver_type = value( P_REC_TYPE );
		m_transmittance_cover = value( P_TRANSMITTANCE_COVER );
		m_manufacturer = (int) value( P_MANUFACTURER );
		m_P_cav = value( P_P_CAV );

		switch( m_manufacturer )
		{
		case 1:							// SES System = 1
			m_alpha_absorber = 0.90;
			m_A_absorber = 0.6;
			m_alpha_wall = 0.6;
			m_A_wall = 0.6;
			m_L_insulation = 0.075 ;
			m_k_insulation = 0.06;
			m_d_cav = 0.46;
			m_L_cav = m_d_cav;
			m_delta_T_DIR = 90.0;
			m_delta_T_reflux = 40.0;
			m_T_heater_head_high = 993.0;
			m_T_heater_head_low = 973.0;
			break;

		case 2:							// WGA System = 2
			m_alpha_absorber = 0.9;
			m_A_absorber = 0.15;
			m_alpha_wall = 0.6;
			m_A_wall = 0.15;
			m_L_insulation = 0.075;
			m_k_insulation = 0.06;
			m_d_cav = 0.35;
			m_L_cav = m_d_cav;
			m_delta_T_DIR = 70.0;
			m_delta_T_reflux = 30.0;
			m_T_heater_head_high = 903.0;
			m_T_heater_head_low = 903.0;
			break;

		case 3:							// SBP System = 3
			m_alpha_absorber = 0.90;
			m_A_absorber = 0.15;
			m_alpha_wall = 0.6;
			m_A_wall = 0.15;
			m_L_insulation = 0.075;
			m_k_insulation = 0.06;
			m_d_cav = 0.37;
			m_L_cav = m_d_cav;
			m_delta_T_DIR = 70.0;
			m_delta_T_reflux = 30.0;
			m_T_heater_head_high = 903.0;
			m_T_heater_head_low = 903.0;
			break;

		case 4:							// SAIC System = 4
			m_alpha_absorber = 0.90;
			m_A_absorber = 0.8;
			m_alpha_wall = 0.6;
			m_A_wall = 0.8;
			m_L_insulation = 0.075;
			m_k_insulation = 0.06;
			m_d_cav = 0.5;
			m_L_cav = m_d_cav;
			m_delta_T_DIR = 90.0;
			m_delta_T_reflux = 40.0;
			m_T_heater_head_high = 993.0;
			m_T_heater_head_low = 973.0;
			break;

		case 5:
			m_alpha_absorber = value( P_ALPHA_ABSORBER );
			m_A_absorber = value( P_A_ABSORBER );
			m_alpha_wall = value( P_ALPHA_WALL );
			m_A_wall = value( P_A_WALL );
			m_L_insulation = value( P_L_INSULATION );
			m_k_insulation = value( P_K_INSULATION );
			m_d_cav = value( P_D_CAV );
			m_L_cav = value( P_L_CAV );
			m_delta_T_DIR = value( P_DELTA_T_DIR );
			m_delta_T_reflux = value( P_DELTA_T_REFLUX );
			m_T_heater_head_high = value( P_T_HEATER_HEAD_HIGH );
			m_T_heater_head_low = value( P_T_HEATER_HEAD_LOW );
			break;

		default:
			message( TCS_ERROR, "Manufacturer integer needs to be from 1 to 5" );
			return -1;
		}

		return 0;
	}

	virtual int call( double /*time*/, double /*step*/, int /*ncall*/ )
	{		
		double Power_in = value( I_POWER_IN_REC );
		double T_amb = value( I_T_AMB ) + 273.15;
		double P_atm = value( I_P_ATM ) * 100.0;
		double wind_speed = value( I_WIND_SPEED );
		double sun_angle = 90.0 - value( I_SUN_ANGLE );
		//double N_collectors = value( I_N_COLLECTORS );
		//double T_heater_head_high = value( I_T_HEATER_HEAD_HIGH );
		//double T_heater_head_low = value( I_T_HEATER_HEAD_LOW );
		//double DNI = value( I_DNI );
		//double I_cut_in = value( I_I_CUT_IN );
		double d_ap = value( I_D_AP );		

		/*========================================================
		Determine average receiver temperature for a DIR or reflux receiver
		Reflux receivers should have a lower receiver temp and higher 
		heater head operating temp.....ie....lower receiver losses 
		and higher engine efficiency */

		double T_rec_ave, T_heater_head_operate;
		if( m_receiver_type == 1 )					// DIR
		{
			T_rec_ave = m_T_heater_head_high + m_delta_T_DIR;
			T_heater_head_operate = m_T_heater_head_low;
		}
		else if( m_receiver_type == 2 )
		{
			T_rec_ave = m_T_heater_head_high + 100.0 + m_delta_T_reflux;
			T_heater_head_operate = m_T_heater_head_low + 100.0;
		}
		else
		{
			message( TCS_ERROR, "Receiver type must be set to 1 or 2" );
			return -1;
		}

		double A_ap = CSP::pi*pow( (d_ap/2.0), 2 );
		double A_cav = m_A_absorber + m_A_wall;
		double theta_rad = sun_angle*2.0*CSP::pi/360.0;

		// air properties  (curve fits generated in EES)
		double k_air = 0.00169319 + 0.0000794814*T_amb;
		double beta_air = 0.00949962 - 0.0000297215*T_amb + 3.06353*10E-08*pow(T_amb,2);  
		double mu_air = 0.00000499562 + 4.50917E-08*T_amb;
		double M_air = 28.97;		// [kg/kmol]  molar mass of air
		double R_bar = 8314;		// [J/kmol-K]  gas constant
		double R_air = R_bar / M_air;
		double rho_air = P_atm/(R_air*T_amb);  // ideal gas law
		double nu_air = mu_air / (rho_air+0.0000001);
		//double Pr_air = 0.832636 - 0.000460708*T_amb + 3.67609E-07*pow(T_amb,2);
		
		// Receiver conduction (losses)
		double h_out = 20.0;	//[W/K-m^2]	External housing convective estimate
		// Resistance due to conduction
		double R_cond_ins = m_L_insulation/(m_k_insulation*A_cav+0.0000001);
		// Rresistance due to convection
		double R_conv_housing = 1.0 / (h_out*1.5*A_cav+0.0000001);
		double q_cond_loss= (T_rec_ave-T_amb)/(R_cond_ins+R_conv_housing+0.0000001)/1000.0;
		                
		// Receiver natural convection (losses)...Stine and McDonald (1989) model
		double Lc_3 = m_d_cav;			// characteristic length 
		double S3 = -0.982 * (d_ap / (Lc_3+0.0000001)) + 1.12;
		// Grashof number
		double Gr3 = (CSP::grav * beta_air * (T_rec_ave-T_amb) * pow(Lc_3,3))/(pow(nu_air,2)+0.0000001);
		// Nusselt number
		double Nu3 = 0.088 * pow(Gr3,0.3333) * pow( (T_rec_ave/(T_amb+0.0000001)), 0.18 )* pow( (cos(sun_angle*2.0*CSP::pi/360.0)), 2.47 ) * pow( (d_ap / (Lc_3+0.0000001)), S3 );		// Nusselt number
		double h_cav3 = Nu3*k_air/(Lc_3+0.0000001);		// convection heat transfer coefficient
		
		double q_conv_loss=(h_cav3*A_cav*(T_rec_ave - T_amb)) / 1000.0;   	               
		
		// Receiver cavity forced convection losses due to wind speed
		double h_forced_wind = 0.1967 * pow( wind_speed, 1.849 );
		double q_conv_forced=(h_forced_wind*A_cav*(T_rec_ave-T_amb))/1000.0;   
		
		// total convection losses
		q_conv_loss = q_conv_loss + q_conv_forced;
		
		// Receiver radiation emitted (losses)
		double EPSILON_rad = 1.0;			// slight increase in losses from effective absorptance ok
		double q_rad_emission = EPSILON_rad*A_ap*CSP::sigma*(pow(T_rec_ave,4)-pow(T_amb,4))/1000.0;
		
		// Receiver radiation reflected (losses)
		double alpha_cav_ave = (m_alpha_absorber+m_alpha_wall)/2.0;		// approx ave cavity apsorptance
		
		double transmit_diffuse = 0.85*m_transmittance_cover;				// approximation

		double alpha_eff;
		if( m_transmittance_cover < 1 )
			alpha_eff = m_transmittance_cover*alpha_cav_ave / (alpha_cav_ave + (1-alpha_cav_ave)*transmit_diffuse*(A_ap/(A_cav+0.0000001))+0.0000001);
		else
			alpha_eff = alpha_cav_ave / (alpha_cav_ave + (1-alpha_cav_ave)*(A_ap/(A_cav+0.0000001))+0.0000001);
		
		double q_rad_reflection = (1.0 - alpha_eff) * Power_in;
		
		// Total receiver radiation (losses)
		double q_rad_loss;
		if( m_transmittance_cover < 1 )
			q_rad_loss = q_rad_reflection;
		else
			q_rad_loss = q_rad_emission + q_rad_reflection;

		// Total receiver losses (kW)
		double q_rec_losses_kW;
		if(Power_in >= 0.001 )
		{
				if(m_transmittance_cover < 1.0 )
					q_rec_losses_kW = (q_cond_loss+q_rad_loss);
				else
					q_rec_losses_kW = (q_cond_loss+q_conv_loss+q_rad_loss);
		}
		else
			q_rec_losses_kW = 0;
		
		// ============================================================
		// If receiver cover is used::::
		// ============================================================
		// Estimate of convection from inside of cavity to glass cover
		double Q_reject = std::numeric_limits<double>::quiet_NaN();
		if(m_transmittance_cover < 1.0 )
		{
			double tolerance = 5.0;			// 2 eq's for glass temp must be within tolerance [K]
			double residual = 100.0;		// initialize residual to be greater than tolerance
			//double T_glass = T_rec_ave;		// need to initialize
			double d_T = 1.0;				// increment T_glass by 1 degree
			
			for( double T_glass = T_rec_ave; T_glass <= 1500; T_glass += d_T )
			{
				if( residual >= tolerance )
				{
					// Use film temperature!
					double T_film_in = (T_glass + T_rec_ave)/2;
					double k_air_in=0.00169319 + 0.0000794814*T_film_in;
					double rho_air_in= m_P_cav /(R_air*T_film_in);					// cavity can be pressurized
					double Cp_air_in=1017.7-0.136681*T_film_in+0.000311257*pow(T_film_in,2);
					double mu_air_in = 0.00000499562 + 4.50917E-08*T_film_in;   
					double BETA_in=1.0/T_film_in;
					
					double alpha_in=k_air_in/(rho_air_in*Cp_air_in);				// thermal diffusivity
					double nu_in = mu_air_in/rho_air_in;							// kinematic viscosity
					double Gr_in = CSP::grav*BETA_in*(T_rec_ave-T_glass)*pow(d_ap,3)/pow(nu_in,2);
					double Pr_in = nu_in/alpha_in;
					double Ra_in = Pr_in * Gr_in;
					
					double Nusselt_90 = 0.18*pow( (Pr_in/(0.2+Pr_in)*Ra_in), 0.29 );	// p563 Incropera
					
					double Nusselt_in = 1.0+ (Nusselt_90-1)*sin(1.5708+theta_rad);		// P.564 Incropera
					// Find tot convective heat xfer coef
					double h_glass_in = max(0.000001,Nusselt_in/m_L_cav*k_air_in);

					// Estimate free (natural) convection from outside aperture plate
					// Estimate film temperature!!!
					double T_film_out = (T_amb + T_glass)/2.0;
					double k_air_out = 0.00169319 + 0.0000794814*T_film_out;
					double rho_air_out= P_atm/(R_air*T_film_out);
					double Cp_air_out = 1017.7-0.136681*T_film_out+0.000311257*pow(T_film_out,2);
					double mu_air_out = 0.00000499562 + 4.50917E-08*T_film_out;
					double BETA_out=1.0/T_film_out;
					
					double alpha_out=k_air_out/(rho_air_out*Cp_air_out);			// thermal diffusivity
					double nu_out=mu_air_out/rho_air_out;							// kinematic viscosity
					double Gr_out = CSP::grav*BETA_out*(T_glass-T_amb)*pow(d_ap,3)/pow(nu_out,2);		// Grashof number
					double Pr_out = nu_out/alpha_out;								// Prandtl number
					double Ra_out = Pr_out*Gr_out;									// Rayleigh number
					
					double Nusselt_vertical =0.68+(0.67*pow( (Ra_out*cos(theta_rad)),0.25 )/pow( (1+pow( (0.492/Pr_out),(9/16))), (4/9) ));  // for theta between 0-60 degrees
					double Nusselt_horiz =0.27*pow(Ra_out,0.25);				// for theta between 60-90 degrees
					double Nusselt_out_max = max(Nusselt_vertical,Nusselt_horiz);
					
					double h_glass_out = Nusselt_out_max * k_air_out / d_ap;

					// forced convection from outside plate
					double Re_air_out = rho_air_out * wind_speed*d_ap/mu_air_out;		// Reynolds number
					//double Pe_out = Re_air_out * Pr_out;								// Peclet number
					
					double Nusselt_out_forced;
					if (Re_air_out >= 500000 )
						Nusselt_out_forced=(0.037*pow(Re_air_out,0.8)-871)*pow(Pr_out,(1/3));	// turb flow	
					else
						Nusselt_out_forced=0.664*pow(Re_air_out,0.5)*pow(Pr_out,0.3333);		// laminar flow
					
					double h_out_forced = Nusselt_out_forced * k_air_out / d_ap;
					double h_outside_total=pow( pow(h_glass_out,3)+pow(h_out_forced,3) , (1/3) );	// p924 Klein/Nellis
					
					// ======================================
					double R_rad_out=1.0/(A_ap*CSP::sigma*(pow(T_glass,2)+pow(T_amb,2))*(T_glass+T_amb));
					double R_conv_out = 1.0/( h_outside_total * A_ap);
					double R_rad_in = 1.0 / (A_ap *CSP::sigma*(pow(T_rec_ave,2)+pow(T_glass,2))*(T_rec_ave+T_glass) );
					double R_conv_in = 0.001 + 1.0/(h_glass_in * A_ap+0.0000001);
					
					double R1 = pow( (1/(R_rad_in+0.0000001)+1/(R_conv_in+0.0000001)+0.000001), -1 );

					double R2 = pow( (1/(R_rad_out+0.0000001)+1/(R_conv_out+0.0000001)+0.000001), -1 );
					
					double T_glass_res = R2*(T_rec_ave-T_amb)/(R1+R2) + T_amb;
					Q_reject = (T_glass_res - T_amb)/R2;
					
					// determine error in Q_losses......they should all be equal
					residual = fabs(T_glass_res-T_glass);
				}
				else
					break;
			}
			q_rec_losses_kW = q_rec_losses_kW + (Q_reject/1000.0);		// Convert to kW
		}
		
		if( m_transmittance_cover < 1 )
		{
			q_conv_loss = Q_reject/1000.0;
			q_rad_emission = 0.0;
		}

		if( Power_in < 0.001 )
		{
			q_rec_losses_kW = 0.0;
			q_conv_loss = 0.0;
			q_rad_emission = 0.0;
			q_cond_loss = 0.0;
			q_rad_reflection = 0.0;
		}

		if( q_rec_losses_kW <= Power_in )
			value( O_P_OUT_REC, Power_in - q_rec_losses_kW );
		else
			value( O_P_OUT_REC, 0.0 );

		value( O_Q_REC_LOSSES, q_rec_losses_kW );
		value( O_ETA_REC, value( O_P_OUT_REC )/(Power_in + 0.0000001) );
		value( O_T_HEATER_HEAD_OPERATE, T_heater_head_operate );
		value( O_Q_RAD_REFLECTION, q_rad_reflection );
		value( O_Q_RAD_EMISSION, q_rad_emission );
		value( O_Q_CONV, q_conv_loss );
		value( O_Q_COND, q_cond_loss );

		return 0;
	}

	virtual int converged( double /*time*/ )
	{
		
		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_pf_dish_receiver_type296, "Collector Dish", "Ty Neises", 1, sam_pf_dish_receiver_type296_variables, NULL, 1 )

